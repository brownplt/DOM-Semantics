#lang racket
(require redex)
(require "redex-domv2.rkt")
(require (planet "html-writing.rkt" ("neil" "html-writing.plt" 1 0)))
(require (planet "html-parsing.rkt" ("neil" "html-parsing.plt" 1 2)))
(provide model->xexp xexp->html)

(define (bind-ref binds key)
  (let ((b (findf (lambda (v) (equal? (bind-name v) key))
                  (match-bindings (first binds)))))
    (if (bind? b) (bind-exp b) (raise (list "Key not found:" key)))))

(define (bind-refs binds keys)
  (if (pair? binds)
      (map (lambda (binds)
             (map (lambda (key)
                    (let ((b (findf (lambda (v) (equal? (bind-name v) key))
                                    (match-bindings binds))))
                      (if (bind? b) (bind-exp b) empty)))
                  keys))
           binds)
      empty))

(define node1 (term (node "n1" () (loc_node2) null)))
(define node2 (term (node "n2" () () loc_node1)))
(define node3 (term (node "n3" () (loc_node4 loc_node5) null)))
(define node4 (term (node "n4" () () loc_node3)))
(define node5 (term (node "n5" () () loc_node3)))
(define node6 (term (node "n6" () () null)))
(define store
  (term ((loc_node1 ,node1)
         (loc_node2 ,node2)
         (loc_node3 ,node3)
         (loc_node4 ,node4)
         (loc_node5 ,node5)
         (loc_node6 ,node6))))


(define store2
  (term 
   ((loc_child
    (node
     "child"
     ((("click" target) ((listener #t loc-l1) (listener #f loc-l2)))
      (("click" capture) ((listener #t loc-l1) (listener #f loc-l2)))
      (("click" bubble) ()))
     ()
     loc_mid))
   (loc_mid (node "middle" 
                  ((("keydown" target) ((handler (debug-print "H3")) (listener #t loc-skip)))
                   (("keyup" target) ((handler (debug-print "H4")))))
                  (loc_child) loc_parent))
   (loc_parent (node "parent" 
                     ()
                     (loc_mid) null))
   (loc-l1 (seq (debug-print "L1") skip))
   (loc-l2 (debug-print "L2"))
   (loc-skip skip))))
   


(define (indent s)
  (regexp-replace* #rx"(?m:^)" s "  "))
(define (bool->string b)
  (if b "true" "false"))

;(define-metafunction DOM
;  [(map-closure (seq S_1 S_2) any_lsmap)
;   (map-closure S_2 (map-closure S_1 any_lsmap))]
;  [(map-closure (if-phase P S_t S_f) any_lsmap)
;   (map-closure S_f (map-closure S_t any_lsmap))]
;  [(map-closure (if-curTarget loc S_t S_f) any_lsmap)
;   (map-closure S_f (map-closure S_t any_lsmap))]
;  [(map-closure (addEventListener loc_target T bool loc_listener)
;                (any ...
;                 (loc_listener S_listener)
;                 any ...))
;   (map-closure S_listener ,(hash-set (term any_lsmap) (term loc_listener) (gensym "listener")))]
;  [(map-closure (removeEventListener loc_target T bool loc_listener)
;                (any ...
;                 (loc_listener S_listener)
;                 any ...))
;   (map-closure S_listener ,(hash-set (term any_lsmap) (term loc_listener) (gensym "listener")))]    
;  [(map-closure (setEventHandler loc T S_listener) any_lsmap)
;   (map-closure S_listener any_lsmap)]
;  [(map-closure any any_lsmap)
;   any_lsmap])

(define-metafunction DOM
  [(s->js (seq S_1 S_2))
   ,(string-append (term (s->js S_1)) "\n" (term (s->js S_2)))]
  [(s->js (return bool))
   ,(string-append "return " (bool->string (term bool)) ";")]
  [(s->js skip)
   "/* skip */"]
  [(s->js stop-prop)
   "event.stopPropagation();"]
  [(s->js stop-immediate)
   "event.stopImmediatePropagation();"]
  [(s->js prevent-default)
   "event.preventDefault();"]
  [(s->js mutate)
   "debug.print(\"mutation\");"]
  [(s->js (debug-print string))
   ,(string-append "debug.print(\"" (term string) "\");")]
  [(s->js (addEventListener loc_target T bool loc_listener))
   ,(string-append 
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").addEventListener(\""
     (term T)
     "\", "
     (bool->string (term bool))
     ", "
     (symbol->string (term loc_listener))
     ");")]
  [(s->js (removeEventListener loc_target T bool loc_listener))
   ,(string-append 
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").removeEventListener(\""
     (term T)
     "\", "
     (bool->string (term bool))
     ", "
     (symbol->string (term loc_listener))
     ");")]  
  [(s->js (setEventHandler loc_target T S))
   ,(string-append 
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").setAttribute(\"on"
     (term T)
     "\", "
     (term (s->js S))
     ");")]
  [(s->js (if-phase P S_t S_f))
   ,(string-append
     "if (event.eventPhase === "
     (case (term P)
       [(term capture) "Event.CAPTURING_PHASE"]
       [(term target) "Event.AT_TARGET"]
       [(term bubble) "Event.BUBBLING_PHASE"])
     ") {\n"
     (indent (term (s->js S_t)))
     "\n} else {\n"
     (indent (term (s->js S_f)))
     "\n}")]
  [(s->js (if-curTarget loc S_t S_f))
   ,(string-append
     "if (event.curTarget === document.getElementById(\""
     (symbol->string (term loc))
     "\") {\n"
     (indent (term (s->js S_t)))
     "\n} else {\n"
     (indent (term (s->js S_f)))
     "\n}")]
  [(s->js (pre-dispatch loc () (event T Bubbles Cancels Trusted Meta loc_default)))
   ,(string-append
     "var evt = document.createEvent(\"" (term T) "\");\n"
     "evt.initEvent(\"" (term T) "\", "
     (bool->string (term Bubbles))
     ", "
     (bool->string (term Cancels))
     ");\n"
     "document.getElementById(\"" (symbol->string (term loc)) "\").dispatchEvent(evt);\n")]
  [(s->js any)
   ,(let* ((string (open-output-string))
           (stmt-as-string (begin (displayln (term any) string) (get-output-string string))))
      (string-append
       "/* unknown internal statement */\n"
       "/*\n  "
       stmt-as-string
       "*/"))]
  )

(define (handlers-if-any ls)
  (let* ([target-phases (or
                         (redex-match DOM ((TP_a (HL_a ...)) ...
                                           ((T target) ((handler S) (listener bool_l loc_l) ...))
                                           (TP_b (HL_b ...)) ...) ls)
                         empty)]
         [handlers (bind-refs target-phases (list 'T 'S))]
         [handlerAttrs (map (lambda (h) (list (string->symbol (string-append "on" (first h)))
                                              (term (s->js ,(second h))))) handlers)])
    handlerAttrs)
  )

(define (model->xexp model)
  (let* ([program (second model)]
         [store (third model)]
         [top-node-matches (redex-match DOM 
                                        ((loc_a N_a) ...
                                         (loc_top (name node_top (node string LS (loc ...) null)))
                                         (loc_b N_b) ...
                                         (loc_c S_c) ...)
                                        store)]
         [all-locs (bind-ref (redex-match DOM 
                                           ((loc_a N_a) ...
                                            (loc_c S_c)...)
                                           store) 'loc_a)]
         [top-locs (map (lambda (m) (bind-exp
                                     (findf (lambda (v) (equal? (bind-name v) 'loc_top))
                                            (match-bindings m)))) top-node-matches)]
         [lookup-node (lambda (loc)
                        (second (findf (lambda (p) (equal? (first p) loc)) store)))]
         [lookup-listener (lambda (loc) ;same as lookup-node's implementation
                            (second (findf (lambda (p) (equal? (first p) loc)) store)))]
         [node-to-xexp 
          (letrec ((node-to-xexp
                    (lambda (loc)
                         (let* ((node (lookup-node loc))
                                (binds (redex-match DOM (node string_name LS (loc_kid ...) parent) node)))
                           (append
                            (list (string->symbol (bind-ref binds 'string_name)))
                            (list 
                             (append
                              (list '@)
                              (list (list 'id (symbol->string loc)))
                              (handlers-if-any (bind-ref binds 'LS))))
                            (map node-to-xexp (bind-ref binds 'loc_kid)))
                           ))))
            node-to-xexp)]
         [listeners (bind-ref (redex-match DOM 
                                           ((loc_a N_a) ...
                                            (name ls (loc_c S_c)) ...)
                                           store) 'ls)]
         [define-listeners
           (map
              (lambda (l)
                (let ([loc (first l)]
                      [body (second l)])
                  (string-append "\nfunction " 
                                 (symbol->string loc)
                                 "(event) {\n"
                                 (indent (term (s->js ,body)))
                                 "\n}\n")))
              listeners
             )]
         [install-listeners 
          (lambda (loc)
            (let* ([node (lookup-node loc)]
                   [binds (redex-match DOM (node string_name LS (loc_kid ...) parent) node)]
                   [all-targets (or
                                 (redex-match DOM ((TP_a (HL_a ...)) ...
                                                   ((T target) ((handler S_h) HL ...))
                                                   (TP_b (HL_b ...)) ...) (bind-ref binds 'LS))
                                 (redex-match DOM ((TP_a (HL_a ...)) ...
                                                   ((T target) (HL ...))
                                                   (TP_b (HL_b ...)) ...) (bind-ref binds 'LS))
                                 empty)]
                   [target-lists (bind-refs all-targets (list 'T 'HL))]
                   [targets 
                    (append-map (lambda (target)
                                  (let ([type (first target)]
                                        [listeners (second target)])
                                    (map (lambda (t)
                                           (append-map 
                                            (lambda (parts) 
                                              (let ((ret (list 
                                                          type
                                                          (first parts)
                                                          (lookup-listener (second parts))
                                                          (symbol->string (second parts))
                                                          )))
                                                ret))
                                            (bind-refs (redex-match DOM (listener bool loc) t)
                                                       (list 'bool 'loc))))
                                         listeners)))
                                target-lists)]
                   [listener-to-js 
                    (lambda (type useCapture body fnName)
                      (let ((js (string-append "document.getElementById(\""
                                               (symbol->string loc)
                                               "\").addEventListener(\""
                                               type
                                               "\", "
                                               (bool->string (term ,useCapture))
                                               ", "
                                               fnName
                                               ");\n")))
                        js))]
                     )
              (apply string-append (map (lambda (t) (if (null? t) "" (apply listener-to-js t))) targets))))])
    (append
     (list (string->symbol "html"))
     (map node-to-xexp top-locs)
     (list (list 'script (apply string-append define-listeners)))
     (list (list 'script (string-append "\n" (apply string-append (map install-listeners all-locs)))))
     (list (list 'script (string-append "\n" (term (s->js ,program))))))))

(define seq (lambda S
  (foldr (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
           #f
           S)))

(define listener1 
  (seq
   (term skip)
   (term (return #t))
   (term stop-prop)
   (term stop-immediate)
   (term (debug-print "hello!"))
   (term (addEventListener loc_kid "click" #t skip))
   (term (if-phase target skip (debug-print "here")))))

; (displayln (term (s->js ,listener1 ,(term ,(make-immutable-hasheq empty)))))
