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
     ((("click" target) ((listener #t (seq (debug-print "L1") skip)) (listener #f (debug-print "L2"))))
      (("click" capture) ((listener #t (debug-print "L1")) (listener #f (debug-print "L2"))))
      (("click" bubble) ()))
     ()
     loc_mid))
   (loc_mid (node "middle" 
                  ((("keydown" target) ((handler (debug-print "H3")) (listener #t skip)))
                   (("keyup" target) ((handler (debug-print "H4")))))
                  (loc_child) loc_parent))
   (loc_parent (node "parent" 
                     ()
                     (loc_mid) null)))))
   


(define (indent s)
  (regexp-replace* #rx"(?m:^)" s "  "))
(define (bool->string b)
  (if b "true" "false"))

(define (map-closure S ls-map)
  (case (if (pair? S) (first S) S)
    ['seq (map-closure (third S) (map-closure (second S) ls-map))]
    ['if-phase (map-closure (fourth S) (map-closure (third S) ls-map))]
    ['if-curTarget (map-closure (fourth S) (map-closure (third S) ls-map))]
    ['addEventListener 
     (begin
       (displayln (hash-count ls-map))
     (map-closure (fifth S) (hash-set ls-map (fifth S) (gensym "listener"))))]
    ['removeEventListener 
     (map-closure (fifth S (hash-set ls-map (fifth S) (gensym "listener"))))]
    ['setEventHandler ls-map]
    ['listener
     (map-closure (third S) ls-map)] ; assuming this listener has already been added
    ['handler
     ls-map] ; handlers aren't functions, just strings...
    [else
     ls-map]))
 
(define (s->js S ls-map)
  (case (if (pair? S) (first S) S)
    ['seq
     (string-append (s->js (second S) ls-map) "\n" (s->js (third S) ls-map))]
    ['return
     (string-append "return " (bool->string (second S)) ";")]
    ['skip
     "/* skip */"]
    ['stop-prop
     "event.stopPropagation();"]
    ['stop-immediate
     "event.stopImmediatePropagation();"]
    ['prevent-default
     "event.preventDefault();"]
    ['mutate
     "debug.print(\"mutation\");"]
    ['debug-print
     (string-append "debug.print(\"" (second S) "\");")]
    ['addEventListener
     (string-append 
      "document.getElementById(\""
      (symbol->string (second S))
      "\").addEventListener(\""
      (third S)
      "\", "
      (bool->string (fourth S))
      ", "
      (symbol->string (hash-ref ls-map (fifth S) (gensym "unknownAddListener")))
      ");")]
    ['removeEventListener
     (string-append 
      "document.getElementById(\""
      (symbol->string (second S))
      "\").removeEventListener(\""
      (third S)
      "\", "
      (bool->string (fourth S))
      ", "
      (symbol->string (hash-ref ls-map (fifth S) (gensym "unknownRemListener")))
      ");")]  
    ['setEventHandler
     (string-append 
      "document.getElementById(\""
      (symbol->string (second S))
      "\").setAttribute(\"on"
      (third S)
      "\", "
      (s->js (fourth S) ls-map)
      ");")]
    ['if-phase
     (string-append
      "if (event.eventPhase === "
      (case (second S)
        [(term capture) "Event.CAPTURING_PHASE"]
        [(term target) "Event.AT_TARGET"]
        [(term bubble) "Event.BUBBLING_PHASE"])
      ") {\n"
      (indent (s->js (third S) ls-map))
      "\n} else {\n"
      (indent (s->js (fourth S) ls-map))
      "\n}")]
    ['if-curTarget
     (string-append
      "if (event.curTarget === document.getElementById(\""
      (symbol->string (second S))
      "\") {\n"
      (indent (s->js (third S) ls-map))
      "\n} else {\n"
      (indent (s->js (fourth S) ls-map))
      "\n}")]
    ['pre-dispatch
     (let ((E (fourth S)))
       (string-append
        "var evt = document.createEvent(\"" (second E) "\");\n"
        "evt.initEvent(\"" (second E) "\", "
        (bool->string (third E))
        ", "
        (bool->string (fourth E))
        ");\n"
        "document.getElementById(\"" (symbol->string (second S)) "\").dispatchEvent(evt);\n"))]
    [else
     (let* ((string (open-output-string))
            (stmt-as-string (begin (displayln S string) (get-output-string string))))
       (string-append
        "/* unknown internal statement */\n"
        "/*\n  "
        stmt-as-string
        "*/"))]
  ))
   
(define (handlers-if-any ls ls-map)
  (let* ([target-phases (or
                         (redex-match DOM ((TP_a (L_a ...)) ...
                                           ((T target) ((handler S) (listener S_l) ...))
                                           (TP_b (L_b ...)) ...) ls)
                         empty)]
         [handlers (bind-refs target-phases (list 'T 'S))]
         [handlerAttrs (map (lambda (h) (list (string->symbol (string-append "on" (first h)))
                                              (s->js (second h) ls-map))) handlers)])
    handlerAttrs)
  )

(define (model->xexp model)
  (let* ([store (third model)]
         [program (second model)]
         [all-ls (bind-ref (redex-match DOM ((loc (node string LS (loc_kid ...) parent)) ...) store) 'LS)]
         [flat-all-ls (if (pair? all-ls) (apply append all-ls) empty)]
         [binds (bind-refs (redex-match DOM ((TP_a (L_a ...)) ...
                                             ((T target) (L ...))
                                             (TP_b (L_b ...)) ...) flat-all-ls) (list 'L))]
         [all-targets (if (pair? binds) (apply append (apply append binds)) empty)]
         [ls-map (make-immutable-hasheq (map (lambda (ls) 
                                               (if (equal? (first ls) 'listener)
                                                   (cons ls (gensym "listener"))
                                                   (cons ls (gensym "handler")))) all-targets))]
         [ls-map (foldl (lambda (key acc) (map-closure key acc))
                        (map-closure program ls-map) (hash-keys ls-map))]
         [top-node-matches (redex-match DOM 
                                        ((loc_a N_a) ...
                                         (loc_top (name node_top (node string LS (loc ...) null)))
                                         (loc_b N_b) ...)
                                        store)]
         [all-locs (map first store)]
         [top-locs (map (lambda (m) (bind-exp
                                     (findf (lambda (v) (equal? (bind-name v) 'loc_top))
                                            (match-bindings m)))) top-node-matches)]
         [lookup-node (lambda (loc)
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
                              (handlers-if-any (bind-ref binds 'LS) ls-map)))
                            (map node-to-xexp (bind-ref binds 'loc_kid)))
                           ))))
            node-to-xexp)]
         [define-listeners
           (lambda (ls-map)
             (hash-map
              ls-map
              (lambda (listener key)
                (let ((body (if (pair? listener)
                                (case (first listener) 
                                  ['listener (third listener)]
                                  ['handler (second listener)]
                                  [else listener])
                                listener)))
                  (string-append "\nfunction " 
                                 (symbol->string key)
                                 "(event) {\n"
                                 (indent (s->js body ls-map))
                                 "\n}\n")))
             ))]
         [install-listeners 
          (lambda (loc)
            (let* ((node (lookup-node loc))
                     (binds (redex-match DOM (node string_name LS (loc_kid ...) parent) node))
                     (all-targets (or
                                   (redex-match DOM ((TP_a (L_a ...)) ...
                                                     ((T target) ((handler S_h) L ...))
                                                     (TP_b (L_b ...)) ...) (bind-ref binds 'LS))
                                   (redex-match DOM ((TP_a (L_a ...)) ...
                                                     ((T target) (L ...))
                                                     (TP_b (L_b ...)) ...) (bind-ref binds 'LS))
                                   empty))
                     (target-lists (bind-refs all-targets (list 'T 'L)))
                     (targets 
                      (append-map (lambda (target)
                                    (map (lambda (t)
                                           (append-map 
                                            (lambda (parts) 
                                              (let ((ret (list 
                                                          (first target)
                                                          (first parts)
                                                          (second parts)
                                                          t)))
                                                ret))
                                            (bind-refs (redex-match DOM (listener bool S) t)
                                                       (list 'bool 'S))))
                                         (second target)))
                                  target-lists))
                     (listener-to-js 
                      (lambda (type useCapture body listener)
                        (let ((js (string-append "document.getElementById(\""
                                                  (symbol->string loc)
                                                  "\").addEventListener(\""
                                                  type
                                                  "\", "
                                                  (bool->string (term ,useCapture))
                                                  ", "
                                                  (symbol->string (hash-ref ls-map listener 
                                                                            (gensym "unknownListener")))
                                                  ");\n")))
                          js)))
                     )
              (apply string-append (map (lambda (t) (if (null? t) "" (apply listener-to-js t))) targets))))])
    (append
     (list (string->symbol "html"))
     (map node-to-xexp top-locs)
     (list (list 'script (apply string-append (define-listeners ls-map))))
     (list (list 'script (string-append "\n" (apply string-append (map install-listeners all-locs)))))
     (list (list 'script (string-append "\n" (s->js program ls-map)))))))

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
