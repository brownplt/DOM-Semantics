#lang racket
(require redex/reduction-semantics)
(require racket/date)
(require racket/serialize)
(require "redex-domv2.rkt")
(require "generate-tests.rkt")

(define (lookup loc store)
  (second (findf (lambda (p) (equal? (first p) loc)) store)))
(define (update-store store loc val)
  (if (findf (lambda (p) (equal? (first p) loc)) store)
      (map (lambda (entry) (if (equal? (first entry) loc) (list loc val) entry)) store)
      (append store (list (list loc val)))))
(define (bind-ref binds key)
  (if (equal? binds #f) #f
      (let ((b (findf (lambda (v) (equal? (bind-name v) key)) 
                      (match-bindings (first binds)))))
        (if (bind? b) (bind-exp b) (raise (list "Key not found:" key))))))
(define (later-in-list fst snd list)
  (let* ([fst-tail (and (list? list) (member fst list))]
         [snd-tail (and (list? fst-tail) (member snd (rest fst-tail)))])
    (not (equal? snd-tail #f))))



(define (propPath loc store)
  (if (equal? loc 'null) 
      empty
      (let* ([node (lookup loc store)]
             [parent (last node)])
        (cons loc (propPath parent store)))))
(define (propPath-with-phases loc store)
  (let ([propPath (propPath loc store)])
    (append (map (lambda (l) (cons l 'capture)) (reverse (rest propPath)))
            (list (cons (first propPath) 'target))
            (map (lambda (l) (cons l 'bubble)) (rest propPath)))))

(define (eval-and-annotate state annot)
  (let* ([next-states (apply-reduction-relation/tag-with-names DOM-reduce state)]
         [condense (lambda (cur-rule cur-annot more)
                     (let ([next-state (car more)]
                           [next-reductions (cdr more)])
                       (if (empty? next-reductions)
                           (list (list cur-rule cur-annot more))
                           (map (lambda (next)
                                  (let-values ([(next-rule next-annot next-more) (apply values next)])
                                    (list (string-append cur-rule ", " next-rule) next-annot next-more)))
                                next-reductions))))]
         [annotated-next-states 
          (append-map
           (lambda (next) 
             (let* ([rule (first next)]
                    [next-state (second next)]
                    [annotation (annot state rule next-state)]
                    [more (eval-and-annotate next-state annot)])
               (if (and (equal? (length (cdr more)) 1) (equal? annotation #f) (equal? (second (first (cdr more))) #f))
                   (condense rule annotation more)
                   (list (list rule annotation more)))))
           next-states)])
    (cons state annotated-next-states)))

;(define (test) 
;  (random-seed 100)
;  (gen-test (make-testparams 1 10 2 2 0 0 1)))

;(define test 
;  '(state
;    (seq
;     (addEventListener loc_div "click" #t loc-listener1)
;     (pre-dispatch loc_span () (event "click" #t #t #t () loc-clickDefault)))
;    ((loc_div (node "div" () (loc_p) null))
;     (loc_p (node "p" () (loc_span) loc_div))
;     (loc_span (node "span" () () loc_p))
;     (loc-listener1
;      (seq (debug-print "Adding listener2 to loc_div and capture while in loc_div and capture") (addEventListener loc_div "click" #t loc-listener2)))
;     (loc-listener2 (if-curTarget loc_div (if-phase capture (debug-print "In listener2 on correct target and phase") (debug-print "In listener2 on correct target but wrong phase")) (debug-print "In listener2 on wrong target")))
;     (loc-clickDefault skip))
;    ()))

(define test
  (deserialize '((3) 0 () 2 ("click" "div") () (c state c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener3 to node loc_node6 that is capturing for event type click")) c (c addEventListener c loc_node6 c (? . 0) c #t c loc_listener3)) c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node3 to Starting handler 9")) c (c setEventHandler c loc_node3 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 9")) c (c seq c skip c (c seq c skip c (c seq c skip c (c seq c skip c (c seq c (c debug-print c (u . "Finished handler 9")) c (c return c #f))))))))) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener4 to node loc_node7 that is bubbling for event type click")) c (c addEventListener c loc_node7 c (? . 0) c #f c loc_listener4)) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener3 to node loc_node4 that is capturing for event type click")) c (c addEventListener c loc_node4 c (? . 0) c #t c loc_listener3)) c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node7 to Starting handler 5")) c (c setEventHandler c loc_node7 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 5")) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener1 from node loc_node7 that was capturing for event type click")) c (c removeEventListener c loc_node7 c (? . 0) c #t c loc_listener1)) c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node10 to Starting handler 6")) c (c setEventHandler c loc_node10 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 6")) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener2 to node loc_node3 that is bubbling for event type click")) c (c addEventListener c loc_node3 c (? . 0) c #f c loc_listener2)) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node2 to Starting handler 7")) c (c setEventHandler c loc_node2 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 7")) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener4 to node loc_node5 that is bubbling for event type click")) c (c addEventListener c loc_node5 c (? . 0) c #f c loc_listener4)) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node9 to Starting handler 8")) c (c setEventHandler c loc_node9 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 8")) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener1 from node loc_node2 that was capturing for event type click")) c (c removeEventListener c loc_node2 c (? . 0) c #t c loc_listener1)) c (c seq c skip c (c seq c skip c (c seq c (c debug-print c (u . "Finished handler 8")) c (c return c #t))))))))) c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node2")) c (c removeEventHandler c loc_node2 c (? . 0))) c (c seq c (c debug-print c (u . "Finished handler 7")) c (c return c #t))))))))) c (c seq c skip c (c seq c (c debug-print c (u . "Finished handler 6")) c (c return c #f))))))))) c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node1")) c (c removeEventHandler c loc_node1 c (? . 0))) c (c seq c (c debug-print c (u . "Finished handler 5")) c (c return c #t))))))))) c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node1")) c (c removeEventHandler c loc_node1 c (? . 0))) c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener3 from node loc_node9 that was capturing for event type click")) c (c removeEventListener c loc_node9 c (? . 0) c #t c loc_listener3)) c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node3")) c (c removeEventHandler c loc_node3 c (? . 0))) c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener2 from node loc_node8 that was bubbling for event type click")) c (c removeEventListener c loc_node8 c (? . 0) c #f c loc_listener2)) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener1 to node loc_node4 that is capturing for event type click")) c (c addEventListener c loc_node4 c (? . 0) c #t c loc_listener1)) c (c seq c (c debug-print c (u . "Dispatching a bubbling, non-cancelable click event to loc_node9")) c (c pre-dispatch c loc_node9 c () c (c event c (? . 0) c #t c #f c #t c () c loc_skip))))))))))))) c (c (c loc_node1 c (c node c (? . 1) c () c (c loc_node2 c loc_node3) c null)) c (c loc_node2 c (c node c (? . 1) c () c () c loc_node1)) c (c loc_node3 c (c node c (? . 1) c () c (c loc_node4 c loc_node5 c loc_node6) c loc_node1)) c (c loc_node4 c (c node c (? . 1) c () c () c loc_node3)) c (c loc_node5 c (c node c (? . 1) c () c () c loc_node3)) c (c loc_node6 c (c node c (? . 1) c () c (c loc_node7) c loc_node3)) c (c loc_node7 c (c node c (? . 1) c () c (c loc_node8 c loc_node10) c loc_node6)) c (c loc_node8 c (c node c (? . 1) c () c (c loc_node9) c loc_node7)) c (c loc_node9 c (c node c (? . 1) c () c () c loc_node8)) c (c loc_node10 c (c node c (? . 1) c () c () c loc_node7)) c (c loc_listener1 c (c seq c (c debug-print c (u . "Starting listener 1")) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener4 to node loc_node10 that is capturing for event type click")) c (c addEventListener c loc_node10 c (? . 0) c #t c loc_listener4)) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node10 to Starting handler 1")) c (c setEventHandler c loc_node10 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 1")) c (c seq c skip c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node7 to Starting handler 2")) c (c setEventHandler c loc_node7 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 2")) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener1 to node loc_node9 that is capturing for event type click")) c (c addEventListener c loc_node9 c (? . 0) c #t c loc_listener1)) c (c seq c skip c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node2")) c (c removeEventHandler c loc_node2 c (? . 0))) c (c seq c (c debug-print c (u . "Finished handler 2")) c (c return c #t))))))))) c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node1")) c (c removeEventHandler c loc_node1 c (? . 0))) c (c seq c (c debug-print c (u . "Finished handler 1")) c (c return c #f))))))))) c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node8")) c (c removeEventHandler c loc_node8 c (? . 0))) c (c debug-print c (u . "Finished listener 1")))))))) c (c loc_listener2 c (c seq c (c debug-print c (u . "Starting listener 2")) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener1 from node loc_node4 that was capturing for event type click")) c (c removeEventListener c loc_node4 c (? . 0) c #t c loc_listener1)) c (c seq c skip c (c seq c skip c (c debug-print c (u . "Finished listener 2")))))))) c (c loc_listener3 c (c seq c (c debug-print c (u . "Starting listener 3")) c (c seq c (c seq c (c debug-print c (u . "Adding listener loc_listener1 to node loc_node5 that is capturing for event type click")) c (c addEventListener c loc_node5 c (? . 0) c #t c loc_listener1)) c (c seq c skip c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node9")) c (c removeEventHandler c loc_node9 c (? . 0))) c (c debug-print c (u . "Finished listener 3")))))))) c (c loc_listener4 c (c seq c (c debug-print c (u . "Starting listener 4")) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener1 from node loc_node4 that was bubbling for event type click")) c (c removeEventListener c loc_node4 c (? . 0) c #f c loc_listener1)) c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node7 to Starting handler 3")) c (c setEventHandler c loc_node7 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 3")) c (c seq c skip c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Setting handler for event type click on node loc_node1 to Starting handler 4")) c (c setEventHandler c loc_node1 c (? . 0) c (c seq c (c debug-print c (u . "Starting handler 4")) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Removing listener loc_listener1 from node loc_node3 that was capturing for event type click")) c (c removeEventListener c loc_node3 c (? . 0) c #t c loc_listener1)) c (c seq c skip c (c seq c (c seq c (c debug-print c (u . "Clearing handler for event type click on node loc_node10")) c (c removeEventHandler c loc_node10 c (? . 0))) c (c seq c (c debug-print c (u . "Finished handler 4")) c (c return c #f))))))))) c (c seq c skip c (c seq c (c debug-print c (u . "Finished handler 3")) c (c return c #t))))))))) c (c seq c skip c (c debug-print c (u . "Finished listener 4")))))))) c (c loc_skip c skip)) c ()))))

;(propPath-with-phases 'loc_node8 (third (first (test))))
(define (interesting cur rule next)
  (cond
    [(equal? rule "do-addEventListener")
     (let* ([binds (redex-match DOM
                                (state (in-hole Ctx
                                                (addEventListener
                                                 loc_target string_type bool_useCapture loc_listener))
                                       ((loc_a N_a) ...
                                        (loc_target (node string_name LS (loc_kids ...) parent_node))
                                        (loc_b N_b) ...
                                        (loc_c S_c) ...
                                        (loc_listener S_listener)
                                        (loc_d S_d) ...)
                                       Log) 
                                cur)]
            [aEL-string (string-append "Adding " (symbol->string (bind-ref binds 'loc_listener))
                                       " to " (symbol->string (bind-ref binds 'loc_target)) (if (bind-ref binds 'bool_useCapture) ",capture" ",bubble") ": ")]
            [already-present (redex-match DOM (any_a ... (listener bool loc_listener) any_b ...) (bind-ref binds 'LS))]
            [in-dispatch (redex-match DOM (state (in-hole Ctx 
                                                          (dispatch E parent P PDef SP SI (loc ...) (L ...) L_cur)) 
                                                 N-store 
                                                 Log) 
                                      cur)]
            [later-in-dispatch (if (bind-ref binds 'bool_useCapture)
                                   (later-in-list (bind-ref in-dispatch 'parent) (bind-ref binds 'loc_target) (bind-ref in-dispatch 'loc))
                                   (later-in-list (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent) (bind-ref in-dispatch 'loc))
                                   )])
     (string-append
      aEL-string 
      (cond
        [already-present "This addEventListener will have no effect because the listener is already installed."]
        [(not in-dispatch) "This addEventListener will be visible on the next event dispatch."]
        [later-in-dispatch "This addEventListener will be visible because the receiver is later in dispatch than the current target"]
        [(not later-in-dispatch) "This addEventListener will not be visible until the next dispatch because the receiver has already been visited."]
        [else "I don't know what this addEventListener will do!"])))]
    
    [(equal? rule "do-removeEventListener")
     (let* ([binds (redex-match DOM
                                (state (in-hole Ctx
                                                (removeEventListener
                                                 loc_target string_type bool_useCapture loc_listener))
                                       ((loc_a N_a) ...
                                        (loc_target (node string_name LS (loc_kids ...) parent_node))
                                        (loc_b N_b) ...
                                        (loc_c S_c) ...
                                        (loc_listener S_listener)
                                        (loc_d S_d) ...)
                                       Log) 
                                cur)]
            [rEL-string (string-append "Removing " (symbol->string (bind-ref binds 'loc_listener))
                                       " from " (symbol->string (bind-ref binds 'loc_target)) (if (bind-ref binds 'bool_useCapture) ",capture" ",bubble") ": ")]
            [already-present (redex-match DOM (any_a ... (listener bool loc_listener) any_b ...) (bind-ref binds 'LS))]
            [in-dispatch (redex-match DOM (state (in-hole Ctx 
                                                          (dispatch E parent P PDef SP SI (loc ...) (L ...) L_cur)) 
                                                 N-store 
                                                 Log) 
                                      cur)]
            [later-in-dispatch (if (bind-ref binds 'bool_useCapture)
                                   (later-in-list (bind-ref in-dispatch 'parent) (bind-ref binds 'loc_target) (bind-ref in-dispatch 'loc))
                                   (later-in-list (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent) (bind-ref in-dispatch 'loc))
                                   )])
     (string-append
      rEL-string 
      (cond
        [(not already-present) "This removeEventListener will have no effect because the listener is already absent."]
        [(not in-dispatch) "This removeEventListener will be visible on the next event dispatch."]
        [later-in-dispatch "This removeEventListener will be visible because the receiver is later in dispatch than the current target"]
        [(not later-in-dispatch) "This removeEventListener will not be visible until the next dispatch because the receiver has already been visited."]
        [else "I don't know what this removeEventListener will do!"])))]
    
    [(equal? rule "do-setEventHandler")
     (let* ([binds (redex-match DOM
                                (state (in-hole Ctx (setEventHandler loc_target string_type S_listener))
                                       ((loc_a N_a) ...
                                        (loc_target (node string_name LS (loc_kids ...) parent_node))
                                        (loc_b N_b) ...
                                        (loc_c S_c) ...)
                                       Log) 
                                cur)]
            [sEH-string (string-append "Setting handler for " (bind-ref binds 'string_type)
                                       " on " (symbol->string (bind-ref binds 'loc_target)) ": ")]
            [in-dispatch (redex-match DOM (state (in-hole Ctx 
                                                          (dispatch E parent P PDef SP SI (loc ...) (L ...) L_cur)) 
                                                 N-store 
                                                 Log) 
                                      cur)]
            [gethandler-present (redex-match DOM (L_a ... (gethandler) L_b ...) (bind-ref in-dispatch 'L))]
            [later-in-dispatch (later-in-list (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent) (bind-ref in-dispatch 'loc))])
     (string-append
      sEH-string 
      (cond
        [(not in-dispatch) "This setEventHandler will be visible on the next event dispatch."]
        [later-in-dispatch "This setEventHandler will be visible because the receiver is later in dispatch than the current target"]
        [(and (equal? (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent))
              (not gethandler-present)) "This setEventHandler will not be visible until the next dispatch because the receiver is being visited and no handler listener comes after the current listener."]
        [(and (equal? (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent))
              gethandler-present) "This setEventHandler will be visible because the receiver is being visited and the handler listener comes after the current listener."]
        [(not later-in-dispatch) "This setEventHandler will not be visible until the next dispatch because the receiver has already been visited."]
        [else "I don't know what this setEventHandler will do!"])))]
    
    [(equal? rule "do-clearEventHandler")
     (let* ([binds (redex-match DOM
                                (state (in-hole Ctx (setEventHandler loc_target string_type S_listener))
                                       ((loc_a N_a) ...
                                        (loc_target (node string_name LS (loc_kids ...) parent_node))
                                        (loc_b N_b) ...
                                        (loc_c S_c) ...)
                                       Log) 
                                cur)]
            [cEH-string (string-append "Clearing handler for " (bind-ref binds 'string_type)
                                       " on " (symbol->string (bind-ref binds 'loc_target)) ": ")]
            [in-dispatch (redex-match DOM (state (in-hole Ctx 
                                                          (dispatch E parent P PDef SP SI (loc ...) (L ...) L_cur)) 
                                                 N-store 
                                                 Log) 
                                      cur)]
            [gethandler-present (redex-match DOM (L_a ... (gethandler) L_b ...) (bind-ref in-dispatch 'L))]
            [later-in-dispatch (later-in-list (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent) (bind-ref in-dispatch 'loc))])
     (string-append
      cEH-string 
      (cond
        [(not in-dispatch) "This clearEventHandler will be visible on the next event dispatch."]
        [later-in-dispatch "This clearEventHandler will be visible because the receiver is later in dispatch than the current target"]
        [(and (equal? (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent))
              (not gethandler-present)) "This clearEventHandler will not be visible until the next dispatch because the receiver is being visited and no handler listener comes after the current listener."]
        [(and (equal? (bind-ref binds 'loc_target) (bind-ref in-dispatch 'parent))
              gethandler-present) "This clearEventHandler will be visible because the receiver is being visited and the handler listener comes after the current listener."]
        [(not later-in-dispatch) "This clearEventHandler will not be visible until the next dispatch because the receiver has already been visited."]
        [else "I don't know what this clearEventHandler will do!"])))]
    
    [else #f]
    ))
     
;    [(equal? rule "seq-skip") #f]
;    [(equal? rule "debug-print") #f]
;    [else 
;     (string-append "Because of rule " rule)]))

(define answer
  (eval-and-annotate test interesting))
