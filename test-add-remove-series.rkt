#lang racket
(require redex)
(require "redex-domv2.rkt")
(require "dom-to-html.rkt")

(provide state-add-listener2-maker)
(provide state-remove-listener2-maker)
;The naming scheme here represents 
;"current node-current phase-node to add to-phase to do the add"
;so div-cap-p-bub means during the capture phase on div, add the listenr
;to the bubble-phase listeners of p

(define (single-step? e)
  (= (length (apply-reduction-relation DOM-reduce e)) 1))
(define (single-step-pat? e f)
  (equal? (apply-reduction-relation DOM-reduce e) (list (f e))))
(define (bind-ref binds key)
  (let ((b (findf (lambda (v) (equal? (bind-name v) key)) 
                  (match-bindings (first binds)))))
    (if (bind? b) (bind-exp b) (raise (list "Key not found:" key)))))


(define (make-seq stmts)
  (foldr (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
         #f
         stmts))

(define-syntax test
  (syntax-rules ()
    [(test pattern exp name)
     (let ((result-str
            (if (redex-match DOM pattern exp) "passed" "FAILED!!!!")))
       (displayln (string-append name ": " result-str)))]))

(define-syntax test-schema
  (syntax-rules ()
    [(test-schema pattern f name)
     
     (let* ((proc (lambda (e)
                    (let ((binds (redex-match DOM pattern e)))
                      (if (equal? binds #f)
                          #f
                          (f binds)))))
            (result (redex-check DOM pattern 
                                 (single-step-pat? 
                                  (term pattern)
                                  proc)
                                 #:print? #f)))
       (if (equal? result #t) 
           (displayln (string-append name ": passed"))
           (begin
             (displayln (string-append name ": FAILED"))
             (displayln "Counterexample:")
             (displayln result)
             (displayln "Counterexample steps to:")
             (displayln (apply-reduction-relation DOM-reduce 
                                                  (counterexample-term result)))
             (displayln "Expected:")
             (displayln (proc (counterexample-term result))))))]))


(define n-div
  (term (node "div"
              ,empty
              (loc_p)
              null)))
;(test N n-div "n-div")

(define n-p
  (term (node "p"
              ,empty
              (loc_span)
              loc_div)))
;(test N n-p "n-p")

(define n-span
  (term (node "span"
              ,empty
              ,empty
              loc_p)))
;(test N n-span "n-span")

(define el-storo
  (term ((loc_div ,n-div)
         (loc_p ,n-p)
         (loc_span ,n-span))))
;(test N-store el-storo "el-storo")


(define (add-listener-to-everything store type useCapture listener)
  (let* ((allLocs (map first store))
         (allAddListeners 
          (map (lambda (loc) 
                 (term (addEventListener ,loc ,type ,useCapture ,listener))) allLocs))
         (addListenerStmt
          (make-seq allAddListeners)))
    addListenerStmt))

(define (in-log msg state)
  (not (not (findf (lambda (m) (equal? msg m))
                   (last state)))))



(define (state-add-listener2-maker trigger-elem trigger-phase check-elem check-phase)
  (let* ([listener2
          (term
           (if-curTarget ,check-elem
                         (if-phase ,check-phase
                                   (debug-print "In listener2 on correct target and phase")
                                   (debug-print "In listener2 on correct target but wrong phase"))
                         (debug-print "In listener2 on wrong target")))]
         [listener1 
          (term
           (seq
            (if-curTarget ,trigger-elem
                          (if-phase ,trigger-phase
                                    (seq
                                     (debug-print ,(string-append
                                                    "Adding listener2 to "
                                                    (symbol->string check-elem)
                                                    " and "
                                                    (symbol->string check-phase)
                                                    " while in "
                                                    (symbol->string trigger-elem)
                                                    " and "
                                                    (symbol->string trigger-phase)))
                                     (addEventListener 
                                      ,check-elem
                                      "click" 
                                      ,(equal? check-phase (term capture))
                                      loc-listener2))
                                    skip)
                          skip)
            prevent-default))])
    (term 
     (state 
      (seq ,(add-listener-to-everything el-storo "click" #t (term loc-listener1))
           (seq ,(add-listener-to-everything el-storo "click" #f (term loc-listener1))
                (pre-dispatch loc_span ,empty 
                              (event "click" #t #t #t ,empty loc-clickDefault))
                ))
      ,(append el-storo
               (list 
                (term (loc-listener1 ,listener1))
                (term (loc-listener2 ,listener2))
                (term (loc-clickDefault skip))))
      ,empty))))

(define (oracle-add-listener trigger-elem trigger-phase check-elem check-phase)
  (let* ((dispatch-indices (list
                            (list (term loc_div) (term capture))
                            (list (term loc_p) (term capture))
                            (list (term loc_span) (term target))
                            (list (term loc_p) (term bubble))
                            (list (term loc_div) (term bubble))))
         (later-phases (member (list trigger-elem trigger-phase)
                               dispatch-indices)))
    (if (and (list? later-phases) (not (empty? later-phases)))
        (not (not (member (list check-elem check-phase) (rest later-phases))))
        #f)))

(let ([nodes empty];(list (term loc_div) (term loc_p) (term loc_span))]
      [phases empty]);(list (term capture) (term target) (term bubble))])
  (lambda (result) (equal? (first result) "failed: "))
  (map (lambda (test)
         (displayln (third test))
         (let ((output (apply-reduction-relation* DOM-reduce (second test))))
           (if (and
                (not (empty? (last (first output))))
                (equal? (first test)
                        (in-log "In listener2 on correct target and phase"
                                (first output))))
               (list "passed: " (third test))
               (list "failed: " (third test) 
                     (first test)
                     (in-log "In listener2 on correct target and phase"
                             (first output))
                     (last (first output))))))
       (append-map 
        (lambda (trigger-elem) 
          (append-map (lambda (trigger-phase) 
                        (append-map (lambda (check-elem) 
                                      (map (lambda (check-phase)
                                             (list
                                              (oracle-add-listener trigger-elem trigger-phase check-elem check-phase)
                                              (state-add-listener2-maker trigger-elem trigger-phase check-elem check-phase)
                                              (list trigger-elem trigger-phase check-elem check-phase))
                                             )
                                           phases))
                                    nodes)) 
                      phases))
        nodes)))





(define (state-remove-listener2-maker trigger-elem trigger-phase check-elem check-phase)
  (let* ([listener2
          (term
           (if-curTarget ,check-elem
                         (if-phase ,check-phase
                                   (debug-print "In listener2 on correct target and phase")
                                   (debug-print "In listener2 on correct target but wrong phase"))
                         (debug-print "In listener2 on wrong target")))]
         [listener1 
          (term
           (seq
            (if-curTarget ,trigger-elem
                          (if-phase ,trigger-phase
                                    (seq
                                     (debug-print ,(string-append
                                                    "Removing listener2 from "
                                                    (symbol->string check-elem)
                                                    " and "
                                                    (symbol->string check-phase)
                                                    " while in "
                                                    (symbol->string trigger-elem)
                                                    " and "
                                                    (symbol->string trigger-phase)))
                                     (removeEventListener 
                                      ,check-elem
                                      "click" 
                                      ,(equal? check-phase (term capture))
                                      loc-listener2))
                                    skip)
                          skip)
            prevent-default))])
    (term 
     (state 
      (seq ,(add-listener-to-everything el-storo "click" #t (term loc-listener1))
           (seq ,(add-listener-to-everything el-storo "click" #f (term loc-listener1))
                (seq ,(add-listener-to-everything el-storo "click" #t (term loc-listener2))
                     (seq ,(add-listener-to-everything el-storo "click" #f (term loc-listener2))
                          (pre-dispatch loc_span ,empty 
                                        (event "click" #t #t #t ,empty loc-clickDefault))
                          ))))
      ,(append el-storo
               (list 
                (term (loc-listener1 ,listener1))
                (term (loc-listener2 ,listener2))
                (term (loc-clickDefault skip))))
      ,empty))))

(define (oracle-remove-listener trigger-elem trigger-phase check-elem check-phase)
  (let* ((dispatch-indices (list
                            (list (term loc_div) (term bubble))
                            (list (term loc_p) (term bubble))
                            (list (term loc_span) (term target))
                            (list (term loc_p) (term capture))
                            (list (term loc_div) (term capture))))
         (earlier-phases (member (list trigger-elem trigger-phase)
                                 dispatch-indices)))
    (or (equal? (list check-elem check-phase)
                (list (term loc_span) (term target)))
        (not (not (member (list check-elem check-phase) (if (list? earlier-phases)
                                                            earlier-phases
                                                            dispatch-indices)))))))

(let ([nodes empty];(list (term loc_div) (term loc_p) (term loc_span))]
      [phases empty]);(list (term capture) (term target) (term bubble))])
  (lambda (result) (equal? (first result) "failed: "))
  (map (lambda (test)
         (displayln (third test))
         (let ((output (apply-reduction-relation* DOM-reduce (second test))))
           (if (and
                (not (empty? (last (first output))))
                (equal? (first test)
                        (in-log "In listener2 on correct target and phase"
                                (first output))))
               (list "passed: " (third test))
               (list "failed: " (third test) 
                     (first test)
                     (in-log "In listener2 on correct target and phase"
                             (first output))
                     (last (first output))))))
       (append-map 
        (lambda (trigger-elem) 
          (append-map (lambda (trigger-phase) 
                        (append-map (lambda (check-elem) 
                                      (map (lambda (check-phase)
                                             (list
                                              (oracle-remove-listener trigger-elem trigger-phase check-elem check-phase)
                                              (state-remove-listener2-maker trigger-elem trigger-phase check-elem check-phase)
                                              (list trigger-elem trigger-phase check-elem check-phase))
                                             )
                                           phases))
                                    nodes)) 
                      phases))
        nodes)))


;(let* ([nodes (list (term loc_div) (term loc_p) (term loc_span))]
;       [phases (list (term capture) (term target) (term bubble))]
;       [all-files
;        (append-map
;         (lambda (trigger-elem) 
;           (append-map
;            (lambda (trigger-phase) 
;              (append-map
;               (lambda (check-elem) 
;                 (append-map
;                  (lambda (check-phase)
;                    (let* ([setup (term (,trigger-elem ,trigger-phase ,check-elem ,check-phase))]
;                           [state-add (apply state-add-listener2-maker setup)]
;                           [oracle-add (apply oracle-add-listener setup)]
;                           [state-remove (apply state-remove-listener2-maker setup)]
;                           [oracle-remove (apply oracle-remove-listener setup)]
;                           [test-name-add (string-append
;                                           "gentests/test_addListener_"
;                                           (string-join
;                                            (map symbol->string setup)
;                                            "_")
;                                           ".html"
;                                           )]
;                           [test-name-remove (string-append
;                                              "gentests/test_removeListener_"
;                                              (string-join
;                                               (map symbol->string setup)
;                                               "_")
;                                              ".html"
;                                              )])
;                      (map
;                       (lambda (test)
;                         (let* ([test-name (first test)]
;                                [oracle (second test)]
;                                [state (third test)]
;                                [check-done (string-append
;                                             "debug.checkOutput("
;                                             (if oracle "true" "false")
;                                             ", \"In listener2 on correct target and phase\");")])
;                           (if (file-exists? test-name) (delete-file test-name) empty)
;                           (with-output-to-file test-name
;                             (lambda ()
;                               (displayln (model->xexp state check-done))))
;                           test-name))
;                       (list (list test-name-add oracle-add state-add)
;                             (list test-name-remove oracle-remove state-remove)))
;                      ))
;                  phases))
;               nodes))
;            phases))
;         nodes)]
;       [run-tests "runtests.html"])
;  (if (file-exists? run-tests) (delete-file run-tests) empty)
;  (with-output-to-file run-tests
;    (lambda ()
;      (displayln "<!DOCTYPE HTML>")
;      (displayln (xexp->html
;                  (list 'html
;                        (cons 'body
;                              (map (lambda (test-name)
;                                     (list 'p 
;                                           (list 'a 
;                                                 (list '@
;                                                       (list 'href test-name))
;                                                 test-name)
;                                           (list 'iframe (list '@ 
;                                                               (list 'src test-name)
;                                                               (list 'width "100px")
;                                                               (list 'height "25px")
;                                                               (list 'frameborder "0"))))) all-files))))))))


;(let* ([setup (term (loc_div capture loc_span target))]
;       [state (apply state-add-listener2-maker setup)]
;       [done-state (first (apply-reduction-relation* DOM-reduce state))]
;       [oracle (apply oracle-add-listener setup)]
;       [check-done (string-append
;                    "debug.checkOutput("
;                    (if oracle "true" "false")
;                    ", \"In listener2 on correct target and phase\");")])
;  (displayln (model->xexp state check-done)))

(define test-handler-order-state-2
  (let ([l1 (term (debug-print "ONE"))]
        [l2 (term (debug-print "TWO"))]
        [l3 (term (debug-print "THREE"))]
        [l4 (term (debug-print "FOUR"))])
    (term 
     (state ,(make-seq
              (term (
               (addEventListener loc_child "click" #f loc-l1)
               (setEventHandler loc_child "click" (debug-print "NOT CALLED"))
               (addEventListener loc_child "click" #f loc-l3)
               (removeEventHandler loc_child "click")
               (addEventListener loc_child "click" #f loc-l4)
               (setEventHandler loc_child "click" ,l2)
               (pre-dispatch loc_child ,empty (event "click" #f #f #t ,empty loc-skip)))
               ))
            ((loc_child (node "child" ,empty ,empty loc_parent))
             (loc_parent (node "parent" ,empty (loc_child) null))
             (loc-l1 ,l1)
             (loc-l3 ,l3)
             (loc-l4 ,l4)
             (loc-skip skip))
            ,empty))))
;(displayln (model->xexp test-handler-order-state-2))