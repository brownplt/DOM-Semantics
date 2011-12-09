#lang racket
(require redex)
(require "redex-domv2.rkt")
(require "dom-to-html.rkt")

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
          (foldr
           (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
           #f
           allAddListeners)))
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

(let ([nodes (list (term loc_div) (term loc_p) (term loc_span))]
      [phases (list (term capture) (term target) (term bubble))])
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

(let ([nodes (list (term loc_div) (term loc_p) (term loc_span))]
      [phases (list (term capture) (term target) (term bubble))])
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

(define (state-add-listener-dont-run-maker trigger-elem trigger-phase check-elem check-phase)
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
           ,(add-listener-to-everything el-storo "click" #f (term loc-listener1))
           )
      ,(append el-storo
               (list 
                (term (loc-listener1 ,listener1))
                (term (loc-listener2 ,listener2))
                (term (loc-clickDefault skip))))
      ,empty))))

(let* ([state (state-add-listener-dont-run-maker (term loc_div) (term capture) (term loc_span) (term target))]
       [done-state (first (apply-reduction-relation* DOM-reduce state))])
  (displayln (xexp->html (model->xexp state))))