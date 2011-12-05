#lang racket
(require redex)
(require "redex-domv2.rkt")

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

(define tp-click-cap
  (list "click" (term capture)))
;(test TP tp-click-cap "tp-click-cap")

(define tp-click-targ
  (list "click" (term target)))
;(test TP tp-click-targ "tp-click-targ")

(define tp-click-bub
  (list "click" (term bubble)))
;(test TP tp-click-bub "tp-click-bub")

(define l1-add-div-cap-div-cap
  (term (listener 
         (if-curTarget loc_div
                       (if-phase capture
                                 (addEventListener 
                                  loc_div 
                                  "click" 
                                  #t 
                                  (debug-print "L2 div capture: BAD"))
                                 skip)
                       skip))))
;(test L l1-add-div-cap-div-cap "l1-add-div-capture-div-capture")

;(define l-test-div-cap
;  (term (listener
;         (debug-print "Node div, phase capture"))))
;(test L l-test-div-cap "test-l-div-cap")
;
;(define l-test-div-bub
;  (term (listener
;         (debug-print "Node div, phase bubble"))))
;(test L l-test-div-bub "test-l-div-bub")
;
;(define l-test-p-cap
;  (term (listener
;         (debug-print "Node p, phase capture"))))
;(test L l-test-p-cap "test-l-p-cap")
;
;(define l-test-p-bub
;  (term (listener
;         (debug-print "Node p, phase bubble"))))
;(test L l-test-p-bub "test-l-p-bub")
;
;(define l-test-span-cap
;  (term (listener
;         (debug-print "Node span, phase capture"))))
;(test L l-test-span-cap "test-l-span-cap")
;
;(define l-test-span-bub
;  (term (listener
;         (debug-print "Node span, phase bubble"))))
;(test L l-test-span-bub "test-l-span-bub")

(define n-div
  (term (node "div"
              ,empty
              ,(list (term loc_p))
              null)))
;(test N n-div "n-div")

(define n-p
  (term (node "p"
              ,empty
              ,(list (term loc_span))
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

;(define state-add-div-cap-div-cap
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_div "click" #t
;                                     (if-curTarget loc_div
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_div 
;                                                              "click" 
;                                                              #t
;                                                              (debug-print "L2 div capture: BAD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-div-cap-div-cap "state-add-div-cap-div-cap")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-div-cap-div-cap)
;
;(define state-add-div-cap-div-bub
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_div "click" #t
;                                     (if-curTarget loc_div
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_div 
;                                                              "click" 
;                                                              #f
;                                                              (debug-print "L2 div bubble: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-div-cap-div-bub "state-add-div-cap-div-bub")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-div-cap-div-bub)
;
;(define state-add-div-cap-p-cap
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_div "click" #t
;                                     (if-curTarget loc_div
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_p 
;                                                              "click" 
;                                                              #t
;                                                              (debug-print "L2 p capture: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-div-cap-p-cap "state-add-div-cap-p-cap")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-div-cap-p-cap)
;
;(define state-add-div-cap-p-bub
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_div "click" #t
;                                     (if-curTarget loc_div
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_p 
;                                                              "click" 
;                                                              #f
;                                                              (debug-print "L2 p bubble: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-div-cap-p-bub "state-add-div-cap-p-bub")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-div-cap-p-bub)
;
;(define state-add-div-cap-span-cap
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_div "click" #t
;                                     (if-curTarget loc_div
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_span 
;                                                              "click" 
;                                                              #t
;                                                              (debug-print "L2 span capture: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-div-cap-span-cap "state-add-div-cap-span-cap")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-div-cap-span-cap)
;
;(define state-add-div-cap-span-bub
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_div "click" #t
;                                     (if-curTarget loc_div
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_span 
;                                                              "click" 
;                                                              #f
;                                                              (debug-print "L2 span bubble: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-div-cap-span-bub "state-add-div-cap-span-bub")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-div-cap-span-bub)
;
;(define state-add-p-cap-div-cap
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_p "click" #t
;                                     (if-curTarget loc_p
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_div 
;                                                              "click" 
;                                                              #t
;                                                              (debug-print "L2 div capture: BAD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-p-cap-div-cap "state-add-p-cap-div-cap")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-p-cap-div-cap)
;
;(define state-add-p-cap-div-bub
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_p "click" #t
;                                     (if-curTarget loc_p
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_div 
;                                                              "click" 
;                                                              #f
;                                                              (debug-print "L2 div bubble: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-p-cap-div-bub "state-add-p-cap-div-bub")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-p-cap-div-bub)
;
;(define state-add-p-cap-p-cap
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_p "click" #t
;                                     (if-curTarget loc_p
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_p
;                                                              "click" 
;                                                              #t
;                                                              (debug-print "L2 p capture: BAD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-p-cap-p-cap "state-add-p-cap-p-cap")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-p-cap-p-cap)
;
;(define state-add-p-cap-p-bub
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_p "click" #t
;                                     (if-curTarget loc_p
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_p
;                                                              "click" 
;                                                              #f
;                                                              (debug-print "L2 p bubble: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-p-cap-p-bub "state-add-p-cap-p-bub")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-p-cap-p-bub)
;
;(define state-add-p-cap-span-cap
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_p "click" #t
;                                     (if-curTarget loc_p
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_span
;                                                              "click" 
;                                                              #t
;                                                              (debug-print "L2 span capture: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-p-cap-span-cap "state-add-p-cap-span-cap")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-p-cap-span-cap)
;
;(define state-add-p-cap-span-bub
;  (term 
;   (state 
;    ,(foldr
;            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
;            #f
;            (list
;             (term (addEventListener loc_p "click" #t
;                                     (if-curTarget loc_p
;                                                   (if-phase capture
;                                                             (addEventListener 
;                                                              loc_span
;                                                              "click" 
;                                                              #f
;                                                              (debug-print "L2 span bubble: GOOD"))
;                                                             skip)
;                                                   skip)))
;             (term (addEventListener loc_div "click" #t
;                                     (debug-print "Node div, phase capture")))
;             (term (addEventListener loc_div "click" #f
;                                     (debug-print "Node div, phase bubble")))
;             (term (addEventListener loc_p "click" #t
;                                     (debug-print "Node p, phase capture")))
;             (term (addEventListener loc_p "click" #f
;                                     (debug-print "Node p, phase bubble")))
;             (term (addEventListener loc_span "click" #t
;                                     (debug-print "Node span, phase capture")))
;             (term (addEventListener loc_span "click" #f
;                                     (debug-print "Node span, phase bubble")))
;             (term (pre-dispatch loc_span ,empty 
;                                 (event "click" #t #t #t))
;                   )))
;          ,el-storo)))
;(test M state-add-p-cap-span-bub "state-add-p-cap-span-bub")
;
;(apply-reduction-relation* DOM-reduce 
;                                  state-add-p-cap-span-bub)

(define state-add-span-cap-div-cap
  (term 
   (state 
    ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_span "click" #t
                                     (if-curTarget loc_span
                                                   (if-phase target
                                                             (addEventListener 
                                                              loc_div 
                                                              "click" 
                                                              #t
                                                              (debug-print "L2 div capture: BAD"))
                                                             skip)
                                                   skip)))
             (term (addEventListener loc_div "click" #t
                                     (debug-print "Node div, phase capture")))
             (term (addEventListener loc_div "click" #f
                                     (debug-print "Node div, phase bubble")))
             (term (addEventListener loc_p "click" #t
                                     (debug-print "Node p, phase capture")))
             (term (addEventListener loc_p "click" #f
                                     (debug-print "Node p, phase bubble")))
             (term (addEventListener loc_span "click" #t
                                     (debug-print "Node span, phase capture")))
             (term (addEventListener loc_span "click" #f
                                     (debug-print "Node span, phase bubble")))
             (term (pre-dispatch loc_span ,empty 
                                 (event "click" #t #t #t))
                   )))
          ,el-storo)))
(test M state-add-span-cap-div-cap "state-add-span-cap-div-cap")

(apply-reduction-relation* DOM-reduce 
                                  state-add-span-cap-div-cap)

(define state-add-span-cap-div-bub
  (term 
   (state 
    ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_span "click" #t
                                     (if-curTarget loc_span
                                                   (if-phase target
                                                             (addEventListener 
                                                              loc_div 
                                                              "click" 
                                                              #f
                                                              (debug-print "L2 div bubble: GOOD"))
                                                             skip)
                                                   skip)))
             (term (addEventListener loc_div "click" #t
                                     (debug-print "Node div, phase capture")))
             (term (addEventListener loc_div "click" #f
                                     (debug-print "Node div, phase bubble")))
             (term (addEventListener loc_p "click" #t
                                     (debug-print "Node p, phase capture")))
             (term (addEventListener loc_p "click" #f
                                     (debug-print "Node p, phase bubble")))
             (term (addEventListener loc_span "click" #t
                                     (debug-print "Node span, phase capture")))
             (term (addEventListener loc_span "click" #f
                                     (debug-print "Node span, phase bubble")))
             (term (pre-dispatch loc_span ,empty 
                                 (event "click" #t #t #t))
                   )))
          ,el-storo)))
(test M state-add-span-cap-div-bub "state-add-span-cap-div-bub")

(apply-reduction-relation* DOM-reduce 
                                  state-add-span-cap-div-bub)

(define state-add-span-cap-p-cap
  (term 
   (state 
    ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_p "click" #t
                                     (if-curTarget loc_span
                                                   (if-phase target
                                                             (addEventListener 
                                                              loc_p
                                                              "click" 
                                                              #t
                                                              (debug-print "L2 p capture: BAD"))
                                                             skip)
                                                   skip)))
             (term (addEventListener loc_div "click" #t
                                     (debug-print "Node div, phase capture")))
             (term (addEventListener loc_div "click" #f
                                     (debug-print "Node div, phase bubble")))
             (term (addEventListener loc_p "click" #t
                                     (debug-print "Node p, phase capture")))
             (term (addEventListener loc_p "click" #f
                                     (debug-print "Node p, phase bubble")))
             (term (addEventListener loc_span "click" #t
                                     (debug-print "Node span, phase capture")))
             (term (addEventListener loc_span "click" #f
                                     (debug-print "Node span, phase bubble")))
             (term (pre-dispatch loc_span ,empty 
                                 (event "click" #t #t #t))
                   )))
          ,el-storo)))
(test M state-add-span-cap-p-cap "state-add-span-cap-p-cap")

(apply-reduction-relation* DOM-reduce 
                                  state-add-span-cap-p-cap)

(define state-add-span-cap-p-bub
  (term 
   (state 
    ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_span "click" #t
                                     (if-curTarget loc_span
                                                   (if-phase target
                                                             (addEventListener 
                                                              loc_p
                                                              "click" 
                                                              #f
                                                              (debug-print "L2 p bubble: GOOD"))
                                                             skip)
                                                   skip)))
             (term (addEventListener loc_div "click" #t
                                     (debug-print "Node div, phase capture")))
             (term (addEventListener loc_div "click" #f
                                     (debug-print "Node div, phase bubble")))
             (term (addEventListener loc_p "click" #t
                                     (debug-print "Node p, phase capture")))
             (term (addEventListener loc_p "click" #f
                                     (debug-print "Node p, phase bubble")))
             (term (addEventListener loc_span "click" #t
                                     (debug-print "Node span, phase capture")))
             (term (addEventListener loc_span "click" #f
                                     (debug-print "Node span, phase bubble")))
             (term (pre-dispatch loc_span ,empty 
                                 (event "click" #t #t #t))
                   )))
          ,el-storo)))
(test M state-add-span-cap-p-bub "state-add-span-cap-p-bub")

(apply-reduction-relation* DOM-reduce 
                                  state-add-span-cap-p-bub)

(define state-add-span-cap-span-cap
  (term 
   (state 
    ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_span "click" #t
                                     (if-curTarget loc_span
                                                   (if-phase target
                                                             (addEventListener 
                                                              loc_span
                                                              "click" 
                                                              #t
                                                              (debug-print "L2 span capture: BAD"))
                                                             skip)
                                                   skip)))
             (term (addEventListener loc_div "click" #t
                                     (debug-print "Node div, phase capture")))
             (term (addEventListener loc_div "click" #f
                                     (debug-print "Node div, phase bubble")))
             (term (addEventListener loc_p "click" #t
                                     (debug-print "Node p, phase capture")))
             (term (addEventListener loc_p "click" #f
                                     (debug-print "Node p, phase bubble")))
             (term (addEventListener loc_span "click" #t
                                     (debug-print "Node span, phase capture")))
             (term (addEventListener loc_span "click" #f
                                     (debug-print "Node span, phase bubble")))
             (term (pre-dispatch loc_span ,empty 
                                 (event "click" #t #t #t))
                   )))
          ,el-storo)))
(test M state-add-span-cap-span-cap "state-add-span-cap-span-cap")

(apply-reduction-relation* DOM-reduce 
                                  state-add-span-cap-span-cap)

(define state-add-span-cap-span-bub
  (term 
   (state 
    ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_span "click" #t
                                     (if-curTarget loc_span
                                                   (if-phase target
                                                             (addEventListener 
                                                              loc_span
                                                              "click" 
                                                              #f
                                                              (debug-print "L2 span bubble: BAD"))
                                                             skip)
                                                   skip)))
             (term (addEventListener loc_div "click" #t
                                     (debug-print "Node div, phase capture")))
             (term (addEventListener loc_div "click" #f
                                     (debug-print "Node div, phase bubble")))
             (term (addEventListener loc_p "click" #t
                                     (debug-print "Node p, phase capture")))
             (term (addEventListener loc_p "click" #f
                                     (debug-print "Node p, phase bubble")))
             (term (addEventListener loc_span "click" #t
                                     (debug-print "Node span, phase capture")))
             (term (addEventListener loc_span "click" #f
                                     (debug-print "Node span, phase bubble")))
             (term (pre-dispatch loc_span ,empty 
                                 (event "click" #t #t #t))
                   )))
          ,el-storo)))
(test M state-add-span-cap-span-bub "state-add-span-cap-span-bub")

(apply-reduction-relation* DOM-reduce 
                                  state-add-span-cap-span-bub)