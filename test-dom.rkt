#lang racket
(require redex)
(require "redex-dom.rkt")

(define-syntax test
  (syntax-rules ()
    [(test pattern exp name)
     (let ((result-str
            (if (redex-match DOM pattern exp) "passed" "failed")))
       (displayln (string-append name ": " result-str)))]))

(define test-event
  (term (event click #t #t #t)))
(test E test-event "event")

(define test-listener
  (term (listener click capture ,(list (term mutate)))))
(test L test-listener "listener")

(define test-dom
  (term (node ,(list test-listener) ,empty ,empty ,empty null-node)))
(test N test-dom "DOM node")

(define test-predispatch
  (term (pre-dispatch ,test-dom ,empty ,test-event)))
(test S test-predispatch "pre-dispatch")

(define test-state
  (term (state ,(list test-predispatch) ,test-dom)))
(test M test-state "machine state")