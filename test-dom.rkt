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

(define root
  (term (node ,empty ,empty ,empty ,(list (term loc-child)) null)))
(test N root "root node")

(define child
  (term (node ,(list test-listener) ,empty ,empty ,empty loc-parent)))
(test N child "child node")

(define store
  (term ((loc-child ,child) (loc-parent ,root))))
(test N-store store "node store")

(define test-init-pd
  (term (pre-dispatch loc-child ,empty ,test-event)))
(test S test-init-pd "initial pre-dispatch")