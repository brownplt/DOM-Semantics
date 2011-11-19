#lang racket
(require redex)
(require "redex-dom.rkt")

(define test-event
  (term (event click #t #t #t)))

(define test-listener
  (term (listener click capture ,(list (term mutate)))))

(define test-node-listener
  (term ,(list (term capture) (list test-listener))))

(define test-dom
  (term (node ,(list test-node-listener) ,empty null-node)))

(define test-predispatch
  (term (pre-dispatch ,test-dom ,empty ,test-event)))

(define test-state
  (term (state ,(list test-predispatch) ,test-dom)))