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

(define start-state
  (term (state ,(list test-init-pd) ,store)))
(test M start-state "start state")

(define pd-after-step
  (term (pre-dispatch loc-parent ,(list (term loc-child)) ,test-event)))
(test S pd-after-step "predispatch after 1 step")

; Test for arbitrary step while building path in pre-dispatch
(define next-state
  (term (state ,(list pd-after-step) ,store)))
(test M next-state "state after pd reduction step")

(test--> DOM-reduce start-state next-state)

; Test for transition from pre-dispatch to dispatch
; - Case: root node has no capture-phase listeners
(define dispatch-start
  (term (dispatch 
         ,test-event 
         loc-parent 
         capture
         ,(list (term loc-parent) (term loc-child))
         ,empty
         ,empty)))
(test S dispatch-start "dispatch-start")

(define dispatch-start-state
  (term (state ,(list dispatch-start) ,store)))
(test M dispatch-start-state "dispatch-start-state")

(test--> DOM-reduce next-state dispatch-start-state)

; - Case: root node has a capture-phase listener of the same type as
; the event (in this case, click)
(define root-listener
  (term (listener click capture ,(list (term mutate) (term prevent-default)))))
(test L root-listener "root-listener")

(define root-with-listener
  (term (node ,(list root-listener) ,empty ,empty ,(list (term loc-child)) null)))
(test N root-with-listener "root-with-listener")

(define root-listener-store
  (term ((loc-child ,child) (loc-parent ,root-with-listener))))
(test N-store root-listener-store "root-listener-store")

(define finished-other-case
  (term (state ,(list pd-after-step) ,root-listener-store)))
(test M finished-other-case "state for testing other pd finished case")

(define dispatch-other-case
  (term (dispatch
         ,test-event
         loc-parent
         capture
         ,(list (term loc-parent) (term loc-child))
         ,empty
         ,(list (term mutate) (term prevent-default)))))
(test S dispatch-other-case "dispatch other case")

(define dispatch-other-start
  (term (state ,(list dispatch-other-case) ,root-listener-store)))
(test M dispatch-other-start "dispatch other start")

(test--> DOM-reduce finished-other-case dispatch-other-start)