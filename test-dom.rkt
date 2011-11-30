#lang racket
(require redex)
(require "redex-dom.rkt")

(define-syntax test
  (syntax-rules ()
    [(test pattern exp name)
     (let ((result-str
            (if (redex-match DOM pattern exp) "passed" "FAILED!!!!")))
       (displayln (string-append name ": " result-str)))]))

(define test-event
  (term (event "click" #t #t #t)))
(test E test-event "event")

(define test-listener
  (term (listener "click" capture ,(list (term mutate)))))
(test L test-listener "listener")

(define root
  (term (node ,empty ,(list (term loc-child)) null)))
(test N root "root node")

(define test-pm
  (list (list (term capture) (list test-listener))))
(test PM test-pm "test-pm")

(define test-ls
  (list (list "click" test-pm)))
(test LS test-ls "test-ls")

(define child
  (term (node ,test-ls ,empty loc-parent)))
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
; Case: root node has no click listeners
(define dispatch-start
  (term (dispatch
         ,test-event
         loc-parent
         capture
         #f
         ,(list (term loc-parent) (term loc-child))
         ,empty
         ,empty)))
(test S dispatch-start "dispatch-start")

(define dispatch-start-state
  (term (state ,(list dispatch-start) ,store)))
(test M dispatch-start-state "dispatch-start-state")

(test--> DOM-reduce next-state dispatch-start-state)

; - Case: root node has a click listener
(define root-listener
  (term (listener "click" capture ,(list (term mutate) (term prevent-default)))))
(test L root-listener "root-listener")

(define listener-pm
  (list (list (term capture) (list root-listener))))
(test PM listener-pm "listener-pm")

(define listener-ls
  (list (list "click" listener-pm)))
(test LS listener-ls "listener-ls")

(define root-with-listener
  (term (node ,listener-ls ,(list (term loc-child)) null)))
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
         #f
         ,(list (term loc-parent) (term loc-child))
         ,empty
         ,(list (term mutate) (term prevent-default)))))
(test S dispatch-other-case "dispatch other case")

(define dispatch-other-start
  (term (state ,(list dispatch-other-case) ,root-listener-store)))
(test M dispatch-other-start "dispatch other start")

(test--> DOM-reduce finished-other-case dispatch-other-start)

; -Case: root node has a capture-phase listener of a different type from
; the event
(define root-other-listener
  (term (listener "keydown" capture ,(list (term mutate) (term mutate)))))
(test L root-other-listener "root-other-listener")

(define root-following-listener
  (term (listener "keydown" capture ,(list (term stop-prop) (term stop-prop)))))
(test L root-following-listener "root-following-listener")

(define other-pm
  (list (list (term capture) (list root-other-listener root-following-listener))))
(test PM other-pm "other-pm")

(define other-ls
  (list (list "keydown" other-pm)))
(test LS other-ls "other-ls")

(define root-with-other
  (term (node ,other-ls
              ,(list (term loc-child))
              null)))
(test N root-with-other "root-with-other")

(define root-other-store
  (term ((loc-child ,child) (loc-parent ,root-with-other))))
(test N-store root-other-store "root-other-store")

(define finished-other2
  (term (state ,(list pd-after-step) ,root-other-store)))
(test M finished-other2 "state for testing root listener of wrong type")

(define dispatch-other2
  (term (dispatch
         ,test-event
         loc-parent
         capture
         #f
         ,(list (term loc-parent) (term loc-child))
         ,empty
         ,empty)))
(test S dispatch-other-case "dispatch other2")

(define dispatch-other2-start
  (term (state ,(list dispatch-other2) ,root-other-store)))
(test M dispatch-other-start "dispatch other2 start")

(test--> DOM-reduce finished-other2 dispatch-other2-start)

(define last-store
  (term ((loc-root ,root))))
(test N-store store "last-store")

(define dispatch-last
  (term (dispatch 
         ,test-event
         loc-root
         bubble
         #f
         ,(list (term loc-root))
         ,empty
         ,empty)))
(test S dispatch-last "dispatch-last")

(define finished-last
  (term (state ,(list dispatch-last) ,last-store)))
(test M finished-last "finished-last")

(test--> DOM-reduce finished-last #t)

(define dispatch-last-stopprop
  (term (dispatch 
         ,test-event
         loc-root
         bubble
         #f
         ,empty
         ,empty
         ,empty)))
(test S dispatch-last-stopprop "dispatch-last-stopprop")

(define prop-stop
  (term (state ,(list dispatch-last-stopprop) ,last-store)))
(test M prop-stop "prop-stop")

(test--> DOM-reduce prop-stop #t)

(define dispatch-end-steps
  (term (state ,(list dispatch-last test-init-pd) ,last-store)))
(test M dispatch-end-steps "dispatch-end-steps")

(define next-pd
  (term (state ,(list test-init-pd) ,last-store)))
(test M next-pd "next-pd")

(test--> DOM-reduce dispatch-end-steps next-pd)

(define prop-stop-steps
  (term (state ,(list dispatch-last-stopprop test-init-pd) ,last-store)))
(test M prop-stop-steps "prop-stop-steps")

(test--> DOM-reduce prop-stop-steps next-pd)

(define test-listener-target
  (term (listener "click" target ,(list (term mutate)))))
(test L test-listener-target "test-listener-target")

(define test-pm-target
  (list (list (term target) (list test-listener-target))))
(test PM test-pm-target "test-pm-target")

(define test-ls-target
  (list (list "click" test-pm-target)))
(test LS test-ls-target "test-ls-target")

(define dispatch-capture-switch
  (term (dispatch
         ,test-event
         loc-parent
         capture
         #f
         ,(list (term loc-parent) (term loc-target))
         ,empty
         ,empty)))
(test S dispatch-capture-switch "dispatch-capture-switch")

(define switch-listener
  (term (listener "click" target ,(list (term mutate)))))
(test L switch-listener "switch-listener")

(define switch-pm
  (list (list (term target) (list switch-listener))))
(test PM switch-pm "switch-pm")

(define switch-ls
  (list (list "click" switch-pm)))
(test LS switch-ls "switch-ls")

(define switch-node
  (term (node ,switch-ls ,empty loc-parent)))
(test N switch-node "switch-node")

(define switch-store
  (term ((loc-target ,switch-node))))
(test N-store switch-store "switch-store")

(define state-capture-switch
  (term (state ,(list dispatch-capture-switch) ,switch-store)))
(test M state-capture-switch "state-capture-switch")
  
(define dispatch-target
  (term (dispatch
         ,test-event
         loc-target
         target
         #f
         ,(list (term loc-parent) (term loc-target))
         ,(list switch-listener)
         ,empty)))
(test S dispatch-target "dispatch-target")

(define state-target
  (term (state ,(list dispatch-target) ,switch-store)))
(test M state-target "state-target")

(test--> DOM-reduce state-capture-switch state-target)


(define test-listener-bubble
  (term (listener "click" bubble ,(list (term mutate)))))
(test L test-listener-bubble "test-listener-bubble")

(define test-pm-bubble
  (list (list (term bubble) (list test-listener-bubble))))
(test PM test-pm-bubble "test-pm-bubble")

(define test-ls-bubble
  (list (list "click" test-pm-bubble)))
(test LS test-ls-bubble "test-ls-bubble")

(define dispatch-target-switch
  (term (dispatch
         ,test-event
         loc-target
         target
         #f
         ,(list (term loc-parent) (term loc-target))
         ,empty
         ,empty)))
(test S dispatch-target-switch "dispatch-target-switch")

(define switch-bubble-listener
  (term (listener "click" bubble ,(list (term mutate)))))
(test L switch-bubble-listener "switch-bubble-listener")

(define switch-bubble-pm
  (list (list (term bubble) (list switch-bubble-listener))))
(test PM switch-bubble-pm "switch-bubble-pm")

(define switch-bubble-ls
  (list (list "click" switch-bubble-pm)))
(test LS switch-bubble-ls "switch-bubble-ls")

(define switch-bubble-node
  (term (node ,switch-bubble-ls ,empty loc-parent)))
(test N switch-bubble-node "switch-bubble-node")

(define switch-bubble-store
  (term ((loc-target ,switch-bubble-node))))
(test N-store switch-bubble-store "switch-bubble-store")

(define state-target-switch
  (term (state ,(list dispatch-target-switch) ,switch-bubble-store)))
(test M state-target-switch "state-target-switch")

(define dispatch-bubble
  (term (dispatch
         ,test-event
         loc-target
         bubble
         #f
         ,(list (term loc-parent) (term loc-target))
         ,(list switch-bubble-listener)
         ,empty)))
(test S dispatch-bubble "dispatch-bubble")

(define state-bubble
  (term (state ,(list dispatch-bubble) ,switch-bubble-store)))
(test M state-bubble "state-bubble")

(test--> DOM-reduce state-target-switch state-bubble)
