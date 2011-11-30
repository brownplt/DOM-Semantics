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

; Test capture-next-node reduction:
; Handles case of transitioning to the next node on the path during
; the capture phase, when the next node has listeners for the correct
; event type AND the capture phase
(define cnn-next-listener
  (term (listener "click" capture (stop-immediate))))
(test L cnn-next-listener "cnn-next-listener")
  
(define cnn-before
  (term (state ((dispatch (event "click" #t #t #t)
                          loc-current
                          capture
                          #f
                          ,(list (term loc-parent) (term loc-current) (term loc-next) (term loc-target))
                          ,empty
                          ,empty))
               ((loc-next
                 (node ,(list (list "click" (list (list (term capture) (list cnn-next-listener)))))
                       ,(list (term loc-target))
                       loc-current))))))
(test M cnn-before "cnn-before")

(define cnn-after
  (term (state ((dispatch (event "click" #t #t #t)
                          loc-next
                          capture
                          #f
                          ,(list (term loc-parent) (term loc-current) (term loc-next) (term loc-target))
                          ,(list cnn-next-listener)
                          ,(list (term stop-immediate))))
               ((loc-next
                 (node ,(list (list "click" (list (list (term capture) (list cnn-next-listener)))))
                       ,(list (term loc-target))
                       loc-current))))))
(test M cnn-after "cnn-after")

(test--> DOM-reduce cnn-before cnn-after)

; Test capture-next-none reduction
; Handles case of transitioning to the next node on the path during the capture
; phase, when the next node has listeners for the correct event type, BUT NOT
; for the capture phase
(define cn-none-listener
  (term (listener "click" bubble (stop-immediate))))
(test L cn-none-listener "cn-none-listener")
(define cn-none-listener-t
  (term (listener "click" target (stop-immediate))))
(test L cn-none-listener-t "cn-none-listener-t")

(define cn-none-before
  (term (state ((dispatch (event "click" #t #t #t)
                          loc-current
                          capture
                          #f
                          ,(list (term loc-parent) (term loc-current) (term loc-next) (term loc-target))
                          ,empty
                          ,empty))
               ((loc-next
                 (node ,(list (list "click" (list (list (term bubble) (list cn-none-listener))
                                                  (list (term target) (list cn-none-listener-t)))))
                       ,(list (term loc-target))
                       loc-current))))))
(test M cn-none-before "cn-none-before")

(define cn-none-after
  (term (state ((dispatch (event "click" #t #t #t)
                          loc-next
                          capture
                          #f
                          ,(list (term loc-parent) (term loc-current) (term loc-next) (term loc-target))
                          ,empty
                          ,empty))
               ((loc-next
                 (node ,(list (list "click" (list (list (term bubble) (list cn-none-listener))
                                                  (list (term target) (list cn-none-listener-t)))))
                       ,(list (term loc-target))
                       loc-current))))))
(test M cn-none-after "cn-none-after")

(test--> DOM-reduce cn-none-before cn-none-after)

; Test capture-next-skip reduction
; Handles case of transitioning to next node on the path during capture phase
; when the next node has no listeners for the correct event type
(define cn-skip-listener
  (term (listener "keydown" capture (stop-immediate))))
(test L cn-skip-listener "cn-skip-listener")

(define cn-skip-before
  (term (state ((dispatch (event "click" #t #t #t)
                          loc-current
                          capture
                          #f
                          ,(list (term loc-parent) (term loc-current) (term loc-next) (term loc-target))
                          ,empty
                          ,empty))
               ((loc-next
                 (node ,(list (list "keydown" (list (list (term bubble) (list cn-skip-listener)))))
                       ,(list (term loc-target))
                       loc-current))))))
(test M cn-skip-before "cn-skip-before")

(define cn-skip-after
  (term (state ((dispatch (event "click" #t #t #t)
                          loc-next
                          capture
                          #f
                          ,(list (term loc-parent) (term loc-current) (term loc-next) (term loc-target))
                          ,empty
                          ,empty))
               ((loc-next
                 (node ,(list (list "keydown" (list (list (term bubble) (list cn-skip-listener)))))
                       ,(list (term loc-target))
                       loc-current))))))
(test M cn-skip-after "cn-skip-after")

(test--> DOM-reduce cn-skip-before cn-skip-after)