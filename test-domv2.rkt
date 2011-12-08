#lang racket
(require redex)
(require "redex-domv2.rkt")

; Version 2

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

(define-syntax test-log
  (syntax-rules ()
    [(test-log log state name)
     (let ((out (last (first (apply-reduction-relation* DOM-reduce state)))))
       (if (equal? out log)
           (displayln (string-append name ": passed"))
           (begin
             (displayln (string-append name ": FAILED"))
             (displayln "Got:")
             (displayln log)
             (displayln "Expected:")
             (displayln out))))]))     

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

; Test for arbitrary step while building path in pre-dispatch
; Tests pd-build-path

(define test-event
  (term (event "click" #t #t #t ,empty skip)))
(test E test-event "event")

(define test-listener-cap
  (term (listener #t mutate)))
(test L test-listener-cap "listener-cap")

(define test-listener-bub
  (term (listener #f mutate)))
(test L test-listener-bub "listener-bub")

(define root
  (term (node "root"
              ,empty 
              ,(list (term loc-child)) 
              null)))
(test N root "root node")

(define test-tp-cap
  (list "click" (term capture)))
(test TP test-tp-cap "test-tp-cap")

(define child
  (term (node "child"
              ,(list (list test-tp-cap
                           (list test-listener-cap)))
              ,empty
              loc-parent)))
(test N child "child node")

(define store
  (term ((loc-child ,child) (loc-parent ,root))))
(test N-store store "node store")

(define test-init-pd
  (term (pre-dispatch loc-child ,empty ,test-event)))
(test S test-init-pd "initial pre-dispatch")

(define simple-log
  (list "debug 1" "debug 2"))
(test Log simple-log "simple-log")

(define start-state
  (term (state ,test-init-pd ,store ,simple-log)))
(test M start-state "start state")

(define pd-after-step
  (term (pre-dispatch loc-parent ,(list (term loc-child)) ,test-event)))
(test S pd-after-step "predispatch after 1 step")

(define next-state
  (term (state ,pd-after-step ,store ,simple-log)))
(test M next-state "state after pd reduction step")

(test--> DOM-reduce start-state next-state)

;; Test transition to dispatch
;; (pd-to-dispatch)
(define pd-end
  (term (pre-dispatch null ,(list (term loc-parent)
                                  (term loc-child))
                      ,test-event)))
(test S pd-end "pd-end")

(define pd-end-state
  (term (state ,pd-end
               ,store
               ,simple-log)))
(test M pd-end-state "pd-end-state")

(define dispatch-start
  (term (dispatch-collect ,test-event
                          loc-parent
                          capture
                          #f #f #f
                          ,(list (term loc-parent) (term loc-child)))))
(test DC dispatch-start "dispatch-start")

(define dispatch-start-state
  (term (state ,dispatch-start ,store ,simple-log)))
(test M dispatch-start-state "dispatch-start-state")

(test--> DOM-reduce next-state pd-end-state)
(test--> DOM-reduce pd-end-state dispatch-start-state)

;; Test finishing a dispatch
;; (bubble-to-default)
(define dispatch-end
  (term (dispatch-next ,test-event
                       loc-parent
                       bubble
                       #f #f #f
                       ,(list (term loc-parent) (term loc-child))
                       ,empty)))
(test DN dispatch-end "dispatch-end")

(define de-state
  (term (state ,dispatch-end
               ,store
               ,simple-log)))
(test M de-state "de-state")

(define default-start
  (term (dispatch-default ,test-event
                          #f
                          loc-child)))
(test DD default-start "default-start")

(define ds-state
  (term (state ,default-start
               ,store
               ,simple-log)))
(test M ds-state "ds-state")

(test--> DOM-reduce de-state ds-state)

; Test finishing current listener steps, going to dispatch-next
; (finished-listener)     
(test-schema (state (in-hole Ctx
                             (dispatch E
                               parent
                               P
                               PDef SP SI
                               (loc ...)
                               (L ...)
                               (listener #t skip)))
                        N-store
                        Log)
             (lambda (binds)
               (term (state 
                      (in-hole ,(bind-ref binds 'Ctx) 
                        (dispatch-next ,(bind-ref binds 'E)
                                             ,(bind-ref binds 'parent)
                                             ,(bind-ref binds 'P)
                                             ,(bind-ref binds 'PDef)
                                             ,(bind-ref binds 'SP)
                                             ,(bind-ref binds 'SI)
                                             ,(bind-ref binds 'loc)
                                             ,(bind-ref binds 'L)
                                             ))
                      ,(bind-ref binds 'N-store)
                      ,(bind-ref binds 'Log))
                 ))
             "finished-listener")
(test-schema (state (in-hole Ctx
                             (dispatch-next E parent P PDef SP #t 
                                            (loc ... loc_target) (L ...)))
                    N-store Log)
             (lambda (binds) 
               (term (state 
                      (in-hole ,(bind-ref binds 'Ctx)
                               (dispatch-default ,(bind-ref binds 'E)
                                                 ,(bind-ref binds 'PDef)
                                                 ,(bind-ref binds 'loc_target)))
                      ,(bind-ref binds 'N-store)
                      ,(bind-ref binds 'Log))))
             "stop-immediate-called")

(define add-event-state
  (term 
   (state ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_parent "click" #t
                                     (debug-print "1")))
             (term (addEventListener loc_current "click" #t 
                                     (seq (debug-print "2") prevent-default)))
             (term (setEventHandler loc_current "click"
                                    (if-phase target (debug-print "before 2") (debug-print "unknown phase"))))
             (term (addEventListener loc_current "click" #f 
                                     (debug-print "3")))
             (term (addEventListener loc_parent "click" #f 
                                     (debug-print "4")))
             (term (setEventHandler loc_parent "click"
                                    (debug-print "before 4")))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty skip)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null)))
          ,empty)))
(test M add-event-state "add-event-state")

(test-schema (state (in-hole Ctx (debug-print string_new))
                    N-store
                    Log)
             (lambda (binds)
               (term (state
                      (in-hole ,(bind-ref binds 'Ctx)
                               skip)
                      ,(bind-ref binds 'N-store)
                      ,(append
                        (bind-ref binds 'Log)
                        (list (bind-ref binds 'string_new))))))
             "debug-print-logged")

;(first (apply-reduction-relation* DOM-reduce add-event-state))

(define add-remove-event-state-1
  (let ([l1 (term (debug-print "L1"))]
        [l2 (term (debug-print "L2"))])
  (term 
   (state ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_current "click" #t
                                     ,l1))
             (term (addEventListener loc_current "click" #t 
                                     ,l2))
             (term (addEventListener loc_current "click" #f
                                     ,l1))
             (term (removeEventListener loc_current "click" #f
                                     ,l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty skip)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null)))
          ,empty))))
(test M add-remove-event-state-1 "add-remove-event-state-1")
(test-log (list "L1" "L2" "default action!") add-remove-event-state-1 "add-remove-event-state-1")

(define add-remove-event-state-2
  (let ([l1 (term (debug-print "L1"))]
        [l2 (term (debug-print "L2"))])
  (term 
   (state ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_current "click" #t
                                     ,l1))
             (term (addEventListener loc_current "click" #t 
                                     ,l2))
             (term (addEventListener loc_current "click" #f
                                     ,l1))
             (term (removeEventListener loc_current "click" #t
                                     ,l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty skip)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null)))
          ,empty))))
(test M add-remove-event-state-2 "add-remove-event-state-2")
(test-log (list "L2" "L1" "default action!") add-remove-event-state-2 "add-remove-event-state-2")


(define add-remove-event-state-3
  (let ([l1 (term (debug-print "L1"))]
        [l2 (term (debug-print "L2"))])
  (term 
   (state ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_current "click" #t
                                     ,l1))
             (term (addEventListener loc_current "click" #t 
                                     ,l2))
             (term (addEventListener loc_current "click" #f
                                     ,l1))
             (term (removeEventListener loc_current "click" #f
                                     ,l2))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty skip)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null)))
          ,empty))))
(test M add-remove-event-state-3 "add-remove-event-state-3")
(test-log (list "L1" "L2" "L1" "default action!") add-remove-event-state-3 "add-remove-event-state-3")

(define cancel-cancelable-state
  (let ([l1 (term prevent-default)])
  (term 
   (state ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_current "click" #t
                                     ,l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty skip)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null)))
          ,empty))))
(test M cancel-cancelable-state "cancel-cancelable-state")
(test-log (list "default-prevented") 
          cancel-cancelable-state 
          "log for cancel-cancelable-state")

(define cancel-uncancelable-state
  (let ([l1 (term prevent-default)])
  (term 
   (state ,(foldr
            (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
            #f
            (list
             (term (addEventListener loc_current "click" #t
                                     ,l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #f #t ,empty skip)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null)))
          ,empty))))
(test M cancel-uncancelable-state "cancel-uncancelable-state")
(test-log (list "default action!") 
          cancel-uncancelable-state 
          "log for cancel-uncancelable state")