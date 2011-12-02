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

; Test for arbitrary step while building path in pre-dispatch
; Tests pd-build-path

(define test-event
  (term (event "click" #t #t #t)))
(test E test-event "event")

(define test-listener
  (term (listener mutate)))
(test L test-listener "listener")

(define root
  (term (node "root"
              ,empty 
              ,(list (term loc-child)) 
              null)))
(test N root "root node")

(define test-tp
  (list "click" (term capture)))
(test TP test-tp "test-tp")

(define child
  (term (node "child"
              ,(list (list test-tp
                           (list test-listener)))
              ,empty
              loc-parent)))
(test N child "child node")

(define store
  (term ((loc-child ,child) (loc-parent ,root))))
(test N-store store "node store")

(define test-init-pd
  (term (pre-dispatch loc-child ,empty ,test-event)))
(test S test-init-pd "initial pre-dispatch")

(define start-state
  (term (state ,test-init-pd ,store)))
(test M start-state "start state")

(define pd-after-step
  (term (pre-dispatch loc-parent ,(list (term loc-child)) ,test-event)))
(test S pd-after-step "predispatch after 1 step")

(define next-state
  (term (state ,pd-after-step ,store)))
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
               ,store)))
(test M pd-end-state "pd-end-state")

(define dispatch-start
  (term (dispatch-collect ,test-event
                          loc-parent
                          capture
                          #f #f #f
                          ,(list (term loc-parent) (term loc-child)))))
(test DC dispatch-start "dispatch-start")

(define dispatch-start-state
  (term (state ,dispatch-start ,store)))
(test M dispatch-start-state "dispatch-start-state")

(test--> DOM-reduce next-state pd-end-state)
(test--> DOM-reduce pd-end-state dispatch-start-state)

;; Test finishing a dispatch
;; (finish-dispatch)
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
               ,store)))
(test M de-state "de-state")

(define default-start
  (term (dispatch-default ,test-event
                          #f
                          ,(list (term loc-parent) (term loc-child)))))
(test DD default-start "default-start")

(define ds-state
  (term (state ,default-start
               ,store)))
(test M ds-state "ds-state")

(test--> DOM-reduce de-state ds-state)

;; Test finishing current listener steps, going to dispatch-next
;; (finished-listener)
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

     
(test-schema (state (in-hole Ctx
                             (dispatch E
                               parent
                               P
                               PDef SP SI
                               (loc ...)
                               (L ...)
                               (listener skip)))
                        N-store)
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
                      ,(bind-ref binds 'N-store))
                 ))
             "finished-listener")
(test-schema (state (in-hole Ctx
                             (dispatch-next E parent P PDef SP #t 
                                            (loc ...) (L ...))) 
                    N-store)
             (lambda (binds) 
               (term (state 
                      (in-hole ,(bind-ref binds 'Ctx)
                               (dispatch-default ,(bind-ref binds 'E)
                                                 ,(bind-ref binds 'PDef)
                                                 ,(bind-ref binds 'loc)))
                      ,(bind-ref binds 'N-store))))
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
                                 (event "click" #t #t #t))
                   )))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_child) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))))))
(test M add-event-state "add-event-state")

(first (apply-reduction-relation* DOM-reduce add-event-state))
