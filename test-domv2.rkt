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
             (displayln out)
             (displayln "Expected:")
             (displayln log))))]))     

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
  (term (event "click" #t #t #t ,empty loc_skip)))
(test E test-event "event")

(define test-heap-listener-cap
  (term (listener #t loc-mutate)))
(test HL test-heap-listener-cap "heap-listener-cap")

(define test-heap-listener-bub
  (term (listener #f loc-mutate)))
(test HL test-heap-listener-bub "heap-listener-bub")

(define root
  (term (node "root"
              ,empty 
              ,(list (term loc-child)) 
              null)))
(test N root "root node")

(define test-tp-cap
  (term ("click" capture)))
(test TP test-tp-cap "test-tp-cap")

(define child
  (term (node "child"
              ((,test-tp-cap
                (,test-heap-listener-cap)))
              ,empty
              loc-parent)))
(test N child "child node")

(define store
  (term ((loc-child ,child) 
         (loc-parent ,root)
         (loc-mutate mutate))))
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

(define (make-seq stmts)
  (foldr (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
         #f
         stmts))

(define add-event-state
  (term 
   (state ,(make-seq
            (list
             (term (addEventListener loc_parent "click" #t loc_parent1))
             (term (addEventListener loc_current "click" #t loc_current2))
             (term (setEventHandler loc_current "click" 
                                    (if-phase target (debug-print "before 2") (debug-print "unknown phase"))))
             (term (addEventListener loc_current "click" #f loc_current3))
             (term (addEventListener loc_parent "click" #f loc_parent4))
             (term (setEventHandler loc_parent "click" 
                                    (debug-print "before 4")))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty loc_clickDefault)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_current) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc_parent1 (debug-print "1"))
           (loc_current2 (seq (debug-print "2") prevent-default))
           (loc_current3 (debug-print "3"))
           (loc_parent4 (debug-print "4"))
           (loc_clickDefault skip))
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
   (state ,(make-seq
            (list
             (term (addEventListener loc_current "click" #t
                                     loc-l1))
             (term (addEventListener loc_current "click" #t 
                                     loc-l2))
             (term (addEventListener loc_current "click" #f
                                     loc-l1))
             (term (removeEventListener loc_current "click" #f
                                     loc-l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty loc-clickDefault)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_current) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc-l1 ,l1)
           (loc-l2 ,l2)
           (loc-clickDefault (debug-print "default action!")))
          ,empty))))
(test M add-remove-event-state-1 "add-remove-event-state-1")
(test-log (list "L1" "L2" "default action!") add-remove-event-state-1 "add-remove-event-state-1")

(define add-remove-event-state-2
  (let ([l1 (term (debug-print "L1"))]
        [l2 (term (debug-print "L2"))])
  (term 
   (state ,(make-seq
            (list
             (term (addEventListener loc_current "click" #t
                                     loc-l1))
             (term (addEventListener loc_current "click" #t 
                                     loc-l2))
             (term (addEventListener loc_current "click" #f
                                     loc-l1))
             (term (removeEventListener loc_current "click" #t
                                     loc-l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty loc-clickDefault)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_current) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc-l1 ,l1)
           (loc-l2 ,l2)
           (loc-clickDefault (debug-print "default action!")))
          ,empty))))
(test M add-remove-event-state-2 "add-remove-event-state-2")
(test-log (list "L2" "L1" "default action!") add-remove-event-state-2 "add-remove-event-state-2")


(define add-remove-event-state-3
  (let ([l1 (term (debug-print "L1"))]
        [l2 (term (debug-print "L2"))])
  (term 
   (state ,(make-seq
            (list
             (term (addEventListener loc_current "click" #t
                                     loc-l1))
             (term (addEventListener loc_current "click" #t 
                                     loc-l2))
             (term (addEventListener loc_current "click" #f
                                     loc-l1))
             (term (removeEventListener loc_current "click" #f
                                     loc-l2))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty loc-clickDefault)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_current) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc-l1 ,l1)
           (loc-l2 ,l2)
           (loc-clickDefault (debug-print "default action!")))
          ,empty))))
(test M add-remove-event-state-3 "add-remove-event-state-3")
(test-log (list "L1" "L2" "L1" "default action!") add-remove-event-state-3 "add-remove-event-state-3")

(define cancel-cancelable-state
  (let ([l1 (term prevent-default)])
  (term 
   (state ,(make-seq
            (list
             (term (addEventListener loc_current "click" #t
                                     loc-l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #t #t ,empty loc-clickDefault)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_current) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc-l1 ,l1)
           (loc-clickDefault (debug-print "default action!")))
          ,empty))))
(test M cancel-cancelable-state "cancel-cancelable-state")
(test-log (list "default-prevented") 
          cancel-cancelable-state 
          "log for cancel-cancelable-state")

(define cancel-uncancelable-state
  (let ([l1 (term prevent-default)])
  (term 
   (state ,(make-seq
            (list
             (term (addEventListener loc_current "click" #t
                                     loc-l1))
             (term (pre-dispatch loc_current ,empty 
                                 (event "click" #t #f #t ,empty loc-clickDefault)))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_current) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc-l1 ,l1)
           (loc-clickDefault (debug-print "default action!")))
          ,empty))))
(test M cancel-uncancelable-state "cancel-uncancelable-state")
(test-log (list "default action!") 
          cancel-uncancelable-state 
          "log for cancel-uncancelable state")

(define test-event-meta
  (term (event "click" #t #f #t ,(list (list "k1" 'abcdefg)
                                       (list "k2" "hi there")
                                       (list "k3" 3000)
                                       (list "k4" 
                                             (cons 1 (cons 2 (cons 3 empty)))))
               loc-clickDefault)))
(test E test-event-meta "event with metadata")

(define cancel-uncancelable-state-meta
  (let ([l1 (term prevent-default)])
  (term 
   (state ,(make-seq
            (list
             (term (addEventListener loc_current "click" #t
                                     loc-l1))
             (term (pre-dispatch loc_current ,empty 
                                 ,test-event-meta))
             ))
          ((loc_current (node "child" ,empty ,empty loc_mid))
           (loc_mid (node "middle" ,empty (loc_currrent) loc_parent))
           (loc_parent (node "parent" ,empty (loc_mid) null))
           (loc-l1 ,l1)
           (loc-clickDefault (debug-print "default action!")))
          ,empty))))
(test M cancel-uncancelable-state-meta "cancel-uncancelable-state-meta")
(test-log (list "default action!") 
          cancel-uncancelable-state-meta
          "log for cancel-uncancelable state, with metadata event")

;(define test-gda-state
;  (term (state (pre-dispatch loc_current ,empty
;                             (event "keydown" #t #t #t
;                                    (("char" "a")
;                                     ("key" "a")
;                                     ("location" "Providence")
;                                     ("altKey" #f)
;                                     ("shiftKey" #f)
;                                     ("ctrlKey" #f)
;                                     ("metaKey" #f)
;                                     ("repeat" #f)
;                                     ("locale" "USA"))))
;               ((loc_current (node "child" ,empty ,empty loc_mid))
;                (loc_mid (node "middle" ,empty (loc_current) loc_parent))
;                (loc_parent (node "parent" ,empty (loc_mid) null)))
;               ,empty)))
;(test M test-gda-state "test-gda-state")
;(first (apply-reduction-relation* DOM-reduce test-gda-state))

(define test-non-bubbling-state
  (let ([l1 (term (debug-print "capture listener"))]
        [l2 (term (debug-print "bubble listener"))])
    (term 
     (state ,(make-seq
              (list
               (term (addEventListener loc_current "nobubble" #t
                                       loc_l1))
               (term (addEventListener loc_mid "nobubble" #f
                                       loc_l2))
               (term (pre-dispatch loc_current ,empty
                                   ; This event does not bubble
                                   (event "nobubble"
                                          #f #f #t
                                          ,empty
                                          loc_default)))))
            ((loc_current (node "child" ,empty ,empty loc_mid))
             (loc_mid (node "middle" ,empty (loc_current) loc_parent))
             (loc_parent (node "parent" ,empty (loc_mid) null))
             (loc_default (debug-print "nobubble default action"))
             (loc_l1 ,l1)
             (loc_l2 ,l2))
            ,empty))))
(test M test-non-bubbling-state "test-non-bubbling-state")

(test-log (list "capture listener" "nobubble default action")
          test-non-bubbling-state
          "log for test-non-bubbling-state")

(define pd-untrusted
  (term (pre-dispatch loc_current 
                      ,empty
                      (event "untrusted" #t #t #f ,empty loc_untrusted))))
(test PD pd-untrusted "pd-untrusted")

(define test-skip-untrusted-default
  (term 
   (state 
    ,pd-untrusted
   ((loc_current (node "child" ,empty ,empty loc_mid))
    (loc_mid (node "middle" ,empty (loc_current) loc_parent))
    (loc_parent (node "parent" ,empty (loc_mid) null))
    (loc_untrusted (debug-print "untrusted DA")))
   ,empty)))
(test M test-skip-untrusted-default "test-skip-untrusted-default")

(test-log (list "untrusted event, default action skipped") 
          test-skip-untrusted-default 
          "log for test-skip-untrusted-default")

(define pd-untrusted-click
  (term
   (pre-dispatch loc_current
                 ,empty
                 (event "click" #t #t #f (("fromscript" #f)) loc_clickaction))))

(test PD pd-untrusted-click "pd-untrusted-click")

(define test-do-untrusted-click
  (term 
   (state
    ,pd-untrusted-click
    ((loc_current (node "child" ,empty ,empty loc_mid))
     (loc_mid (node "middle" ,empty (loc_current) loc_parent))
     (loc_parent (node "parent" ,empty (loc_mid) null))
     (loc_clickaction (debug-print "click default action")))
    ,empty)))
(test M test-do-untrusted-click "test-do-untrusted-click")

(test-log (list "click default action")
          test-do-untrusted-click
          "log for test-do-untrusted-click")

(define pd-untrusted-trusted-click
  (term
   (pre-dispatch loc_current
                 ,empty
                 (event "click" #t #t #f (("fromscript" #t)) loc_clickaction))))
(test PD pd-untrusted-trusted-click "pd-untrusted-trusted-click")

(define test-skip-untrusted-click
  (term
   (state
    ,pd-untrusted-trusted-click
    ((loc_current (node "child" ,empty ,empty loc_mid))
     (loc_mid (node "middle" ,empty (loc_current) loc_parent))
     (loc_parent (node "parent" ,empty (loc_mid) null))
     (loc_clickaction (debug-print "click default action")))
    ,empty)))
(test M test-skip-untrusted-click "test-skip-untrusted-click")

(test-log (list "click or DOMActivate from script, skipped DA")
          test-skip-untrusted-click
          "log for test-skip-untrusted-click")

;<button id="test">Start Demo</button>
;<script>
; var button = document.getElementById('test');
; button.addEventListener('click', function () { alert('ONE') }, false);
; button.setAttribute('onclick', "alert('NOT CALLED')"); // event handler listener is registered here
; button.addEventListener('click', function () { alert('THREE') }, false);
; button.onclick = function () { alert('TWO'); };
; button.addEventListener('click', function () { alert('FOUR') }, false);
;</script>

(define test-handler-order-state-1
  (let ([l1 (term (debug-print "ONE"))]
        [l2 (term (debug-print "TWO"))]
        [l3 (term (debug-print "THREE"))]
        [l4 (term (debug-print "FOUR"))])
    (term 
     (state ,(make-seq
              (term (
               (addEventListener loc_child "click" #f loc-l1)
               (setEventHandler loc_child "click" (debug-print "NOT CALLED"))
               (addEventListener loc_child "click" #f loc-l3)
               (setEventHandler loc_child "click" ,l2)
               (addEventListener loc_child "click" #f loc-l4)
               (pre-dispatch loc_child ,empty (event "click" #f #f #t ,empty loc-skip)))
               ))
            ((loc_child (node "child" ,empty ,empty loc_parent))
             (loc_parent (node "parent" ,empty (loc_child) null))
             (loc-l1 ,l1)
             (loc-l3 ,l3)
             (loc-l4 ,l4)
             (loc-skip skip))
            ,empty))))
(test M test-handler-order-state-1 "test-handler-order-state-1")

(test-log (list "ONE" "TWO" "THREE" "FOUR")
          test-handler-order-state-1
          "log for test-handler-order-state-1")


(define test-handler-order-state-2
  (let ([l1 (term (debug-print "ONE"))]
        [l2 (term (debug-print "TWO"))]
        [l3 (term (debug-print "THREE"))]
        [l4 (term (debug-print "FOUR"))])
    (term 
     (state ,(make-seq
              (term (
               (addEventListener loc_child "click" #f loc-l1)
               (setEventHandler loc_child "click" (debug-print "NOT CALLED"))
               (addEventListener loc_child "click" #f loc-l3)
               (removeEventHandler loc_child "click")
               (addEventListener loc_child "click" #f loc-l4)
               (setEventHandler loc_child "click" ,l2)
               (pre-dispatch loc_child ,empty (event "click" #f #f #t ,empty loc-skip)))
               ))
            ((loc_child (node "child" ,empty ,empty loc_parent))
             (loc_parent (node "parent" ,empty (loc_child) null))
             (loc-l1 ,l1)
             (loc-l3 ,l3)
             (loc-l4 ,l4)
             (loc-skip skip))
            ,empty))))
(test M test-handler-order-state-2 "test-handler-order-state-2")

(test-log (list "ONE" "TWO" "THREE" "FOUR")
          test-handler-order-state-2
          "log for test-handler-order-state-2")
