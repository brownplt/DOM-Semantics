#lang racket
(require redex)
(require "redex-domv2.rkt")
(require "test-readxml.rkt")

(define-metafunction DOM
  [(add-to-store ((loc_s any_s) ...) loc any)
   ((loc_s any_s) ... (loc any))])
   
(define test-nostalgy-conversations  
  (let* ([target (first (findf (lambda (b) (not (equal? #f (regexp-match #rx"TARGETED" (second (second b)))))) loc-store))]
         [e_keydown (term (event "keydown" #t #t #t ,empty loc-keydownDefault))]
         [e_keypress (term (event "keypress" #t #t #t ,empty loc-keypressDefault))]
         [keydownDefault (term (seq (debug-print "In keydown::default, Starting pre-dispatch of keypress event") 
                                    (pre-dispatch ,target ,empty ,e_keypress)))]
         [keypressDefault (term (seq (debug-print "In keydown::default, Starting pre-dispatch of textinput event") 
                                     (pre-dispatch ,target ,empty 
                                              (event "textinput" #t #t #t ,empty loc-skip))))])
    (term 
     (state
      (pre-dispatch ,target ,empty ,e_keydown)
      (add-to-store
       (add-to-store ,loc-store loc-keydownDefault ,keydownDefault)
       loc-keypressDefault ,keypressDefault)
      ()))
    ))

(apply-reduction-relation* DOM-reduce test-nostalgy-conversations)
