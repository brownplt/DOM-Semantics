#lang racket
(require redex)
(require srfi/1) ; for "zip"
(require xml)
(provide create-dom loc-store js->listener-step)

; Takes a string of JavaScript and makes a mild attempt to get interesting
; information out of it. Doesn't look for control structures yet and currently
; does not attempt to "read into" function calls, although this will
; eventually be necessary.
; TODO list below
; next: add/removeEventListener, setEventHandler, "return" (?)
; last: if-curTarget, if-phase
(define (js->listener-step js)
  (let* ([stop-prop-pos (regexp-match-positions #rx"\\.stopPropagation\\(\\);" js)]
         [stop-imm-pos  (regexp-match-positions #rx"\\.stopImmediatePropagation\\(\\);" js)]
         [prev-def-pos  (regexp-match-positions #rx"\\.preventDefault\\(\\);" js)]
         [mutate-pos  (regexp-match-positions #rx"mutate" js)]
         [sp-begin      (if (list? stop-prop-pos) (caar stop-prop-pos) +inf.0)]
         [si-begin      (if (list? stop-imm-pos) (caar stop-imm-pos) +inf.0)]
         [pd-begin      (if (list? prev-def-pos) (caar prev-def-pos) +inf.0)]
         [mutate-begin  (if (list? mutate-pos) (caar mutate-pos) +inf.0)]
         [min-begin     (min sp-begin si-begin pd-begin mutate-begin)])
        (cond [(= min-begin +inf.0)
                (term (debug-print ,js))]
              [(= min-begin sp-begin)
                (term (seq (seq (debug-print "Stopping propagation") stop-prop)
                           ,(js->listener-step
                             (substring js (cdar stop-prop-pos)))))]
              [(= min-begin si-begin)
                (term (seq stop-immediate
                           ,(js->listener-step
                             (substring js (cdar stop-imm-pos)))))]
              [(= min-begin pd-begin)
                (term (seq prevent-default
                           ,(js->listener-step
                             (substring js (cdar prev-def-pos)))))]
              [(= min-begin mutate-begin)
                (term (seq mutate
                           ,(js->listener-step
                              (substring js (cdar mutate-pos)))))])))

; Takes a string of JS and returns a corresponding loc, which is added to the
; end of the loc-store with the listener-step associated with this JS string.
(define (js->loc js)
  (let ([cur-loc (term ,(gensym 'loc_))])
         (begin (set! loc-store (append loc-store
                                        (list (list
                                                cur-loc
                                                (term (seq (debug-print ,(symbol->string cur-loc))
                                                           ,(js->listener-step js)))))))
                cur-loc)))

; When a JSON object is supposed to represent a list, it looks like this:
;   { "0" : A, "1" : B, ...}
; so each entry in the list looks like:
;   (member (@ (name "0")) (object ...))
; but we want just the (object ...), so we use this unpacking function.
(define (unpack-json-list json-list)
  (map (lambda (o) (list-ref o 2))
       (rest (list-ref json-list 2))))

; Takes a JSON object, returns its children as JSON objects.
(define (extract-children-as-json json-obj)
  (append (unpack-json-list (list-ref json-obj 5))
          ; grab JSON "subDoc" member objects as children for browser/iframe
          ; TODO should we prevent propagation between documents?
          (if (or (equal? (second (third (third json-obj)))
                          "browser")
                  (equal? (second (third (third json-obj)))
                          "iframe"))
              (list (third (list-ref json-obj 6)))
              empty)))

; Takes a JSON object, returns the listeners in that object as Redex values.
(define (extract-listeners-as-redex json-obj)
 (map (lambda (l)
        (let* ([type (cadr (list-ref (list-ref l 2) 2))]
               [capture (eq? (first (list-ref (list-ref l 3) 2)) 'true)]
               [phase (if (eq? (first (list-ref (list-ref l 3) 2))
                               'false)
                          (term bubble)
                          (term capture))]
               [maybe-source (list-ref (list-ref l 1) 2)]
               [source (if (eq? (first maybe-source) 'null)
                           "null"
                           (cadr maybe-source))])
          (list (list type phase)
                (list (term (listener ,capture ,(js->loc source)))))))
      (unpack-json-list (list-ref json-obj 1))))

; Takes an XML object, returns the handlers in that object as Redex values.
(define (extract-handlers-as-redex xml-obj)
  (map (lambda (handler)
         (list (list (substring (symbol->string (first handler)) 2)
                     (term bubble)) ; TODO this depends on event type...
               (list (term (handler ,(js->listener-step (second handler)))))))
       (filter (lambda (attr)
                 (string=? (substring (symbol->string (first attr)) 0 2)
                           "on"))
               (second xml-obj))))

; Creates a set of (loc->DOM) mappings in loc-store, returns root loc.
(define (create-dom xml-obj json-obj parent-loc)
  (define (merge-duplicate-tps ls)
    (let ([unique-tps (remove-duplicates (map first ls))])
      (map (lambda (tp)
             (let ([listeners
                    (map second
                         (filter (lambda (tp-l-pair)
                                   (equal? (first tp-l-pair) tp))
                                 ls))])
               (list tp (foldr append '() listeners))))
           unique-tps)))
  (let* ([cur-loc (term ,(gensym 'loc_))]
         [listeners (merge-duplicate-tps
                      ; TODO do XML too
                      (append (extract-handlers-as-redex xml-obj)
                              (extract-listeners-as-redex json-obj)))]
         [new-node
           (term (node ,(string-append
                          (symbol->string (first xml-obj))
                          "::"
                          (let ([id-attr
                                  (filter (lambda (attr)
                                            (eq? (first attr) 'id))
                                          (second xml-obj))])
                               (if (empty? id-attr)
                                   "___"
                                   (second (first id-attr))))
                          "::"
                          (symbol->string cur-loc))
                       ,listeners
                       ; recursively call on all children of this node
                       ,(map (lambda (c) ; (list xml-children json-children)
                               (create-dom (first c)
                                           (second c)
                                           cur-loc))
                             ; zip XML and JSON child lists together
                             (zip (filter (lambda (c)
                                            (and (not (string? c))
                                                 ; XML directives unused
                                                 (not (cdata? c))))
                                          (rest (rest xml-obj)))
                                  (extract-children-as-json json-obj)))
                       ,parent-loc))])
    (begin (set! loc-store (cons (list cur-loc new-node) loc-store))
           cur-loc)))

; The node-store, which is a map from locations to nodes.
(define loc-store empty)
