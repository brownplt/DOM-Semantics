#lang racket
(require redex)
(require srfi/1) ; for "zip"
(require xml)
(require (planet neil/json-parsing:1:=2))
(provide create-dom loc-to-node)

; Takes a string of JavaScript and makes a mild attempt to get interesting
; information out of it. Doesn't look for control structures yet and currently
; does not attempt to "read into" function calls, although this will
; eventually be necessary.
;;;;; TODO list below
; first: search for stopProp, stopImm, preventDefault, "return"
; next: add/removeEventListener, setEventHandler
; last: if-curTarget, if-phase
(define (js->listener-step js)
  (term (debug-print ,js)))

; When a JSON object is supposed to represent a list, it looks like this:
;   { "0" : A, "1" : B, ...}
; so each entry in the list looks like:
;   (member (@ (name "0")) (object ...))
; but we want just the (object ...), so we use this unpacking function.
(define (unpack-json-list json-list)
  (map (lambda(o) (list-ref o 2))
       (rest (list-ref json-list 2))))

; Takes a JSON object, returns its children as JSON objects.
(define (extract-children-as-json json-obj)
  (unpack-json-list (list-ref json-obj 5)))

; Takes a JSON object, returns the listeners in that object as Redex values.
(define (extract-listeners-as-redex json-obj)
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
  (merge-duplicate-tps
   (map (lambda(l)
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
                  (list (term (listener ,capture ,(js->listener-step source)))))))
        (unpack-json-list (list-ref json-obj 1)))))

; Creates a set of (loc->DOM) mappings in loc-to-node, returns root loc.
(define (create-dom xml-obj json-obj parent-loc)
  (let* ([cur-loc (term ,(gensym 'loc_))]
         [listeners (extract-listeners-as-redex json-obj)]
         [new-node
           (term (node ,(string-append (symbol->string (first xml-obj))
                                       "::"
                                       (symbol->string cur-loc))
                       ,listeners
                       ;;;;; TODO ^^^ include handlers from XML attributes
                       ; recursively call on all children of this node
                       ,(map (lambda(c) ; (list xml-children json-children)
                               (create-dom (first c)
                                           (second c)
                                           cur-loc))
                             ; zip XML and JSON child lists together
                             (zip (filter (lambda(c)
                                            (and (not (string? c))
                                                 ; CDATA XML directives unused
                                                 (not (cdata? c))))
                                          (rest (rest xml-obj)))
                                  (extract-children-as-json json-obj)))
                       ,parent-loc))])
    (begin (set! loc-to-node (cons (list cur-loc new-node) loc-to-node))
           cur-loc)))

; The node-store, which is a map from locations to nodes.
(define loc-to-node empty)
