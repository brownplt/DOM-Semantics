#lang racket
(require redex)
(require srfi/1) ; for "zip"
(require xml)
(require (planet neil/json-parsing:1:=2))
(provide create-dom loc-to-node)

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
  ; TODO merge listener lists with the same type/phase pair
  (map (lambda(l)
         (let* ([type (cadr (list-ref (list-ref l 2) 2))]
                [phase (if (eq? (first (list-ref (list-ref l 3) 2))
                                'false)
                           (term bubble)
                           (term capture))]
                [maybe-source (list-ref (list-ref l 1) 2)]
                [source (if (eq? (first maybe-source) 'null)
                            "null"
                            (cadr maybe-source))])
           (list (list type phase)
                 (list (term (listener ,#t (debug-print ,source)))))))
                 ; TODO                ^^^ what does this bool mean?
                 ; TODO ^^^ this part should translate JS into S steps
                 ;          but instead it just makes debug-print terms
       (unpack-json-list (list-ref json-obj 1))))

; Creates a set of (loc->DOM) mappings in loc-to-node.
(define (create-dom xml-obj json-obj parent-loc)
  (let* ([cur-loc (term ,(gensym 'loc_))]
         [listeners (extract-listeners-as-redex json-obj)]
         [listeners2 (foldl (lambda(f r)
                    (let ([match-list (filter
                                        (lambda(l)
                                          (equal? (first (first l))
                                                  (first (first f))))
                                        r)])
                         (if (empty? match-list)
                             (cons f r)
                             (cons (list (first f)
                                         (append (second f)
                                                 (second (first match-list))))
                                   (filter (lambda(l)
                                             (not (equal? (first (first l))
                                                          (first (first f))))) 
                                           r)))))
                  empty
                  listeners)] ; TODO wtf why doesn't this work
         [new-node
           (term (node ,(string-append (symbol->string (first xml-obj))
                                       "::"
                                       (symbol->string cur-loc))
                       ,listeners
                       ; TODO ^^^ include handlers from XML attributes
                       ;;;;; recursively call on all children of this node
                       ,(map (lambda(c) ; (list xml-children json-children)
                               (create-dom (first c)
                                           (second c)
                                           cur-loc))
                             ;;;;; zip XML and JSON child lists together
                             (zip (filter (lambda(c)
                                            (and (not (string? c))
                                                 (not (cdata? c)))) ; TODO include cdata children
                                          (rest (rest xml-obj)))
                                  (extract-children-as-json json-obj)))
                       ,parent-loc))])
    (begin (set! loc-to-node (cons (list cur-loc new-node) loc-to-node))
           (display listeners2)
           (newline)
           cur-loc)))

; The node-store, which is a map from locations to nodes.
(define loc-to-node empty)
