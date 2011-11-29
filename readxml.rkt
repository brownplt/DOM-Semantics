#lang racket
(require redex)
(require xml)
(require "redex-dom.rkt")

(define test-doc
  (xml->xexpr (document-element
               (read-xml (open-input-string
                          "<html><head><script>alert('hi');</script><br /></head><body></body></html>")))))


;
;(define (get-child-locs node)
;  (first (rest (rest node))))
;
;(define (combine l1 l2)
;  (if (empty? l1)
;      empty
;      (let ([new-pair (list (first l1) (first l2))])
;        (cons new-pair (combine (rest l1) (rest l2))))))
;
;(define (create-store x-expr my-loc parent)
;  (let* ([root-pair (create-node-pair x-expr my-loc parent)]
;         [children-locs (get-child-locs (second root-pair))]
;         [combined (combine children-locs (children x-expr))]
;         [child-pairs (map (lambda (pair) 
;                             (create-node-pair (second pair) (first pair) root-loc))
;                           combined)])
;    child-pairs))
;
;(define (create-node-pair x-expr my-loc parent)
;  (let ([child-locs (map (lambda (c) (gensym 'loc))
;                         (children x-expr))])
;    (list my-loc (term (node ,empty ,child-locs ,parent)))))        
(define make-tree cons)
(define tree-datum first)
(define (tree-children tree)
  (filter list? (rest (rest tree))))

(define (treemap fn tree)
  (make-tree (fn (tree-datum tree))
             (forest-map fn (tree-children tree))))

(define (forest-map fn forest)
  (if (empty? forest)
      '()
      (cons (treemap fn (first forest))
            (forest-map fn (rest forest)))))

(define doc-with-locations
  (treemap (lambda (tag) (gensym 'loc))
           test-doc))

(define (update-store s-exp store parent)
  (let* ([my-loc (first s-exp)]
        [all-children-store
         (foldr
          (lambda (child prior-store)
            (update-store child prior-store my-loc))
          store
          (rest s-exp))]
        [child-locs (map first (rest s-exp))]
        [this-node (term (node ,empty ,child-locs ,parent))])
    (cons (list my-loc this-node) all-children-store)))

(define initial-store '())
(define initial-parent (term null))
(define doc-store (update-store doc-with-locations initial-store initial-parent))

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

(define test-pm
  (list (list (term capture) (list test-listener))))
(test PM test-pm "test-pm")

(define test-ls
  (list (list (term "click") test-pm)))
(test LS test-ls "test-ls")
;
(define root
  (term (node ,test-ls ,(list (term loc-child)) null)))
(test N root "root node")



;'(loc51262 
;  (loc51263 
;   (loc51264) 
;   (loc51265)) 
;  (loc51266))
;
;'(html 
;  (head 
;   (script ) 
;   (br )) 
;  (body ))