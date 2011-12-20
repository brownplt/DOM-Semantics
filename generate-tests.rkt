#lang racket
(require redex)
(require racket/date)
(require "redex-domv2.rkt")
(require "dom-to-html.rkt")


(define (make-seq stmts)
  (foldr (lambda (t acc) (if (equal? acc #f) t (term (seq ,t ,acc))))
         #f
         stmts))

(define (lookup loc store)
  (second (findf (lambda (p) (equal? (first p) loc)) store)))
(define (update-store store loc val)
  (if (findf (lambda (p) (equal? (first p) loc)) store)
      (map (lambda (entry) (if (equal? (first entry) loc) (list loc val) entry)) store)
      (append store (list (list loc val)))))

(define (rand-bool)
  (equal? (random 2) 0))

(define n-div
  (term (node "div"
              ,empty
              (loc_p)
              null)))
;(test N n-div "n-div")

(define n-p
  (term (node "p"
              ,empty
              (loc_span)
              loc_div)))
;(test N n-p "n-p")

(define n-span
  (term (node "span"
              ,empty
              ,empty
              loc_p)))
;(test N n-span "n-span")

(define el-storo
  (term ((loc_div ,n-div)
         (loc_p ,n-p)
         (loc_span ,n-span))))
;(test N-store el-storo "el-storo")


(define (add-listener-to-everything store type useCapture listener)
  (let* ((allLocs (map first store))
         (allAddListeners 
          (map (lambda (loc) 
                 (term (addEventListener ,loc ,type ,useCapture ,listener))) allLocs))
         (addListenerStmt
          (make-seq allAddListeners)))
    addListenerStmt))


(define (appendChild loc_parent loc_child store)
  (let*-values ([(parent) (lookup loc_parent store)]
                [(child) (lookup loc_child store)]
                [(node_parent name_parent ls_parent kids_parent parent_parent) (apply values parent)]
                [(node_child name_child ls_child kids_child parent_child) (apply values child)]
                [(new_parent) 
                 (term (node ,name_parent ,ls_parent ,(append kids_parent (list loc_child)) ,parent_parent))]
                [(new_child)
                  (term (node ,name_child ,ls_child ,kids_child ,loc_parent))])
    (if (member loc_child kids_parent)
        store
        (update-store (update-store store loc_parent new_parent) loc_child new_child))))
                
  

(define-struct testparams
  (num-tests num-nodes num-addListeners num-removeListeners num-setHandlers num-clearHandlers num-dispatches))

(define (nth n lst)
  (first (list-tail lst (- n 1))))
(define (random-in-list lst)
  (nth (+ 1 (random (length lst))) lst))

(define (make-tree root locs store)
  (cond
    [(empty? locs) store]
    [else
     (case (random 2)
       ; done with current node
       ['0 (let ((root-node (lookup root store)))
             (if (equal? (fifth root-node) 'null)
                 (make-tree root locs store) ; this node has no parent; try again
                 (make-tree (fifth root-node) locs store)))]
       ; make next node a subtree
       ['1 (let ((new-store (appendChild root (first locs) store)))
            (make-tree (first locs) (rest locs) new-store))])]))

(define (make-prog number-of-tests events node-locs listener-locs handlers)
  (let ([addListeners (build-list (testparams-num-addListeners number-of-tests) 
                                  (lambda (i)
                                    (term (addEventListener
                                           ,(random-in-list node-locs)
                                           ,(random-in-list events)
                                           ,(rand-bool)
                                           ,(random-in-list listener-locs)))))]
        [remListeners (build-list (testparams-num-removeListeners number-of-tests) 
                                  (lambda (i)
                                    (term (removeEventListener
                                           ,(random-in-list node-locs)
                                           ,(random-in-list events)
                                           ,(rand-bool)
                                           ,(random-in-list listener-locs)))))]
        [setHandlers (build-list (testparams-num-setHandlers number-of-tests) 
                                 (lambda (i)
                                   (term (setEventHandler
                                          ,(random-in-list node-locs)
                                          ,(random-in-list events)
                                          ,(random-in-list handlers)))))]
        [clearHandlers (build-list (testparams-num-clearHandlers number-of-tests) 
                                   (lambda (i)
                                     (term (removeEventHandler
                                            ,(random-in-list node-locs)
                                            ,(random-in-list events)))))]
        [dispatches (build-list (testparams-num-dispatches number-of-tests)
                                (lambda (i)
                                  (term (pre-dispatch 
                                         ,(random-in-list (rest node-locs)) ; we don't dispatch correctly to root nodes
                                         ,empty
                                         (event ,(random-in-list events)
                                                ,(rand-bool)
                                                ,(rand-bool)
                                                #t 
                                                ,empty
                                                loc_skip)))))])
    (make-seq (shuffle (append addListeners remListeners setHandlers clearHandlers dispatches)))))

(define (make-listener i number-of-tests events node-locs listener-locs)
  (make-seq
   (term 
    (
     (debug-print ,(string-append "Starting listener " (number->string i)))
     ,(if (rand-bool)
          (term (addEventListener 
                 ,(random-in-list node-locs)
                 ,(random-in-list events)
                 ,(rand-bool) 
                 ,(random-in-list listener-locs)))
          (term skip))
     ,(if (rand-bool)
          (term (removeEventListener 
                 ,(random-in-list node-locs)
                 ,(random-in-list events)
                 ,(rand-bool) 
                 ,(random-in-list listener-locs)))
          (term skip))
     ,(if (and (rand-bool) (> (testparams-num-setHandlers number-of-tests) 0))
          (term (setEventHandler
                 ,(random-in-list node-locs)
                 ,(random-in-list events)
                 ,(make-handler (- 0 i) number-of-tests events node-locs listener-locs)))
          (term skip))
     ,(if (and (rand-bool) (> (testparams-num-clearHandlers number-of-tests) 0))
          (term (removeEventHandler
                 ,(random-in-list node-locs)
                 ,(random-in-list events)))
          (term skip))
     (debug-print ,(string-append "Finished listener " (number->string i)))
     ))))

(define (make-handler i number-of-tests events node-locs listener-locs)
  (make-seq
   (term 
    (
     (debug-print ,(string-append "Starting handler " (number->string i)))
     ,(if (rand-bool)
          (term (addEventListener 
                 ,(random-in-list node-locs)
                 ,(random-in-list events)
                 ,(rand-bool) 
                 ,(random-in-list listener-locs)))
          (term skip))
     ,(if (rand-bool)
          (term (removeEventListener 
                 ,(random-in-list node-locs)
                 ,(random-in-list events)
                 ,(rand-bool) 
                 ,(random-in-list listener-locs)))
          (term skip))
    ,(if (and (rand-bool) (> (testparams-num-setHandlers number-of-tests) 0))
          (term (setEventHandler
                 ,(random-in-list node-locs)
                 ,(random-in-list events)
                 ,(make-handler (- 0 i) number-of-tests events node-locs listener-locs)))
          (term skip))
     ,(if (and (rand-bool) (> (testparams-num-clearHandlers number-of-tests) 0))
          (term (removeEventHandler
                 ,(random-in-list node-locs)
                 ,(random-in-list events)))
          (term skip))
     (debug-print ,(string-append "Finished handler " (number->string i)))
     (return ,(rand-bool))
     ))))

(define (generate-tests number-of-tests)
  (build-list (testparams-num-tests number-of-tests) (lambda (i) (gen-test number-of-tests))))
(define (gen-test number-of-tests)
  (let* ([node-locs (build-list (testparams-num-nodes number-of-tests) (lambda (i) (gensym "loc_node")))]
         [init-store (map (lambda (loc) (list loc (term (node "div" ,empty ,empty null)))) node-locs)]
         [tree-store (make-tree (first node-locs) (rest node-locs) init-store)]
         [listener-locs (build-list (testparams-num-addListeners number-of-tests) (lambda (i) (gensym "loc_listener")))]
         [events (list "click")]
         [listeners (build-list (testparams-num-addListeners number-of-tests) (lambda (i) (make-listener (+ 1 i) number-of-tests events node-locs listener-locs)))]
         [handlers (build-list (testparams-num-setHandlers number-of-tests) (lambda (i) (make-handler (+ 1 i) number-of-tests events node-locs listener-locs)))]
         [setup (make-prog number-of-tests events node-locs listener-locs handlers)]
         [js-store 
          (update-store
           (foldl (lambda (loc listener store)
                    (update-store store loc listener)) tree-store listener-locs listeners)
           (term loc_skip) (term skip))]
         [model (term (state ,setup ,js-store ,empty))]
         [final (apply-reduction-relation* DOM-reduce model)]
         [final-log (last (first final))]
         [log-test (string-append "debug.checkLog(["
                                  (string-join (map (lambda (s) (string-append "\"" s "\"")) final-log) ", ")
                                  "]);")])
    (model->xexp model log-test)))

(define (save-test directory name test)
  (let ((test-name (string-append directory "/" name)))
    (if (file-exists? test-name) (delete-file test-name) empty)
    (with-output-to-file test-name
      (lambda () (displayln test)))))

(define (save-test-runner name tests)
  (if (file-exists? name) (delete-file name) empty)
  (with-output-to-file name
    (lambda ()
      (displayln "<!DOCTYPE HTML>")
      (displayln (xexp->html
                  (list 'html
                        (cons 'body
                              (map (lambda (test-name)
                                     (list 'p 
                                           (list 'a 
                                                 (list '@
                                                       (list 'href test-name))
                                                 test-name)
                                           (list 'iframe (list '@ 
                                                               (list 'src test-name)
                                                               (list 'width "100px")
                                                               (list 'height "25px")
                                                               (list 'frameborder "0"))))) tests))))))))

(define (cur-date-string)
  (let ((cur-date (current-date)))
    (format "~a.~a.~a-~a-~a"
     (date-year cur-date)
     (date-month cur-date)
     (date-day cur-date)
     (date-hour cur-date)
     (date-minute cur-date))))

(define (test-explorer explorer-directory-prefix number-of-tests)
  (let* ([tests (generate-tests number-of-tests)]
         [cur-date (cur-date-string)]
         [explorer-directory (string-append explorer-directory-prefix "." cur-date)]
         [test-root (string-append explorer-directory "/")]
         [test-files-names 
          (map (lambda (test) 
                 (list test-root (string-append (symbol->string (gensym cur-date)) ".html") test)
                 ) tests)])
    (if (not (directory-exists? explorer-directory)) (make-directory explorer-directory) empty)
    (map (lambda (test) (apply save-test test)) test-files-names)
    (save-test-runner (string-append test-root "testrun.html") (map second test-files-names))))

