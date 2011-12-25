#lang racket
(require redex/reduction-semantics)
(require racket/date)
(require racket/serialize)
(require racket/place)
(require "redex-domv2.rkt")
(require "dom-to-html.rkt")

(provide main)
(provide place-main)
(provide gen-test)
(provide make-testparams)

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
                
  

(define-serializable-struct testparams
  (num-tests num-nodes num-addListeners num-removeListeners num-setHandlers num-clearHandlers num-dispatches))

(define (nth n lst)
  (first (list-tail lst (- n 1))))
(define (random-in-list lst)
  (nth (+ 1 (random (length lst))) lst))

(define next-nums (make-hash))
(define (clear-next-nums)
  (for-each (lambda (key) (dict-remove! next-nums key)) (dict-keys next-nums)))
(define (next-num key)
  (let ((next-num (dict-ref next-nums key 1)))
    (dict-set! next-nums key (+ 1 next-num))
    next-num))
(define (next-sym prefix)
  (string->symbol (string-append prefix (number->string (next-num prefix)))))

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

(define (makeAddEventListener target event useCapture listener)
  (term 
   (seq
    (debug-print ,(string-append "Adding listener " (symbol->string listener) 
                                 " to node " (symbol->string target)
                                 " that is " (if useCapture "capturing" "bubbling")
                                 " for event type " event))
    (addEventListener ,target ,event ,useCapture ,listener))))
(define (makeRemoveEventListener target event useCapture listener)
  (term 
   (seq
    (debug-print ,(string-append "Removing listener " (symbol->string listener) 
                                 " from node " (symbol->string target)
                                 " that was " (if useCapture "capturing" "bubbling")
                                 " for event type " event))
    (removeEventListener ,target ,event ,useCapture ,listener))))
(define (makeSetEventHandler target event handler)
  (term 
   (seq
    (debug-print ,(string-append "Setting handler"
                                 " for event type " event
                                 " on node " (symbol->string target)
                                 " to " (second (second handler))
                                 ))
    (setEventHandler ,target ,event ,handler))))
(define (makeRemoveEventHandler target event)
  (term 
   (seq
    (debug-print ,(string-append "Clearing handler"
                                 " for event type " event
                                 " on node " (symbol->string target)
                                 ))
    (removeEventHandler ,target ,event))))

(define (make-prog number-of-tests events node-locs listener-locs handlers)
  (let ([addListeners (build-list (testparams-num-addListeners number-of-tests) 
                                  (lambda (i)
                                    (makeAddEventListener
                                     (random-in-list node-locs)
                                     (random-in-list events)
                                     (rand-bool)
                                     (random-in-list listener-locs))))]
        [remListeners (build-list (testparams-num-removeListeners number-of-tests) 
                                  (lambda (i)
                                    (makeRemoveEventListener
                                     (random-in-list node-locs)
                                     (random-in-list events)
                                     (rand-bool)
                                     (random-in-list listener-locs))))]
        [setHandlers (build-list (testparams-num-setHandlers number-of-tests) 
                                 (lambda (i)
                                   (makeSetEventHandler
                                    (random-in-list node-locs)
                                    (random-in-list events)
                                    (random-in-list handlers))))]
        [clearHandlers (build-list (testparams-num-clearHandlers number-of-tests) 
                                   (lambda (i)
                                     (makeRemoveEventHandler
                                      (random-in-list node-locs)
                                      (random-in-list events))))]
        [dispatches (build-list (testparams-num-dispatches number-of-tests)
                                (lambda (i)
                                  (let ([event (random-in-list events)]
                                        [bubbles (rand-bool)]
                                        [cancels (rand-bool)]
                                        [target (random-in-list (rest node-locs))] ; we don't dispatch correctly to root nodes
                                        )
                                  (term 
                                   (seq
                                    (debug-print ,(string-append "Dispatching a "
                                                                 (if bubbles "bubbling, " "non-bubbling, ")
                                                                 (if cancels "cancelable " "non-cancelable ")
                                                                 event " event to " (symbol->string target)))
                                    (pre-dispatch 
                                     ,target 
                                     ,empty
                                     (event ,event
                                            ,bubbles
                                            ,cancels
                                            #t 
                                            ,empty
                                            loc_skip)))))))])
    (make-seq (shuffle (append addListeners remListeners setHandlers clearHandlers dispatches)))))

(define (make-listener number-of-tests events node-locs listener-locs)
  (let ((listener-num (number->string (next-num "listener"))))
    (make-seq
     (term 
      (
       (debug-print ,(string-append "Starting listener " listener-num))
       ,(if (and (rand-bool) (> (testparams-num-addListeners number-of-tests) 0))
            (makeAddEventListener 
             (random-in-list node-locs)
             (random-in-list events)
             (rand-bool) 
             (random-in-list listener-locs))
            (term skip))
       ,(if (and (rand-bool) (> (testparams-num-removeListeners number-of-tests) 0))
            (makeRemoveEventListener 
             (random-in-list node-locs)
             (random-in-list events)
             (rand-bool) 
             (random-in-list listener-locs))
            (term skip))
       ,(if (and (rand-bool) (> (testparams-num-setHandlers number-of-tests) 0))
            (makeSetEventHandler
             (random-in-list node-locs)
             (random-in-list events)
             (make-handler number-of-tests events node-locs listener-locs))
            (term skip))
       ,(if (and (rand-bool) (> (testparams-num-clearHandlers number-of-tests) 0))
            (makeRemoveEventHandler
             (random-in-list node-locs)
             (random-in-list events))
            (term skip))
       (debug-print ,(string-append "Finished listener " listener-num))
     )))))

(define (make-handler number-of-tests events node-locs listener-locs)
  (let ((handler-num (number->string (next-num "handler"))))
    (make-seq
     (term 
      (
       (debug-print ,(string-append "Starting handler " handler-num))
       ,(if (and (rand-bool) (> (testparams-num-addListeners number-of-tests) 0))
            (makeAddEventListener 
             (random-in-list node-locs)
             (random-in-list events)
             (rand-bool) 
             (random-in-list listener-locs))
            (term skip))
       ,(if (and (rand-bool) (> (testparams-num-removeListeners number-of-tests) 0))
            (makeRemoveEventListener 
             (random-in-list node-locs)
             (random-in-list events)
             (rand-bool) 
             (random-in-list listener-locs))
            (term skip))
       ,(if (and (rand-bool) (> (testparams-num-setHandlers number-of-tests) 0))
            (makeSetEventHandler
             (random-in-list node-locs)
             (random-in-list events)
             (make-handler number-of-tests events node-locs listener-locs))
            (term skip))
       ,(if (and (rand-bool) (> (testparams-num-clearHandlers number-of-tests) 0))
            (makeRemoveEventHandler
             (random-in-list node-locs)
             (random-in-list events))
            (term skip))
       (debug-print ,(string-append "Finished handler " handler-num))
       (return ,(rand-bool))
       )))))
  
(define (generate-tests number-of-tests)
  (build-list (testparams-num-tests number-of-tests) (lambda (i) (gen-test number-of-tests))))
(define (gen-test number-of-tests)
  (clear-next-nums)
  (let* ([node-locs (build-list (testparams-num-nodes number-of-tests) (lambda (i) (next-sym "loc_node")))]
         [init-store (map (lambda (loc) (list loc (term (node "div" ,empty ,empty null)))) node-locs)]
         [tree-store (make-tree (first node-locs) (rest node-locs) init-store)]
         [listener-locs (build-list (testparams-num-addListeners number-of-tests) (lambda (i) (next-sym "loc_listener")))]
         [events (list "click")]
         [listeners (build-list (testparams-num-addListeners number-of-tests) (lambda (i) (make-listener number-of-tests events node-locs listener-locs)))]
         [handlers (build-list (testparams-num-setHandlers number-of-tests) (lambda (i) (make-handler number-of-tests events node-locs listener-locs)))]
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
    (list model (model->xexp model log-test))))

(define (save-test directory html-name rkt-name model test)
  (let ((test-name (string-append directory "/" html-name))
        (model-name (string-append directory "/" rkt-name)))
    (if (file-exists? test-name) (delete-file test-name) empty)
    (if (file-exists? model-name) (delete-file model-name) empty)
    (with-output-to-file test-name
      (lambda () (displayln test)))
    (with-output-to-file model-name
      (lambda () (write (serialize model))))))


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
  (let-values ([(directory prefix isDir) (split-path explorer-directory-prefix)])
    (let* ([tests (generate-tests number-of-tests)]
           [cur-date (cur-date-string)]
           [explorer-directory (path->string (make-temporary-file 
                                              (string-append (path->string prefix) "." cur-date "-~a")
                                              'directory
                                              directory))]
           [test-root (string-append explorer-directory "/")]
           [test-files-names 
            (map (lambda (test) 
                   (let ((test-name (symbol->string (gensym cur-date))))
                     (append (list test-root (string-append test-name ".html") (string-append test-name ".rkt")) test))
                   ) tests)])
      (map (lambda (test) (apply save-test test)) test-files-names)
      (save-test-runner (string-append test-root "testrun.html") (map second test-files-names)))))
  
(define (many-tests num-suites explorer-directory-prefix number-of-tests)
  (for-each (lambda (i)
              (let ((start-time (current-seconds))
                    (finished-msg (string-append "Finished " (number->string (+ i 1)) " of " (number->string num-suites) " tests"
                                                 " in " explorer-directory-prefix)))
                (test-explorer explorer-directory-prefix number-of-tests)
                (if (< i (- num-suites 1))
                    (let ((delay (max 0 (- 61 (- (current-seconds) start-time)))))
                      (displayln (string-append finished-msg
                                                ", Waiting " (number->string delay) " seconds"))
                      (sleep delay))
                    (displayln finished-msg))))
            (build-list num-suites (lambda (i) i))))

(define (place-main ch)
  (let ([args-to-manytests (deserialize (place-channel-get ch))])
    (apply many-tests args-to-manytests)))

(define (main)
  (random-seed (modulo (current-milliseconds) (sub1 (expt 2 31))))
  (let* ([test-dir (lambda (dir) (path->string (build-path (current-directory) dir)))]
         [pls (for/list ([i (in-range 4)])
                (dynamic-place (build-path (current-directory) "generate-tests.rkt") 'place-main))]
         [args (list
                (list 1 (test-dir "test-listeners-handlers-par") (make-testparams 500 10 4 2 2 2 1))
                (list 1 (test-dir "test-listeners-handlers-par") (make-testparams 500 10 4 2 2 2 1))
                (list 1 (test-dir "test-listeners-handlers-par") (make-testparams 500 10 4 2 2 2 1))
                (list 1 (test-dir "test-listeners-handlers-par") (make-testparams 500 10 4 2 2 2 1)))])
    (for ([arg (in-list args)]
          [p pls])
      (place-channel-put p (serialize arg)))
    (map place-wait pls)))
  