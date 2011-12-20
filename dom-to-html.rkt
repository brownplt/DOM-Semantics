#lang racket
(require redex)
(require "redex-domv2.rkt")
(provide model->xexp xexp->html)

(define (bind-ref binds key)
  (let ((b (findf (lambda (v) (equal? (bind-name v) key))
                  (match-bindings (first binds)))))
    (if (bind? b) (bind-exp b) (raise (list "Key not found:" key)))))

(define (bind-refs binds keys)
  (if (pair? binds)
      (map (lambda (binds)
             (map (lambda (key)
                    (let ((b (findf (lambda (v) (equal? (bind-name v) key))
                                    (match-bindings binds))))
                      (if (bind? b) (bind-exp b) empty)))
                  keys))
           binds)
      empty))

(define node1 (term (node "n1" () (loc_node2) null)))
(define node2 (term (node "n2" () () loc_node1)))
(define node3 (term (node "n3" () (loc_node4 loc_node5) null)))
(define node4 (term (node "n4" () () loc_node3)))
(define node5 (term (node "n5" () () loc_node3)))
(define node6 (term (node "n6" () () null)))
(define store
  (term ((loc_node1 ,node1)
         (loc_node2 ,node2)
         (loc_node3 ,node3)
         (loc_node4 ,node4)
         (loc_node5 ,node5)
         (loc_node6 ,node6))))


(define store2
  (term 
   ((loc_child
    (node
     "child"
     ((("click" target) ((listener #t loc-l1) (listener #f loc-l2)))
      (("click" capture) ((listener #t loc-l1) (listener #f loc-l2)))
      (("click" bubble) ()))
     ()
     loc_mid))
   (loc_mid (node "middle" 
                  ((("keydown" target) ((handler (debug-print "H3")) (listener #t loc-skip)))
                   (("keyup" target) ((handler (debug-print "H4")))))
                  (loc_child) loc_parent))
   (loc_parent (node "parent" 
                     ()
                     (loc_mid) null))
   (loc-l1 (seq (debug-print "L1") skip))
   (loc-l2 (debug-print "L2"))
   (loc-skip skip))))
   


(define (indent s)
  (regexp-replace* #rx"(?m:^)" s "  "))
(define (loc->id s)
  (regexp-replace* #rx"-" s "_"))
(define (bool->string b)
  (if b "true" "false"))
(define (single-line s)
  (regexp-replace* #rx"\n" s "  "))

(define (xexp->html sxml)
  (let* ([tagname (first sxml)]
         [attrsPresent (and (> (length sxml) 1)
                            (list? (second sxml))
                            (equal? '@ (first (second sxml))))]
         [attrs (if attrsPresent
                    (rest (second sxml))
                    empty)]
         [body (if attrsPresent (cddr sxml) (cdr sxml))])
    (string-append
     "<" (symbol->string tagname)
     (apply 
      string-append 
      (map (lambda (attr) (string-append " " (symbol->string (first attr))
                                         "=\"" (second attr) "\"")) attrs))
     ">\n"
     (if (and (> (length body) 0)
              (string? (first body)))
         (indent (first body))
         (string-join (map indent (map xexp->html body)) "\n"))
     "\n</" (symbol->string tagname) ">")))


(define-metafunction DOM
  [(s->js (seq S_1 S_2))
   ,(string-append (term (s->js S_1)) "\n" (term (s->js S_2)))]
  [(s->js (return bool))
   ,(string-append "return " (bool->string (term bool)) ";")]
  [(s->js skip)
   "/* skip */"]
  [(s->js stop-prop)
   "event.stopPropagation();"]
  [(s->js stop-immediate)
   "event.stopImmediatePropagation();"]
  [(s->js prevent-default)
   "event.preventDefault();"]
  [(s->js mutate)
   "debug.print(\"mutation\");"]
  [(s->js (debug-print string))
   ,(string-append "debug.print(\"" (term string) "\");")]
  [(s->js (addEventListener loc_target T bool loc_listener))
   ,(string-append 
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").addEventListener(\""
     (term T)
     "\", "
     (loc->id (symbol->string (term loc_listener)))
     ", "
     (bool->string (term bool))
     ");")]
  [(s->js (removeEventListener loc_target T bool loc_listener))
   ,(string-append 
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").removeEventListener(\""
     (term T)
     "\", "
     (loc->id (symbol->string (term loc_listener)))
     ", "
     (bool->string (term bool))
     ");")]  
  [(s->js (setEventHandler loc_target T S))
   ,(string-append 
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").setAttribute(\"on"
     (term T)
     "\", \""
     ;(single-line (regexp-replace* #rx"(?m:\")" (term (s->js S)) "\\\\\""))
     (single-line
      (string-join (regexp-split (regexp "[\"]") (string-join (regexp-split (regexp "[\\]")
                                                                            (term (s->js S))) "\\\\"))
                   "\\\""))
     "\");")]
  [(s->js (removeEventHandler loc_target T))
   ,(string-append
     "document.getElementById(\""
     (symbol->string (term loc_target))
     "\").removeAttribute(\"on"
     (term T)
     "\");")]
  [(s->js (if-phase P S_t S_f))
   ,(string-append
     "if (event.eventPhase === "
     (case (term P)
       [(term capture) "Event.CAPTURING_PHASE"]
       [(term target) "Event.AT_TARGET"]
       [(term bubble) "Event.BUBBLING_PHASE"])
     ") {\n"
     (indent (term (s->js S_t)))
     "\n} else {\n"
     (indent (term (s->js S_f)))
     "\n}")]
  [(s->js (if-curTarget loc S_t S_f))
   ,(string-append
     "if (event.currentTarget === document.getElementById(\""
     (symbol->string (term loc))
     "\")) {\n"
     (indent (term (s->js S_t)))
     "\n} else {\n"
     (indent (term (s->js S_f)))
     "\n}")]
  [(s->js (pre-dispatch loc () (event T Bubbles Cancels Trusted Meta loc_default)))
   ,(string-append
     "var evt = document.createEvent(\"Event\");\n"
     "evt.initEvent(\"" (term T) "\", "
     (bool->string (term Bubbles))
     ", "
     (bool->string (term Cancels))
     ");\n"
     "document.getElementById(\"" (symbol->string (term loc)) "\").dispatchEvent(evt);\n")]
  [(s->js any)
   ,(let* ((string (open-output-string))
           (stmt-as-string (begin (displayln (term any) string) (get-output-string string))))
      (string-append
       "/* unknown internal statement */\n"
       "/*\n  "
       stmt-as-string
       "*/"))]
  )

(define (handlers-if-any ls)
  (let* ([target-phases (or
                         (redex-match DOM ((TP_a (HL_a ...)) ...
                                           ((T target) ((listener bool_a loc_a) ...
                                                        (handler S)
                                                        (listener bool_b loc_b) ...))
                                           (TP_b (HL_b ...)) ...) ls)
                         empty)]
         [handlers (bind-refs target-phases (list 'T 'S))]
         [handlerAttrs (map (lambda (h) (list (string->symbol (string-append "on" (first h)))
                                              (term (s->js ,(second h))))) handlers)])
    handlerAttrs)
  )

(define (model->xexp model . extras)
  (let* ([program (second model)]
         [store (third model)]
         [top-node-matches (redex-match DOM 
                                        ((loc_a N_a) ...
                                         (loc_top (name node_top (node string LS (loc ...) null)))
                                         (loc_b N_b) ...
                                         (loc_c S_c) ...)
                                        store)]
         [all-locs (bind-ref (redex-match DOM 
                                           ((loc_a N_a) ...
                                            (loc_c S_c)...)
                                           store) 'loc_a)]
         [top-locs (map (lambda (m) (bind-exp
                                     (findf (lambda (v) (equal? (bind-name v) 'loc_top))
                                            (match-bindings m)))) top-node-matches)]
         [lookup-node (lambda (loc)
                        (second (findf (lambda (p) (equal? (first p) loc)) store)))]
         [lookup-listener (lambda (loc) ;same as lookup-node's implementation
                            (second (findf (lambda (p) (equal? (first p) loc)) store)))]
         [node-to-xexp 
          (letrec ((node-to-xexp
                    (lambda (loc)
                         (let* ((node (lookup-node loc))
                                (binds (redex-match DOM (node string_name LS (loc_kid ...) parent) node)))
                           (append
                            (list (string->symbol (bind-ref binds 'string_name)))
                            (list 
                             (append
                              (list '@
                                    (list 'id (symbol->string loc))
                                    (list 'style "display: none;"))
                              (handlers-if-any (bind-ref binds 'LS))))
                            (map node-to-xexp (bind-ref binds 'loc_kid)))
                           ))))
            node-to-xexp)]
         [listeners (bind-ref (redex-match DOM 
                                           ((loc_a N_a) ...
                                            (name ls (loc_c S_c)) ...)
                                           store) 'ls)]
         [define-listeners
           (map
              (lambda (l)
                (let ([loc (first l)]
                      [body (second l)])
                  (string-append "\nfunction " 
                                 (loc->id (symbol->string loc))
                                 "(event) {\n"
                                 (indent (term (s->js ,body)))
                                 "\n}\n")))
              listeners
             )]
         [install-listeners 
          (lambda (loc)
            (let* ([node (lookup-node loc)]
                   [binds (redex-match DOM (node string_name LS (loc_kid ...) parent) node)]
                   [all-targets (or
                                 (redex-match DOM ((TP_a (HL_a ...)) ...
                                                   ((T target) ((handler S_h) HL ...))
                                                   (TP_b (HL_b ...)) ...) (bind-ref binds 'LS))
                                 (redex-match DOM ((TP_a (HL_a ...)) ...
                                                   ((T target) (HL ...))
                                                   (TP_b (HL_b ...)) ...) (bind-ref binds 'LS))
                                 empty)]
                   [target-lists (bind-refs all-targets (list 'T 'HL))]
                   [targets 
                    (append-map (lambda (target)
                                  (let ([type (first target)]
                                        [listeners (second target)])
                                    (map (lambda (t)
                                           (append-map 
                                            (lambda (parts) 
                                              (let ((ret (list 
                                                          type
                                                          (first parts)
                                                          (lookup-listener (second parts))
                                                          (loc->id (symbol->string (second parts)))
                                                          )))
                                                ret))
                                            (bind-refs (redex-match DOM (listener bool loc) t)
                                                       (list 'bool 'loc))))
                                         listeners)))
                                target-lists)]
                   [listener-to-js 
                    (lambda (type useCapture body fnName)
                      (let ((js (string-append "document.getElementById(\""
                                               (symbol->string loc)
                                               "\").addEventListener(\""
                                               type
                                               "\", "
                                               (bool->string (term ,useCapture))
                                               ", "
                                               fnName
                                               ");\n")))
                        js))]
                     )
              (apply string-append (map (lambda (t) (if (null? t) "" (apply listener-to-js t))) targets))))]
         [scaffolding #<<SCAFFOLDING
if (!Array.prototype.push) {
  Array.prototype.push = function (item) {
    this[this.length] = item;
    return this.length;
  }
}
// From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/indexOf
if (!Array.prototype.indexOf) {  
  Array.prototype.indexOf = function (searchElement /*, fromIndex */ ) {  
    "use strict";  
    if (this === void 0 || this === null) {  
      throw new TypeError();  
    }  
    var t = Object(this);  
    var len = t.length >>> 0;  
    if (len === 0) {  
      return -1;  
    }  
    var n = 0;  
    if (arguments.length > 0) {  
      n = Number(arguments[1]);  
      if (n !== n) { // shortcut for verifying if it's NaN  
        n = 0;  
      } else if (n !== 0 && n !== Infinity && n !== -Infinity) {  
        n = (n > 0 || -1) * Math.floor(Math.abs(n));  
      }  
    }  
    if (n >= len) {  
      return -1;  
    }  
    var k = n >= 0 ? n : Math.max(len - Math.abs(n), 0);  
    for (; k < len; k++) {  
      if (k in t && t[k] === searchElement) {  
        return k;  
      }  
    }  
    return -1;  
  }  
}
// Scaffolding
var debug = {
  log: [],
  print: function (msg) {
    debug.log.push(msg);
  },
  inLog: function (msg) {
    return (debug.log.indexOf(msg) != -1);
  },
  emptyLog: function (msg) {
    return (debug.log.length > 0);
  },
  clearLog: function() {
    debug.log = [];
  },
  printResult: function (msg) {
    var txt = document.createElement("p");
    txt.setAttribute("style", "margin: 0px;");
    txt.textContent = msg;
    document.body.appendChild(txt);
  },
  checkOutput: function (expected, msg) {
    var result = ((!!expected) == (!!debug.inLog(msg))) ? "PASSED" : "FAILED";
    debug.printResult(result);
    if (result === "FAILED") {
      for (var i = 0; i < debug.log.length; i++) {
        debug.printResult(debug.log[i]);
      }
    }
  },
  checkLog: function (expected) {
    var result = (expected.length === debug.log.length);
    if (result) {
      for (var i = 0; i < debug.log.length; i++) {
        result = result && (debug.log[i] === expected[i]);
      }
    }
    debug.printResult(result ? "PASSED" : "FAILED");
    if (!result) {
      debug.printResult("Expected:");
      for (var i = 0; i < expected.length; i++) {
        debug.printResult(expected[i]);
      }
      debug.printResult("Actual:");
      for (var i = 0; i < debug.log.length; i++) {
        debug.printResult(debug.log[i]);
      }
    }
  }
}
SCAFFOLDING
                      ]
         )
    (string-append
     "<!DOCTYPE HTML>\n"
     (xexp->html
      (list (string->symbol "html")
            (append
             (list 'body (list '@ (list 'style "margin: 0px;")))
             (map node-to-xexp top-locs)
             (list (list 'script scaffolding))
             (list (list 'script (apply string-append define-listeners)))
             (list (list 'script (string-append "\n" (apply string-append (map install-listeners all-locs)))))
             (list (list 'script (string-append "\n" (term (s->js ,program)))))
            (map (lambda (str) (list 'script str)) extras)))))))
