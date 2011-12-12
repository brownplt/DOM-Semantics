#lang racket
(require redex)
(require "readxml.rkt")
(require "redex-domv2.rkt")
(require xml)
(require (planet neil/json-parsing:1:=2))

(define-syntax test
  (syntax-rules ()
    [(test pattern exp name)
     (let ((result-str
            (if (redex-match DOM pattern exp) "passed" "FAILED!!!!")))
       (displayln (string-append name ": " result-str)))]))

(define test-xml
  (xml->xexpr (document-element
               (read-xml (open-input-file "sample-data/TARGETED.xml")))))

(define test-json
  (rest (json->sxml (open-input-file "sample-data/TARGETED.json"))))

(create-dom test-xml (first test-json) (term null))

; Test each item in the loc-store to ensure it is a listener or a node.
(for-each (lambda (loc-plus)
            (if (or (redex-match DOM N (second loc-plus))
                    (redex-match DOM S (second loc-plus)))
                (begin '())
                (begin (display "FAILED: ")
                       (display (second loc-plus))
                       (newline))))
          loc-store)

; Test the node store to make sure it satisfies the matching requirements.
(test N-store loc-store "loc-store")

; Find the TARGETED node
(define targeted
  (filter (lambda (loc-plus)
          (and (redex-match DOM N (second loc-plus))
               (list? (regexp-match-positions "TARGETED" ;(symbol->string 'loc_4093)
                                              (second (second loc-plus))))))
        loc-store))

; For use with the test data below.
;(extract-children-as-json   test-json)
;(extract-listeners-as-redex test-json)
;(extract-children-as-json   (first (extract-children-as-json test-json)))
;(extract-listeners-as-redex (first (extract-children-as-json test-json)))

;;;;; OLD TEST DATA ;;;;;

;'(window
;  ((chromehidden "")
;    (height "1080")
;    (id "messengerWindow")
;    (lightweightthemes "true")
;    (lightweightthemesfooter "status-bar")
;    (onload "OnLoadMessenger()")
;    (onunload "OnUnloadMessenger()")
;    (persist "width height screenX screenY sizemode")
;    (screenX "0")
;    (screenY "0")
;    (sizemode "normal")
;    (title "Inbox - Mozilla Thunderbird")
;    (titlemenuseparator " - ")
;    (titlemodifier "Mozilla Thunderbird")
;    (toggletoolbar "true")
;    (width "1024")
;    (windowtype "mail:3pane")
;    (xmlns "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"))
;  "\r\n  "
;  (data
;    ((defaultset
;      "button-getmsg,button-print,separator,button-goback,button-goforward,spring,gloda-search")
;     (id "mail-bar2")
;     (persist "currentset iconsize labelalign")))
;  "\r\n  "
;  (stringbundleset
;   ((id "stringbundleset"))
;   "\r\n  ")
;  (commandset
;   ((commandupdater "true")
;    (events "create-menu-file")
;    (id "mailFileMenuItems")
;    (oncommandupdate "goUpdateMailMenuItems(this)"))
;   "\r\n      ")))

;'((object
;  (member (@ (name "listeners"))  (object))
;  (member (@ (name "tagName"))    (string "window"))
;  (member (@ (name "id"))         (string "messengerWindow"))
;  (member (@ (name "class"))      (string ""))
;  (member (@ (name "children"))
;   (object
;    (member
;     (@ (name "0"))
;     (object
;      (member (@ (name "listeners"))
;              (object (member (@ (name "0"))
;                              (object (member
;                                       (@ (name "source"))
;                                       (string "function oncommandupdate(event) {goUpdateMailMenuItems(this);}"))
;                                      (member (@ (name "type"))     (string "commandupdate"))
;                                      (member (@ (name "capture"))  (false))))))
;      (member (@ (name "tagName"))    (string "commandset"))
;      (member (@ (name "id"))         (string "mailFileMenuItems"))
;      (member (@ (name "class"))      (string ""))
;      (member (@ (name "children"))   (object))))
;    (member
;     (@ (name "1"))
;     (object
;      (member (@ (name "listeners"))  (object))
;      (member (@ (name "tagName"))    (string "stringbundleset"))
;      (member (@ (name "id"))         (string "stringbundleset"))
;      (member (@ (name "class"))      (string ""))
;      (member (@ (name "children"))   (object)))))))))
