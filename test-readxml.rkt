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
               (read-xml (open-input-file "../tbird.xml")))))

(define test-json
  (rest (json->sxml (open-input-file "../tbird-events.json"))))

(create-dom test-xml (first test-json) (term null))

; Test each node to see if it satisfies the matching requirements.
(for-each (lambda(loc-and-node)
            (test N (second loc-and-node) "node"))
          loc-to-node)

; Test the node store to make sure it satisfies the matching requirements.
(test N-store loc-to-node "node store")

; Note: The listener list associated with a node will not always match
;       the LS pattern because some (type phase) pairs are the same for the
;       same nodes, and we should therefore be merging these lists. However,
;       this is not what we talked about, so I am deferring the work until
;       I can ask about it again.

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
