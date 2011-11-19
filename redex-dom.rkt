#lang racket
(require redex)
(provide DOM DOM)

(define-language DOM
  [bool #t #f]
  ; Event types
  [T click keydown]
  ; Event of type, bubble, cancelable, trusted
  [E (event T bool bool bool)]
  ; Phases
  [P capture target bubble]
  ; Locations are like pointers.  The machine state maintains
  ; a list of (location, DOM node) pairs.
  [loc (variable-prefix loc)]
  ; parent is possibly null (parent of root node)
  [parent null loc]
  ; DOM nodes contain lists of listeners for each of the 3 phases
  ; (capture, target, bubble), a list of children, and a parent
  [N (node (L ...) (L ...) (L ...) (loc ...) parent)]
  ; Event listeners
  [L (listener T P (S ...))]
  ; Listener steps
  [S stop-prop
     stop-immediate
     prevent-default 
     mutate
     ; Predispatch of target node, path, event
     (pre-dispatch loc (loc ...) E)
     ; Dispatch of event, current node, phase, path, pending listeners
     (dispatch E loc P (loc ...) (L ...))]
  ; Machine state
  [N-store ((loc_!_ N) ...)]
  [M (state (S ...) N-store)])