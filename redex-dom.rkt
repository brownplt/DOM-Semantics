#lang racket
(require redex)

(define-language DOM
  [bool #t #f]
  ; Event types
  [T click keydown]
  ; Event of type, bubble, cancelable, trusted
  [E (event T bool bool bool)]
  ; Phases
  [P capture target bubble]
  ; DOM nodes
  ; This representation only contains a list of (phase, listener list) pairs
  [N (node (P (L ...) ...))]
  ; Event listeners
  [L (listener E P (S ...))]
  ; Listener steps
  [S stop-prop
     stop-immediate
     prevent-default 
     mutate
     ; Predispatch of target node, path, event
     (pre-dispatch N (N ...) E)
     ; Dispatch of event, current node, phase, path, pending listeners
     (dispatch E N P (N ...) (L ...))]
  ; Machine state
  [M (state (S ...) N)])
  