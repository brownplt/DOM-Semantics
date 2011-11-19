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
  ; DOM nodes
  ; This representation contains a list of (phase, listener list) pairs, a 
  ; list of children, and a parent
  [N-listener (P (L ...))]
  [N null-node (node (node-listener ...) (N ...) N)]
  ; Event listeners
  [L (listener T P (S ...))]
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