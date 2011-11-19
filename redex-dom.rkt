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
  ; DOM nodes are either null (for the parent of the root)
  ; or a node - consisting of a list of listeners for the capture phase,
  ; listeners for target phase, listeners for bubble phase, list of children,
  ; and reference to the parent node.
  [N null-node 
     (node (L ...) (L ...) (L ...) (N ...) N)]
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