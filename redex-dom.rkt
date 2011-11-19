#lang racket
(require redex)
(provide DOM DOM-reduce)

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
  ; Predispatch: target node, path, event
  [PD (pre-dispatch loc (loc ...) E)]
  ; Dispatch: event, current node, phase, path, pending listeners
  [D (dispatch E loc P (loc ...) (L ...))]
  [S stop-prop
     stop-immediate
     prevent-default 
     mutate
     PD
     D]
  ; Machine state
  [N-store ((loc_!_ N) ...)]
  [M (state (S ...) N-store)])

(define DOM-reduce
  (reduction-relation
   DOM
   
   ; Path building complete, transition to dispatch
   (--> (state ((pre-dispatch loc_current (loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) null))
                (loc_a N_a) ...))
        (state ((dispatch E loc_current capture (loc_current loc ...) (L_c ...))
                S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) null))
                (loc_a N_a) ...))
        pd-finish-path)
   
   ; Building path in pre-dispatch
   (--> (state ((pre-dispatch loc_current (loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        (state ((pre-dispatch loc_parent (loc_current loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        pd-build-path)))