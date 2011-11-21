#lang racket
(require redex)
(provide DOM DOM-reduce)

(define-language DOM
  [bool #t #f]
  ; Event types
  [T click keydown]
  ; Event: type, bubble, cancelable, trusted
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
  ; Predispatch: target node, path, event
  [PD (pre-dispatch loc (loc ...) E)]
  ; Dispatch: event, current node, phase, path, pending listeners, stack
  [D (dispatch E loc P (loc ...) (L ...) (S ...))]
  ; Listener steps
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
   ; - Case: root node has no capture-phase listeners
   (--> (state ((pre-dispatch loc_current (loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node () (L_t ...) (L_b ...) (loc_children ...) null))
                (loc_a N_a) ...))
        ; Just load the empty list to the dispatch pending listeners and stack, 
        ; then another reduction will transition to the next path node
        (state ((dispatch E
                          loc_current
                          capture
                          (loc_current loc ...)
                          ()
                          ()) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node () (L_t ...) (L_b ...) (loc_children ...) null))
                (loc_a N_a) ...))
        pd-finish-no-listeners)
   ; - Case: root node has capture-phase listeners, and the first one
   ; is of the same event type as the current event
   (--> (state ((pre-dispatch loc_current 
                              (loc ...) 
                              (event T bool_1 bool_2 bool_3)) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node ((listener T P (S_inner ...)) L_c ...) 
                       (L_t ...) 
                       (L_b ...) 
                       (loc_children ...) 
                       null))
                (loc_a N_a) ...))
        (state ((dispatch (event T bool_1 bool_2 bool_3)
                          loc_current 
                          capture 
                          (loc_current loc ...)
                          (L_c ...)
                          (S_inner ...)) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node ((listener T P (S_inner ...)) L_c ...) 
                       (L_t ...) 
                       (L_b ...) 
                       (loc_children ...) 
                       null))
                (loc_a N_a) ...))
        pd-finish-dofirst)
   ; - Case: root node has some capture-phase listeners, but the first one
   ; is not of the same type as the current event
   (--> (state ((pre-dispatch loc_current 
                              (loc ...) 
                              (name E (event T_!_1 bool_1 bool_2 bool_3))) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node ((name L_cfirst (listener T_!_1 P (S_inner ...))) L_c ...)
                       (L_t ...) 
                       (L_b ...)
                       (loc_children ...)
                       null))
                (loc_a N_a) ...))
        ; Load the rest of the capture-phase listeners, but ignore the stack of
        ; the first listener
        (state ((dispatch E
                          loc_current
                          capture
                          (loc_current loc ...)
                          (L_c ...)
                          ()))
               ((loc_b N_b) ...
                (loc_current
                 (node (L_cfirst L_c ...) (L_t ...) (L_b ...) (loc_children ...) null))
                (loc_a N_a) ...))
        pd-finish-skipfirst)
                          
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