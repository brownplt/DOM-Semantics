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
  ; Dispatch: event, curr node, phase, prevent default?, path, pending listeners, stack
  [D (dispatch E loc P bool (loc ...) (L ...) (S ...))]
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
                          #f
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
                          #f
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
                          #f
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
        pd-build-path)



   ; Dispatch: event, current node, phase, path, pending listeners, stack
   ; ========== reduction steps to consider ==========
   ; outer stack    - empty
   ; -------------------------------------------
   ; path & phase   - empty
   ;                - curr = second-to-last of path, phase = capture
   ;                - curr = last of path, phase = target
   ;                - curr = first of path, phase = bubble
   ; -------------------------------------------
   ; listener list  - empty, phase = capture
   ;                - empty, phase = target
   ;                - empty, phase = bubble
   ; -------------------------------------------
   ; inner stack    - empty
   ;                - nonempty

   ;;;;;;;;;; Reductions end b/c at end of outer stack ;;;;;;;;;;

   ; Done handling last dispatch on stack.
   ; TODO default handler
   ; TODO is there a real return value?
   (--> (state ((dispatch E loc_current bubble bool (loc_current loc ...) () ()))
               N-store)
        #t)

   ; Done handling last dispatch on stack and stopProp[Immediate] was called.
   ; TODO default handler
   ; TODO is there a real return value?
   (--> (state ((dispatch E loc P bool () () ()))
               N-store)
        #t)

   ;;;;;;;;;; Dispatch exiting b/c at end of path or after stopProp ;;;;;;;;;;

   ; All done handling this dispatch, falling back to stack steps.
   ; TODO default handler
   ; TODO should pass back return value of dispatch call
   (--> (state ((dispatch E loc_current bubble bool (loc_current loc ...) () ())
                S ...)
               N-store)
        (state (S ...)
               N-store))
   ; All done handling dispatch (stopProp'ed), falling back to stack steps.
   ; TODO default handler
   (--> (state ((dispatch E loc P bool () () ())
                S ...)
               N-store)
        (state (S ...)
               N-store))

   ;;;;;;;;;; Dispatch moving between path nodes ;;;;;;;;;;

   ; Time to switch capture->target phase.
   ; TODO get listeners
   (--> (state ((dispatch E
                          loc_current   ; parent of target node
                          capture
                          bool
                          (loc ... loc_current loc_target)
                          ()            ; empty listener list
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_target
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        (state ((dispatch E
                          loc_target    ; target node
                          target        ; target phase
                          bool
                          (loc ... loc_current loc_target)
                          (L_t ...)     ; target phase listeners TODO filter for event
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_target
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...)))
   ; Time to switch target->bubble phase.
   ; TODO get listeners
   (--> (state ((dispatch E
                          loc_target    ; target node
                          target        ; target phase
                          bool
                          (loc ... loc_target)
                          ()
                          ())
                S ...)
                ((loc_b N_b) ...
                 (loc_target
                  (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                 (loc_a N_a) ...))
        (state ((dispatch E
                          loc_target    ; target node
                          bubble        ; bubble phase
                          bool
                          (loc ... loc_target)
                          (L_b ...)     ; bubble phase listeners TODO filter for event
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_target
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...)))
   ; Fill the pending listeners list from the next node during capture phase.
   ; TODO get listeners
   (--> (state ((dispatch E
                          loc_current   ; any node (but the second-to-last)
                          capture       ; capture phase
                          bool
                          (loc ... loc_current loc_next loc_nnext loc ...)
                          ()
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_next
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        (state ((dispatch E
                          loc_next      ; next node
                          capture       ; capture phase
                          bool
                          (loc ... loc_current loc_next loc_nnext loc ...)
                          (L_c ...)     ; bubble phase listeners TODO filter for event
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_target
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...)))
   ; Fill the pending listeners list from the next node during bubble phase.
   ; TODO get listeners
   (--> (state ((dispatch E
                          loc_current   ; any node (but the rootmost)
                          bubble        ; bubble phase
                          bool
                          (loc ... loc_next loc_current loc ...)
                          ()
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_next
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        (state ((dispatch E
                          loc_next      ; next node
                          bubble        ; bubble phase
                          bool
                          (loc ... loc_next loc_current loc ...)
                          (L_b ...)     ; bubble phase listeners TODO filter for event
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_next
                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
                (loc_a N_a) ...)))

   ;;;;;;;;;; Dispatch loading next handler, running handler steps ;;;;;;;;;;

   ; Load next listener from the listener list.
   (--> (state ((dispatch E loc_curr P bool (loc ...)
                          ((listener T P (S ...)) L ...) ; listener list
                          ())           ; empty inner stack
                S ...)
               N-store)
        (state ((dispatch E loc_curr P bool (loc ...)
                          (L ...)       ; rest of listener list
                          (S ...))      ; newly filled inner stack
                S ...)
               N-store))
   ; Deal with a stop-prop step from inner stack.
   ; TODO prevent default?
   (--> (state ((dispatch E loc_curr P bool (loc ...) (L ...) (stop-prop S ...))
                 S ...)
                N-store)
         ; Empty the path so we exit after handling this node.
         (state ((dispatch E loc_curr P bool () (L ...) (S ...))
                 S ...)
                N-store))
   ; Deal with a stop-immediate step from inner stack.
   ; TODO prevent default?
   (--> (state ((dispatch E loc_curr P bool (loc ...) (L ...) (stop-immediate S ...))
                S ...)
               N-store)
        ; Empty the path and the pending listeners on this node.
        (state ((dispatch E loc_curr P bool () () (S ...))
                S ...)
               N-store))

   ; Deal with a prevent-default step from inner stack.
   (--> (state ((dispatch E loc_curr P bool (loc ...) (L ...) (prevent-default S ...))
                S ...)
               N-store)
        (state ((dispatch E loc_curr P #t (loc ...) (L ...) (S ...))
                S ...)
               N-store))

   ; Load next listener step from the inner stack.
   (--> (state ((dispatch E loc_curr P bool (loc ...) (L ...) (S_next S ...))
                S ...)
               N-store)
        (state (S_next
                (dispatch E loc_curr P bool (loc ...) (L ...) (S ...))
                S ...)
               N-store))))
