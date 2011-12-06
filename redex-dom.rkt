#lang racket
(require redex)
(provide DOM DOM-reduce)

;; TODO: remove T and P from listener/handler
;;       add node name to node

(define (all-unique? l) 
  (equal? (remove-duplicates l) l))

(define-language DOM
  [bool #t #f]
  ; Event types
  [T string]
  ; Event: type, bubble, cancelable, trusted
  [E (event T bool bool bool)]
  ; Phases
  [P capture target bubble]
  ; Locations are like pointers.  The machine state maintains
  ; a list of (location, DOM node) pairs.
  [loc (variable-prefix loc)]
  ; parent is possibly null (parent of root node)
  [parent null loc]
  ; Node: node name, listeners, children, parent
  [N (node LS (loc ...) parent)]
  [LS (side-condition (((name t T) PM) ...) (all-unique? (term (t ...))))]
  [PM ((P_!_ (L ...)) ...)]
  ; Event listeners
  [L (listener T P (S ...))
     (handler T P (S ...))]
  ; Predispatch: target node, path root->target, event
  [PD (pre-dispatch loc (loc ...) E)]
  ; Dispatch: event, curr node/null, phase, prevent default?, path, pending listeners, stack
  [D (dispatch E parent P bool (loc ...) (L ...) (S ...))]
  ; Listener steps
  [S stop-prop
     stop-immediate
     prevent-default
     mutate
     (debug-print string)
     PD
     D]
  ; Machine state
  [N-store ((loc_!_ N) ...)]
  [M (state (S ...) N-store)])

(define DOM-reduce
  (reduction-relation
   DOM
   
   ; Building path in pre-dispatch
   (--> (state ((pre-dispatch loc_current (loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node LS (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        (state ((pre-dispatch loc_parent (loc_current loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node LS (loc_children ...) loc_parent))
                (loc_a N_a) ...))
        pd-build-path)

   ; Path building complete, transition to dispatch
   (--> (state ((pre-dispatch null (loc ...) E S ...)
                N-store))
        (state ((dispatch E null capture false (loc ...) () ())
                N-store)))
   
;   (--> (state ((dispatch E loc_done P bool (loc_a ... loc_done loc_next loc_b ...) () ())
;                S ...)
;               N-store)
;        (state ((dispatch E loc_next P bool (loc_a ... loc_done loc_next loc_b ...) (stuff ) (nonsense))
;                S ...)
;               N-store))
   
   
;   ; Case where parent doesn't have any listeners for type of event
;   (--> (side-condition 
;         (state ((pre-dispatch loc_current (loc ...) (event (name t1 T) bool_1 bool_2 bool_3)) S ...)
;                ((loc_b N_b) ...
;                 (loc_current
;                  (node (((name t2 T_different) PM) ...) (loc_children ...) null))
;                 (loc_a N_a) ...)) 
;         (empty? (filter (lambda (t) (equal? t (term t1))) (term (t2 ...)))))
;        (state ((dispatch (event t1 bool_1 bool_2 bool_3)
;                          loc_current
;                          capture
;                          #f
;                          (loc_current loc ...)
;                          ()
;                          ()) S ...)
;               ((loc_b N_b) ...
;                (loc_current
;                 (node ((t2 PM) ...) (loc_children ...) null))
;                (loc_a N_a) ...))
;        pd-finish-no-listeners)
;   
;   ; Case where parent has listeners for same type as event
;   (--> (state ((pre-dispatch 
;                 loc_current 
;                 (loc ...) 
;                 (event T bool_1 bool_2 bool_3)) 
;                S ...)
;               ((loc_b N_b) ...
;                (loc_current
;                 (node ((T_before PM_before) ... 
;                        (T ((P_before (L_before ...)) ... (capture ((listener T capture (S_l ...))
;                                                                    L ...)) (P_after (L_after ...)) ...)) 
;                        (T_after PM_after) ...) (loc_children ...) null))
;                (loc_a N_a) ...))
;        (state ((dispatch (event T bool_1 bool_2 bool_3)
;                          loc_current
;                          capture
;                          #f
;                          (loc_current loc ...)
;                          (L ...)
;                          (S_l ...)) S ...)
;               ((loc_b N_b) ...
;                (loc_current
;                 (node ((T_before PM_before) ... 
;                        (T ((P_before (L_before ...)) ... (capture ((listener T capture (S_l ...))
;                                                                    L ...)) (P_after (L_after ...)) ...)) 
;                        (T_after PM_after) ...) (loc_children ...) null))
;                (loc_a N_a) ...))
;        pd-finish-listeners)

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
   (--> (state ((dispatch E loc_current bubble bool (loc_current loc ...) () ()))
               N-store)
        (not bool))

   ; Done handling last dispatch on stack and stopProp[Immediate] was called.
   ; TODO default handler
   (--> (state ((dispatch E loc P bool () () ()))
               N-store)
        (not bool))

   ;;;;;;;;;; Dispatch exiting b/c at end of path or after stopProp ;;;;;;;;;;

   ; All done handling this dispatch, falling back to stack steps.
   ; !! Cahnged to require that the listener steps stack is not empty to 
   ;    avoid conflict with previous rules!!
   ; TODO default handler
   ; TODO should pass back return value of dispatch call
   (--> (state ((dispatch E loc_current bubble bool (loc_current loc ...) () ())
                S_1 S ...)    ;made it so that this doesn't conflict with the last dispatch rule
               N-store)
        (state (S_1 S ...)
               N-store))
   ; All done handling dispatch (stopProp'ed), falling back to stack steps.
   ; TODO default handler
   (--> (state ((dispatch E loc P bool () () ())
                S_1 S ...)   ;made it so that this doesn't conflict with the last dispatch rule
               N-store)
        (state (S_1 S ...)
               N-store))

   ;;;;;;;;;; Dispatch moving between path nodes ;;;;;;;;;;
;;[T click keydown]
;[T string]
;; Event: type, bubble, cancelable, trusted
;[E (event T bool bool bool)]
   ; Time to switch capture->target phase.
   ; TODO get listeners
   (--> (state ((dispatch (event T_2 bool_1 bool_2 bool_3)
                          loc_current   ; parent of target node
                          capture
                          bool
                          (loc ... loc_current loc_target)
                          ()            ; empty listener list
                          ())
                S ...)               
               ((loc_b N_b) ...
                (loc_target
                 (node 
                  ((T_1 PM_1) ... 
                   (T_2 ((P (L ...)) ... (target (L_1 ...))))
                   (T_after PM_after) ...)
                  (loc_children ...)
                  parent))
                (loc_a N_a) ...))
        (state ((dispatch (event T_2 bool_1 bool_2 bool_3)
                          loc_target    ; target node
                          target        ; target phase
                          bool
                          (loc ... loc_current loc_target)
                          (L_1 ...)   ; target phase listeners TODO filter for event
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_target
                 (node 
                  ((T_1 PM_1) ... 
                   (T_2 ((P (L ...)) ... (target (L_1 ...))))
                   (T_after PM_after) ...)
                  (loc_children ...) parent))
                (loc_a N_a) ...)))
               
   ; Time to switch target->bubble phase.
   ; TODO get listeners
   (--> (state ((dispatch (event T_2 bool_1 bool_2 bool_3)
                          loc_target    ; target node
                          target        ; target phase
                          bool
                          (loc ... loc_target)
                          ()
                          ())
                S ...)
                ((loc_b N_b) ...
                (loc_target
                 (node 
                  ((T_1 PM_1) ... 
                   (T_2 ((P (L ...)) ... (bubble (L_1 ...))))
                   (T_after PM_after) ...)
                  (loc_children ...)
                  parent))
                (loc_a N_a) ...))
        (state ((dispatch (event T_2 bool_1 bool_2 bool_3)
                          loc_target    ; target node
                          bubble        ; bubble phase
                          bool
                          (loc ... loc_target)
                          (L_1 ...)     ; bubble phase listeners TODO filter for event
                          ())
                S ...)
               ((loc_b N_b) ...
                (loc_target
                 (node 
                  ((T_1 PM_1) ... 
                   (T_2 ((P (L ...)) ... (bubble (L_1 ...))))
                   (T_after PM_after) ...)
                  (loc_children ...)
                  parent))
                (loc_a N_a) ...)))
   
   ; The following reductions handle advancing to the next node during the
   ; capture phase:
   ;
   ;     * capture-next-node
   ;     * capture-next-none
   ;     * capture-next-skip
   
   ;;;;;;;;;;;;;;;;;;;;;
   ; capture-next-node
   ;;;;;;;;;;;;;;;;;;;;;
   ; Case: next node has listeners for the event's type
   ; AND next node has listeners for the capture phase
   (--> (state ((dispatch (name E (event T_event bool_b bool_c bool_t))
                         loc_current
                         capture
                         bool
                         (name path (loc_before ... loc_current loc_next loc_nnext loc_after ...))
                         ()
                         ())
                S ...)
               (name store 
                     ((loc_b N_b) ...
                      (loc_next
                       (node ((T_b PM_b) ...
                              (T_event
                               ((P_b (L_b ...)) ...
                                (capture ((name first-listener 
                                                (listener T capture (S_toload ...))) 
                                          L_rest ...))
                                (P_a (L_a ...)) ...))
                              (T_a PM_a) ...)
                             (loc_child ...)
                             parent))
                      (loc_a N_a) ...)))
        (state ((dispatch E
                          loc_next
                          capture
                          bool
                          path
                          (first-listener L_rest ...)
                          (S_toload ...))
                S ...)
               store)
        capture-next-node)
   
   ;;;;;;;;;;;;;;;;;;;;;
   ; capture-next-none
   ;;;;;;;;;;;;;;;;;;;;;
   ; Case: next node has listener's for the event's type but NOT for the capture phase
   (--> (state ((dispatch (name E (event T_event bool_b bool_c bool_t))
                         loc_current
                         capture
                         bool
                         (name path (loc_before ... loc_current loc_next loc_nnext loc_after ...))
                         ()
                         ())
                S ...)
               (name store 
                     ((loc_b N_b) ...
                      (loc_next
                       (node ((T_b PM_b) ...
                              (T_event
                               (((side-condition P_target
                                                 (not (equal? (term P_target) (term capture))))
                                 (L_target ...)) ...))
                              (T_a PM_a) ...)
                             (loc_child ...)
                             parent))
                      (loc_a N_a) ...)))
        (state ((dispatch E
                          loc_next
                          capture
                          bool
                          path
                          ,empty
                          ,empty)
                S ...)
               store)
        capture-next-none)
   
   ;;;;;;;;;;;;;;;;;;;;;
   ; capture-next-skip
   ;;;;;;;;;;;;;;;;;;;;;
   ; Case: next node has no listeners for the event's type
   (--> (side-condition (state ((dispatch (name E (event T_event bool_b bool_c bool_t))
                                          loc_current
                                          capture
                                          bool
                                          (name path (loc_before ... loc_current loc_next loc_nnext loc_after ...))
                                          ()
                                          ())
                                S ...)
                               (name store
                                     ((loc_b N_b) ...
                                      (loc_next
                                       (node ((T PM) ...) (loc_child ...) parent))
                                      (loc_a N_a) ...)))
                        (empty? (filter (lambda (t) (equal? t (term T_event)))
                                        (term (T ...)))))
        (state ((dispatch E
                          loc_next
                          capture
                          bool
                          path
                          ,empty
                          ,empty)
                S ...)
               store)
        capture-next-skip)
   
   ; The following reductions handle advancing to the next node during the
   ; bubble phase:
   ;
   ;     * bubble-next-node
   ;     * bubble-next-none
   ;     * bubble-next-skip
   
   ;;;;;;;;;;;;;;;;;;;;;
   ; bubble-next-node
   ;;;;;;;;;;;;;;;;;;;;;
   ; Case: next node has listeners for the correct type AND the bubble phase
   (--> (state ((dispatch (name E (event T_event bool_b bool_c bool_t))
                          loc_current
                          bubble
                          bool
                          (name path (loc_before ... loc_next loc_current loc_after ...))
                          ()
                          ())
                S ...)
               (name store
                     ((loc_b N_b) ...
                      (loc_current
                       (node LS (loc_cchildren ...) loc_next))
                      (loc_between N_between) ...
                      (loc_next
                       (node ((T_b PM_b) ...
                              (T_event ((P_b (L_b ...)) ...
                                        (bubble ((name first-listener 
                                                       (listener T_event bubble (S_toload ...)))
                                                 L_bubble ...))
                                        (P_a (L_a ...)) ...))
                              (T_a PM_a) ...)
                             (loc_pchildren ...) loc_somewhere))
                      (loc_a N_a) ...)))
        (state ((dispatch E
                          loc_next
                          bubble
                          bool
                          path
                          (first-listener L_bubble ...)
                          (S_toload ...))
                S ...)
               store)
        bubble-next-node)
                          
;   ; Fill the pending listeners list from the next node during bubble phase.
;   ; TODO get listeners
;   (--> (state ((dispatch E
;                          loc_current   ; any node (but the rootmost)
;                          bubble        ; bubble phase
;                          bool
;                          (loc ... loc_next loc_current loc ...)
;                          ()
;                          ())
;                S ...)
;               ((loc_b N_b) ...
;                (loc_next
;                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
;                (loc_a N_a) ...))
;        (state ((dispatch E
;                          loc_next      ; next node
;                          bubble        ; bubble phase
;                          bool
;                          (loc ... loc_next loc_current loc ...)
;                          (L_b ...)     ; bubble phase listeners TODO filter for event
;                          ())
;                S ...)
;               ((loc_b N_b) ...
;                (loc_next
;                 (node (L_c ...) (L_t ...) (L_b ...) (loc_children ...) loc_parent))
;                (loc_a N_a) ...)))

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
