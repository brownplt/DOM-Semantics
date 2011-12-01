#lang racket
(require redex)
(provide DOM DOM-reduce)

;; Version 2

(define (all-unique? l) 
  (equal? (remove-duplicates l) l))

(define-language DOM
  [bool #t #f]
  ; Event types
  [T string]
  ; Event: type, bubble, cancelable, trusted
  [Bubbles bool]
  [Cancels bool]
  [Trusted bool]
  [E (event T Bubbles Cancels Trusted)]
  ; Phases
  [P capture target bubble]
  ; Locations are like pointers.  The machine state maintains
  ; a list of (location, DOM node) pairs.
  [loc (variable-prefix loc)]
  ; parent is possibly null (parent of root node)
  [parent null loc]
  ; Node: node name, listeners, children, parent
  ; Key: event type, phase
  [TP (T P)]
  [N (node string LS (loc ...) parent)]
  [LS (side-condition (((name tp TP) (L ...)) ...)
                      (all-unique? (term (tp ...))))]
  ; Event listeners
  [L (listener (S ...))
     (handler (S ...))]
  ; Predispatch: target node, path root->target, event
  [PD (pre-dispatch parent (loc ...) E)]
  [PDef bool]
  [SP bool]
  [SI bool]
  ; Dispatch types: 
  ; event, curr node/null, phase, prevent default?, path, pending listeners, stack
  ; dispatch executes listener steps
  [D (dispatch E parent P PDef SP SI (loc ...) (L ...) (S ...))]
  ; dispatch-collect looks for listeners
  [DC (dispatch-collect E parent P PDef SP SI (loc ...))]
  ; dispatch-next determines whether to visit a next node, to jump
  ; to the default action, or to terminate
  [DN (dispatch-next E parent P PDef SP SI (loc ...) (L ...))]
  ; dispatch-default executes the default action, unless prevented
  [DD (dispatch-default E PDef (loc ...))]
  ; dispatch-stupid handles the degenerate case of a single-node path
  [DS (dispatch-stupid loc E)]
  ; Listener steps
  [S stop-prop
     stop-immediate
     prevent-default
     mutate
     (debug-print string)
     PD
     D
     DC
     DN
     DD
     DS]
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
                 (node string LS (loc_children ...) parent))
                (loc_a N_a) ...))
        (state ((pre-dispatch parent (loc_current loc ...) E) S ...)
               ((loc_b N_b) ...
                (loc_current
                 (node string LS (loc_children ...) parent))
                (loc_a N_a) ...))
        pd-build-path)

   ; Path building complete, transition to dispatch
   (--> (state ((pre-dispatch null (loc_first loc ...) E) S ...)
                N-store)
        (state ((dispatch-collect E 
                                  loc_first 
                                  capture 
                                  #f #f #f 
                                  (loc_first loc ...))
                S ...)
               N-store)
        pd-to-dispatch)
   
   ; Single-node path degenerate case
   (--> (state ((pre-dispatch null (loc) E) S ...) N-store)
        (state ((dispatch-stupid loc E) S ...) N-store)
        dont-do-this)
   
   ; done with current listener, determine next listener to run (if any)
   (--> (state ((dispatch E parent P PDef SP SI
                          (loc_child ...) (L ...) ())
                S ...)
               N-store)
        (state ((dispatch-next E parent P PDef SP SI (loc_child ...) (L ...)) S ...)
               N-store)
        finished-listener)
   
   ; stop-immediate-prop called, abort dispatch and jump to default action
   (--> (state ((dispatch-next E parent P PDef SP #t (loc_child ...) (L ...)) S ...)
               N-store)
        (state ((dispatch-default E PDef (loc_child ...)) S ...)
               N-store)
        stop-immediate-called)
   ; stop-propagation called, finish current node,
   (--> (state ((dispatch-next E parent P PDef #t #f (loc_child ...) 
                               (L_next L_rest ...)) S ...)
               N-store)
        (state ((dispatch E parent P PDef #t #f (loc_child ...) (L_rest ...) 
                          L_next) S ...)
               N-store)
        stop-prop-called-more-to-do)
   ; and then abort dispatch and jump to default action
   (--> (state ((dispatch-next E parent P PDef #t #f (loc_child ...) 
                               ()) S ...)
               N-store)
        (state ((dispatch-default E PDef (loc_child ...)) S ...)
               N-store)
        stop-prop-called-done-with-node)
   ; neither stop-prop nor stop-imm-prop called, do next listener on current node
   (--> (state ((dispatch-next E parent P PDef #f #f (loc_child ...) 
                               (L_next L_rest ...)) S ...)
               N-store)
        (state ((dispatch E parent P PDef #f #f (loc_child ...) (L_rest ...) 
                          L_next) S ...)
               N-store)
        more-to-do)
   
   ; neither stop-prop nor stop-imm-prop called, no listeners left on current node,
   ; rules for capture, target, bubble
   ; capture->capture
   (--> (state ((dispatch-next E loc_parent capture PDef #f #f 
                               (loc_a ... loc_parent loc_child 
                                      loc_grand loc_b ...) 
                               ()) S ...)
               N-store)
        (state ((dispatch-collect E loc_child capture PDef #f #f 
                                  (loc_a ... loc_parent loc_child 
                                         loc_grand loc_b ...))
                S ...)
               N-store)
        capture-to-capture-collect)
   ; capture->target
   (--> (state ((dispatch-next E loc_parent capture PDef #f #f 
                               (loc_a ... loc_parent loc_child) 
                               ()) S ...)
               N-store)
        (state ((dispatch-collect E loc_child target PDef #f #f 
                                  (loc_a ... loc_parent loc_child))
                S ...)
               N-store)
        capture-to-target-collect)
   ; target->bubble & event bubbles
   (--> (state ((dispatch-next (event T_event #t Cancels Trusted)
                               loc_child target PDef #f #f 
                               (loc_a ... loc_parent loc_child) 
                               ()) S ...)
               N-store)
        (state ((dispatch-collect (event T_event #t Cancels Trusted) 
                                  loc_parent bubble PDef #f #f 
                                  (loc_a ... loc_parent loc_child))
                S ...)
               N-store)
        target-to-bubble-collect)
   ; target->default & event doesn't bubble
   (--> (state ((dispatch-next (event T_event #f Cancels Trusted)
                               loc_child target PDef #f #f 
                               (loc_a ... loc_child) 
                               ()) S ...)
               N-store)
        (state ((dispatch-default (event T_event #f Cancels Trusted) 
                                  PDef
                                  (loc_a ... loc_child))
                S ...)
               N-store)
        target-to-default)
   ; bubble->bubble
   (--> (state ((dispatch-next E loc_child bubble PDef #f #f 
                               (loc_a ... loc_parent loc_child loc_b ...) 
                               ()) S ...)
               N-store)
        (state ((dispatch-collect E loc_parent bubble PDef #f #f 
                                  (loc_a ... loc_parent loc_child loc_b ...))
                S ...)
               N-store)
        bubble-to-bubble-collect)
   ; bubble->default
   (--> (state ((dispatch-next E loc_root bubble PDef #f #f 
                               (loc_root loc_b ...) 
                               ()) S ...)
               N-store)
        (state ((dispatch-default E PDef (loc_root loc_b ...))
                S ...)
               N-store)
        bubble-to-default)
   
   ; collecting listeners on current node, and listeners are found
   (--> (state ((dispatch-collect (event T_event Bubbles Cancels Trusted)
                                  loc_target P PDef #f #f 
                                  (loc_a ... loc_target loc_b ...))
                S ...)
               ((loc_c N_c) ...
                (loc_target 
                 (node string 
                       ((TP_a (L_a ...)) ...
                        ((T_event P) (L_wanted ...))
                        (TP_b (L_b ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...))
        (state ((dispatch-next (event T_event Bubbles Cancels Trusted)
                               loc_target P PDef #f #f 
                               (loc_a ... loc_target loc_b ...)
                               (L_wanted ...))
                S ...)
               ((loc_c N_c) ...
                (loc_target 
                 (node string 
                       ((TP_a (L_a ...)) ...
                        ((T_event P) (L_wanted ...))
                        (TP_b (L_b ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...))
        collect-found-listeners)
   ; collecting listeners on current node, and listeners are not found
   (--> (side-condition 
         (state ((dispatch-collect (event T_event Bubbles Cancels Trusted)
                                  loc_target P PDef #f #f 
                                  (loc_a ... loc_target loc_b ...))
                S ...)
               ((loc_c N_c) ...
                (loc_target 
                 (node string 
                       ((TP_a (L_a ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...))
         (empty? (filter (lambda (tp) (equal? tp (list (term T_event) (term P))))
                         (term (TP_a ...)))))
        (state ((dispatch-next (event T_event Bubbles Cancels Trusted)
                               loc_target P PDef #f #f 
                               (loc_a ... loc_target loc_b ...)
                               ())
                S ...)
               ((loc_c N_c) ...
                (loc_target 
                 (node string 
                       ((TP_a (L_a ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...))
        collect-found-no-listeners)
        

        
   


   
   
   ))