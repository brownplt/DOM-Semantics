#lang racket
(require redex)
(provide DOM DOM-reduce)

;; Version 2

(define (all-unique? l)
  (equal? (remove-duplicates l) l))

(define (not-in? T_event P LS)
  (empty? (filter (lambda (tp) (equal? tp (list T_event P))) LS)))

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
  [L (listener S)
     (handler S)]
  ; Predispatch: target node, path root->target, event
  [PD (pre-dispatch parent (loc ...) E)]
  [PDef bool]
  [SP bool]
  [SI bool]
  ; Dispatch types:
  ; event, curr node/null, phase, prevent default?, path, pending listeners,
  ; stack dispatch executes listener steps
  [D (dispatch E parent P PDef SP SI (loc ...) (L ...) L)]
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
  [S skip
     (return bool)
     (seq S S)
     stop-prop
     stop-immediate
     prevent-default
     mutate
     (debug-print string)
     (addEventListener loc string bool S)
     (removeEventListener loc string bool S)
     (setEventHandler loc string S)
     (if-phase P S S)
     (if-curTarget loc S S)
     PD
     D
     DC
     DN
     DD
     DS]
  [DispCtx hole
           (if-phase DispCtx S S)
           (if-curTarget DispCtx S S)
           (seq DispCtx S)]
  [LCtx (listener Ctx)
        (handler Ctx)]
  [Ctx hole
       (seq Ctx S)
       (dispatch E parent P PDef SP SI (loc ...) (L ...) LCtx)]
  ; Machine state
  [N-store ((loc_!_ N) ...)]
  [Log (string ...)]
  [M (state S N-store Log)])

(define-metafunction DOM
  [(addListenerHelper ((TP_a (L_a ...)) ...
                       ((string_type P) (L_p ...))
                       (TP_b (L_b ...)) ...)
                      string_type P
                      S_listener)
   ((TP_a (L_a ...)) ...
    ((string_type P) (L_p ... (listener S_listener)))
    (TP_b (L_b ...)) ...)] ;when (string_type P) is present
  [(addListenerHelper ((TP_a (L_a ...)) ...)
                      string_type P
                      S_listener)
   ((TP_a (L_a ...)) ...
    ((string_type P) ((listener S_listener))))]) ;when (string_type P) is absent

(define-metafunction DOM
  [(addListener LS string_type bool S_listener)
   (addListenerHelper
    (addListenerHelper LS string_type target S_listener)
    string_type
    ,(if (term bool) (term capture) (term bubble))
    S_listener)])

(define-metafunction DOM
  [(removeListenerHelper ((TP_a (L_a ...)) ...
                          ((string_type P) (L_p ... (listener S_listener) L_q ...))
                          (TP_b (L_b ...)) ...)
                         string_type P
                         S_listener)
   ((TP_a (L_a ...)) ...
    ((string_type P) (L_p ... L_q ...))
    (TP_b (L_b ...)) ...)] ;when (string_type P) is present
  [(removeListenerHelper ((TP_a (L_a ...)) ...)
                         string_type P
                         S_listener)
   ((TP_a (L_a ...)) ...)]) ;when (string_type P) is already absent

(define-metafunction DOM
  [(removeListener LS string_type bool S_listener)
   (removeListenerHelper
    (removeListenerHelper LS string_type target S_listener)
    string_type
    ,(if (term bool) (term capture) (term bubble))
    S_listener)])


(define-metafunction DOM
  [(setHandlerHelper ((TP_a (L_a ...)) ...
                      ((string_type P) ((handler S_ignore) L_p ...))
                      (TP_b (L_b ...)) ...)
                     string_type P
                     S_handler)
   ((TP_a (L_a ...)) ...
    ((string_type P) ((handler S_handler) L_p ...))
    (TP_b (L_b ...)) ...)] ;if a handler was present for this (type, phase) pair, overwrite it
  [(setHandlerHelper ((TP_a (L_a ...)) ...
                      ((string_type P) (L_p ...))
                      (TP_b (L_b ...)) ...)
                     string_type P
                     S_handler)
   ((TP_a (L_a ...)) ...
    ((string_type P) ((handler S_handler) L_p ...))
    (TP_b (L_b ...)) ...)] ;if a handler was missing for this (type, phase) pair, add it
  [(setHandlerHelper ((TP_a (L_a ...)) ...)
                     string_type P
                     S_handler)
   ((TP_a (L_a ...)) ...
    ((string_type P) ((handler S_handler))))]
  ) ; if nothing exists for this (type, phase) pair, add it

(define-metafunction DOM
  [(setHandler LS string_type S_handler)
   (setHandlerHelper
    (setHandlerHelper LS string_type target S_handler)
    string_type bubble S_handler)])


(define DOM-reduce
  (reduction-relation
   DOM

   ; Building path in pre-dispatch
   (--> (state (in-hole Ctx (pre-dispatch loc_current (loc ...) E))
               ((loc_b N_b) ...
                (loc_current
                 (node string LS (loc_children ...) parent))
                (loc_a N_a) ...)
               Log)
        (state (in-hole Ctx (pre-dispatch parent (loc_current loc ...) E))
               ((loc_b N_b) ...
                (loc_current
                 (node string LS (loc_children ...) parent))
                (loc_a N_a) ...)
               Log)
        pd-build-path)

   ; Path building complete, transition to dispatch
   (--> (state (in-hole Ctx
                        (pre-dispatch null (loc_first loc ...) E))
                N-store
                Log)
        (state (in-hole Ctx
                        (dispatch-collect E
                                          loc_first
                                          capture
                                          #f #f #f
                                          (loc_first loc ...)))
               N-store
               Log)
        pd-to-dispatch)

   ; Single-node path degenerate case
   (--> (state (in-hole Ctx (pre-dispatch null (loc) E)) N-store Log)
        (state (in-hole Ctx (dispatch-stupid loc E)) N-store Log)
        dont-do-this)

   ; done with current listener, determine next listener to run (if any)
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI
                                  (loc_child ...) (L ...) (listener skip)))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-next E parent P PDef SP SI
                                       (loc_child ...) (L ...)))
               N-store
               Log)
        finished-listener)
   ; done with current handler, determine what steps to take
   (--> (state (in-hole Ctx
                        (dispatch (event T_event Bubbles Cancels Trusted)
                                  parent P PDef SP SI
                                  (loc_child ...) (L ...)
                                  (handler (in-hole DispCtx (return bool)))))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch (event T_event Bubbles Cancels Trusted)
                                  parent P PDef SP SI
                                  (loc_child ...) (L ...)
                                  (listener
                                   ,(if (equal? (term bool)
                                                (equal? (term T_event) "mouseover"))
                                        (term prevent-default)
                                        (term skip)))))
               N-store
               Log)
        return-from-handler) ; when we finish a handler, this rule transmutes it into the exit from a listener
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI
                                  (loc_child ...) (L ...) (handler skip)))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI
                                  (loc_child ...) (L ...) (handler (return #f))))
               N-store
               Log)
        finished-handler)

   ; stop-immediate-prop called, abort dispatch and jump to default action
   (--> (state (in-hole Ctx
                        (dispatch-next E parent P PDef SP #t
                                       (loc_child ...) (L ...)))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-default E PDef (loc_child ...)))
               N-store
               Log)
        stop-immediate-called)
   ; stop-propagation called, finish current node,
   (--> (state (in-hole Ctx
                        (dispatch-next E parent P PDef #t #f
                                       (loc_child ...)
                                       (L L_rest ...)))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef #t #f
                                  (loc_child ...) (L_rest ...)
                                  L))
               N-store
               Log)
        stop-prop-called-more-to-do)
   ; and then abort dispatch and jump to default action
   (--> (state (in-hole Ctx
                        (dispatch-next E parent P PDef #t #f (loc_child ...)
                               ()))
               N-store
               Log)
        (state (in-hole Ctx (dispatch-default E PDef (loc_child ...)))
               N-store
               Log)
        stop-prop-called-done-with-node)
   ; neither stop-prop nor stop-imm-prop called, do next listener on current node
   (--> (state (in-hole Ctx
                        (dispatch-next E parent P PDef #f #f (loc_child ...)
                               (L L_rest ...)))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef #f #f
                                  (loc_child ...) (L_rest ...)
                                  L))
               N-store
               Log)
        more-to-do)

   ; neither stop-prop nor stop-imm-prop called, no listeners left on current node,
   ; rules for capture, target, bubble
   ; capture->capture
   (--> (state (in-hole Ctx
                        (dispatch-next E loc_parent capture PDef #f #f
                                       (loc_a ... loc_parent loc_child
                                              loc_grand loc_b ...)
                                       ()))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-collect E loc_child capture PDef #f #f
                                          (loc_a ... loc_parent loc_child
                                                 loc_grand loc_b ...)))
               N-store
               Log)
        capture-to-capture-collect)
   ; capture->target
   (--> (state (in-hole Ctx
                        (dispatch-next E loc_parent capture PDef #f #f
                               (loc_a ... loc_parent loc_child)
                               ()))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-collect E loc_child target PDef #f #f
                                          (loc_a ... loc_parent loc_child)))
               N-store
               Log)
        capture-to-target-collect)
   ; target->bubble & event bubbles
   (--> (state (in-hole Ctx
                        (dispatch-next (event T_event #t Cancels Trusted)
                                       loc_child target PDef #f #f
                                       (loc_a ... loc_parent loc_child)
                                       ()))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-collect (event T_event #t Cancels Trusted)
                                          loc_parent bubble PDef #f #f
                                          (loc_a ... loc_parent loc_child)))
               N-store
               Log)
        target-to-bubble-collect)
   ; target->default & event doesn't bubble
   (--> (state (in-hole Ctx
                        (dispatch-next (event T_event #f Cancels Trusted)
                                       loc_child target PDef #f #f
                                       (loc_a ... loc_child)
                                       ()))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-default (event T_event #f Cancels Trusted)
                                          PDef
                                          (loc_a ... loc_child)))
               N-store
               Log)
        target-to-default)
   ; bubble->bubble
   (--> (state (in-hole Ctx
                        (dispatch-next E loc_child bubble PDef #f #f
                                       (loc_a ... loc_parent loc_child loc_b ...)
                                       ()))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch-collect E loc_parent bubble PDef #f #f
                                          (loc_a ... loc_parent loc_child
                                                 loc_b ...)))
               N-store
               Log)
        bubble-to-bubble-collect)
   ; bubble->default
   (--> (state (in-hole Ctx
                        (dispatch-next E loc_root bubble PDef #f #f
                                       (loc_root loc_b ...)
                                       ()))
               N-store
               Log)
        (state (in-hole Ctx (dispatch-default E PDef (loc_root loc_b ...)))
               N-store
               Log)
        bubble-to-default)

   ; collecting listeners on current node, and listeners are found
   (--> (state (in-hole Ctx
                        (dispatch-collect (event T_event Bubbles Cancels Trusted)
                                          loc_target P PDef #f #f
                                          (loc_a ... loc_target loc_b ...)))
               ((loc_c N_c) ...
                (loc_target
                 (node string
                       ((TP_a (L_a ...)) ...
                        ((T_event P) (L_wanted ...))
                        (TP_b (L_b ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...)
               Log)
        (state (in-hole Ctx
                        (dispatch-next (event T_event Bubbles Cancels Trusted)
                                       loc_target P PDef #f #f
                                       (loc_a ... loc_target loc_b ...)
                                       (L_wanted ...)))
               ((loc_c N_c) ...
                (loc_target
                 (node string
                       ((TP_a (L_a ...)) ...
                        ((T_event P) (L_wanted ...))
                        (TP_b (L_b ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...)
               Log)
        collect-found-listeners)
   ; collecting listeners on current node, and listeners are not found
   (--> (side-condition
         (state (in-hole Ctx
                         (dispatch-collect (event T_event Bubbles Cancels Trusted)
                                           loc_target P PDef #f #f
                                           (loc_a ... loc_target loc_b ...)))
               ((loc_c N_c) ...
                (loc_target
                 (node string
                       ((TP_a (L_a ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...)
               Log)
         (not-in? (term T_event) (term P) (term (TP_a ...))))
        (state (in-hole Ctx
                        (dispatch-next (event T_event Bubbles Cancels Trusted)
                                       loc_target P PDef #f #f
                                       (loc_a ... loc_target loc_b ...)
                                       ()))
               ((loc_c N_c) ...
                (loc_target
                 (node string
                       ((TP_a (L_a ...)) ...)
                       (loc_kids ...)
                       parent))
                (loc_d N_d) ...)
               Log)
        collect-found-no-listeners)

   ; seq-skip
   (--> (state (in-hole Ctx (seq skip S))
               N-store
               Log)
        (state (in-hole Ctx S)
               N-store
               Log)
        seq-skip)

   ; addEventListener
   (--> (state (in-hole Ctx
                        (addEventListener
                         loc_target string_type bool S_listener))
               ((loc_a N_a) ...
                (loc_target
                 (node
                  string_name
                  LS
                  (loc_kids ...)
                  parent_node))
                (loc_b N_b) ...)
               Log)
        (state (in-hole Ctx skip)
               ((loc_a N_a) ...
                (loc_target
                 (node string_name
                       (addListener LS string_type bool S_listener)
                       (loc_kids ...)
                       parent_node))
                (loc_b N_b) ...)
               Log)
        do-addEventListener)
   ; removeEventListener
   (--> (state (in-hole Ctx
                        (removeEventListener
                         loc_target string_type bool S_listener))
               ((loc_a N_a) ...
                (loc_target
                 (node
                  string_name
                  LS
                  (loc_kids ...)
                  parent_node))
                (loc_b N_b) ...)
               Log)
        (state (in-hole Ctx skip)
               ((loc_a N_a) ...
                (loc_target
                 (node string_name
                       (removeListener LS string_type bool S_listener)
                       (loc_kids ...)
                       parent_node))
                (loc_b N_b) ...)
               Log)
        do-removeEventListener)
   ; setEventHandler
   (--> (state (in-hole Ctx
                        (setEventHandler
                         loc_target string_type S_listener))
               ((loc_a N_a) ...
                (loc_target
                 (node
                  string_name
                  LS
                  (loc_kids ...)
                  parent_node))
                (loc_b N_b) ...)
               Log)
        (state (in-hole Ctx skip)
               ((loc_a N_a) ...
                (loc_target
                 (node string_name
                       (setHandler LS string_type S_listener)
                       (loc_kids ...)
                       parent_node))
                (loc_b N_b) ...)
               Log)
        do-setEventHandler)

   ; debug-print
   (--> (state (in-hole Ctx (debug-print string_new))
               N-store
               (string_log ...))
        (state (in-hole Ctx skip)
               N-store
               (string_new string_log ...))
        debug-print)

   ; if-phase
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx
                                           (in-hole DispCtx
                                                    (if-phase P_check S_true S_false)))))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx
                                           (in-hole DispCtx
                                                    ,(if (equal? (term P) (term P_check))
                                                         (term S_true)
                                                         (term S_false))))))
               N-store
               Log)
        do-if-phase)

   ; if-curTarget
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx
                                           (in-hole DispCtx
                                                    (if-curTarget loc_check S_true S_false)))))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx
                                           (in-hole DispCtx
                                                    ,(if (equal? (term parent) (term loc_check))
                                                         (term S_true)
                                                         (term S_false))))))
               N-store
               Log)
        do-if-curTarget)

   ; stop-prop
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx (in-hole DispCtx stop-prop))))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef #t SI (loc ...) (L ...)
                                  (in-hole LCtx (in-hole DispCtx skip))))
               N-store
               Log)
        do-stop-prop)


   ; stop-immediate
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx (in-hole DispCtx stop-immediate))))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P PDef #t #t (loc ...) (L ...)
                                  (in-hole LCtx (in-hole DispCtx skip))))
               N-store
               Log)
        do-stop-immediate)
   ; prevent-default
   (--> (state (in-hole Ctx
                        (dispatch E parent P PDef SP SI (loc ...) (L ...)
                                  (in-hole LCtx (in-hole DispCtx prevent-default))))
               N-store
               Log)
        (state (in-hole Ctx
                        (dispatch E parent P #t SP SI (loc ...) (L ...)
                                  (in-hole LCtx (in-hole DispCtx skip))))
               N-store
               Log)
        do-prevent-default)


   ; dispatch-default-prevented
   (--> (state (in-hole Ctx
                        (dispatch-default E_inner
                                          #t
                                          (loc_inner ...)))
               N-store
               (string_logged ...))
        (state (in-hole Ctx skip)
               N-store
               ("default-prevented" string_logged ...))
        dispatch-default-prevented)
   
   ; dispatch-default-not-prevented
   (--> (state (in-hole Ctx
                        (dispatch-default E_inner
                                          #f
                                          (loc_inner ...)))
               N-store
               (string_logged ...))
        (state (in-hole Ctx skip)
               N-store
               ("default action!" string_logged ...))
        dispatch-not-default-prevented)

   
   ))