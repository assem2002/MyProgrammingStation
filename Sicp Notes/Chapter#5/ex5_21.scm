ev-let
(save continue)
(save exp)
(assign unev (op fetch-vars) (reg exp))
(save unev)
(save env)
(test (op last-exp?) (reg unev))
(branch last-let-vars)
(assign unev (op first-element) (reg unev))
(assign unev (op get-value-let) (reg unev))
(assign continue (label continue-let-var))
(goto (label eval-dispatch))
continue-let-var
(restore env)
(restore unev)
(save unev)
(assign unev (op first-element) (reg unev))
(assign unev (op get-var-let) (reg unev))
(perfrom (op extend-environment) (reg env) (reg unev) (reg val))
(restore unev)
(assign unev (op rest-exps) (reg unev))
(goto (label ev-let))
last-let-vars
(assign continue (label last-let-finish))
(goto (label eval-dispatch))
last-let-finish
(restore env)
(restore unev)
(assign unev (op first-element) (reg unev))
(assign unev (op get-var-let) (reg unev))
(perfrom (op extend-environment) (reg env) (reg unev) (reg val))
(restore unev)
(restore exp)
(assign unev (op rest-exps) (reg unev))
(assign unev (op body-let) (reg exp))
(goto ev-sequence)


; for Cond, if i have something that just transforms cond -> if 
; we would just go throught it.

ev-cond
(assign exp (op cond->if) (reg exp))
(goto eval-dispatch)


