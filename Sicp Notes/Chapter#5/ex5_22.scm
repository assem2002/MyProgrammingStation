ev-cond
(save continue)
(save unev)
(test (op last-exp?) (reg exp))
(goto ev-cond-predicate-last)
(assign unev (op get-first) (reg exp))
(assign unev (op get-predicate) (reg unev))
(save env)
(assign continue (label ev-cond-predicate-continue))
(goto eval-dispatch)

ev-cond-predicate-continue
(test (op true?) (reg val))
(branch (label cond-true))
(restore env)
(restore unev)
(assign unev (op rest-exps) (reg unev))
(goto ev-cond)

cond-true
(restore unev)
(restore env)
(restore continue)
(assign unev (op get-first) (reg exp))
(assign exp (op get-true-exp) (reg unev))
(assign unev (op get-body) (reg unev))
(goto ev-sequence)

ev-cond-predicate-last
(restore unev)
(save unev)
(assign unev (op get-first) (reg unev)) 
(assign unev (op get-predicate) (reg unev))
(test (op else?) (reg unev))
(goto (label cond-true))
(assign continue (label ev-cond-predicate-last-last))
(restore unev)
(save unev)
(save env)
(assign exp (op get-first) (reg unev)) 
(goto (label eval-dispatch))

ev-cond-predicate-last-last
(test (op true?) (reg val))
(branch (label cond-true))
(restore unev)
(restore env)
(restore continue)
(assign val nil)
(goto continue)






