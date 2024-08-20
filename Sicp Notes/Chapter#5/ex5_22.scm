;append
(assign continue (label done))
(save continue)
entry
(test (op null?) (reg x))
(branch (label cond1-true))
(goto (label cond1-false))
cond1-true
(assign val (reg y))
(restore continue)
(goto continue)
cond1-false
(save x)
(assign x (op cdr) (reg x))
(assign continue (label after-inner-call))
(save continue)
(goto entry)
after-inner-call
(restore x)
(assign temp-car (op car) (reg x))
(assign val (op cons) (reg val) (reg temp-car) )
(restore continue)
(goto continue)


; append!
(assign continue (label done-last-pair))
(save x)
(save continue)
entry-last-pair
(assign temp (op cdr) (reg x))
(test (op null?) (reg temp))
(branch (label cond1-true))
(goto (label cond1-false))
cond1-true
(assign ret1 (reg x))
(restore continue)
(goto continue)
cond1-false
(assign val (op cdr) (reg x))
(assign continue (label after-inner-call))
(save continue)
(goto entry)
after-inner-call
; You should've assigned something to val in this spot by the force of the laws we stated before, but it's not needed here. 
(restore continue)
(goto continue)
done-last-pair
entry-append!
(restore x)
(perfrom (op set-cdr!) (reg val) (reg y))
(assign val (reg x)) ; I intended to use val as the register that maintains the result.
