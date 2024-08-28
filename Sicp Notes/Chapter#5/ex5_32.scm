; I don't know how would be an operand be a symbol !!!!!!!
; isn't always a variable ?? or primtive (this case doesn't save things anyways)
; instad I handled the operands :)


ev-appl-did-operator
(restore unev) ; the operands
(restore env)
(assign argl (op empty-arglist))
(assign proc (reg val)) ; the operator - evaluated
(test (op no-operands?) (reg unev))
(branch (label apply-dispatch))
(save proc)
ev-appl-operand-loop ; it must be attached to the previous label :)
(assign exp (op first-operand) (reg unev)) 
(test (op last-operand?) (reg unev)) ; this last call is crucial for tail recursion.
(branch (label ev-appl-last-arg))
(test (op symbol?) (reg unev))
(branch (label savers))
HELLLLO
(assign continue (label ev-appl-accumulate-arg))
(goto (label eval-dispatch))
ev-appl-accumulate-arg
(restore unev)
(restore env)
(restore argl)
(assign argl (op adjoin-arg) (reg val) (reg argl))
(assign unev (op rest-operands) (reg unev))
(goto (label ev-appl-operand-loop))
ev-appl-last-arg
(assign continue (label ev-appl-accum-last-arg))
(goto (label eval-dispatch))
ev-appl-accum-last-arg
(restore argl)
(assign argl (op adjoin-arg) (reg val) (reg argl))
(restore proc)
(goto (label apply-dispatch))
savers
(save argl)
(save env)
(save unev)
(goto (label HELLLLO))


; It could help with some encapulated expression like a function call
; but when things are a little separated and not handled by the same sequence, it may get complicated 
; to calculate if something you expect (want to check so you can optimize) exists.