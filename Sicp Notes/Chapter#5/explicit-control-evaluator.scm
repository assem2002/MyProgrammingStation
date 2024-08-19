; There is preassumption that register continue hold the place to go to after going into `eval`
; So If we're about to use the register, We must save it (that's what is happening in `ev-application`)


eval-dispatch ; It just dispatches on the type of the expression we're trying to evaluate. (The book says it isn't the best way to write this piece of code, as there would've been more convinent way in the register machine level).
(test (op self-evaluating?) (reg exp))
(branch (label ev-self-eval))
(test (op variable?) (reg exp))
(branch (label ev-variable))
(test (op quoted?) (reg exp))
(branch (label ev-quoted))
(test (op assignment?) (reg exp))
(branch (label ev-assignment))
(test (op definition?) (reg exp))
(branch (label ev-definition))
(test (op if?) (reg exp))
(branch (label ev-if))
(test (op lambda?) (reg exp))
(branch (label ev-lambda))
(test (op begin?) (reg exp))
(branch (label ev-begin))
(test (op application?) (reg exp))
(branch (label ev-application))
(goto (label unknown-expression-type))

; These are the exact same as the normal evaluator we built before.
ev-self-eval 
(assign val (reg exp))
(goto (reg continue))
ev-variable
(assign val (op lookup-variable-value) (reg exp) (reg env))
(goto (reg continue))
ev-quoted
(assign val (op text-of-quotation) (reg exp))
(goto (reg continue))
ev-lambda
(assign unev (op lambda-parameters) (reg exp))
(assign exp (op lambda-body) (reg exp))
(assign val (op make-procedure) (reg unev) (reg exp) (reg env))
(goto (reg continue))   


; ex (+ 1 2) --> We would start out be memorizing registers (continue, env) as what the next label is doing requires utilizing `continue` register.
; then memorizing the operands for later evaluation 
; then we would assign continue to label `ev-appl-did-operator` to handle the operands just after handling the operator
ev-application
(save continue)
(save env)
(assign unev (op operands) (reg exp))
(save unev)
(assign exp (op operator) (reg exp))
(assign continue (label ev-appl-did-operator))
(goto (label eval-dispatch))

; After coming from the journey of tyring to evaluate the operator of some application expression, we would restore the operands and the env we had while processing the application call.
; We would then handle the operands 
; We would start out in label `ev-appl-operand-loop`, from the name you can see that it's a loop that's used over and over till some breaking moment (which is evaluating the last operand)
; `ev-appl-operand-loop` --> We have the list of unevaluated operands in list called unev, we fetch the first element of this list and assign `continue` register a label `ev-appl-accumulate-arg` and go to evaluate this operand.
; `ev-appl-accumulate-arg` -->  Just add the returning operand evaluation in argl, then pushes the list of unevaluated operands one place then hit back to the loop we talked about.
; if we hit the last operand in the loop, instead of setting `continue` to `ev-appl-accumulate-arg` we just change to path to other thing label `ev-appl-accum-last-arg` which wraps every thing up and move to really applying the procedure call we've been trying to evaluate all this time :).
ev-appl-did-operator
(restore unev) ; the operands
(restore env)
(assign argl (op empty-arglist))
(assign proc (reg val)) ; the operator - evaluated
(test (op no-operands?) (reg unev))
(branch (label apply-dispatch))
(save proc)
ev-appl-operand-loop ; it must be attached to the previous label :)
(save argl)
(assign exp (op first-operand) (reg unev)) 
(test (op last-operand?) (reg unev))
(branch (label ev-appl-last-arg))
(save env)
(save unev)
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



; apply dispatch is now ready to operate, with proc assigned the procedure that's gonna be used (primitive or compound)
; and argl that has all the operands evaulated and ready for use 
; what is left now is to handle how we treat primitve procedure calls and how we handle compound procedure call.
apply-dispatch
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-apply))
(test (op compound-procedure?) (reg proc))
(branch (label compound-apply))
(goto (label unknown-procedure-type))
primitive-apply ; It uses underlying scheme procedure `apply-primtive-procedure`, nothing fancy here (just for sake of simplicity).
(assign val (op apply-primitive-procedure)
(reg proc)
(reg argl))
(restore continue)
(goto (reg continue))