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

; It fetches the expressions and heads off to `ev-sequence`
ev-begin
(assign unev (op begin-actions) (reg exp))
(save continue)
(goto (label ev-sequence))

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
(test (op last-operand?) (reg unev)) ; this last call is crucial for tail recursion.
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
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)
(goto (reg continue))

; it fetches the environment of proc (I think the env is important in case of there's some insider function),
; and fetches the parameters
; we then extend the environemnt with the argl attached to the paramters names (as I think)
; We then save the body of the procedure in register `unev`
; then we head over to label `ev-sequence`
compound-apply 
(assign unev (op procedure-parameters) (reg proc))
(assign env (op procedure-environment) (reg proc))
(assign env (op extend-environment) (reg unev) (reg argl) (reg env))
(assign unev (op procedure-body) (reg proc))
(goto (label ev-sequence))

;These 3 labels are just a while loop
; It fetches expression by expression from the body of the proc save in register `unev`
; We save unev and env incase some one of the expression were a procedure itself or anything that would utilize `unev` and `env`.
; then we just eval-dispatch (it needs to be a primtive expression to us either an assignment or accessing a variable - it's eval-apply loop)
; and we save the continue point to be label `ev-sequence-continue`.
; then in `ev-sequence-continue` --> We just move the list of expression by one
; When we hit the final expression we restore continue to branch out of this loop and use the continue that was meant to
; be used when we first started evaluating the procedure application expression.
ev-sequence
(assign exp (op first-exp) (reg unev))
(test (op last-exp?) (reg unev))
(branch (label ev-sequence-last-exp))
(save unev)
(save env)
(assign continue (label ev-sequence-continue))
(goto (label eval-dispatch))
ev-sequence-continue
(restore env)
(restore unev)
(assign unev (op rest-exps) (reg unev))
(goto (label ev-sequence))
ev-sequence-last-exp
(restore continue)
(goto (label eval-dispatch))

; Save the if expression and the environment and continue, They should be save for later as the predicate could
; be for example a procedure that utilized `exp` and `env` and `continue`
ev-if
(save exp) ; save expression for later
(save env)
(save continue)
(assign continue (label ev-if-decide))
(assign exp (op if-predicate) (reg exp))
(goto (label eval-dispatch)) ; evaluate the predicate

; We here just restore the saved values + branch to the needed code
; the book emphasizes on the notion of bringing back the correct environment that `if` statment should be evaluted at.
ev-if-decide
(restore continue)
(restore env)
(restore exp)
(test (op true?) (reg val))
(branch (label ev-if-consequent))
ev-if-alternative
(assign exp (op if-alternative) (reg exp))
(goto (label eval-dispatch))
ev-if-consequent
(assign exp (op if-consequent) (reg exp))
(goto (label eval-dispatch))


; it saves the variable + env + continue
; then goes to `eval-dispatch`, and for sure we would save in register `continue` the entry point to the conituation
; when we return back from evaluating the expression, we use some assumed procedure set-variable-value!` to set the variable
; in the environment with the evaluated value in `val`
ev-assignment
(assign unev (op assignment-variable) (reg exp))
(save unev)
; save variable for later
(assign exp (op assignment-value) (reg exp))
(save env)
(save continue)
(assign continue (label ev-assignment-1))
(goto (label eval-dispatch))
; evaluate the assignment value
ev-assignment-1
(restore continue)
(restore env)
(restore unev)
(perform (op set-variable-value!) (reg unev) (reg val) (reg env))
(assign val (const ok))
(goto (reg continue))

; This is similar to assignment
; but how come this handles function defintions
ev-definition
(assign unev (op definition-variable) (reg exp))
(save unev) ; save variable for later
(assign exp (op definition-value) (reg exp))
(save env)
(save continue)
(assign continue (label ev-definition-1))
(goto (label eval-dispatch)) ; evaluate the deﬁnition value
ev-definition-1
(restore continue)
(restore env)
(restore unev)
(perform (op define-variable!) (reg unev) (reg val) (reg env))
(assign val (const ok))
(goto (reg continue))



; THE Driver Loop 
read-eval-print-loop
(perform (op initialize-stack))
(perform
(op prompt-for-input) (const ";;EC-Eval input:"))
(assign exp (op read))
(assign env (op get-global-environment))
(assign continue (label print-result))
(goto (label eval-dispatch))
print-result
(perform (op announce-output) (const ";;EC-Eval value:"))
(perform (op user-print) (reg val))
(goto (label read-eval-print-loop))
; Error Handling
unknown-expression-type
(assign val (const unknown-expression-type-error))
(goto (label signal-error))
unknown-procedure-type
(restore continue)
; clean up stack (from apply-dispatch)
(assign val (const unknown-procedure-type-error))
(goto (label signal-error))
signal-error
(perform (op user-print) (reg val))
(goto (label read-eval-print-loop))

