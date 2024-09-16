; dispatch on the type of the expression we're evaluating
; why did they use a scheme function, why not to go with the same flow as the interpter we built and write every thing in register machine language
; I think it won't hurt

; Brief on the process of the compiler
; - compile = eval ; it takes an expresssion and do case analysis then it dispatches on to some code that
;   should produce some code to be added
; - there's what is called `compile-linkage`, which appends a linkage code at the end of every code producing procedure.
;   - return ---> (goto (reg continue))
;   - next ---> just proceeds with the code
;   - jump ---> uses normal goto (out of the linkage descriptor)
; - end-with-linkage ---> is suppose to preserve on the continue as it's needed by the linkage code.

; I think `preserving` is used when you feel that you need to use it to make sure you're preserving what's important for you from your
; own current pointer of view.


; needed by me --> means I need this register to be not modified be any one that's gonna be inserted before me
; so who would actually use this register if all of the people before you won't mutate it? 

; modified by me --> tells the second sequence that's gonna append to me that I will change that register
;
; Why linkage? what is it greatly useful at?
(define (compile exp target linkage)
    (cond ((self-evaluating? exp)
        (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp)
        (compile-variable exp target linkage))
    ((assignment? exp)
        (compile-assignment exp target linkage))
    ((definition? exp)
        (compile-definition exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((begin? exp)
        (compile-sequence
            (begin-actions exp) target linkage))
    ((cond? exp)
        (compile (cond->if exp) target linkage))
    ((application? exp)
        (compile-application exp target linkage))
    (else
        (error "Unknown expression type: COMPILE" exp))))


(define (compile-linkage linkage)
    (cond ((eq? linkage 'return)
    (make-instruction-sequence '(continue) '()
    '((goto (reg continue)))))
    ((eq? linkage 'next)
    (empty-instruction-sequence))
    (else
    (make-instruction-sequence '() '()
    `((goto (label ,linkage)))))))


(define (end-with-linkage linkage instruction-sequence)
    (preserving '(continue)
        instruction-sequence
        (compile-linkage linkage)))

; all of these instruction are just gonna assign the expression to the traget value
; and that would be wrapped with the linkage, then get returned.
(define (compile-self-evaluating exp target linkage)
    (end-with-linkage linkage
        (make-instruction-sequence '() (list target)
            `((assign ,target (const ,exp))))))
 
(define (compile-quoted exp target linkage)
(end-with-linkage linkage
(make-instruction-sequence '() (list target)
`((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
(end-with-linkage linkage
(make-instruction-sequence '(env) (list target)
`((assign ,target
(op lookup-variable-value)
(const ,exp)
(reg env))))))

; Assignment and defination, just compiles the value that would be assigned and fetches the result on reg `val`
; then we would append this to the instruction of actually assigning the variable

(define (compile-assignment exp target linkage)
    (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
(preserving '(env)
get-value-code
(make-instruction-sequence '(env val) (list target)
`((perform (op set-variable-value!)
(const ,var)
(reg val)
(reg env))
(assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
(let ((var (definition-variable exp))
    (get-value-code
(compile (definition-value exp) 'val 'next)))
(end-with-linkage linkage
(preserving '(env)
get-value-code
(make-instruction-sequence '(env val) (list target)
`((perform (op define-variable!)
(const ,var)
(reg val)
(reg env))
(assign ,target (const ok)))))))) ; This is critical as we shouldn't add anything to the passed `target` register.


; function to produce 
(define label-counter 0)
(define (new-label-number)
    (set! label-counter (+ 1 label-counter)) label-counter)
(define (make-label name)
    (string->symbol
        (string-append (symbol->string name)
            (number->string (new-label-number)))))

;  It compiles the predicate saving the result in val for later testing in the over all sequence.
;  then it would create the code for the predicate and for the alternative
; notice that it creates special linkage to perform the same structure described in the book (page 783).
; It combines all of that by using `preserving` to preserve the `env` and `continue` as they could be changed by the predicate compilation process.
; and it uses what's called parallel appending, which is still not obvious for me  

(define (compile-if exp target linkage)
    (let ((t-branch (make-label 'true-branch))
            (f-branch (make-label 'false-branch))
            (after-if (make-label 'after-if)))
        (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
            (let ((p-code (compile (if-predicate exp) 'val 'next))
(c-code
(compile
(if-consequent exp) target
consequent-linkage))
(a-code
(compile (if-alternative exp) target linkage)))
(preserving '(env continue)
p-code
(append-instruction-sequences
(make-instruction-sequence '(val) '()
`((test (op false?) (reg val))
(branch (label ,f-branch))))
(parallel-instruction-sequences
(append-instruction-sequences t-branch c-code)
(append-instruction-sequences f-branch a-code))
after-if))))))


; recursivly creating nested preserving call to preserve `continue` and `env`
; while I think that conitune isn't neccessary needed to be saved and restored with every sequence excution
; but it would just complex the implemntation of the function and it also keep you save in case you didn't notice an edge case.
(define (compile-sequence seq target linkage)
(if (last-exp? seq)
(compile (first-exp seq) target linkage)
(preserving
'(env continue)
(compile (first-exp seq) target 'next)
(compile-sequence (rest-exps seq) target linkage))))


; This mimics what is needed when you create a lambda,
; it creates the function with its body somewhere and store it and just gives you like a pointer
; or an address to invoke this function later by passing arguments

; so it creates an `entry point` label and `after-lambda` label 
; entry point --> to add the enviornment switcher to swtich the environments so we can have correct variable bindings 
; with the passed arguments to the lambda call.
; after-lambda --> is used to be a place to jump to as we would have the following :
; assign to target the lambda object (that would later be used to call)
; jump to after-lambda
; lambda actuall implementaion
; after-lambda label

(define (compile-lambda exp target linkage)
(let ((proc-entry (make-label 'entry))
(after-lambda (make-label 'after-lambda)))
(let ((lambda-linkage
(if (eq? linkage 'next) after-lambda linkage)))
(append-instruction-sequences
(tack-on-instruction-sequence
(end-with-linkage lambda-linkage
(make-instruction-sequence '(env) (list target)
`((assign ,target
(op make-compiled-procedure)
(label ,proc-entry)
(reg env)))))
(compile-lambda-body exp proc-entry))
after-lambda))))

; This just fetches the env of the lambda and extend it with the passed arguments
; then it compiles the sequence normally using `compile-sequence`
; notice we use `return` linkage to use continue so it knows where to get back to after finishing.
; notice it's not this function concern to think about continue.
; the function that's gonna use another `compile` should be the one conerned about getting its compilation maniuplated by others.
(define (compile-lambda-body exp proc-entry)
(let ((formals (lambda-parameters exp)))
(append-instruction-sequences
(make-instruction-sequence '(env proc argl) '(env)
`(,proc-entry
(assign env
(op compiled-procedure-env)
(reg proc))
(assign env
(op extend-environment)
(const ,formals)
(reg argl)
(reg env))))
(compile-sequence (lambda-body exp) 'val 'return))))


; It's the same as we did with the interperter
; we first compile the operator while preserving the `env` and `continue`, you would ask why!
; let's assume you created a lambda and instatnly called it, the compilation of lambda by creating an initaliazion code and then the code sequence
; all of this could potenially mutate these register that I need with the other parts I'm trying to build compiled code for
; secondly, while compiling the opearnds the actuall call sequence would need the `proc` and the `continue` so we preserve them while
; trying to compile the operands.

;Notice the structure if the procedure was a lambda for instance (actually normal definition == lambda declartioin)
; - assign to target (`proc`) the lambda object
; - continue to after-lambda
; - entry point to the actual lambda sequence
; - after-lambda label
; - compiling all operands + building up in `argl` register
; - using the `proc` to fetch the entrypoint and enviornment to start compiling the sequence

(define (compile-application exp target linkage)
    (let ((proc-code (compile (operator exp) 'proc 'next))
(operand-codes
(map (lambda
(operand) (compile operand 'val 'next))
(operands exp))))
(preserving '(env continue)
proc-code
(preserving '(proc continue)
(construct-arglist operand-codes)
(compile-procedure-call target linkage)))))



; This is responsible for:
; building up the `argl` register from the resulting data by getting the data out of `val` register
; if it happens that we don't have operands at first place so it just assigns `argl` to empty list
; if there's some operands it's mainly resposible for appending a list initalization of `argl` then it proceeds
; if it happens that there's more that this it moves to `code-to-get-rest-args` that concatentes the rest
; of the operands
; Notice that we preserve `argl` between the compilation of the operand and building up it
; then between the operands + building up, we preserve the env to make sure we have the correct env to compile the operands in. 
(define (construct-arglist operand-codes)
(let ((operand-codes (reverse operand-codes)))
(if (null? operand-codes)
(make-instruction-sequence '() '(argl)
'((assign argl (const ()))))
(let ((code-to-get-last-arg(append-instruction-sequences
(car operand-codes)
(make-instruction-sequence '(val) '(argl)
'((assign argl (op list) (reg val)))))))
(if (null? (cdr operand-codes))
code-to-get-last-arg
(preserving '(env)
code-to-get-last-arg
(code-to-get-rest-args
(cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
(let ((code-for-next-arg
(preserving '(argl)
(car operand-codes)
(make-instruction-sequence '(val argl) '(argl)
'((assign argl
(op cons) (reg val) (reg argl)))))))
(if (null? (cdr operand-codes))
code-for-next-arg
(preserving '(env)
code-for-next-arg
(code-to-get-rest-args (cdr operand-codes))))))



; It follow this structure :
; - Test whether it's a primitve or a compiled procedure that we're trying to apply
; - compiled-label
; - Fetching data from the procedure object (contains entry point and env) + branch to after-call(if the linkage is next)
; - Primitve-label
; - the call to the underlying scheme primitve application
; - after-call label

(define (compile-procedure-call target linkage)
(let ((primitive-branch (make-label 'primitive-branch))
(compiled-branch (make-label 'compiled-branch))
(after-call (make-label 'after-call)))
(let ((compiled-linkage
(if (eq? linkage 'next) after-call linkage)))
(append-instruction-sequences
(make-instruction-sequence '(proc) '()
`((test (op primitive-procedure?) (reg proc))
(branch (label ,primitive-branch))))
(parallel-instruction-sequences
(append-instruction-sequences
compiled-branch
(compile-proc-appl target compiled-linkage))
(append-instruction-sequences
primitive-branch
(end-with-linkage linkage
(make-instruction-sequence '(proc argl)
(list target)
`((assign ,target
(op apply-primitive-procedure)
(reg proc)
(reg argl)))))))
after-call))))


; I'll write what I understand till the moment
; this is a directing to the entry point of the function
; the passed linkage could be label,return,next

; If it's a label or next + target =val (not possible as I think) ; we could utilize `continue` to be the linkage
; and while we're excuting a sequence the last sequence would've been built with last sequence linkage of return(contiue)

;if it's a target != val + linkage=label ;  We would have a label that would return to after finishing from the entrypoint
; this label would set the val into the target

; if it's of target = val + linage = return ; this means we're in the last sequence of the list of sequences to be evaluated and we would
; just use the continue we have

; other wise this is case that's not possible

; This structure makes us able to apply `tail-recursion` as now when we start go into the entry point
; we just go for it with no building up continue savings into the stack that needs to be restored all when we hit the base case.


; NOTICE : this is always called from compile-procedure-call() which\
; always sets the likage to after-call if the likage of it is 'next'
(define (compile-proc-appl target linkage)
(cond ((and (eq? target 'val) (not (eq? linkage 'return)))
(make-instruction-sequence '(proc) all-regs
`((assign continue (label ,linkage))
(assign val (op compiled-procedure-entry)
(reg proc))
(goto (reg val)))))
((and (not (eq? target 'val))
(not (eq? linkage 'return)))
(let ((proc-return (make-label 'proc-return)))
(make-instruction-sequence '(proc) all-regs
`((assign continue (label ,proc-return))
(assign val (op compiled-procedure-entry)
(reg proc))
(goto (reg val))
,proc-return
(assign ,target (reg val))
(goto (label ,linkage))))))
((and (eq? target 'val) (eq? linkage 'return))
(make-instruction-sequence
'(proc continue)
all-regs
'((assign val (op compiled-procedure-entry)
(reg proc))
(goto (reg val)))))
((and (not (eq? target 'val))
(eq? linkage 'return))
(error "return linkage, target not val: COMPILE"
target))))

; The flow isn't easy to grasp on
; specially that we enforce some behaviour from some places and 
; according to it we start implementing in special way in another function
; due to the overhead that happens, sometime you just forget and just keep asking yourself what if what if what if, and eventually you realize 
; we enforced that behaviour and the case you're trying to think about isn't gonna happen.


(define (registers-needed s)
(if (symbol? s) '() (car s)))
(define (registers-modified s)
(if (symbol? s) '() (cadr s)))
(define (statements s)
(if (symbol? s) (list s) (caddr s)))


(define (needs-register? seq reg)
(memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
(memq reg (registers-modified seq)))


; I got no idea what would it make a union of the needed register by seq1 and (list difference of needed registers by seq2 and modified register by seq2)
; it wants to remove the registers modified by seq1 and needed by seq2 but not in needed by seq1
; I think it concludes that these removed registers are not needed by seq2 as seq1 already modifies it and don't need to intialize it.

; Update : It just removes the needed registers by seq2 that seq1 modifies as it should be removed cuz if they preserved seq1 would take the job of needing that register.
; otherwise it means that this need is false and should be removed.
(define (append-instruction-sequences . seqs)
(define (append-2-sequences seq1 seq2)
(make-instruction-sequence
(list-union
(registers-needed seq1)
(list-difference (registers-needed seq2)
(registers-modified seq1)))
(list-union (registers-modified seq1)
(registers-modified seq2))
(append (statements seq1) (statements seq2))))
(define (append-seq-list seqs)
(if (null? seqs)
(empty-instruction-sequence)
(append-2-sequences
(car seqs)
(append-seq-list (cdr seqs)))))
(append-seq-list seqs))


(define (list-union s1 s2)
(cond ((null? s1) s2)((memq (car s1) s2) (list-union (cdr s1) s2))
(else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
(cond ((null? s1) '())
((memq (car s1) s2) (list-difference (cdr s1) s2))
(else (cons (car s1)
(list-difference (cdr s1) s2)))))

; What I get about this thing is tangled some how
; preserving was meant for making sure to save and restore bunch of registers around some sequence
; but appreantly, It works only if seq2 needs it and seq1 modifies it
; why not to just go for it, It won't be that benefical to have all this needed modified paramters that
; we're carefully trying to set.
; so it checks if it's necessary or not by checking modified by seq1 and needed by seq2.
; if it's correct that means okay go for it and sandwich it with save and restore
; but here comes a weird part, why does it add it to the needed. may be because the implementation of append-sequence

; I think it removes this register that has just been preserved as there's no need for that
; as it would now make the resulting instruction sequence having the `modified` paramter with that preserved register removed
; which makes this new combined instruction sequence not good for preserving that register again as it already has been preserved there's not need.


(define (preserving regs seq1 seq2)
(if (null? regs)
(append-instruction-sequences seq1 seq2)
(let ((first-reg (car regs)))
(if (and (needs-register? seq2 first-reg)
(modifies-register? seq1 first-reg))
(preserving (cdr regs)
(make-instruction-sequence
(list-union (list first-reg)
(registers-needed seq1))
(list-difference (registers-modified seq1)
(list first-reg))
(append `((save ,first-reg))
(statements seq1)
`((restore ,first-reg))))
seq2)
(preserving (cdr regs) seq1 seq2)))))

; We're basically creating independant set of instructinos to be the function
; so no need for caring about the body if it has needed register and modified register
; it's a block you get into and come out of. 

(define (tack-on-instruction-sequence seq body-seq)
(make-instruction-sequence
(registers-needed seq)
(registers-modified seq)
(append (statements seq)
(statements body-seq))))

; I think this explaination isn't correct 100%
; These instructions won't be excuted sequentially, which means if the seq1 modifies some register that
; is needed be seq2, we won't remove it from the overall need of the resulting instruction sequence
; because basically seq1 won't save and restore that register


(define (parallel-instruction-sequences seq1 seq2)
(make-instruction-sequence
(list-union (registers-needed seq1)
(registers-needed seq2))
(list-union (registers-modified seq1)
(registers-modified seq2))
(append (statements seq1)
(statements seq2))))