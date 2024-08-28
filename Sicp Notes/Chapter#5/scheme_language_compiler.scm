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
(assign ,target (const ok))))))))


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