(define (unless condition usual-value exceptional-value)
    (if condition exceptional-value usual-value))
(define (unless? exp) (tagged-list? exp 'unless))


(define (eval-unless exp env) 
    (if (eval (cadr exp)) 
        (eval (cadddr exp))
        (eval (caddr exp)))) ; should check for alternative, did that for sake of simplicity.

;If this what Ben thought of, then they both have small detail that they don't notice : We are dealing with an evalutor
; everything here is plain text -> I can evaluate the expression as wish as i want.
; yeah we can't install a function called (unless) in our environment because it would fail in its purpose.
; But if we can supress the evalution of the arguments passed, then we can have unless as install procedure in the system.
; then alyssa would be correct.
; where to use unless could be with a sequence-operation procedure --> though this unless could be dealed as "negated-if" with predicate negated.

