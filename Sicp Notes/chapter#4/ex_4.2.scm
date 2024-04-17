; The predicate of application is depending on telling whehter this is a pair or not
; which would lead to the exp to be evaluated as expression mostly.

(define (application? exp) (tagged-list? exp 'call))

(define (eval exp env)
(cond ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp) (make-procedure (lambda-parameters exp)
                                (lambda-body exp)
                                env))
                            ((begin? exp)
    (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
    (apply (eval (operator (cdr exp)) env) ;push cdr of exp
    (list-of-values (operands (cdr exp)) env)))
    (else
    (error "Unknown expression type: EVAL" exp))))