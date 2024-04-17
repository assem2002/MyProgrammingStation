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
    ((cond? exp) (if (eq? cadr '=>) 
        (eval (cond-special->if exp) env)
        (eval (cond->if exp) env)))
    ((application? exp)
    (apply (eval (operator exp) env)
    (list-of-values (operands exp) env)))
    (else
    (error "Unknown expression type: EVAL" exp))))


(define (cond-special->if exp) (expnad-caluses-special (cond-clauses exp)))    

(define (expnad-caluses-special caluses)
    (define (cond-predicate clause) (car clause))
    (define (cond-actions clause) (caddr clause))

    (define (expand-clauses clauses)
        (if (null? clauses)
            'false
        (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
             (sequence->exp (cond-actions first))
    (error "ELSE clause isn't last: COND->IF"
    clauses))
    (make-if (cond-predicate first)
    (sequence->exp ((cond-actions first) cond-predicate))
    (expand-clauses rest))))))
)