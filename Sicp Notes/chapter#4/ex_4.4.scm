;('or (list-values))
;('and (list-values))

;There's some redundency :).
(define (eval-and exp env)
    (define (internal last-evaluated current)
        (cond ((null? current) (eval(car last-evaluated)))
                ((True? (eval (car current ))) (internal current (cdr current)) ) 
                (else false)))
    (internal exp exp)
)

(define (eval-or exp env)
    (cond ((null? exp) false)
            ((True? (eval (car exp))) (eval (car exp)))
        (else (eval-or (cdr exp) env))))
