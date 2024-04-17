(define (make-unbound! var env)
    (define (scan vars vars-before vals vals-before)(
        (cond ((null? vars) (error "This variable isn't existing.") )
            ((eq? (car vars) var) (if (null? vars-before) 
                                        (begin (set-car! env (cdr vars)) 
                                        (set-cdr! env (cdr vals)))
                                    (begin (set-cdr! vars-before (cdr var))
                                         ((set-cdr! vals-before (cdr vals))))))
            (else (scan (cdr vars) vars (cdr vals) vals)))))
    (scan (frame-variables env) nil (frame-values env ) nil))

