(define (loop-through var val env func null-action)
(define (env-loop env)
    (define (scan vars vals)
        (cond ((null? vars)
            null-action)
        ((eq? var (car vars)) func)
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
            (scan (frame-variables frame) (frame-values frame)))))
(env-loop env))

(define (set-variable-value! var val env)
    (loop-through var val env (lambda () (set-car! vals val))  (lambda () (env-loop (enclosing-environment env)))))

(define (lookup-variable-value var env)
    (loop-through var -1 env (lambda () (car! vals))  (lambda () (env-loop (enclosing-environment env)))))

(define (define-variable! var val env)
    (loop-through var val env (lambda () (set-car! vals val)) (lambda ()  (add-binding-to-frame! var val frame))))

