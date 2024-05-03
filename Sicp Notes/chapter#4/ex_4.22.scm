;I handle everything in here (better way is do use this format ((lambda (x) x ) arg)), That format would be handled by the eval.
(define (let? exp) (tagged-list? exp 'let))
(define (eval-let exp)
    (let ((vars (map car (cadr exp)))
            (vals (map cadr (cadr exp)))
            (body (cddr exp)))
            (display vars)
        (lambda (env) (execute-application (make-procedure vars (analyze-sequence body) env) (map (lambda (x) (x env))(map analyze vals))))))
(define the-global-environment (setup-environment))