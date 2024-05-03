(define (analyze-assignment-permenant exp)
(let ((var (assignment-variable exp))
      (vproc (analyze (assignment-value exp))))
        (lambda (env succeed fail)
            (vproc env
                (lambda (val fail2) ; *1*
                        (set-variable-value! var val env)
                        (succeed 'ok fail2)) ;just change that
                fail))))
; it would always return 1