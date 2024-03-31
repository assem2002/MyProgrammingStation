(load "../streams.scm")


(define (RC r c dt)
    (define (integral integrand initial-value)
        (define int
            (cons-stream initial-value
                        (add-streams (scale-stream integrand dt)
                                        int)))
        int)
    (lambda (i-stream v0)
         (add-streams (scale-stream (integral i-stream v0) (/ 1 c))
                        (scale-stream i-stream r))))
(define a ((RC 1 5 0.5) integers 1))
