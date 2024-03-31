(load "../streams.scm")
(define (integral integrand initial-value dt)
    (cons-stream
            initial-value
            (if (stream-null? integrand)
                the-empty-stream
                (integral (cdr (force integrand))
                            (+ (* dt (car ( force integrand)))
                            initial-value)
                dt))))

(define a (integral (delay integers) 1 2))