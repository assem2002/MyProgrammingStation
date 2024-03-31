(define (integral delayed-integrand initial-value dt)
    (define int
            (cons-stream
                initial-value
                (let ((integrand (force delayed-integrand)))
                    (add-streams (scale-stream integrand dt) int))))
    int)

(define (solve2 a b dt y0 dy0)
    (define dy (integral (delay ddy) dy0 dt ))
    (define y (integral (delay dy) y0 dt )) ; no need for delay, but integral expects a delayed-integrand.
    (define ddy (add-streams (scale-stream dy a) (scale-stream y b )))
    y)