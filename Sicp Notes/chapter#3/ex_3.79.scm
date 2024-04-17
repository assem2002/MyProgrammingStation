(define (solve2 f dt y0 dy0)
(define dy (integral (delay ddy) dy0 dt ))
(define y (integral (delay dy) y0 dt )) ; no need for delay, but integral expects a delayed-integrand.
(define ddy (stream-map f y dy))
y)