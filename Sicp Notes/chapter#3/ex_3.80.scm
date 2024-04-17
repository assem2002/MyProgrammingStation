(define (integral delayed-integrand initial-value dt)
    (define int
        (cons-stream
            initial-value
            (let ((integrand (force delayed-integrand)))
                (add-streams (scale-stream integrand dt) int))))
    int)
(define (RLC R L C dt)
    (lambda (vc0 il0)
        (define il (integral (delayed dil) il0 dt))
        (define vc (integral (delayed dvc) vc0 dt))
        (define dil (stream-add (scale-stream il (/ (- R) L))
                                (scale-stream vc (/ 1 L))))
        (define dvc (scale-stream il (- (/ 1 c))))
        (stream-map cons il vc)))