(define (make-zero-crossings input-stream last-value proc)
    (define (internal s last)
        (cons-stream
                (sign-change-detector (stream-car s) last)
                (internal
                    (stream-cdr s) (stream-car s))))
    (internal (proc input-stream last-value) last-value))


(define (smooth input-stream last-value)
    (scale (add-streams (cons-stream last-value input-stream)
                                input-stream)
                    0.5))