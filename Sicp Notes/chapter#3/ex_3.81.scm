;request stream is either a 'generate word or a value that indicates the reset value.
(define (random-number request-stream)
    (define s (cons-stream (stream-car request-stream)
                            (stream-map (lambda(x y) 
                            (if (eq? y 'generate) (rand-update x)
                                                (rand-update y)))
                                             s request-stream)))
    s)