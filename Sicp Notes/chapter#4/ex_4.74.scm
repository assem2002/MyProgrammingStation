(define (simple-stream-flatmap proc s)
    (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
    (stream-map (lambda(x) (stream-car x))
    (stream-filter (lambda (x)(if (not(stream-null? x)) )) stream)))

;  I think it won't change as interleave didn't really do interleaving.
