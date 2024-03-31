;i though it needed to average the current with all of its previous signals, so I implemented it this way.
(define (make-zero-crossings input-stream last-value sum-data  total )
    (let ((avpt (/ (+ (stream-car input-stream)
                        (stream-car sum-data))
                total)))
    (cons-stream
        (sign-change-detector avpt last-value)
        (make-zero-crossings
            (stream-cdr input-stream) avpt (stream-cdr sum-data) (+ total 1)))))

(define zero-crossings
        (make-zero-crossings (stream-cdr sense-data) 0 (integral sense-date 0 1) 2))