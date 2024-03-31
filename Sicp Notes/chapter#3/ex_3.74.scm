(load "../streams.scm")

(define zero-crossings
    (stream-map sign-change-detector
                sense-data
                (cons-stream 0 zero-crossings)))