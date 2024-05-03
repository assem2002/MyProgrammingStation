(define (op p n1 n2)
    (p n1 n2))

(op + 1 2)
; + operator here would be a delayed argument that needs to get unpacked from its 'thunk.