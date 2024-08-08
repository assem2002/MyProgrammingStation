(define (lookup-register name)
(let ((val (assoc name register-table)))
    (if val (cadr val) (begin(allocate name) (lookup-register name)) )))
