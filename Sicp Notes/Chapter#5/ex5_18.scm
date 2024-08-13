(define (make-register name)
(let ((contents '*unassigned*)
    (trace-on #f))
(define (dispatch message)
    (cond ((eq? message 'get) contents)
        ((eq? message 'set) (lambda (value) 
            (begin (display name "old-value --> " contents "new-value --> " value) (set! contents value))))
        ((eq? message 'trace-on) (set! trace-on #t))
        ((eq? message 'trace-off) (set! trace-on #f))
        (else (error "Unknown request: REGISTER" message))))
dispatch))
; This feature is gonna be used for each register, so whatever is
; the use case for this feature, it should provide the name of name of the register and the machine or to use this feature from the machine dispatch function.