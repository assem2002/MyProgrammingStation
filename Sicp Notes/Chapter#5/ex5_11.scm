;a) There's save and restore on register contiune, that's totally useless. We could remove that one.
; there's also in fib-n-2 
; (assign n (reg val))
; (restore val)
; we could change these two expression into (restore n), even the last saved value was saved from val register.

; b) 
(define (make-save inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda () (push stack reg) (advance-pc pc)))) ;just push reg

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda () (set-contents! reg 
                        (let (retsored-reg (pop stack)) 
                            (if (eq? (car reg) (car retsored-reg))
                                (cadr retsored-reg) 
                                (error "register in stack is" (car retsored-reg))))) (advance-pc pc))))

;c)
(define (make-register name)
    (let ((stack (make-stack)))
    (contents "*unassigned*")
    (define (dispatch message)
        (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value) (set! contents value)))
            ((eq? message 'push) (stack 'push))
            ((eq? message 'pop) (stack 'pop))
            ((eq? message 'initialize) (stack 'intialize))
            (else (error "Unknown request: REGISTER" message))))
    dispatch))

(define (pop reg) (reg 'pop))
(define (push reg value) ((reg 'push) value))

; for the initiliazation you would just map on the register table while making a new machine and intialize all of them.