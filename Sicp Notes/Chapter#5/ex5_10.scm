; I have in mind a termination instruction 
; It's easy to do.
; just terminate the pc counter so it go nothing else to do

(define (make-execution-procedure inst labels machine pc flag stack ops)
(cond ((eq? (car inst) 'terminate) (make-terminate pc)))) ; install this condition

(define (make-terminate pc) (set! pc '()))



;another thing could be increment
(define (make-execution-procedure inst labels machine pc flag stack ops)
(cond ((eq? (car inst) 'inc) (make-increment inst machine pc))))

(define (make-increment inst machine pc)
    (let ( reg (get-register machine (register-exp-reg inst)))
        (lambda () (set-contents! reg (+ (get-contents reg) 1 )))))


