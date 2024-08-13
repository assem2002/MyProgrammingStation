; I used a variable to maintain any label that's in use
(define (make-new-machine)
(let ((pc (make-register 'pc))
      (flag (make-register 'flag))
      (stack (make-stack))
      (the-instruction-sequence '())
      (label-name "*No-Label*")) ;HERE
        (let ((the-ops (list (list 'initialize-stack (lambda () (stack 'initialize)))))
            (register-table (list (list 'pc pc) (list 'flag flag))))
            (define (allocate-register name)
                (if (assoc name register-table)(error "Multiply defined register: " name)
                    (set! register-table 
                        (cons (list name (make-register name)) register-table)))
                'register-allocated)
            (define (lookup-register name)
                (let ((val (assoc name register-table)))
                    (if val (cadr val) (error "Unknown register:" name))))
            (define (execute)
                (let ((insts (get-contents pc)))
                    (if (null? insts) 'done
                    (if (null? (car insts)) (begin (set! label-name (cadr insts)) (advance-pc pc)) ;here
                        ; You can manitiain all the changes 
                        ;done in the previous 2 exercises by adding their statements inside the begin statement below.
                        (begin 
                            (display label-name)
                            (newline)
                            ((instruction-execution-proc (car insts)))
                            (execute))))))
            (define (dispatch message)
                (cond ((eq? message 'start)(set-contents! pc the-instruction-sequence) (execute))
                      ((eq? message 'install-instruction-sequence)
                        (lambda (seq)(set! the-instruction-sequence seq)))
                      ((eq? message 'allocate-register) allocate-register)
                      ((eq? message 'get-register) lookup-register)
                      ((eq? message 'install-operations)
                        (lambda (ops)
                            (set! the-ops (append the-ops ops))))
                      ((eq? message 'stack) stack)
                      ((eq? message 'operations) the-ops)
                      ((eq? message 'set-label) (lambda (name) (set! label-name name))) ;HERE
                (else (error "Unknown request: MACHINE" message))))
        dispatch)))

(define (make-branch inst machine labels flag pc)
(let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
            (lambda ()
                (if (get-contents flag)
                    (begin (set-contents! pc insts) ((machine 'set-label) (label-exp-label dest))) ; HERE
                    (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))


(define (extract-labels text receive)
(if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
        (lambda (insts labels)
            (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (receive (cons (cons '() next-inst) insts) (cons (make-label-entry next-inst (cons (cons '() next-inst) insts)) labels)) ; preserve the label in the instruction sequence
                (receive (cons (make-instruction next-inst) insts) labels)))))))
(define (update-insts! insts labels machine)
    (let ((pc (get-register machine 'pc))
            (flag (get-register machine 'flag))
            (stack (machine 'stack))
            (ops (machine 'operations)))
            (for-each
            (lambda (inst) 
            (if  (not (null? (car inst))) ; means not a label
                (set-instruction-execution-proc! inst (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops))))
                insts)))