(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each
            (lambda (register-name)
                ((machine 'allocate-register) register-name))register-names)
        ((machine 'install-operations) ops)
        ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine))

;Registers

(define (make-register name)
    (let ((contents '*unassigned*))
    (define (dispatch message)
        (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value) (set! contents value)))
            (else (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))


; Stack

(define (make-stack)
    (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
        (if (null? s) (error "Empty stack: POP")
            (let ((top (car s)))
                (set! s (cdr s))
            top)))
    (define (initialize) (set! s '()) 'done)
    (define (dispatch message)
        (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request: STACK" message))))
dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;The default machine

(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
          (stack (make-stack))
          (the-instruction-sequence '()))
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
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (execute)))))
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
                    (else (error "Unknown request: MACHINE" message))))
            dispatch)))

;Convinent accessors

(define (get-register machine reg-name)
    ((machine 'get-register) reg-name))
(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
    (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
    (set-contents! (get-register machine register-name)value)'done)

;assembler

(define (assemble controller-text machine)
    (extract-labels controller-text (lambda (insts labels) (update-insts! insts labels machine) insts)))

;DFS
; It digs deep into the text variable (the instructions provided by user).
; while its digging process it creates bigger and bigger lambda calls like dominos queue that just collapses when we hit the end of text list.
; when it faces a label, it uses `make-label-entry` to build some sort of label pointing to some set of instructions.
; when it faces normal inst in appends it to insts (as we're doing it from down up, we use cons instead of append).
; THE BENEFIT from this code is to create labels that points to some set of instructinos + creating instructions without labels.
; THE WEIRD thing is, labels don't point for their own part they point to all code below it (I think it won't matter, but it's not intuitive to see that).
; The trick is that the last lambda call is the function that waits for two arguments.
; so instead of using compound data structure, we made a chain of calls instead of depending on the return to get passed by me to `update-insts!`
(define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
            (lambda (insts labels)
                (let ((next-inst (car text)))
                (if (symbol? next-inst)
                    (receive insts (cons (make-label-entry next-inst insts) labels))
                    (receive (cons (make-instruction next-inst) insts) labels)))))))


; It binds the text with the appropriate actual working procedure.
(define (update-insts! insts labels machine)
    (let ((pc (get-register machine 'pc))
          (flag (get-register machine 'flag))
          (stack (machine 'stack))
          (ops (machine 'operations)))
          (for-each
            (lambda (inst) (set-instruction-execution-proc! inst (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops)))
            insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc) (set-cdr! inst proc))

;Labels

(define (make-label-entry label-name insts) (cons label-name insts))
(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
        (if val (cdr val) (error "Undefined label: ASSEMBLE" label-name))))


; Dispatch on the type of the inst
(define (make-execution-procedure inst labels machine pc flag stack ops)
(cond ((eq? (car inst) 'assign)
        (make-assign inst machine labels ops pc))
    ((eq? (car inst) 'test)
        (make-test inst machine labels ops flag pc))
    ((eq? (car inst) 'branch)
        (make-branch inst machine labels flag pc))
    ((eq? (car inst) 'goto)
        (make-goto inst machine labels pc))
    ((eq? (car inst) 'save)
        (make-save inst machine stack pc))
    ((eq? (car inst) 'restore)
        (make-restore inst machine stack pc))
    ((eq? (car inst) 'perform)
        (make-perform inst machine labels ops pc))
    (else
        (error "Unknown instruction type: ASSEMBLE" inst))))


; make assign

; fetchs target + value (whether it's result from operation or otherwise ) and assigns the designated register with the needed value.
(define (make-assign inst machine labels operations pc)
    (let ((target (get-register machine (assign-reg-name inst)))
          (value-exp (assign-value-exp inst)))
        (let ((value-proc
            (if (operation-exp? value-exp)
                    (make-operation-exp
                        value-exp machine labels operations)
                    (make-primitive-exp (car value-exp) machine labels))))
            (lambda () (set-contents! target (value-proc)) (advance-pc pc)))))


(define (assign-reg-name assign-instruction) (cadr assign-instruction))
(define (assign-value-exp assign-instruction) (cddr assign-instruction))
(define (advance-pc pc) (set-contents! pc (cdr (get-contents pc))))


;make-test
; It fetches the condition.
; checks if we have some operation, if yes just evaluate it, otherwise raise error.  
(define (make-test inst machine labels operations flag pc)
    (let ((condition (test-condition inst)))
        (if (operation-exp? condition)
            (let ((condition-proc (make-operation-exp condition machine labels operations)))
            (lambda () (set-contents! flag (condition-proc))
                (advance-pc pc)))(error "Bad TEST instruction: ASSEMBLE" inst))))

;make-branch
; It fetches the label name
; then it fetches the instructions attached to this label
; then based on the flag (previous test), It would either assing pc to these instructions connected to the label, or would continue (advance).
(define (make-branch inst machine labels flag pc)
    (let ((dest (branch-dest inst)))
        (if (label-exp? dest)
            (let ((insts (lookup-label labels (label-exp-label dest))))
                (lambda ()
                    (if (get-contents flag)
                        (set-contents! pc insts)
                        (advance-pc pc))))
            (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction) (cadr branch-instruction))

;make-goto
; If the destination is a label, it fetches the  instructions related to this label and sets the pc register to it.
; otherwise, it would be a register (I don't know this case till the moment, maybe I can't recall it now).
(define (make-goto inst machine labels pc)
    (let ((dest (goto-dest inst)))
        (cond ((label-exp? dest)
            (let ((insts (lookup-label labels (label-exp-label dest)))) 
                (lambda () (set-contents! pc insts))))
            ((register-exp? dest)
                (let ((reg (get-register machine (register-exp-reg dest))))
                    (lambda () (set-contents! pc (get-contents reg)))))
            (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction) (cadr goto-instruction))



