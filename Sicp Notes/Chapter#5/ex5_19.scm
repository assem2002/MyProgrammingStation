
; The solution is wrapped around preserving the pointers of insts, we just mutate its data by appending special node indicating a breakpoint
; we use the labels structure built be the assembler (it's great structure to do this exercise)
(define (update-insts! insts labels machine)
(let ((pc (get-register machine 'pc))
      (flag (get-register machine 'flag))
      (stack (machine 'stack))
      (ops (machine 'operations)))
      ((machine 'set-labels-list) labels) ; assume that machine have a variable named labels.
      (for-each
        (lambda (inst) (set-instruction-execution-proc! inst (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops)))
        insts)))


    (define (make-new-machine)
(let ((pc (make-register 'pc))
      (flag (make-register 'flag))
      (stack (make-stack))
      (the-instruction-sequence '())
      (labels '()))
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
                    (cond ((null? insts) 'done)
                          ((eq? (caar insts) 'break-point) 
                            (begin (display "break point reached"(cadar insts) (caddar insts))(advance-pc pc)))
                        (else (begin
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
                      ((eq? message 'set-labels-list) (lambda (labels-list) (set! labels labels-list)))
                      ((eq? message 'get-labels-list) labels)
                      ((eq? message 'excute) (excute))
                (else (error "Unknown request: MACHINE" message))))
        dispatch)))

(define (set-breakpoint machine label n)
    (let (insts (lookup-label (machine 'get-labels-list) label))
        (define (add-breakpoint-and-adjust-list current-inst-cnt insts-temp)
            (cond ((null? insts-temp)(error "provided n can't be used with this label, Max n is" current-inst-cnt))
                  ((= current-inst-cnt n) (begin 
                        (set-cdr! insts-temp (cons (car insts-temp) (cdr insts-temp))) ; we're trying to preserve the same pointers
                        (set-car! insts-temp (cons 'break-point (cons label n)) )))
                  (else (add-breakpoint-and-adjust-list (+current-inst-cnt 1) (cdr insts-temp)))))
        (add-breakpoint-and-adjust-list 1 insts)))

(define (proceed-machine machine) (machine 'execute) ) ;assume now that machine object provide access to execute.

; It just would reverse what we just did in set-breakpoint
(define (cancel-breakpoint machine label n) ; n = instruction under the label (don't count breakpoint statements we embed eariler)
    (let (insts (lookup-label (machine 'get-labels-list) label)) 
        (define (looper current-inst-cnt insts-temp)
            (cond ((null? insts-temp)
                    (error "provided n is larger than instructions under provided label" current-inst-cnt))
                ((and (eq? 'break-point (caar insts-temp)) ( = current-inst-cnt n) )
                    (begin (set-car! insts-temp (cadr insts-temp)) (set-cdr! insts-temp (cddr insts-temp)))) 
                ((and (eq? 'break-point (caar insts-temp)) (not( = current-inst-cnt n))) 
                    (looper  current-inst-cnt (cdr insts-temp))) 
                ((and (not (eq? 'break-point (caar insts-temp))) (not( = current-inst-cnt n)))
                    (looper (+ current-inst-cnt 1) (cdr insts-temp)))
                ((and (not (eq? 'break-point (caar insts-temp))) ( = current-inst-cnt n))
                    (error "we can't find a break-point in the provided place"))))
        (looper 1 insts)))

(define (cancel-all-breakpoints machine )
    (define (looper insts-temp) 
        (cond ((eq? 'break-point (caar insts-temp)) 
            (begin (set-car! insts-temp (cadr insts-temp)) (set-cdr! insts-temp (cddr insts-temp))))
            ((null? insts-temp) 'done)
            (else (looper (cdr insts-temp)))))
        ;fetch instructions starting off from first label 
        (looper (cdar(machine 'get-labels-list))))