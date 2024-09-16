; We would have dedicated register for the arguments : reg1 reg2
; It doesn't like how we fetch the primitve function from the underlying lisp implmentation
; It wants to look like that our compiler has its own implementation that get added inline
; everytime such primitve call happens, an open coded primive implementation would be added in the
; instructions

; We would need to change the compile dispatcher to detect these primitves and do spread-argument
; then act upon these (arg1 and arg2)

(define (get-operand exp) (cdr exp))

(define (compile exp target linkage)
(cond ((self-evaluating? exp)
    (compile-self-evaluating exp target linkage))
((quoted? exp) (compile-quoted exp target linkage))
((variable? exp)
    (compile-variable exp target linkage))
((assignment? exp)
    (compile-assignment exp target linkage))
((definition? exp)
    (compile-definition exp target linkage))
((if? exp) (compile-if exp target linkage))
((lambda? exp) (compile-lambda exp target linkage))
((begin? exp)
    (compile-sequence
        (begin-actions exp) target linkage))
((cond? exp)
    (compile (cond->if exp) target linkage))
((is-primitve-plus? exp)
    (compile-primitive-plus (get-operands exp) target linkage))
((is-primitve-minus? exp)
    (compile-primitive-minus (get-operands exp) target linkage))
((is-primitve-mul? exp)
    (compile-primitive-mul (get-operands exp) target linkage))
((application? exp)
    (compile-application exp target linkage))
(else
    (error "Unknown expression type: COMPILE" exp))))

(define (spread-argument operands)  
    (let ((first-operand (compile (car operands) reg1 'next)) 
        (second-operand (compile (cdr operands) reg2 'next)))
        (preserving '(reg1)
         (append-instruction-sequences first-operand second-operand))))
(define (compile-primitive-plus operands target linkage)
    (end-with-linkage linkage
    (append-instruction-sequences
        (spread-argument opearnds)
        (make-instruction-sequence '(reg1 reg2) '(target)
        '(assign ,target (op +) (reg reg1) (reg reg2))))))
(define (compile-primitive-minus operands target linkage)
    (end-with-linkage linkage
    (append-instruction-sequences
        (spread-argument opearnds)
        (make-instruction-sequence '(reg1 reg2) '(target)
        '(assign ,target (op -) (reg reg1) (reg reg2))))))
(define (compile-primitive-mul operands target linkage)
    (end-with-linkage linkage
    (append-instruction-sequences
        (spread-argument opearnds)
        (make-instruction-sequence '(reg1 reg2) '(target)
        '(assign ,target (op *) (reg reg1) (reg reg2))))))

; multiple operands
; I think the book chose * and + to make us not worry about the precedence of the operands.
; just make a code that work with these in any precedence.


(define (spread-argument operands)  
(let ((first-operand (compile (car operands) reg1 'next)) 
    (second-operand (compile (cdr operands) reg2 'next)))
    (preserving '(reg1)
        first-operand 
        second-operand)))


(define (compile-primitive-plus-helper operands target linkage)
(if (null? (cddr operands))
    (append-instruction-sequences 
        (spread-argument operands)
        (make-instruction-sequence '(reg1 reg2) '(target)
        '(assign ,target (op +) (reg reg1) (reg reg2))))
        (preserving (reg2)
        (compile-primitive-plus (cons + (cdr operands) reg2 'next))
        (end-with-linkage linkage 
        (append-instruction-sequences 
            (compile (car operands) reg1 'next)
            (make-instruction-sequence (reg1 reg2) (target)
            (assign ,target (op +) (reg reg1) (reg reg2))))))))
; Do the same with mul
