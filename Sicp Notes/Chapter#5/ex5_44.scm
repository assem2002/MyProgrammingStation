; I think I can use my compile function in ex 5.39
; and add to it a predicate to either treat the operator as inlined one or to go and treat it as application


(define (compile exp target linkage emp-env)
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
(if (not(eq? (find-variable exp emp-env) "not-found")) (cond ((is-primitve-plus? exp)
    (compile-primitive-plus (get-operands exp) target linkage))
((is-primitve-minus? exp)
    (compile-primitive-minus (get-operands exp) target linkage))
((is-primitve-mul? exp)
    (compile-primitive-mul (get-operands exp) target linkage)))
((application? exp)
    (compile-application exp target linkage)))
(else
    (error "Unknown expression type: COMPILE" exp))))