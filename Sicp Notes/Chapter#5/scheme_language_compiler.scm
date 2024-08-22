; dispatch on the type of the expression we're evaluating
; why did they use a scheme function, why not to go with the same flow as the interpter we built and write every thing in register machine language
; I think it won't hurt 
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
((application? exp)
(compile-application exp target linkage))
(else
(error "Unknown expression type: COMPILE" exp))))