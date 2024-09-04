; right to left
; it's the construction of `argl` 
; specifically in `construct-arglist()`
; The modification would require going with the normal flow 
; but don't use reverse  + use underlying scheme implemenation of `append` to append the 
; result of `val` in a list.


(define (construct-arglist operand-codes)
(let () ;; HERE
(if (null? operand-codes)
(make-instruction-sequence '() '(argl)
'((assign argl (const ()))))
(let ((code-to-get-last-arg(append-instruction-sequences
(car operand-codes)
(make-instruction-sequence '(val) '(argl)
'((assign argl (op list) (reg val)))))))
(if (null? (cdr operand-codes))
code-to-get-last-arg
(preserving '(env)
code-to-get-last-arg
(code-to-get-rest-args
(cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
(let ((code-for-next-arg
(preserving '(argl)
(car operand-codes)
(make-instruction-sequence '(val argl) '(argl val) ;HERE
'((assign val (op list) (reg val)) ;HERE
'(assign argl
(op append) (reg argl) (reg val)))))))
(if (null? (cdr operand-codes))
code-for-next-arg
(preserving '(env)
code-for-next-arg
(code-to-get-rest-args (cdr operand-codes))))))

; I saw another argument which is to reverse the argl after it's complete
; This would be great as reverse is o(n) while appending is o(n^2) as its exact number of operation would
; be n*(n+1)


; for the effeciency I see no change.