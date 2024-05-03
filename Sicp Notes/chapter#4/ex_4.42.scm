(define ans '())
(define (printer)
    (display '(Mary Kitty Joan Ethel Betty))
  	(newline)
    (define (looper a)
    (if (null? a) 'done 
        (begin (display (car a)) (newline) (looper (cdr a)))))
    (looper ans))

(define (member a b)
    (cond ((null? b) false)
    (else (or (= a (car b)) (member a (cdr b))))))
(define (distinct? items)
(cond ((null? items) true)
((null? (cdr items)) true)
((member (car items) (cdr items)) false)
(else (distinct? (cdr items)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (checker l)
    (let ((Mary (car l))
        (Kitty (cadr l))
        (Joan (caddr l))
        (Ethel (cadddr l))
        (Betty (cadr(cdddr l))))
    (if (and
            (or(= Kitty 2)
            (= Betty 3))
            (or(= Ethel 1)
            (= Joan 2))
            (or(= Joan 3)
            (= Ethel 5 ))
            (or(= Kitty 2)
            (= Mary 4 ))
            (or(= Mary 4)
            (= Betty 1 ))
            (distinct? (list Mary Kitty Joan Ethel Betty))
         	(not
            (or
            (and (= Kitty 2)
            (= Betty 3))
            (and(= Ethel 1)
            (= Joan 2))
            (and(= Joan 3)
            (= Ethel 5 ))
            (and(= Kitty 2)
            (= Mary 4 ))
            (and(= Mary 4)
            (= Betty 1 )))))
        true
        false)))



(define (dwelling)
    (define (looper n all passed)
        (cond ((> n 5)'ok)
                ((> all 5) 
                    (if (checker passed) (set! ans (cons passed ans)) false))
            (else (looper 1 (+ all 1)(cons n passed)) 
                    (looper (+ n 1) all passed) )))
    (looper 1 1 '()))



(dwelling)
(printer)