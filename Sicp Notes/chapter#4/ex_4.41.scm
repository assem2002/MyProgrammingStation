(define ans '())
(define (printer)
    (display '(smith miller fletcher cooper baker))
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

(define (checker l)
    (let ((smith (car l))
        (miller (cadr l))
        (fletcher (caddr l))
        (cooper (cadddr l))
        (baker (cadr(cdddr l))))
    (if (and 
            (not (= cooper 1))
            (not (= fletcher 5))
            (not (= fletcher 1))
            (not (= baker 5))
            (> miller cooper)
            (not (= (abs (- smith fletcher)) 1))
            (not (= (abs (- fletcher cooper)) 1))
            (distinct? (list baker cooper fletcher miller smith))

        ) true false)))



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