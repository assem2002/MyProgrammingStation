
;If the quotation is meant to be used for another purpose other than defining a list this code would fail.
;If we want them to work properly we'd need to set the car and the cdr of the interpreter itself to be the lazy definition.

;Install this in 'eval' procedure in the evaluator.
((quoted? exp) (map-special (text-of-quotation exp) ))
(define (map-special l)
(define (mapp items)
    (if (null? items)
    '()
(list 'lambda '(m) (list 'm (car items) (mapp (cdr items))))))
(mapp  l))

(define a (map-special '(1 2 3)))
