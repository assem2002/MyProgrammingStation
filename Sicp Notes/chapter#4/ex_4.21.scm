((lambda (n)
((lambda (fact) (fact fact n))
(lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))) 5)

;Fibonacci  
((lambda (n) 
   (cond ((= n 1) 0)
    (else ((lambda (fib) (fib fib 2 0 1))
           (lambda (ft k f s) (if (= k n) s
                                  (ft ft (+ k 1) s (+ f s))))))))7)



(define (f x)
((lambda (even? odd?) (even? even? odd? x))
(lambda (ev? od? n)
(if (= n 0) true (od? ev? od? (- n 1))))
(lambda (ev? od? n)
(if (= n 0) false (ev? ev? od? (- n 1))))))