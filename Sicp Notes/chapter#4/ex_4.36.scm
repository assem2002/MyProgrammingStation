(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-starting-from low )))
        (let ((j (an-integer-starting-from i )))
            (let ((k (an-integer-starting-from j)))
                (require (= (+ (* i i) (* j j)) (* k k)))
                (list i j k)))))

;If we use the previous solution the k would work infintly and won't converge at all as k =sqrt(2) which isn't an integer to full fill the require condition.
; (k^2 - i^2 = j^2) the difference between the two parameters should be less than 3, you can prove that from the equation(if j is even or odd).
; if my perception about amb is right the solution would be like that :

(define (a-pythagorean-triple-between low high)
(let ((k (an-integer-starting-from low )))
    (let ((i (an-integer-between k (+ k 2) )))
        (let ((j (an-integer-between i k)))
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))


;The previous solution would work with specifics the best solution is :
(define (a-pythagorean-triple-between low high)
(let ((i (an-integer-starting-from low )))
    (let ((j (an-integer-between low i )))
        (let ((k (an-integer-between i (* 2 (* i i)))))
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))
;If the permutation is an important thing in this problem, so it would be inefficient in this task.


