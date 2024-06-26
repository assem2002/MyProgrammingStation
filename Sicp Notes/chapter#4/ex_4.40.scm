

(define (multiple-dwelling)
    (let ((fletcher (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5)) )
        (require (not (= fletcher 1)))
        (require (not (= cooper 1)))
        (require (not (= (abs (- fletcher cooper)) 1))) 
        (let ((miller (amb 1 2 3 4 5)))
            (require (> miller cooper)) 
            (let ((smith (amb 1 2 3 4 5)) )
                (require (not (= (abs (- smith fletcher)) 1))) 
                (let ((baker (amb 1 2 3 4 5)) )
                    (require (not (= baker 5))) )))))