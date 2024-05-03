(define (not-colliding l current-row )

    (define (looper current rest next-row)
        (cond ((null? rest ) true)
              ((or (= (+ current current-row) (+ (car rest) next-row))
                  (= (abs(- current current-row)) (abs(- (car rest) next-row)))) false)
              (else (looper current (cdr rest) (+ next-row 1)))))

    (if (null? l) 
        'true
        (and (looper (car l) (cdr l) (+ current-row  1))
             (not-colliding (cdr l) (+ current-row 1)))))

(define (eight-queens)
    (let( 
    (q1 (amb 1 2 3 4 5 6 7 8))
    (q2 (amb 1 2 3 4 5 6 7 8))
    (q3 (amb 1 2 3 4 5 6 7 8))
    (q4 (amb 1 2 3 4 5 6 7 8))
    (q5 (amb 1 2 3 4 5 6 7 8))
    (q6 (amb 1 2 3 4 5 6 7 8))
    (q7 (amb 1 2 3 4 5 6 7 8))
    (q8 (amb 1 2 3 4 5 6 7 8)))
    (require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8 ))) ; should've placed this condition for every pairs of queens to prune the branches faster,but i choose violence.
    (requier (not-colliding (list q1 q2 q3 q4 q5 q6 q7 q8 ) 1 ))))