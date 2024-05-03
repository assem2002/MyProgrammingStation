(define (whatever a b c) (begin a b c))
(define count 0)
(whatever 1 (set! count (+ count 1)) 3)
count ; => 0

;The previous program should output count as 1 (intended answer), while the actual output is 0.

;a)
; Ben code is right as the begin procedure applies a procedure that has primitve procedure (newline) and (display)
; thus they converge with the intended evalution of the program.

;b)
; Normal ---> (p1 1) => (1 2) , (p2 1) => 1
; cy's approach ---> (p1 1) => (1 2) , (p2 1) => (1 2)
;NOTE-TO-ME : '(2) would be evaluted to '(2) which is interpreted as (list 2)

;c) actual value work both ways whether you have a 'thunk tagged object or actual expression.

;d) Cy's method is good as it handles the cases we describe, but I've no idea why would some one send a procedure
; that would be already evaluated (if we use applicative order)
; It seems unreasonable to use this case, though it's better to have the implementation of eval-sequence.

