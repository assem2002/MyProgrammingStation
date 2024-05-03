(define (cube x)
    (* x x x))
(cube (map sum '(some-large-list)))
; x here would be calculated 3 times for the sake of same result.

;memoized (normal)
;sqaure -> 100
;count -> 1

;non-momized 
;sqaure -> 100
;count ->2
;as id