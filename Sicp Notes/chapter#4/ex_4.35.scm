(define (a-pythagorean-triple-between low high)
    (require (< low high))
    (amb low (a-pythagorean-triple-between (+ low 1) high)))

