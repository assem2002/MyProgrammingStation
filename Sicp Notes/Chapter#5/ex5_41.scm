(define (find-variable var env)
    (define (search-single-frame frame depth)
        (cond 
            ((null? frame)  -1)
            ((eq? (car frame) var) depth) 
            (else (search-single-frame (cdr frame) (+ depth 1)))))
    (define (search-frames env frame-depth)
        (if (null? frame-depth) "Not-Found"
            (let (current-frame-res (search-single-frame (car env) 0)) 
                (cond 
                    ((= current-frame-res -1) (search-frames (cdr env) (+ frame-depth 1)))
                    (else (cons frame-depth current-frame-res))))))
    (search-frames env 0))