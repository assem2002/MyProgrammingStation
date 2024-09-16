; The structure I remember is --> (env1 env2 env3 ...)
; and each env has this structure --> ((var1 val1) (var2 val2) ...)
(define (lexical-address-lookup address runtime-env)
    (define (fetch-scope runtime-env depth) 
        (if ( = depth 0 ) (car runtime-env) 
            fetch-scope (cdr runtime-env) (- depth 1)))
    (define fetch-var scope depth)
        (if (= depth 0) 
            (car scope)
            (fetch-var (cdr scope) ( - depth 1))))