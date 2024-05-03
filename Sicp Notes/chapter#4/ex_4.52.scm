(define (eval-if-fail exp message)
    (lambda (env succeed fail) 
    ((analyze exp) 
        env 
        succeed 
        (lambda ()  ((analyze message) env succeed fail ) )) ))