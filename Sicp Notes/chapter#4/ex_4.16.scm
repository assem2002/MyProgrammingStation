;scan-out

;a

(define (lookup-variable-value var env)
(define (env-loop env)
    (define (scan vars vals)
        (cond ((null? vars)
                (env-loop (enclosing-environment env)))
            ((and (eq? var (car vars)) (eq? (car vals) '*unassigned*)) (error "This variable isn't assigned yet"))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
    (error "Unbound variable" var)
    (let ((frame (first-frame env)))
        (scan (frame-variables frame)(frame-values frame)))))
(env-loop env))


;b (working but racket has conflicts with internal defines)


(define (scan-out exp)
 
(define (fetch-define body)
  (cond ((null? body) '())
        ((eq? (caar body) 'define) (cons (car body) (fetch-define (cdr body))))
        (else (fetch-define (cdr body)) )))
  (define (fetch-normal body)
  (cond ((null? body) '())
        ((not (eq? (caar body) 'define)) (cons (car body) (fetch-normal (cdr body))))
        (else (fetch-normal (cdr body)) )))
  
  (define normals (fetch-normal (cddr exp)))
    (define defines (fetch-define (cddr exp)))

  (define defines-vars (map cadr defines))
  (define defines-vals (map cddr defines))
  (define (let-vars defines-vars)
    (cond ((null? defines-vars) '()) 
    (else (cons (list (car defines-vars) '*unassigned*) (let-vars (cdr defines-vars)) ))))

  (define let-vars-ready (list'let
                          (let-vars defines-vars) ))
  
  (define (sets defines-vals defines-vars)
    (cond ((null? defines-vals) '())
          (else (cons (list 'set! (car defines-vars) (car defines-vals)) (sets (cdr defines-vals) (cdr defines-vars))))))
  (define body-ready (append (sets defines-vals defines-vars) normals))
  (define final (append let-vars-ready body-ready))
  (make-lambda (cadr exp) final )
  )



;C
;MY solution builds a whole lambda expression from scratch so probaby i would modify 
;the make-procedure to do a scan-out on the lambda and then transform it into procedure to be evaluated.



