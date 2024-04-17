(define (letrec? exp) (tagged-list? exp 'letrec))

(define (eval-letrec exp)
    (define vars (map car (cadr exp)))
    (define lambdas (map cadr (cadr exp)))
    (define rest-of-body (cddr exp))
    (define (construct-defines vars lambdas)
    (cond ((null? vars) rest-of-body)
            (else (cons (list 'define (car vars) (car lambdas))
                        (construct-defines (cdr vars) (cdr lambdas))))))
    (append 'let (construct-defines vars lambdas)))
;Then the code from ex 4.16 would handle the rest of the work.


;The arguments when passed (if procedure) creates its procedure object pointing to the environment that it was created in,
;thus if you use normal let the lambdas passed would point to the outter scope while the internals of the procedures points to some varaibles that aren't defined in that scope.
;If we use the unassigned style the names are already built and existing we just mutate them while we use "set!".

;see this for further explanation -> http://community.schemewiki.org/?sicp-ex-4.20



