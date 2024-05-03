

(define (parse-word word-list)
(require (not (null? *unparsed*)))
(let ((found-word (amb (map (lambda (x) (list (car word-list) x))(cdr word-list)))))
    (set *unparsed*  (cdr *unparsed*)) ; just to stop the code from runing forever
    found-word))


(define *unparsed* '())
(define (parse input)
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
        (require (null? *unparsed*)) 
        sent))

