 
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
; It's the main function that gets things working. it starts of by taking the read and transforming the variable with '?' to (? var) to be proccessed with ease.
; it checks if the q is an assertion(data of rule) otherwise it's a query.
; if assertion is just adds it using add-rule-or-assertion function.
; if it's a query starts evaluating the query and pushing a stream with single empty frame.
; it would map on the resulting frames and instantiate a the query with the values found in the frame.
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map (lambda (frame)(instantiate q frame (lambda (v f) (contract-question-mark v)))) (qeval q (singleton-stream '()))))
(query-driver-loop)))))

; It starts looking through the query and if this variable exist it would return it.
; if it doesn't find a variable it would use the unbound-var-handler which does some weird process :).
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


; this is a function for handling simple queries (the ones that doesn't have and, or ,lisp-value).
; it maps on the frames passed to it and starts looking for assertions + rules and then add the result togehter by appending, finally it flatens the frames as
; find-assertions could result in stream of frames and apply-rules could do the same which is a thing we would to flaten.
(define (simple-query query-pattern frame-stream)
  (stream-flatmap (lambda (frame) (stream-append-delayed (find-assertions query-pattern frame) (delay (apply-rules query-pattern frame)))) frame-stream))

; It's a simple function for handling 'and'.
; It follows the same figure mentioned in the book, it evaluates each query one by one pushing the frame of the current one to the next one and so on.
(define (conjoin conjuncts frame-stream) ;  and
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

; Register conjoin to work with 'and' + 'qeval' combination.
(put 'and 'qeval conjoin)

; It's also a simple function for handling 'or'
; it fetches the content of the 'or' and start evaluating them one by one using the same stream of frames, unlike and it pushed the stream of frames from one to another.
; It uses interleave-delay for a reason I don't know about :).
(define (disjoin disjuncts frame-stream) 
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

; Register disjoin to work with 'or' + 'qeval'
(put 'or 'qeval disjoin)

; It maps through the frames in the stream and passes the frame and query to get evaluated;
; if it happens that there's a match for that query that means we shouldn't use this frame otherwise that frame looks good.
; I'm assuming that qeval returns null stream if it finds to match for the passed query even if the passed frame has some bound variables in it.
(define (negate operands frame-stream)
  (stream-flatmap (lambda (frame) (if (stream-null? (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

; Register negate to work with 'not' + 'qeval'
(put 'not 'qeval negate)

; It uses the instantiate function to create a thing similar predicate operation that are used in lisp syntax just to ease things and use execute
; which uses apply from underlying lisp to evaluate this simple predicate expression.
(define (lisp-value call frame-stream)
  (stream-flatmap (lambda (frame) (if (execute (instantiate call frame (lambda (v f) (error "Unknown pat var: LISP-VALUE" v))))
                                      (singleton-stream frame)
         the-empty-stream))
   frame-stream))

; Register lisp--value to work with 'lisp-value' + 'qeval'
(put 'lisp-value 'qeval lisp-value)

; executes things using the normal apply function provided by lisp.
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

; more of a dummy function just to have a thing that represetns an always true assertion.
(define (always-true ignore frame-stream) frame-stream)
; Register always-true to work with 'or' + 'qeval'
(put 'always-true 'qeval always-true)

; it takes a pattern and a frame to use.
; it maps on canidate patterns (based on index of the first datum of the query),
; then it check whether this assertion completely matches the pattern or not absolutly with respect to the passed frame.
(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

; This function just checks for the assertion if it exist or not (the real work in being done in pattern-match).
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

; loops through the pattern and the candidate pattern and compare with the query.
; if it happens that they're totally equal that means don't extend frame and return that frame.(why won't do anything else? because a complete equality means the primitve block of a query.you have nothing else to do)
; it the query has a pattern we use extend-if-consistent (which mainly extends the frame and if it happens that the variable already exist it would check if the value of it is similar to
; to the pattern or generally it rechecks every thing complete equality, pair check, more variable unwindings)
; if pair just dive one more level deeper.
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

;if exist apply pattern-match again.
; if not just extend the frame with this variable and the value.
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

; it just fetches the candidate rules based on index,
; then in maps of them and use apply-a-rule
(define (apply-rules pattern frame) 
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

; it creates new variable namings for the rule as it the variables could colide with other functions or maybe the same function or even the pattern.
; it creates a frame with the variables being bound to some values or even some other variables.
; if unification works we start evaluating the body based on that frame resulting from unifying.  (You'd notice the similarity between this and function environment)
(define (apply-a-rule rule query-pattern query-frame)
(let ((clean-rule (rename-variables-in rule)))
  (let ((unify-result (unify-match query-pattern
                                   (conclusion clean-rule)
                                   query-frame)))
    (if (eq? unify-result 'failed)
        the-empty-stream
        (qeval (rule-body clean-rule)
               (singleton-stream unify-result))))))

; it walks through the whole structure of the rule (conclusion and body) and replaces the variable names with the same variable+ unique id
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

; THIS FUNCTION IS QUITE WEIRD
; I think it tries to loop on the candidate rule and the pattern and whenever it catcher a variable on one side,
; it starts using extend-if-possible (which finds if there's a binding in the frame for both var and val)
; why do we do that on both of p1 p2?
; they both could be holding a variable so we need to do it both ways.
; the example I have in mind is : (rule (whatever (?x ?y))) against (whatever ?z).
; this for sure needs to bind ?z to the whole pair not the other way around.(forsure, You can't bind (?x to ?z) and (?y to ?z))
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))
; this extend the frame if we have (var against datum)
; do unify-match whenever we have a binding to make sure we set the last piece of this bindings to the right val.( or var :) ).
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding (unify-match (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) 'failed)
          (else (extend var val frame)))))



(define THE-ASSERTIONS the-empty-stream)
; based on it has an index or not, return indexed assertions or all the assertions we have. 
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)

; out of a table and 2 keys we get some stream based on the index. 
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
  (if s s the-empty-stream)))


(define THE-RULES the-empty-stream)
; based on it has an index or not, return indexed rules or all the rules we have. 
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
; an indexed rule means a rule that starts with some pattern we want or a variable '?'.
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

; It's a little foggy for me what kind of situation this function would try to figure out.
; but if they're the same, there would be a dependency and then we should accept this as unification.
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

; self explaining :)
(define (add-rule-or-assertion! assertion)
(if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion)))

; call the index store to handle the assertion if it can be stored or not.
; and adds new assertion to THE-ASSERTIONS data base. 
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

; does the same as with adding assertions.
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

; checks if the assertion is indexable, if so, it calls the stream of assertions tied with the key and adds the assertion to that stream.
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream
                assertion
                current-assertion-stream))))))

; the same as assertion.
; the differnce would be that rules could have their first symbol as variable '?' and this is an indexable too.
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

; self explaining :).
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))
; it deals with '?' as same as a var.
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

; checks whether the car of the pattern is a symbol or not.
(define (use-index? pat) (constant-symbol? (car pat)))

; just appends two streams togther (notice that the other one is delayed)
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))


; it interleaves two streams
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))

; it flattens expression like this (1(2(3(4))))
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

; just creates a stream out of a frame
(define (singleton-stream x)
  (cons-stream x the-empty-stream))
; used to fetch the type of rule or the assertion.
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
; skips the type and fetches the content.
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

; just some accessors.
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

; check if rule or not.
(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

; check if the input is an assertion or not (assertion or rule)
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp) (car (contents exp)))

; it maps over the query and changes any variable from ?var -> (? var)
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))


(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define new-rule-application-id 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

;It should be removing the ? from the frame, but I can't see that.
(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))
; manages a query whether it's an and,or,lisp-value, not or a simple query (assertion or rule)
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))
