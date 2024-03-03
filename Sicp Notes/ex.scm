ex 1.29


(define (cube x)
  (* x x x))
(define (sum term a next b)
  (if(> a b) 0 
     (+ (term a)
        (sum term (next a) next b))))

(define (evaluate-h a b n)  (/ (- b a) n))

(define (simpson-intergral f a b n)
  (simpson-integral-helper f a b n (evaluate-h a b n)))

(define (simpson-integral-helper f a b n h)
  (define (myNext iter) (+ iter 1))
  (define (f-modified x ) (if (even? x) (* 2 (f (+ a (* x h)))) 
                              (* 4 (f (+ a (* x h))))))
  (* (/ h 3.0) (+ (f a) (sum f-modified 1 myNext (- n 1) ) (f b))))
(simpson-intergral cube 0 1 100)


-----------------------------------------------------------------------
ex 1.30


(define (sum term a next b)
(define (iter a result)
(if (> a b ) result
(iter (next a)  (+ result (term a)) )))
(iter a 0))
----------------------------------------------
ex.1.31




(define (product term a next b)
  (if (> a b) 1
      (* (term a ) (product term (next a) next b) )))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* result (term a) ))))
  (iter a 1))


(define (res x) (*(/ x (- x 1)) (/ x (+ x 1))) )
(define (nextt x) (+ x 2))

(define (get-pi limit)
  (* (product-iter res 2 nextt limit) 2))
(get-pi 2000)
-------------------------------------------------------
ex 1.32



(define (accumulator combine null-value term a next b)
  (if (> a b) null-value
  (combine (term a) (accumulator combine null-value term (next a ) next b ))))

(define (cc x y )(+ x y ) )
(define (tt x) x)
(define (nn x) (+ x 1))
(accumulator cc 0 tt 1 nn 10)



(define (accumulator-iter combine null-value term a next b)
  (define (acc-helper a result)
    
    (if (> a b) result
        (acc-helper (next a ) (combine result (term a )) )))
  (acc-helper a null-value))


(accumulator-iter cc 0 tt 1 nn 10)








------------------------------------------------------
ex 1.33 a 



(define (square x)(* x x))



(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))




(define (accumulator filter combine null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combine (term a) (accumulator filter combine null-value term (next a ) next b )))
        (else (combine null-value (accumulator filter combine null-value term (next a ) next b )) )))
        
        
        
        
 ex 1.33 b
 
 
 (define (square x)(* x x))
(define (gcd x y)
  (if (= y 0) x (gcd y (remainder x y))))



(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))




(define (accumulator filter combine null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combine (term a) (accumulator filter combine null-value term (next a ) next b )))
        (else (combine null-value (accumulator filter combine null-value term (next a ) next b )) )))

(define (tt x) x)
(define (nn x) (+ x 1))
(accumulator  prime? + 0 square 0 nn 10)


(define (realtive-primes n)
  (define (gcd-filter x) (= 1 (gcd x n))  )
  (accumulator gcd-filter * 1 tt 1 nn n))

(define (accumulator-iter combine null-value term a next b)
  (define (acc-helper a result)
    
    (if (> a b) result
        (acc-helper (next a ) (combine result (term a )) )))
  (acc-helper a null-value))


(accumulator-iter + 0 tt 1 nn 10)
(gcd 2 4)
(realtive-primes 10)
--------------------------------------------------------------------------------------
ex 1.35





(define (good-enough? x y) ( < (abs (- x y)) 0.00001) )
(define (fixed-point f x) 
  (let ((res (f x)))
    (if (good-enough? res x) res (fixed-point f res))) )
(fixed-point (lambda (x) (+ 1(/ 1 x))) 1)




----------------------------------------------------------------------------------------
ex 1.36




(define (good-enough? x y) ( < (abs (- x y)) 0.00001) )
(define (fixed-point f x) 
  (let ((res (f x)))(display res)(newline)
    (if (good-enough? res x) res (fixed-point f res))) )
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)


--------------------------------------------------------------
ex 1.37 a


(define (cont-frac n d k)
  (if (= k 0) 0 (/ (n 1) (+ (d 1) (cont-frac n d (- k 1)))) ))
(cont-frac (lambda (i) 1.0)
(lambda (i) 1.0)
11)

--------------------------------------------------------------
ex 1.38 b



(define (cont-frac n d k)
 (define (helper result k)
   (if (= k 0) (- result (d 88)) 
       (helper (+ (d 99) (/(n 88) result))(- k 1))))
  (helper 1 k))
(cont-frac (lambda (i) 1.0)
(lambda (i) 1.0)
10)
---------------------------------------------------------------
ex 1.39 

(define (cont-frac n d k)
 (define (helper result k)
   (if (< k 0) result 
       (helper (+ (d k) (/(n k) result))(- k 1))))
  (helper (d k) (- k 1)))




(define (tanFunction x k)
  (cont-frac
   (lambda (i) (if (= i 0) x (* (* x x) -1)))
   (lambda (x) (if(= x 0) 0 (+ x (- x 1))))
             k ))


(tanFunction 1 8)


-------------------------------------------------------------------
ex 1.40

(define (good-enough x y)(< (abs (- x y)) 0.00001))
(define (fixed-point f guess)
  (define (try-again result guess)
    (if (good-enough result guess) result (try-again (f result) result)))
  (try-again (f guess) guess))



(define dx 0.000001)
(define (derivFormula g) 
  (lambda (x) (/ (- (g (+ x dx)) (g x) ) dx) ) )

(define (newtonFormula f)
  (lambda (x) (- x (/(f x) ((derivFormula f)x)))))

(define (gerneralFixedPoint f transfrom guess)
  (fixed-point (transfrom f) guess) )

(define (newtonMethod func guess) 
  (fixed-point (newtonFormula func) guess ) )

(define (find-cubic a b c)
  (newtonMethod (lambda (x) (+(* x x x )(* x x a)(* x b) c) ) 1))
(find-cubic 1 1 1)



-----------------------------------------
ex 1.41


(define (inc x)(+ x 1))
(define (double f) (lambda (x) (f (f x)) ))
(lambda (x) (double (double x)) )
(lambda (x) (s (s x)) )
(((double (double double)) inc) 4)


explaination:


------------------------------------------
ex 1.43


(define (repeated func times)
  (define (internal-repeater given times)
    (if (= times 0 ) given (internal-repeater (compose func given) (- times 1))))
  (internal-repeater func (- times 1)))
((repeated square 2)5)

-----------------------------
ex 1.44

(define (smooth func)
            (lambda (x) (/ (+(func x)(func (- x dx)) (func (+ x dx))) 3)))
              
(define (smooth-N func n)
  (repeated (smooth func) n))
-------------------------------------
ex 1.46


(define (square x)
  (* x x))


(define (iterative-improve isgood improver)
  (lambda (guess) (if(isgood guess) (improver guess) ((iterative-improve isgood improver)(improver guess)))))

(define (squareRoot x)
  (define (isgoodSqrt guess) (< (abs(- (square guess) x)) 0.0001)) 
  ((iterative-improve isgoodSqrt (lambda (y) (/(+(/ x  y)y)2)))1))


(squareRoot 9)
-------------------------------------------

ex 2.1



;start of data representation of rational numbers.

(define (gcd a b) 
  (if (= b 0)
      a
      (gcd b (remainder a b) )
      ))

(define (make-rat x y)
  (define (makeNormalized ) (if (or (and (< x 0 )(< y 0)) (and (> x 0 )(> y 0)) )
                                1 
                                -1))
  (let ((g (gcd (* (makeNormalized) x) ( abs y) )))
    (cons (/ (* (makeNormalized) x) g)
          (/ ( abs y) g)) ))

(define (numer x) 
  (car x))

(define (domen x)
  (cdr x))
;end of the representation.

(define (sum-rat x y)
  (make-rat (+(*(numer x)(domen y )) (*(numer y)(domen x)))
            (*(domen x)(domen y))))


(define (mul-rat x y) 
  (make-rat (*(numer x)(numer y))
            (*(domen x)(domen y))) )
(define half (make-rat -3 -4))
(define quarter (make-rat 5 5))
(mul-rat half quarter)


-------------------------------------------------------------------
ex.2.2



(define (make-point x y)
  (cons x y))
(define (getX point)
  (car point))
(define (getY point)
  (cdr point))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-seg segLine)
  (car segLine))
(define (end-seg segLine)
  (cdr segLine))

; Try to make a layer of abstraction between the points and the segemnt just for the sake of making a barrier (there's better solution for it btw)

(define (getXofPoint p)
  (getX p))
(define (getYofPoint p) 
  (getY p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (getmid-seg segline)
  (let((p1 (start-seg segline)) (p2 (end-seg segline)))
    (make-segment (/ (+(getXofPoint p1)(getXofPoint p2))2)
       (/(+(getYofPoint p1)(getYofPoint p2))2)) 
    ))
(define seg (make-segment (make-point 1 5) (make-point 5 10)))
(getmid-seg seg)


----------------------------------------------------------------------
ex 2.3


(define (make-point x y)
  (cons x y))
(define (getX point)
  (car point))
(define (getY point)
  (cdr point))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Rectangle making
;the rep
(define (create-rec p1 p2 p3)
  (cons (cons p1 p2) p3 ))
(define (height rec-obj)
  (abs (-(getX(car (cdr rec-obj))) ((getX(car (car rec-obj)))) ))
  )
(define (width rec-obj)
  (abs (-(getY(car (cdr rec-obj))) ((getY(cdr (cdr rec-obj)))) ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This layer uses the with and height as interfaces,and you can implement those in so many different ways without violating the contract with the procedures below.
(define (calc-perimeter width height )
  (* 2 (+ height width)))

(define (calc-area width height)
  (* width height))




----------------------------------------------------------------
ex 2.5


(define (square x)
  (* x x))

(define (pow base exp)
  (cond ((= exp 0) 1)
      ((even? exp) (pow (square base) (/ exp 2)))
      (else (* base (pow base (- exp 1))))
        ))


(define (iterNum n div i)
  (if (>(remainder n div)0) i (iterNum (/ n div ) div (+ i 1) )))


(define (cons x y)
  (*(pow 2 x) (pow 3 y))) ;prime works for such a representation.
(define (car x) 
  (iterNum x 2 0 ))
(define (cdr x) 
  (iterNum x 3 0))


-------------------------------------------------------------------  
ex 2.6


(define one (lambda(f) (lambda (x) (f x) )))
(define two (lambda(f) (lambda (x) (f(f x)) )))

(define (addition n1 n2) 
  (lambda(f) (lambda (x) (n2(n1 (f x) )))
  
  
--------------------------------------------------------------------
ex 2.7

(define (max a b)(if (> a b) a b))
(define (min a b)(if (< a b) a b))

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (higher-bound interval) (max (car interval) (cdr interval)))
(higher-bound (make-interval 3 4) )

----------------------------------------------------------------------
ex 2.8


(define (max a b)(if (> a b) a b))
(define (min a b)(if (< a b) a b))

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (higher-bound interval) (max (car interval) (cdr interval)))
(higher-bound (make-interval 3 4) )
 
 
 ;it's seems weird but, it's just arthimtic intervals subtraction.
(define (sub-interval a b )
  (make-interval (- (lower-bound a) (higher-bound b))
                 (- (higer-bound a) (lower-bound b))
                 ))
                 

--------------------------------------------------------------------
ex 2.9


(define (max a b)(if (> a b) a b))
(define (min a b)(if (< a b) a b))





(define (make-interval a b) (cons a b))
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define iter1 (make-interval 3 4) )
(define iter2 (make-interval 2 7))

(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
(+ (upper-bound x) (upper-bound y))))

(define (add-interval-modified x y i )
	(add-interval (make-interval (*(lower-bound x) i)
                   (*(upper-bound x) i))
	(make-interval (*(lower-bound x) i)
                   (*(upper-bound x) i)))
  )
 ;it's seems weird but, it's just arthimtic intervals subtraction.
(define (sub-interval a b )
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))
                 ))

(define (width a mul)
  (/ (-(* (upper-bound a) mul)(* (lower-bound a) mul)) 2))

(define (test iterations i)
  (cond (( = iterations 0) (display "finish")) 
     (else (display (width iter1 i)) 
      (display " ***** ")
      (display (width iter2 i))
      (display " ***** ")
      (display (width (add-interval-modified iter1 iter2 i ) 1))
      ( newline)
      (test (- iterations 1) (+ i 1 ))
     )))


(test 5 1)
; you notice from the results that there's a relationship (you can prove it mathemtically)
---------------------------------------------------

ex 2.17 + 2.18




(define (last-pair l)
  (if(null? (cdr l)) l
     (last-pair (cdr l))))

(define (reverse l)
  (define (reverse-internal l catch)
    (if (null? l) catch
        (reverse-internal (cdr l) (cons (car l) catch))
        ))
      (reverse-internal (cdr l) (cons (car l) nil)))

(reverse (list 1))



------------------------------------------------
ex 2.19



(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10))
(define uk-inver (list  10 20 50 100))


(define (no-more? x) (null? x))
(define (first-denomination l)(car l))
(define (except-first-denomination l) (cdr l))

(define (cc amount coin-values)
(cond ((= amount 0) 1)
((or (< amount 0) (no-more? coin-values)) 0)
(else
(+ (cc amount
(except-first-denomination
coin-values))
(cc (- amount
(first-denomination
coin-values))
coin-values)))))
(cc 100 uk-inver)
(cc 100 uk-coins)


----------------------------------------
ex.2.20
;This Solution has a flaw with a list of size less than 3,but could be handled easily with if statement. 



(define (jumping l)  (if (or (null? (cdr l)) (null? (cdr (cdr l))) ) (cons (car l) nil)
                          (cons (car l) (jumping (cdr(cdr l)))) ))


(define (giveMe y )(cond ((even? y) cdr) (else (lambda (x) x)) ))
(define (same-parity m . w)
  (cons  (car((giveMe m) w)) (jumping (cdr (cdr ((giveMe m)w) )))))
 
(same-parity 1 2 3 4 5 6 7 8 9)
-------------------------------------------------------------

 
ex 2.21



(define (map procedure items)
  (if (null? items) nil
      (cons (procedure (car items)) (map procedure (cdr items)))))
(define (square-list l)
  (map (lambda(x) (* x x)) l))
(define (square-list1 l)
  (if (null? l) nil(cons (*(car l) (car l)) (square-list1 (cdr l)) )))
(square-list1 (list 1 2 3 4))
-----------------------------------------------------------------

ex 2.23

(define (for-each procedure items)
        (cond ((null? items) true)
            (else (procedure (car items)) (for-each procedure (cdr items)) )))

(for-each (lambda (x)
(newline)
(display x))
(list 57 321 88))
---------------------------------------
ex 2.27 (I should've used the map function that i built before to reverse all the sub trees of the main big tree in a recursive way, but i chose violence.)

(define (custom-reverse l)
  (define (iter l catch)
   (cond( (null? l) catch )
        ((pair? (car l)) (iter (cdr l) (cons (iter (car l) nil) catch)))
       (else (iter (cdr l) (cons (car l) catch)))))
  (iter l nil))
 

(custom-reverse (list (list 1 2) (list 3 4) (list 5 6)))


---------------------------------------------
ex 2.28

(define (fringe l)
 (cond 
       ((null? l) nil)
  	((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
       (else (cons (car l) (cdr l))))
  )
(define x (list (list 1 2) (list 3 4)))
 (fringe '((((5) 2) ((3 2) 9))))
 -----------------------------------------
 ex 2.29 a+b
 
(define (make-mobile left right)
(list left right))
(define (make-branch length structure)
(list length structure))


(define (left-branch mobile)(car mobile) )
(define (right-branch  mobile) (car(cdr mobile)))
(define (branch-length branch)(car  branch))
(define (branch-structure branch) (car(cdr branch)))


(define (total-weight mobile)
  (cond ((null? mobile) 0 ) 
        ((pair? (branch-structure mobile) ) (+(total-weight (left-branch mobile)) (total-weight (right-branch mobile))) )
        (else (branch-structure mobile)))
  )
---------------------------------------------


ex 2.30



(define (square x)
  (* x x))

(define (map procedure l)
  (if (null? l) nil
      (cons (procedure (car l)) (map procedure (cdr l)) ))
  )

(define (square-tree l)
  (cond ((null? l) nil)
        ((number? l) (square l))
        (else (cons (square-tree (car l)) (square-tree (cdr l)) ))
        ))

(define (square-tree-mapped l)
  (map (lambda (x) (if (pair? x) (square-tree-mapped x) (square x) )) l)
  )
(square-tree-mapped
(list 1
(list 2 (list 3 4) 5)
(list 6 7)))
;(1 (4 (9 16) 25) (36 49))

--------------------------------------------

ex 2.31



(define (tree-map-yourprocedure procedure l) ; I wrote the name of it as tree-map-yourprocedure cuz it's a little intuitve when you build the procedure that you either do the procedure you want or map over the tree (george's problems not mine to do )  
  (map (lambda (x) (if (pair? x) (tree-map-yourprocedure procedure x) (procedure x) )) l)
  )
(define (square-tree l) 
  (tree-map-yourprocedure square l))
(square-tree (list 1 2 (list 3 4)))
----------------------------------------------------------

ex 2.32



(define (subsets s)
(if (null? s)
(list nil)
(let ((rest (subsets (cdr s))))
(append  (map (lambda (x) (cons (car s) x )) rest)rest))))
(subsets '(1 2 3 ))


----------------------------------------------------------
ex 2.33



(define (accumulate op finisher seq)
  (if (null? seq) finisher 
      (op (car seq) (accumulate op finisher (cdr seq)))
      )
  )
  
  
(define (map p sequence)
(accumulate (lambda (x y) ( cons (p x) y )) nil sequence))
(map square (list 1 2 3))



(define (append seq1 seq2)
(accumulate cons seq2 seq1))

(define (length sequence)
(accumulate (lambda (x y) (+ 1 y)) 0 sequence))

  
-------------------------------------------------
ex 2.34

(define (horner-eval x coefficient-sequence)
(accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
0
coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

-------------------------------------------
ex 2.35

(define (count-leaves t)
(accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1 )) t)))

(count-leaves (list 1 (list 2 3 4 5) 6 7 (list 8 (list 9 10))) )


------------------------------------------------

ex 2.37


(define (dot-product v w)
(accumulate + 0 (map-extended * v w)))


(define (matrix-*-vector m v)
(map (lambda(x) (dot-product x v)) m))

(define (transpose mat)
(accumulate-n cons nil mat))


(matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 2 2 2))

(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (matrix-*-matrix m n)
(let ((cols (transpose n)))
(map (lambda (x) (map (lambda (y) (dot-product x y)) cols) ) m)))

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) )

----------------------------------------------
ex 2.39


(define fold-right accumulate)

(define (reverse sequence)
(fold-right (lambda (x y) (append y (list x)) ) nil sequence))
(define (reverse sequence)
(fold-left (lambda (x y) (append (list x) y) ) nil sequence))
(reverse (list 1 2 3))
------------------------------------------------
ex 2.40 + 2.41



(define (enumerate start finish )
  (if (< finish start ) nil
      (cons start (enumerate (+ start 1) finish))))

(define (flat-map procedure s)
  (accumulate append nil (map procedure s)))
(define (unique-pair n)
(flat-map (lambda (x) (map (lambda (y) (list x y))(enumerate 1 (- x 1 )) )) 
 (enumerate 1 n) ))

(define (get-triples n k) ;deduce the third number from the first two numbers.
(flat-map (lambda (x) (let((first (car x)) (second (cadr x)) (third (- k (car x) (cadr x)) ))
                       (if (or (< third 0)(> third n ) (= third first ) (= third second )) nil (list(list first second third)) )) ) 
 (unique-pair n))
  )
(get-triples 50 30)

------------------------------------------------------
ex 2.42 ( i've stayed like thousands years on this problem because of a partnthses)

(define (adjoin-positions r c board)
  (cons (list r c) board ))

(define (safe? k positions )
  ;(newline)
  ;(display positions)
  (define (iter lookup l)
    (cond( (null? l ) true ) 
         ((= (car lookup) (car (car l)) )false)
         ((= (cadr lookup) (car(cdr(car l))) ) false)
         ((= (+ (car (car l))(car(cdr(car l)))  ) (+(car lookup)(cadr lookup)))  false)
         ((= (- (car (car l))(car(cdr(car l)))) (-(car lookup)(cadr lookup))) false)
         (else (iter lookup (cdr l) ) )
        )
    )
  (iter (car positions) (cdr positions))
  )


(safe? 1 '((1 3)(3 7)))

(define (queens board-size)
(define (queen-cols k)
(if (= k 0)
(list '())
(filter
(lambda (positions) (safe? k positions))
(flat-map
(lambda (rest-of-queens)
(map (lambda (new-row)
(adjoin-positions
new-row k rest-of-queens))
(enumerate 1 board-size)))
(queen-cols (- k 1))))))
(queen-cols board-size))
(queens 8)

--------------------------------------------------------------------------
 ex 2.44
 
 (define (up-split painter n) (if (= n 0) painter 
  (let ((smaller (up-split painter (-n 1) )))
    (below painter (beside smaller))))
  )
 -------------------------------------------------------------------------
 ex 2.45
 
 
 
(define (split  bigcombine smallcombine)
  (define (splitrec painter n) 
    (if (= n 0) painter 
        (let((small (splitrec painter (- n 1))))
          (bigcombine painter (smallcombine small small) ) ))
    )
  )
  
  
--------------------------------------
ex 2.46

(define (make-vec x y) (cons x y) )
(define (xcor-vec vec) (car vec))
(define (ycor-vec vec) (cdr vec))
(define (add-vec v1 v2) (make-vec (+ (xcor-vec v1) (xcor-vec v2))
                                  (+(ycor-vec v1)(ycor-vec v2))))

(define (sub-vec v1 v2) (make-vec (- (xcor-vec v1) (xcor-vec v2))
                                  (- (ycor-vec v1)(ycor-vec v2))))

(define (scale-vec factor v) (make-vec (* factor (xcor-vec v))
                                       (* factor (ycor-vec v))) )
----------------------------------------
ex 2.47


(define (make-frame origin edge1 edge2)
(list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define(edge2-frame) (cadr(cdr frame)))


(define (make-frame origin edge1 edge2)
(cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define(edge2-frame) (cdr(cdr frame)))
 --------------------------------------
 ex 2.48
 
 (define (make-seg v1 v2) (cons v1 v2))
(define (start-seg seg) (car make-seg))
(define (end-seg seg) (cdr make-seg))
---------------------------------------
ex 2.50

(define (flip-horz painter) (transform-painter painter (make-vec 1 0)
                                               			(make-vec 0 0 )
                                               			(make-vec 1 1)
                                               ))
(define (deg180 painter) (transform-painter painter (make-vec 1 1)
                                               			(make-vec 0 1 )
                                               			(make-vec 1 0)
                                               ))
(define (deg270 painter) (transform-painter painter (make-vec 0 1)
                                               			(make-vec 0 0 )
                                               			(make-vec 1 1)
                                               ))
 
------------------------------------------------
ex 2.51
 
 (define (below painter1 painter2)
  (lambda (frame) (((transform-painter painter1 (make-vec 0 0) (make-vec 1 0)(make-vec 0 0.5)) frame) 
                   ((transform-painter painter2 (make-vec 0 .5) (make-vec 1 .5)(make-vec 0 1)) frame) 
                   )
    )
  )
(define (below painter1 painter2)
  (deg90(beside(deg270 painter1) (deg270 painter2))))
 
 ---------------------------------------------
ex 2.54

(define (equal? l1 l2)
  (cond ((and(null? l1)(null? l2)) true)
        ((or (null? l1)(null? l2)) false)
        ((and (pair? (car l1))(pair? (car l2))) (if (equal? (car l1)(car l2)) (equal? (cdr l1)(cdr l2)) false))
        ((not(eq? (car l1) (car l2) )) false)
        (else (equal? (cdr l1) (cdr l2)))
        )
  )


(equal? '(q (a b) c (l) ) '(q (a b) c (l) ) )

-------------------------------------------------
ex 2.56


(define (exponentiation? expr)(and (pair? expr) (eq? '** (car expr))) )
(define (expon expr) (caddr expr))
(define (base expr ) (cadr expr ) )
(define (make-exponent base expon) (cond((= expon 0) 1)
                                        ((= expon 1) base)
                                        ((=number base 1) 1)
                                       (else (list '** base expon))))

(define (deriv exp var)
(cond ((number? exp) 0)
((variable? exp) (if (same-variable? exp var) 1 0))
      ((exponentiation? exp) (make-product ( make-product (expon exp)
                                     			(make-exponent (base exp) (-(expon exp)1)))
                                   (deriv (base exp) var))
 )
((sum? exp) (make-sum (deriv (addend exp) var)
(deriv (augend exp) var)))
((product? exp)
(make-sum
(make-product (multiplier exp)
(deriv (multiplicand exp) var))
(make-product (deriv (multiplier exp) var)
(multiplicand exp))))
(else
(error "unknown expression type: DERIV" exp))))
------------------------------------------------------
ex 2.57



(define (augend x) (if (null? (cdddr x)) (caddr x)
                       (append '(+) (cddr x))))
                       
                       
(define (multiplicand x) (if (null? (cdddr x)) (caddr x)
                             (append '(*) (cddr x))))
                             
-------------------------------------------------------------
             
                             
ex 2.58 a




(define (variable? x) (symbol? x))


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? expr num)
  (and (number? expr) (= expr num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ;(else (list '+ a1 a2))))
        (else (list  a1 '+ a2))))



(define (sum? expr)
  ;(and (pair? expr) (eq? (car expr) '+)))
(and (pair? expr) (eq? (cadr expr) '+)))
;(define addend cadr )
(define addend car)

(define (augend x) (if (null? (cdddr x)) (caddr x)
                        (cddr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ;(else (list '* m1 m2))))
        (else (list  m1 '* m2))))




(define (product? expr)
  ;(and (pair? expr) (eq? (car expr) '*)))
  (and (pair? expr) (eq? (cadr expr) '*)))

;(define multiplier cadr)
(define multiplier car)

(define (multiplicand x) (if (null? (cdddr x)) (caddr x)
                              (cddr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exponentiation? expr)(and (pair? expr) (eq? '** (cadr expr))) )
(define (expon expr) (caddr expr))
(define (base expr ) (car expr ) )
(define (make-exponent base expon) (cond((= expon 0) 1)
                                        ((= expon 1) base)
                                        ((=number? base 1) 1)
                                       (else (list base '** expon))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (deriv exp var)
(cond ((number? exp) 0)
((variable? exp) (if (same-variable? exp var) 1 0))
      ((exponentiation? exp) (make-product ( make-product (expon exp)
                                     			(make-exponent (base exp) (-(expon exp)1)))
                                   (deriv (base exp) var))
 )
((sum? exp) (make-sum (deriv (addend exp) var)
(deriv (augend exp) var)))
((product? exp)
(make-sum
(make-product (multiplier exp)
(deriv (multiplicand exp) var))
(make-product (deriv (multiplier exp) var)
(multiplicand exp))))
(else
(error "unknown expression type: DERIV" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(deriv '(+ x 3 (* 3 x) (* x y) ) 'x)
;(deriv '(* x y) 'x)
;(deriv '(* x  (+ x 4) (+ x 3)) 'x)

;(deriv '(* (* x y) 0) 'x)
;(deriv '(**(* x y) 2) 'x)
;(deriv '(* x y (+ x 3)) 'x)
(deriv '(x + 3 + x) 'x)
(deriv '(x ** 3) 'x)
-----------------------------------------------------------------------------------------------------
ex 2.58 b




(define (variable? x) (symbol? x))


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? expr num)
  (and (number? expr) (= expr num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ;(else (list '+ a1 a2))))
        (else (list  a1 '+ a2))))



(define (sum? expr)
  ;(and (pair? expr) (eq? (car expr) '+)))
(and (pair? expr) (eq? (cadr expr) '+)))
;(define addend cadr )
(define addend car)

(define (augend x) (if (null? (cdddr x)) (caddr x)
                        (cddr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ;(else (list '* m1 m2))))
        (else (list  m1 '* m2))))




(define (product? expr)
  ;(and (pair? expr) (eq? (car expr) '*)))
  (and (pair? expr) (eq? (cadr expr) '*)))

;(define multiplier cadr)
(define multiplier car)

(define multiplicand caddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exponentiation? expr)(and (pair? expr) (eq? '** (cadr expr))) )
(define (expon expr) (caddr expr))
(define (base expr ) (car expr ) )
(define (make-exponent base expon) (cond((= expon 0) 1)
                                        ((= expon 1) base)
                                        ((=number? base 1) 1)
                                       (else (list base '** expon))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (deriv exp var)
(cond ((and (pair? exp) (null?(cdr exp)))  (deriv (car exp) var)) 
 	((number? exp) 0)
      
((variable? exp) (if (same-variable? exp var) 1 0))
      ((exponentiation? exp) (make-product ( make-product (expon exp)
                                     			(make-exponent (base exp) (-(expon exp)1)))
                                   (deriv (base exp) var))
 )
((sum? exp) (make-sum (deriv (addend exp) var)
(deriv (augend exp) var)))
((product? exp) (let((res
(make-sum
(make-product (multiplier exp)
(deriv (multiplicand exp) var))
(make-product (deriv (multiplier exp) var)
(multiplicand exp)))))
                  (cond 
                   		(( null? (cdddr exp) ) res ) 
                        ((eq? (cadddr exp) '+) (make-sum res (deriv (cddddr exp) var)) )
                        ( (eq? (cadddr exp) '*)   (make-product res (deriv (cddddr exp ) var)) )
                        ) ))
(else
(error "unknown expression type: DERIV" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(deriv '(+ x 3 (* 3 x) (* x y) ) 'x)
;(deriv '(* x y) 'x)
;(deriv '(* x  (+ x 4) (+ x 3)) 'x)

;(deriv '(* (* x y) 0) 'x)
;(deriv '(**(* x y) 2) 'x)
;(deriv '(* x y (+ x 3)) 'x)
(deriv '(x + 3 + x) 'x)
(deriv '(x ** 3) 'x)
(deriv '(x * 3   ) 'x)
(deriv'(x + 3 * (x + y + 2)) 'x)
(deriv '(3 * (x + y * 2) + x + 1) 'x)


--------------------------------------------------------
ex 2.59
(define (union-set s1 s2) 
  (if (null? s1 ) s2
      (union-set (cdr s1) (adjoin-set (car s1) s2)) ) 
  )
  
--------------------
ex 2.60 

;wanted a whole new dataStructrue and just did it with small change in the core method of our implementation.
(define (adjoin-set x set)
(cons x set))

--------------------------

ex 2.61

(define (element-of-set? x set)
(cond ((null? set) false)
((= x (car set)) true)
((< x (car set)) false)
(else (element-of-set? x (cdr set)))))

(define (adjoin-element x set)
  (if (element-of-set x set) 
   (cons x set)
   set
   ))
;the unordered set will for sure test till it reach a duplicate item or the end of the set.
;while the ordered will stop if it reaches a duplicate of end of set or a number larger than the one we look for
;thus a set like (1 2 3 4 5 6 7 8 9) if you look for 5 by the second approach you will find it in n/2
;while the same set but not ordered (1 3 2 4 6 9 8 7 5) you' find the number in (n) time.
;still not effiecent enough but for sure has great chance of speeding up your program.


--------------------------------------------------
ex 2.62

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
   (else
  (let ((x1 (car s1) ) (x2 (car s2)))
    (cond 
     ((= x1 x2) (cons x1 (union-set (cdr s1)(cdr s2))) )
      ((< x1 x2) (cons x1 (union-set (cdr s1) s2)) )  
    	((> x1 x2) (cons x2 (union-set s1 (cdr s2)) ) )
        )))))
(union-set '(1 2 4 7) '( 1 4 5 6 7))
; it's like merge sort kinda stuff


-------------------------------------------------
ex 2.63


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))

(define (tree->list-1 tree)
(if (null? tree)
    '()
(append (tree->list-1 (left-branch tree))
(cons (entry tree)
(tree->list-1
(right-branch tree))))))



(define (tree->list-2 tree)
(define (copy-to-list tree result-list)
(if (null? tree)
result-list
(copy-to-list (left-branch tree)
(cons (entry tree)
(copy-to-list
(right-branch tree)
result-list)))))
(copy-to-list tree '()))

(tree->list-1 '(7 (3 ( 1 nil nil) (5 nil nil)) (9 nil (11 nil nil)) ) )
(tree->list-2 '(7 (3 ( 1 nil nil) (5 nil nil)) (9 nil (11 nil nil)) ) )

(tree->list-1 '(3 (1 nil nil) (7 (5 nil nil) (9 nil (11 nil nil))) ) )
(tree->list-2 '(3 (1 nil nil) (7 (5 nil nil) (9 nil (11 nil nil))) ) )

; they both provide the inorder traversal representation of the tree
; they work differently (reverse approach) but the assembling of the parts leads to the same results in both.

--------------------------------------------------------------------
ex 2.64

This procedure seems to be overwhelming as it has alot of branching going.
but if you think about it, it does a really simple thing it divides the list into half ,if it happens that you can't divide it evenly with an element in the middle
you just say that the left branch is the small one like (1 2 3) this could be divided evenly (2-> middle , 1->left , 3 ->right),while something like (1 2 3 4) would go like that
(2 ->middle , 1 -> left ,(3 4) ->right ) 

---------------------------------------------------------------------
ex 6.66

(define (look-up given-key given-Tree)
  (cond ((null? given-Tree ) false)
        ((= given-key (key (car given-Tree))) (car given-Tree))
        ((< given-key (key (car given-Tree))) (look-up given-key (left-branch given-Tree)))
        ((> given-key (key (car given-Tree))) (look-up given-key (right-branch given-Tree)))
   ))
   
-----------------------------------------------------------------------
ex 6.68

(define (exist? theSet symbol)
  (cond((null? theSet) false)
       ((equal? (car theSet) symbol) true)
       (else (exist? (cdr theSet) symbol)))
       )

(define (whichBranch tree symbol)
  (cond ( (exist? (symbols (left-branch tree) ) symbol) (list 0 (left-branch tree)) )
         ((exist? (symbols (right-branch tree)) symbol) (list 1 (right-branch tree)) )
        (else (error  "the symbol isn't represented in the list so far " ) )
  ))


(define (encode-symbols symbols-list tree)
  
  (define (insider currentList currentNode)
    (cond ((null? currentList ) nil)
          ((leaf? currentNode) (insider (cdr currentList) tree) )
          (else    
    			(let ((Tree-bit 
                               (whichBranch currentNode (car currentList) ) ))
       
      (cons (car Tree-bit) (insider currentList (cadr Tree-bit) )
            ))
    
    
  )))
  (insider symbols-list tree)
  
)

-----------------------------------------------------
ex 2.69

(define (successive-merge orderedLeaves)
  (if (null?(cdr orderedLeaves)) (car orderedLeaves)
      (successive-merge 
       (adjoin-set 
        (make-code-tree (car orderedLeaves) (cadr orderedLeaves)) 
        (cddr orderedLeaves)))))
  
---------------------------------------------------------------
ex 2.70


(define lyrics (generate-huffman-tree 
                '((a 2) (get 2) (sha 3) (wah 1) (boom 1) (job 2) (na 16) (yip 9))))

(encode-symbols '(get a job
					sha na na na na na na na na
					get a job
					sha na na na na na na na na
					wah yip yip yip yip yip yip yip yip yip
					sha boom)
                lyrics)
                
; the number of words in the song is 36 and we have a dictonary of 8 words which nees log 8 = 3 -----> 3*36 =108 bits ,so the huffman safes about 20% of space

------------------------------------------------------------------
ex 2.71

;for any number 2^x > sum(2^(x-1)+ ......+2^(0)) ---> so the process of building the tree will cause in what's called a degenerate tree which just a linked list so,
;most frequent letter will be 1 bit
;least frequent letter will be (n-1) bits


------------------------------------------------------------------
ex 2.72

;most frequent symbol -> o(n) if you search the unlucky branch first ,o(1) if you get lucky and search in the good branch.
;least frequent symbol -> you can assume that every step of depth makes the list (n) become (n-1) so the result will be (n*(n-1))/2 which is o(n^2).

---------------------------------------------------------------------
ex 2.73
a)

;what happened : we made a whole a table with indeces (operation (which is just deriv operation) , operators), so based on the operation there's gonna be a certain procedure returened to handle this
; case, though this doesn't much of sense as you have one way of handling the derivation of + or * or whatever.

; why can't we use the predicate number and variable in the table (date directed way)? : the data directed dispatch supposes that the data sent has a tag (type) that corresponds to some procedure in 
;the genreric interface we made but with numbers there's no tag unless you make it like that "(number 1)" which isn't what expected from the user, and for the variable predicate it also needs a tag;
; while something like + - * is a tag for specifc type of deriv procedure,we just dispatch it and operate on the data.
;;;;;;;;;;
b)

(define addened car)
(define augend cadr)
(define multipler car )
(define multiplicand cadr)

;The followings are just different implemntations in case you want to handle multiple argument in the experssion. 

;(define (multiplicad exp) 
;  (if (null?(cddr exp)) (cadr exp)  
;                           (list '* (cdr exp)) ))
;(define (augend exp) 
;  (if (null? (cddr exp)) (cadr exp)
;      (list '+ (cdr exp))))


(define (install-deriv-sum) 
  (define (deriv-sum exp var) ( 
    (make-sum (deriv (addend exp) var)
		(deriv (augend exp) var))))
  (put 'deriv '+ deriv-sum))


(define (install-deriv-product)
  (define (deriv-product exp var) 
    ((make-sum (make-product
				(multiplier exp)
				(deriv (multiplicand exp) var))
					(make-product
					(deriv (multiplier exp) var)
					(multiplicand exp)))))
  
  
  (put 'deriv '* deriv-product ) )
;;;;;;;;;;;;;;;;;;;;;;;
  
  
c)  

(define base car)
(define exponent cadr)

(define (make-exponent base expon)
  (cond ((= expon 0) 1)
        ((= expon 1) base)
  		(else (list '** base expon))))


(define (install-deriv-exponent)
(define base car)
(define exponent cadr)

(define (make-exponent base expon)
  (cond ((= expon 0) 1)
        ((= expon 1) base)
  		(else (list '** base expon))))
  (define (deriv-exponent exp var)
     	(make-product 
      				(make-product 
      						(exponent exp)
      						(make-exponent base (- (exponent exp) 1) ))
                   (deriv (base exp) var)))
  (put 'deriv '** deriv-exponent))
  
  
;;;;;;;;;;
d)
; I don't quite understand what the question implies, but if we switched the indeces of the table we would simply swap the put function arguments.
----------------------------------------------------------------------------

ex 2.74

;The division must supply an install procedure to place its own functions in the divison system table
;The file provided should have its first element the name of the division or a code of the divison (ID)

;you can apply your specific apply prodcedure as the apply generic provided here won't work.
;or you can make the installation based on type of two arguments which honestly doesn't make any sense.

(define div-name car)
(define div-data cadr)

(define (get-record div-file employee-name)
  (let ((proc (get 'get-record (div-name div-file) )))
    (if proc (list (div-name div-file)(proc (div-data div-file) employee-name  )) false )))
;employee-record is represented as a tagged data,so i can use "apply-generic". 
(define (get-salary employee-record)
  (apply-generic 'get-salary employee-record))

(define (find-employee allFiles employee-name)
  (if (null? allFiles) false 
      (or (get-record (car allFiles) employee-name) (find-employee (cdr allFiles) employee-name))))

;when we add new personnel divison into the system all what it wants to just do a put of the procedures it uses into the table of (operations and division-Name)
-----------------------------------------------------------------------------------
ex 2.75


(define (make-from-mag-ang x y )
  (define (dipatch op)
    (cond ((eq? op 'real-part) (* x (cos y) ))
          ((eq? op 'imag-part) (* x (sin y) ))
          ((eq? op 'mag) x)
          ((eq? op ''ang) y)))
  dispatch)

----------------------------------------------------------------------------------
ex 2.76

;For a system that has lots of new operations added (selectors)
; Data-Directed Programming: would require all the different implementations to re install their new packages or make a custom install prcodeure for their new operations
; Explicit dispatch : will result in writing a whole clause procdure to dispatch the data to the appropritate implementation.
; Message Passing : will require nothing (just every one implements his own operation and add to the constructor ) , but this will lead to inconsistency in the system as the old data object wont have
;the new operations and passing the new message would lead to an error.

;For a system that has lots of new types get added to the system 
; Data-Directed programming : would require a single installation for the new package (NO names collision)
; Explicit dispatch : will force you to go through the different dispatch procedure and add a new single clause. (YES names collision)
; message passing : would require just a new implementation (NO names collision)


--------------------------------------------------------------------------------
ex 2.77
;data = '(complex rectangular 3 4)

;before adding the new entries into the table the following happens 
;  (apply-generic 'magnitude data ) --> invokes -->  (get 'magnitude 'complex ) --> which isn't in the table so ERROR invoked.

;After adding the new entries into the table the following happens
;  (apply-generic 'magnitude data ) --> invokes -->  (get 'magnitude 'complex ) --> return magnitude (the gerneric one)
; then... apply-generic completes its job.....
; (apply magnitude (rectagular 3 4)) --> which invokes -> (apply-generic 'magnitude (rectangular 3 4)) --> invokes --> (get 'magnitude 'rectangular) --> returns magnitude-rectangular
; then apply-generic (internal-one) completes its job.
; (apply magnitude-rectangular (3 4)) -> returns 3

; so apply-generic got invoked 2 times.

-----------------------------------------------------------------------------------
ex 2.78

(define (attach-tag type-tag contents)
(cons type-tag contents))
(define (type-tag datum)
(cond( ((pair? datum) (car datum))
       ((number? datum) 'scheme-number)
   	   (else(error "Bad tagged datum: TYPE-TAG" datum)))))

(define (contents datum)
(cond( ((pair? datum) (cdr datum))
       ((number? datum) datum)
   	   (else(error "Bad tagged datum: CONTENTS" datum)))))


(define (install-scheme-number-package)
(put 'add '(scheme-number scheme-number) + )
(put 'sub '(scheme-number scheme-number) - )
(put 'mul '(scheme-number scheme-number) * )
(put 'div '(scheme-number scheme-number) / ))

; you can use the same "install-scheme-number-package" provided in the book with lambda functions
;just change the attach-tag to return a the same the content without combining the type scheme-number to the data.
---------------------------------------------------------------------------------------------
ex 2.79

(define (install-equ-for-all-implementations)
  (define (equ?-ordinary x y) (= x y))
  (define (equ?-rational x y) (eq? x y))
  (define (equ?-complex x y) (and (= (real-part x) (real-part y)) 
                                  (= (imag-part x) (imag-part y))))
  (put 'equ? '(scheme-number scheme-number) equ?-ordinary)
  (put 'equ? '(rational rational) equ?-rational)
  (put 'equ? '(complex complex) equ?-complex))

(define (equ? x y) (apply-genreic 'equ? x y ) )

-----------------------------------------------------------------------------------------------
ex 2.80
;utilize the predone preocedures to solve the problem faster

(define (=zero? x) (equ? (sub x x ) (add x x)))

;when you proceed in the complexity of the system and add polynomial manipulation , this procedure would cause a problem as we didn't supply a sub procedure
-------------------------------------------------------------------------------------------------
ex 2.81
;a) I don't know why did he assume that apply-generic would do such a thing
; but his own implementation would cause an infinite loop of coercion.

;b) It works correctly as I type the type-corecion procedures with my own bare hands,it's my mistake if such a thing happens

;c) we need no modifications


-------------------------------------------------------------------------------------------------
ex 2.82

;I assume that get-coercion sends a false signal if there's no match.
;Pray with me there won't be no syntax errors here.
(define (apply-genreic op . args)
  (define (all-tags-same? tags)
    (if (null? (cdr tags)) true
        (if (eq? (car tags) (cadr tags)) (all-tags-same? (cdr tags))
             								false ))
  (define (check-all-valid? coercion-list)
    (if (null? coercion-list) true 
        (and (car coercion-list) 
             (check-all-valid? (cdr coercion-list)))))
  
  (define (get-coerced to-tag from)
    ( if (eq? to-tag (get-tag from)) from 
    		(let ((proc-coercion (get-coercion (get-tag from) to-tag)))
      				(if proc-coercion (proc-coercion from)
          								false ))))
  
  (define (coerced-then-go tag) 
    (let ((coerced-list (map (lambda (x) (get-coerced tag x)) args)))
      (if (check-all-valid coerced-list) (apply apply-generic (append '(op) coerced-list ))
          								false )))
  
  (let ((tags-only (map get-tag args))
        (content-only (map get-content args)))
    (let ( (proc (get op tags-only)) ) 
      (if proc 
          (apply proc content-only)
		  (if (not(all-tags-same)) (accumulate and true (map coreced-then-go tags-only)))
          							false)))))
;if we assume we have a clock system and it has a procedure (make-time hours minutes) -> it prints this way (HH::MM)
;internally in just deals with hours and minutes data types to construct the time
; if you pass to it (make-time minutes hours ) it will fail to find the procedure so a coercion will get invoked
; if you coerce to single type only it will try (make-time hours hours) and (make-time minutes minutes) which are both return to valid procedure from the table.
  
  )

--------------------------------------------------------------------------------------------------
ex 2.83

(define (install-raise)
  (define (raise-interger-to-rational i) (make-rational i 1))
  (define (raise-rational-to-real x) (make-real x)) ; some magic procedure i don't care about
  (define (raise-real-to-complex x) (make-from-real-imag x 0))
  (put 'raise '(integer) raise-interger-to-rational)
  (put 'raise '(rational) raise-rational-to-real)
  (put 'raise '(real) raise-real-to-complex))

(define (raise data) 
  ((get 'raise (get-tag data)) (content data)))
; we could use apply-generic.


---------------------------------------------------------------------------------------------------
ex 2.84
;I assume that real data type takes to numbers and just divide them and get a decimal number
(define (install-raise)
  (define (raise-interger-to-rational i) (make-rational i 1))
  (define (raise-rational-to-real x) (make-real (numer x) (denom x) )) ; some magic procedure i don't care about
  (define (raise-real-to-complex x) (make-from-real-imag x 0))
  (put 'raise '(integer) raise-interger-to-rational)
  (put 'raise '(rational) raise-rational-to-real)
  (put 'raise '(real) raise-real-to-complex))

(define (raise data) 
  ((get 'raise (get-tag data)) (content data)))
; we could use apply-generic.

(define (apply-generic op . args)
  (define tower '(integer rational real))
  (define tower-up cdr )
  (define tower-base car )
  
  (define (tag-match? current-tag tags-only)
    (accumulate and true
     	(map (lambda (x) (eq? current-tag x) ) tags-only)))
  
  (define (lowest-in-tower-matcher tags-only) 
    (define (insider current-tower)
      (cond ((null? current-tower) false)
          	((tag-match? (tower-base current-tower) tags-only) (tower-base current-tower) )
            (else (insider (tower-up current-tower)))))
   (insider tower))
  
  (define (raise-this-only tag-to-raise)
    (map (lambda (x) (if (eq? tag-to-raise (get-tag x)) x
                         (raise x))) args))
  
  (let ((tags-only (map get-tag args))
        (content-only) (map content args))
    (let ((proc) (get op tags-only)) 
      (if proc (apply proc content-only)
          		(let ((lowest-in-tower (lowest-in-tower-matcher tags-only))) 
                  (if lowest-in-tower (apply-gerneric op (apply (raise-this-only lowest-in-tower) ) )
                      					(error "we can't do this procedure, there's no match")))))))
---------------------------------------------------------------------------------------------------------
ex 2.85


;I assume that real data type takes to numbers and just divide them and get a decimal number

(define (install-raise)
  (define (raise-interger-to-rational i) (make-rational i 1))
  (define (raise-rational-to-real x) (make-real (numer x) (denom x) ) ) ; some magic procedure i don't care about
  (define (raise-real-to-complex x) (make-from-real-imag x 0))
  (put 'raise '(integer) raise-interger-to-rational)
  (put 'raise '(rational) raise-rational-to-real)
  (put 'raise '(real) raise-real-to-complex))

(define (raise data) 
  ((get 'raise (get-tag data)) (content data)))
; we could use apply-generic.

;Sorry, I interchanged the implemntation of project with the drop


(define (install-drop)
  (define (complex->real x) (make-real (real-part x) 1) )
  (define (real->rational x) (make-rational (quotient x 1) 1))
  (define (rational->integer x) (make-integer (/ (numer x) (denom x)) ))
  (put 'drop 'complex complex->real) ; type don't need to be a list (i won't use apply-generic)
  (put 'drop 'real real->rational)
  (put 'drop 'rational rational->integer))

(define (drop x) (get 'drop x))

(define (project x )
  (let ((proc(drop (get-tag x)))) 
    (if (not proc) x
        (let ((dropped (proc (content x)) )) 
          (if (equal? (raise dropped) x ) (project dropped)
              		x)))))


(define (apply-generic op . args)
  (define tower '(integer rational real))
  (define tower-up cdr )
  (define tower-base car )
  
  (define (tag-match? current-tag tags-only)
    (accumulate and true
     	(map (lambda (x) (eq? current-tag x) ) tags-only)))
  
  (define (lowest-in-tower-matcher tags-only) 
    (define (insider current-tower)
      (cond ((null? current-tower) false)
          	((tag-match? (tower-base current-tower) tags-only) (tower-base current-tower) )
            (else (insider (tower-up current-tower)))))
   (insider tower))
  
  (define (raise-this-only tag-to-raise)
    (map (lambda (x) (if (eq? tag-to-raise (get-tag x)) x
                         (raise x))) args))
  
  (let ((tags-only (map get-tag args))
        (content-only) (map content args))
    (let ((proc) (get op tags-only)) 
      (if proc project((apply proc content-only))
          		(let ((lowest-in-tower (lowest-in-tower-matcher tags-only))) 
                  (if lowest-in-tower (apply-gerneric op (apply (raise-this-only lowest-in-tower) ) )
                      					(error "we can't do this procedure there's no match")) )))))


-----------------------------------------------------------------------
ex 2.86



;I made a great use out of the idea that our apply-generic now can raise your data type to the top of the tower
;and then it can down it if possible so i traverse thorugh the whole tower which saves my alot of implementation to write 
;when i wanted to write a trignometic procedures for the types in my system i just went up to the top and did the math on that type and drop back the result.



(define (install-trig-for-real)
  (define (cosine-real x) (make-real(cos x))) ; definition of real numbers isn't known so assume the previous implem.
  (define (sine-real x)  (make-real( sin x)))
  (define (atan2-real x y) (atan (/ y x) ))
  (define (squre-real x) (make-real (square x) 1))
  (define (square-root x) (make-rational (sqrt x) ))
  
  (put 'cosine '(real) cosine-real)
  (put 'sine '(real) sine-real)
  (put 'atan2 '(real real) atan2-real)
  (put 'square-mod '(real) square-real)
  (put 'sqrt-mod '(real) square-root))


(define (cosine x) (apply-genereic 'cosine x))
(define (sine x) (apply-genereic 'sine x))
(define (atan2 x y) (apply-genereic 'atan x y))

(define (install-complex-rectagular)
  (define (make-from-real-imag x y ) (cons x y) )
  (define (real-part z) car )
  (define (imag-part z) cdr)
  (define (angle z) (atan2 (imag-part z) (real-part z) ))
  (define (magnitude z) (sqrt-mod (add (square-mod (real-part z)) (square-mod (imag-part)) )) )
  ;start putting everything into the table.
  )

(define (add-complex z1 z2)
(make-from-real-imag (add (real-part z1) (real-part z2))
(add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
(make-from-real-imag (sub (real-part z1) (real-part z2))
(sub (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
(make-from-mag-ang (mul (magnitude z1) (magnitude z2))
(add (angle z1) (angle z2))))
(define (div-complex z1 z2)
(make-from-mag-ang (div (magnitude z1) (magnitude z2))
(sub (angle z1) (angle z2))))



;conclusion of the previous exercises:

;we started the system by building for data types (integer,rational,real,complex),and we had selectors and constructors for each one which obeyed the data directed programming rule that forces us
;to tag our data whenever we construct them.
;then we wanted a way to coerce any type of them into the other one (was simple as it was (single sub,single sup) hierarchy) and we mangaed to embed this coercion into the apply-generic
;which gave us great capabilites such as (simplifying ,doing operation on any kind of don't matched data types)
;then we added the layer of abstraction that provide arithmtic operation on our system of numbers (add,sub,mul,div) which litterly works with any kind of data type
;then it asked us to do a little stupid thing but it showed us how great our system is built,it asked us to make a complex number out of one of its competetors (real,rational) 
;then we notice that our system is very generic,we can add to numbers easily or subtract or mul or ...; so all what we need to do is just make the implementation of the complex number generic 
;and even we wanted to code triagnomeric procedures as generic procedures we used to idea of coercion to simply coerce any type to supertype and implement small amount of the code
;which in fact opens our eyes to something even more interesting (we can simply implement the complex part for (add,sub,mul,div,cos,sin) and whenever we call something like add integer integer
;we just go up in the hirerarchy and do the job and drop the hirerarchy again.
;BASICALLY THE HIERARCHY HERE MADE US APLE OF THROWING AWAY EVERYTHING WE KNEW AND STICK WITH THE FACT THAT WE CAN JUST GO TO THE MOST GENERIC DATA TYPE AND OPERATE ON  IT AND DROP BACK.
----------------------------------------------------------------------------------------------------------
ex 2.87

(define (new-zero-installation)
  (define (=zero?-polynomial x) 
    (empty-termlist? (termlist x)))
  (put '=zero? 'polynomial =zero?-polynomial))
;The system witht the provided abstractions doesn't support a polynomial with empty terms,it would be just a problem if someone tried
;to make (explictly) a term list out of empty term-lists using the adjoin-term procedure the result would be a '() so we got to handle this
;situtation when we have such a weird user.

-------------------------------------------------------------------------------------------------------------------
ex 2.88


(define (new-installation-to-polynomials)
    (define (make-negated cur-term-list) 
    	(if (null? cur-term-list ) the-empty-term-list 
            (let ((first (first-term cur-term-list)) 
                  (rest (rest-terms cur-term-list)))
                  (adjoin-term 
                   			(make-term (order first) (mul (coeff first) -1 )) 
                   			(make-negated rest)))))
  
  (define (sub-polynomials p1 p2 ) 
    (cond ((same-variable? (variable p1) (variable p2)) 
           (make-poly (variable p1)
                     (add-term (term-list p1)
                     (make-negated (term-list p2)))))
          (else (error "polys to the same variable SUB-POLYNOMIALS"))))
  (put 'sub '(polynomial polynomial) sub-polynomials ))
--------------------------------------------------------------------------------------------------------------------------
ex 2.89 2.90

;the new modifications are analogous and werid


-------------------------------------------------------------------------------------------------------------------------
ex 2.91 to the end of chapter -> not solved yet

--------------------------------------------------------------------------------------------------------------------------
ex 3.1
(define (make-accumulator inital)
  (define (insider amount) (begin (set! inital (+ inital amount)) inital)) insider)
(define m1 (make-accumulator 5))
;(m1 10)
;(m1 4)
--------------------------------------------------------------------------------------------------------------------------
ex 3.2
(define (make-monitered func)
  (define theFunction func)
  (define localStateCnt 0)
  (define (reset) (set! localStateCnt 0))
  (define (increment) (set! localStateCnt (+ localStateCnt 1)))
  (define (dispatch message)
    (cond ((eq? message 'how-many-calls?) localStateCnt)
          ((eq? message 'reset-count) (reset))
          (else  (increment)(theFunction message) )
          ))dispatch)
(define a (make-monitered square))
;(a 4)
;(a 3)
;(a 4)
;(a 'how-many-calls?)
;(a 'reset-count)
;(a 'how-many-calls?)

--------------------------------------------------------------------------------------------------------------------------
ex 3.3 + ex 3.4
(define (make-account user-password balance)
	(define wrong-entries 0)
	(define (reset) (set! wrong-entries 0))
  	(define (increment) (set! wrong-entries (+ wrong-entries 1)))
(define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
		balance)
		"Insufficient funds"))
(define (deposit amount)
	(set! balance (+ balance amount))
	balance)
(define (dispatch entered-password m)
  (cond ((eq? entered-password user-password) 
		(reset)(cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request: MAKE-ACCOUNT"m))))
      ((>= wrong-entries 2) (error "call-the-cops!"))
        (else (increment) (error "Wrong Password!!") )))
	dispatch)
(define w (make-account 'pass 100))
;((w 'passs 'deposit) 10)
;((w 'passs 'deposit) 10)
;((w 'pass 'deposit) 10)
;((w 'passs 'deposit) 10)
;((w 'passs 'deposit) 10)
;((w 'passs 'deposit) 10)




--------------------------------------------------------------------------------------------------------------------------
ex 3.5

(define (square x) (* x x))
(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond ((= trials-remaining 0)
				(/ trials-passed trials))
			  ((experiment)
				(iter (- trials-remaining 1)
				(+ trials-passed 1)))
			  (else (iter (- trials-remaining 1)
							trials-passed))))
	(iter trials 0))

(define (random-in-range low high)
	(let ((range (- high low)))
	(+ low (random range))))

(define (integral1-predicate x y) 
  (<= (+ (square (- x 5)) (square (- y 5))) 1))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (exper) (p 
                 (random-in-range x1 x2) (random-in-range y1 y2)))
 	(define area ( * (- x2 x1) (- y2 y1) ) )
  (* area (monte-carlo trials exper )))
(estimate-integral integral1-predicate 4 6 4 6 1000)

----------------------------------------------------------------------------------------------------------------------------
ex 3.6

(define (random) 
  (define x inital-x)
  (define (reset newX) (set! x newX ))
  (define (generate) (begin (set! x (rand-update x)) x))
  (define (dispatch m)
    (cond ((eq? m 'generate ) (generate))
          ((eq? m 'reset) reset)
          (else (error "wrong Choice!!"))))
  dispatch)

(define rand (random))
((rand 'reset) 20)
(rand 'generate)
(rand 'generate)
((rand 'reset) 20)
(rand 'generate)

-------------------------------------------------------------------------------------------
ex 3.7

(define (make-account user-password balance)
	(define wrong-entries 0)
	(define (reset) (set! wrong-entries 0))
  	(define (increment) (set! wrong-entries (+ wrong-entries 1)))
(define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
		balance)
		"Insufficient funds"))
(define (deposit amount)
	(set! balance (+ balance amount))
	balance)
(define (dispatch entered-password m)
  (cond ((eq? entered-password user-password) 
		(reset)
         (cond
          	((eq? m 'check-password) true)
        	((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request: MAKE-ACCOUNT"))))
      ((>= wrong-entries 2) (error "call-the-cops!"))
        (else (increment) (error "Wrong Password!!") )))
	dispatch)

(define (make-joint account account-pass currentPassword)
  (cond ((account account-pass 'check-password)
         (lambda (password m)
           (cond ((eq? password currentPassword) (account account-pass m))
                 (else (error "wrong passowrd")))))
        (else (error "wrong password to merge with"))))

(define peter-acc (make-account 'pass 100))
(define paul-acc (make-joint peter-acc 'pass 'ww))
((paul-acc 'ww 'deposit) 10)
((peter-acc 'pass 'deposit) 10)

--------------------------------------------------------------------------------------------------
ex 3.8
(define f
  (let ((flag 0))
    (lambda (x) (if (= x 0) flag 
                    (let ((oldflag flag))
                          (begin (set! flag 1) oldflag)) ))))
;(+ (f 0) (f 1)) -> 0
;(+ (f 1) (f 0)) -> 1

