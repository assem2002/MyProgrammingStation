; If the the require doesn't check all requires before it once there happens an error that leads to change, this means their code happen a problem if the order isn't chosen right.
;For example --> 
(require (> miller cooper))
(require (not (= (abs (- fletcher cooper)) 1)))

;sometimes to satisfy the second condition cooper code get changed to the limit that it gets larger or equal to miller,
; which will break the first require condition.
;If it does , so it won't matter at all.





;to place the require of --> (require (not ( = cooper 1)))
;This would make the program prune all the posibilites that has cooper as 1 so the first answer would wait about 5^3 combinations to start to get past the frist reuire 

; found this solution in -- http://community.schemewiki.org/?sicp-ex-4.39
;The solution kills the initilization of a the subtree if that path is bad from the begining.
(define (multiple-dwelling) 
(let ([fletcher (amb 1 2 3 4 5)]) 
  (require (not (= fletcher 1))) 
  (require (not (= fletcher 5))) 
  (let ([cooper (amb 1 2 3 4 5)]) 
    (require (not (= cooper 1))) 
    (require (not (= (abs (- fletcher cooper)) 1))) 
    (let ([smith (amb 1 2 3 4 5)]) 
      (require (not (= (abs (- smith fletcher)) 1))) 
      (let ([miller (amb 1 2 3 4 5)]) 
        (require (> miller cooper)) 
        (let ([baker (amb 1 2 3 4 5)]) 
          (require (not (= baker 5))) 
          (require 
           (distinct? (list baker cooper fletcher miller smith))) 
          (list (list 'baker baker) 
                (list 'cooper cooper) 
                (list 'fletcher fletcher) 
                (list 'miller miller) 
                (list 'smith smith)))))))) 



