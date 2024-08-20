;A)
(assign continue (label done))
(save continue)
entry
cond1
(test (op null?) (reg tree))
(branch (label cond1-true))
cond2
(assign test (op pair?) (reg tree))
(test (op not) (reg test)) 
(branch (label cond2-true))
else
(save tree)
(assign tree (op car) (reg tree))
(assign continue (label after-left))
(goto (label entry))
cond1-true
(assign val (const 0))
(restore continue)
(goto continue)

cond2-true
(assign val (const 1))
(restore continue)
(goto continue)

after-left
(restore tree)
(save val)
(save tree)
(assign tree (op cdr) (reg tree))
(assign continue (label after-right))
(save continue)
(goto after-right)

after-right
(restore tree)
(restore temp-val)
(assign val (op +) (reg val) (reg temp-val))
(save val)
(save tree)
(restore continue)
(goto continue)
done


;B) I return result of the function in register val
(assign n (const 0))
(assign continue (label done))
(save continue)
entry
(test (op null?) (reg tree))
(branch (label cond1-true))

cond1-true
(assign val (reg n))
(restore continue)
(goto continue)
cond1-false
(assign temp-testing (op pair?) (reg tree))
(test (op not) (reg temp-testing))
(branch (label cond2-true))
(goto (label cond2-false))
cond2-true
; (assign n (op +) (reg n) (const 1))
(assign val (op +) (reg n) (const 1))
(restore continue)
(goto continue)

cond2-false
else
(save tree)
(assign continue (label after-inner-call))
(save continue)
(assign tree (op car) (reg tree))
(goto entry)

after-inner-call
(assign n (reg val))
(restore tree)
(assign tree (op cdr) (reg tree))
(assign continue (label finishing-up))
(save continue)
(goto entry)
finishing-up
(assign val (reg))
(restore conitnue)
(go continue)
done

; I think you just have to follow a pattern overall.
; The pattern of :
; - add a continue label to be used later by any statment that's responsible for returning (means this function call is done).
; - mainly, we would save a continue (remaind the program to do something) when you have a recursive call.
; - otherwise you just consume it to see if the program want to remaind you of something.
; - and you would set the returing value to somthing when you start consuming the continue (means you're returing some value).
; - and you would be saving some state on some parameter when you also save a continue state,because you'd be using this state again (maybe) when you come to this place that initiated the call and started to change the state of some parameters.

