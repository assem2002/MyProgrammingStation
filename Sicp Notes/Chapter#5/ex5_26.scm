; I don't have the program working
; but for the answers on the internet 
; it seems that the maixium depth is always 10
; and this is reasonable, as we have a function that's calling itself 
; and the number of n doesn't affect the depth at all as the property of tail recursion 
; is used in such a function and it won't make the stack related at all to the number of n
; and I think that what the author wants to notice that stack with tail recursion won't get affected by n.

; The formula would be linear for the following reasons :
; - We have some stack pushes that is related to n, because the function call we make depends on n
; - some constant that's related to the initalization of the function and calling it
; so a formula like this 35n + 29 ==> though, I don't know if correct, but seems reasonable
