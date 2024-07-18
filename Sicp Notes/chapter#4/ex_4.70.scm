; I think it's due to delay that happens from cons-stram function.
; If we use the same variable it would delay the value of the variable.
; so when that stream is used later it would call wrong value.
; I think it won't lead to useless infinte loop, it would just print out `assertion` forever similar to ones stream.    