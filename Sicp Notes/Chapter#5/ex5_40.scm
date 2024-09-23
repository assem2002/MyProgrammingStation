; I don't get it how extending the environment would match the depth everytime we go one more depth.
; The solution I saw for now are just extending the environment without special work for the depth.


; UPDATE :: Now I see. We would build the data structure to hold just plain variable names.
; then we would build function to get the lexical address of the variable from the current env (depth in env frames, depth in the frame itself)
; I still don't see what is the benefit from doing so, if i'm gonna go everytime an cacluate the depth whenever
; I want to get to some variable, it would be useless unless these things are hashed to be accessed in O(1) 