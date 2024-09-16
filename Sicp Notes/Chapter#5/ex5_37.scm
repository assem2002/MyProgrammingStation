; The current preserving implementation absolutly benefit from the use of `need` and `modify`
; that exist in the structure of the instructions sequence
; while if preserving doesn't get a use out of it
; it would be useless to actually use these two piece of information in the whole implementation 
; of the code.

; I think you can see that difference in the example of factorial
; the one with ( * (factorial (- n  1)) n )
; if we process the argl you would see that we should preserve `env` when we process `n` but that doesn't happen
; because n doesn't modify `env`, so it doesn't get preserved