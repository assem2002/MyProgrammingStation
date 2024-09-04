; I think It would preserve the `env` of every time `*` is invoked as now the factorial function
; call is the first thing to get handled while building the `argl` and the sequence resulting from 
; calling the function modifies `env` so it get preserved.
; While before that we had the `n` to be first added to the argl which also followed the rule
; of preserving and `env` but `n` didn't have it in its modified list as it's just a compilation of variable

; maybe it seems not intuitive to not preserve the `env` while going through a recrusive call
; but with the good optimization built in this compiler, it works!!!
; as now `n` (the important variable here) is preserved in the argl and not any more needed by the the 
; expression that's operating at the current state.
; and for our fortune, no any place in the code requries preserving the env
; so, though the code seems recursive, but it's still good.


; The other notice is argl :
; argl used to get preserved in the last factoral function becuase any function call modifies `argl`
; while now n doesn't modify `argl`