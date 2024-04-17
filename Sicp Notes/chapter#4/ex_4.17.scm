; The normal way would lead to one enviornment extending from the gloabl env.
; The scanned out would lead to an env. extending from the global and that one would have an extedning one (let env.)
; It should't create any problems as the variables are accessable to the child envorinments.

;(lambda ⟨vars⟩
;    (define u ⟨e1⟩)
;    (define v ⟨e2⟩)
;    ⟨e3⟩)

; The previous code code be done using the normal define

; (lambda <vars>
;     (define u *unassigned*)
;     (define v *unassigned*)
;     (set! u <e1>)
;     (set! v <e1>)
;     <e3>)
