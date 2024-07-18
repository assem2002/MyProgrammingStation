(rule (reverse (?x1 . ?x2) ?z)
    (and (reverse (?x2) ?v)
    (append-to-form v x1 ?z)))
; This is the most weird thing i've ever written. :D