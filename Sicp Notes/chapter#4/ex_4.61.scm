; a) (?x next-to ?y in (1 (2 3) 4))
; ans -> ((2 3) next-to 4 in ((2 3) 4)) (second rule )
; ans -> ((1) next-to (2 3)  in ((2 3) 4)) (first rule )


; b ) (?x next-to 1 in (2 1 3 1))
; ans -> (3 next-to 1 in (3 1))  (second rule)
; ans -> (2 next-to 1 in (3 1))  (first rule)

;The second rule takes a part of the whole list and make some tests on the rest of it.
; I have some questions --> which procedure work first and does the second one call itself recursively or does it call the first one?

