;I support Ben point of view, as it's the traditional way of interperting such a thing in most of the other languages.
;but the simulatneous way seems to be more accurate in a prefect world where each procedure is considered as a seperated entity.
;as the book mentions the best way to handle such a thing if you want to build a simultaneous definitions is to raise an error.



; (lambda ⟨vars⟩
;   (let ((u '*unassigned*)
;        (v '*unassigned*))
;       (set! u ⟨e1⟩)
;       (set! v ⟨e2⟩)
;       ⟨e3⟩))

;We could do something related the the graph theory (topological sorting)

;We need to sort the variable is terms of their dependencies and then start to evaluate the expressions related to them.
; It seems to be easy an idea, but with scheme and the existing way of interperting the code, it would be a little hard.