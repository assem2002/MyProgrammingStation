begin-garbage-collection
(assign free (const 0))
(assign scan (const 0))
(assign old (reg root))
(assign relocate-continue (label reassign-root))
(goto (label relocate-old-result-in-new))
reassign-root
(assign root (reg new))
(goto (label gc-loop))
gc-loop
(test (op =) (reg scan) (reg free))
(branch (label gc-flip))
(assign old (op vector-ref) (reg new-cars) (reg scan))
(assign relocate-continue (label update-car))
(goto (label relocate-old-result-in-new))
update-car
(perform (op vector-set!)
(reg new-cars)
(reg scan)
(reg new))
(assign old (op vector-ref) (reg new-cdrs) (reg scan))
(assign relocate-continue (label update-cdr))
(goto (label relocate-old-result-in-new))
update-cdr
(perform (op vector-set!)
(reg new-cdrs)
(reg scan)
(reg new))
(assign scan (op +) (reg scan) (const 1))
(goto (label gc-loop))

relocate-old-result-in-new
(test (op pointer-to-pair?) (reg old))
(branch (label pair))
(assign new (reg old))
(goto (reg relocate-continue))
pair
(assign oldcr (op vector-ref) (reg the-cars) (reg old))
(test (op broken-heart?) (reg oldcr))
(branch (label already-moved))
(assign new (reg free)) ; new location for pair
;; Update free pointer.
(assign free (op +) (reg free) (const 1))
;; Copy the car and cdr to new memory.
(perform (op vector-set!)
(reg new-cars) (reg new) (reg oldcr))
(assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
(perform (op vector-set!)
(reg new-cdrs) (reg new) (reg oldcr))
;; Construct the broken heart.
(perform (op vector-set!)
(reg the-cars) (reg old) (const broken-heart))
(perform
(op vector-set!) (reg the-cdrs) (reg old) (reg new))
(goto (reg relocate-continue))
already-moved
(assign new (op vector-ref) (reg the-cdrs) (reg old))
(goto (reg relocate-continue))

gc-flip
(assign temp (reg the-cdrs))
(assign the-cdrs (reg new-cdrs))
(assign new-cdrs (reg temp))
(assign temp (reg the-cars))
(assign the-cars (reg new-cars))
(assign new-cars (reg temp))

; - relocate-old-result-in-new --> It just sets some place to be used by basically adding the pointer in `free` register into what is called `new` register and then incrementing the `free` register + switch on cases (heartbroken, pointer to pair, pointer but not to pair).
; - gc-flip --> It swaps free memory with working memory.
; - begin-garbage-collection --> It's the start of the program, it creates a space for the root which the head of registers chain at the current moment we are doing garbage collection and adds the data in the space allocated and add a reminder to assign that new value to the root regiter. 
; - gc-loop --> It's the pain of the code, the one that always looks up for issues. It checks the equality of scan and free, if not -- it would start handling the data in car (wheter a normal number OR a pointer to moved place then it would just update the car OR It would allocate a space for the a pointer and copy the data in it and also update the car).
; - Update-car --> is depending on register `new`. It blindly sets the car of pointer in `scan` to the content in `new`. Then, It handles the cdr of the `scan` and do the same as what happend with car.
; - Update-cdr --> Blindly, sets the cdr of `scan` to the what is in `new` and increments `scan` then moves to gc-loop again. 
