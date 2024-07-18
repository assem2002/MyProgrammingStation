(rule (ends-with-grandson (grandson)))
(rule (ends-with-grandson (?x . ?y))
    (ends-with-grandson ?y))

(rule ((grandson) ?x ?y) (grandson ?x ?y))
(rule ((great . ?rel) ?x ?y)
    (and 
        (son ?x ?z)
        (?rel ?z ?y)
        (ends-with-grandson ?rel))) ; I think it must be the last query, as ?rel should be assigned to some value so we can check if it ends with grandson.