;The basic solution would be
(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) (?x)) 
    (last-pair (?v) (?x)))

;This using of the dot noation works on procedure declaration in Scheme interperters
(rule (last-pair ((. ?y) ?x '()) (?x . '())))
(rule (last-pair (?x '()) (?x '())))

; (last-pair (3) ?x) --> (last-pair (3) (3))
; (last-pair (1 2 3) ?x) --> (last-pair (1 2 3) (3))
; (last-pair (2 ?x) (3)) --> (last-pair (2 3) (3))
; (last-pair ?x (3)) --> i think second rule can handle such a thing