
;These solutions aren't tested yet.


;a
(define (yacht-problem)
    ; Yacht names
    (let(
        (morre 'lorna)
        (colonel-downing 'melissa)
        (mr-hall 'rasalind)
        (sir-barnacle 'gabriell)
        (dr-parker 'mary))
        (let ((colonel-downing-daughter (amb 'lorna 'rasalind 'gabriell))
                (mr-hall-daughter (amb 'lorna 'rasalind 'gabriell))
                (dr-parker-daughter (amb 'lorna 'rasalind 'gabriell)))
                (require (not (eq? colonel-downing-daughter colonel-downing)))
                (require (not (eq? mr-hall-daughter mr-hall)))
                (require (not (eq? dr-parker-daughter dr-parker)))
                (require (distinct? (list dr-parker-daughter colonel-downing-daughter mr-hall-daughter)))
                (require (and (eq? mr-hall-daughter 'gabriell) (eq? dr-parker 'rasalind)))
                (list ('colonel-downing-daughter  colonel-downing-daughter) 
                        ('dr-parker-daughter dr-parker-daughter)
                        ('mr-hall-daughter mr-hall-daughter)))))


;b
(define (yacht-problem)
    ; Yacht names
    (let(
        (morre 'lorna)
        (colonel-downing 'melissa)
        (mr-hall 'rasalind)
        (sir-barnacle 'gabriell)
        (dr-parker 'mary))
        (let ((colonel-downing-daughter (amb 'lorna 'rasalind 'gabriell 'mary))
                (mr-hall-daughter (amb 'lorna 'rasalind 'gabriell 'mary))
                (dr-parker-daughter (amb 'lorna 'rasalind 'gabriell 'mary)))
                (require (not (eq? colonel-downing-daughter colonel-downing)))
                (require (not (eq? mr-hall-daughter mr-hall)))
                (require (not (eq? dr-parker-daughter dr-parker)))
                (require (distinct? (list dr-parker-daughter colonel-downing-daughter mr-hall-daughter)))
                (require  (not (eq? colonel-downing-daughter 'gabriell)))
                (list ('colonel-downing-daughter  colonel-downing-daughter) 
                        ('dr-parker-daughter dr-parker-daughter)
                        ('mr-hall-daughter mr-hall-daughter))))
    
)