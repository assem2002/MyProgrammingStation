; the following code would halt when in reaches the end of the sentence
(define (maybe-extend verb-phrase)
(amb verb-phrase
    (maybe-extend (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))

;If i'm right about my knowledge about this evaluator (that i haven't built yet!) our branching now is at it's end branch the previous ones wouldn't be evalutated unless the amp would work in a reversed way :) . 