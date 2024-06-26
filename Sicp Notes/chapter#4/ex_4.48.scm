
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define adjectives '(adjectives tall thin fat short happy sad))
(define adverb '(adverb slowly happly sadly rarely soon carefully))
(define *unparsed* '())
(define (parse input)
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
        (require (null? *unparsed*)) 
        sent))


(define (parse-prepositional-phrase)
(list 'prep-phrase
    (parse-word prepositions)
    (parse-noun-phrase)))
(define (parse-adjectives)
    (list 'adjective-phrase
        (parse-word adjectives)
        (parse-word noun)))

(define (parse-sentence)
(list 'sentence (parse-noun-phrase-with-adj) (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-verb-phrase)
(define (maybe-extend verb-phrase)
    (amb verb-phrase
        (maybe-extend (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))
        (maybe-extend (list 'verb-phrase verb-phrase (parse-word adverb)))
        (maybe-extend (list 'verb-phrase verb-phrase (parse-noun-phrase-with-adj)))
    ))
(maybe-extend (parse-word verbs)))


(define (parse-simple-noun-phrase)
(list 'simple-noun-phrase
(parse-word articles)
(parse-word nouns)))

(define (parse-noun-phrase-with-adj)
(amb
(list 'simple-adj-phrase
(parse-word articles)
(parse-adjectives)
(parse-word nouns))

(list 'simple-adj-phrase
(parse-adjectives)
(parse-word nouns))))

(define (parse-noun-phrase)
(define (maybe-extend noun-phrase)
    (amb noun-phrase 
    (maybe-extend (list 'noun-phrase noun-phrase (parse-prepositional-phrase)))))

(maybe-extend (parse-simple-noun-phrase))
)