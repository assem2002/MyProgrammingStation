The professor lectures to the student in the class with the cat

(sentence (simple-noun-phrase (article The) (noun professor)) (verb lectures) ) ; still not empty
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (artcle the) (noun student)))) ) ; still not empty
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (artcle the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (artcle the) (noun class) )))) ; still not empty
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (artcle the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (artcle the) (noun class) ))) (prep-phrase (prep with) (simple-noun-phrase (artcle with) (noun cat))))) ; correct
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (artcle the) (noun student)))) (noun-phrase (prep-phrase (prep in) (simple-noun-phrase (artcle the) (noun class) )) (prep-phrase (prep with) (simple-noun-phrase (artcle the) (noun cat)))))) ; correct
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (artcle the) (noun student)) (prep-phrase (prep in ) (simple-noun-phrase (article the) (noun class))))))) ; still not empty
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (artcle the) (noun student)) (prep-phrase (prep in ) (simple-noun-phrase (article the) (noun class)))))) (prep-phrase (prep with) (simple-noun-phrase (artcle the) (noun cat))))) ; correct
(sentence (simple-noun-phrase (article The) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (artcle the) (noun student)) (prep-phrase (prep in ) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat))))))))) ; correct

;that's what i've thought of till the moment. I got no amb evaluator working at the moment!!.

