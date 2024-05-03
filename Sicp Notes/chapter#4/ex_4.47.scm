;It shouldn't work correctly as it calls itself again with *unparsed* variable already changed so it won't output the same result
; and even if this doesn't mutate *unparsed* the procedure would call itself forever.


;if we interchage the order it would definitely work forever 