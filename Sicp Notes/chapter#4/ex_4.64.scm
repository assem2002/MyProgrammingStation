; We're simply recursivly calling the rule with same inputs over and over again, which simply would result in infinite loop.
; The previous implementation of 'outranked-by' was calling itself recursivly with one of its argument filled with data which makes the 'supervisor' at some point find no result,
; which should (as i understand) stop the recursive call.
; Note : AND implies that when a variable isn't bound the other asssertions should fail.

