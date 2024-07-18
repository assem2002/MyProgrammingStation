; mainly, the rule is the only thing that could result in recurive calls and then leads to loops.
; so what we could do to avoid such thing it to have a history invoked with every query we try to do.
; that history should memorize only the frames that are getting pushed in.
; if happens that we have the same exact frame that is already in the history, then we should stop the recursive call.
; They got to be totally matching.