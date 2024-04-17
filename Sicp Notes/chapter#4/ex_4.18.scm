; The text in the exercise would lead to an error as dy would need to access y while its value is still *unassigned*.
; The approach described in previously would work fine as y would be defined by the time (stream-map f y) would start to work.
