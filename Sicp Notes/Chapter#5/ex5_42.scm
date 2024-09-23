(define (compile-variable exp target linkage cmp-env)
(end-with-linkage linkage
(make-instruction-sequence '(env) (list target)
`((assign ,target
(op find-variable)
(const ,exp)
(reg cmp-env))
`(assign ,target (op lexical-address-lookup ,target env))
`(test ;We should test if ,target equals `not-found` if so use the normal lookup and pass the global-env and manage the branching correctly. 
 )))))


; Update :: The environment is compile time env, which means we can't use it in the runtime
; so we should look it up now while compiling not at runtime.
; see http://community.schemewiki.org/?sicp-ex-5.42