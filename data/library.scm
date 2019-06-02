;;; SCHEME LIBRARY - to be added to expressions, not evaluated directly

;; (define true #t)
;; (define false #f)

;; needs to be in library or a special case since primitives don't have access to cont
(define (apply proc . args)
  (scheme-assembly
   '(assign argl (op cons*) (reg argl)))
  ;;this only works if thunks get passed argl, which is true right now
  (proc)
  ;;better: something like this
  ;; (scheme-assembly
  ;;  (compile-variable 'proc 'proc 'next this-cenv)
  ;;  (compile-procedure-call 'val 'return #f))
  )
