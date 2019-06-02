;;; SCHEME LIBRARY - to be added to expressions, not evaluated directly

;; (define true #t)
;; (define false #f)

;;problem primitives don't have access to cont, which proc, if compiled, jumps to
;;option 1: apply needs to be in library or a special case
;; and implement (scheme-assembly ..) which passes to the register machine scheme
;;option 2: give primitives access to cont
;;option 3: pass cont to compiled procedure
(define (apply proc . args)
  (scheme-assembly
   (assign argl (op cons*) (reg argl)))
  ;; doesn't work since argl gets replaced
  (proc)
  ;;better: something like below. But requires calls
  ;; (scheme-assembly
  ;;  (compile-variable 'proc 'proc 'next this-cenv)
  ;;  (compile-procedure-call 'val 'return #f))
  )
