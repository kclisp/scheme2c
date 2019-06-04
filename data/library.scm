;;; SCHEME LIBRARY - to be added to expressions, not evaluated directly

;;problem primitives don't have access to cont, which proc, if compiled, jumps to
;;option 1: apply needs to be in library or a special case
;; and implement (scheme-assembly ..) which passes to the register machine scheme
;;option 2: give primitives access to cont
;;option 3: pass cont to compiled procedure
(define (apply f . args)
  (scheme-assembly
   (compile-variable 'args 'argl 'next this-cenv)
   '(assign argl (op cons_star) (reg argl)) ;not nice -- turn into some compile instruction?
   (compile-variable 'f 'proc 'next this-cenv) ;pass to scheme-assembly
   (compile-procedure-call 'val 'return #f)))

(define true #t)
(define false #f)
(define (not exp)
  (if exp false true))

(define (list . args)
  args)

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))

;;only 1 list
(define (map proc list)
  (if (null? list)
      '()
      (cons (proc (car list)) (map proc (cdr list)))))

;; only some of the cxr
(define (cadr exp)
  (car (cdr exp)))
(define (cddr exp)
  (cdr (cdr exp)))
(define (caadr exp)
  (car (cadr exp)))
(define (caddr exp)
  (car (cddr exp)))
(define (cdadr exp)
  (cdr (cadr exp)))
(define (cdddr exp)
  (cdr (cddr exp)))
(define (cadddr exp)
  (car (cdddr exp)))
