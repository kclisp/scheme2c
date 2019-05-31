;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

(define (compile-to-reg exp)
  (compile (macroexpand exp) 'val 'next (get-primitives-cenv)))


(define (primitive-file) "../c/primitives.def")
(define (primitives)
  )

;;make sure cenv is ordered with the c environment
(define (get-primitives-cenv)
  (make-initial-cenv (primitives)))
