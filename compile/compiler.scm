;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

(define (compile-to-reg exp)
  (compile (macroexpand exp) 'val 'next (make-cenv)))

(define (make-cenv) '())
