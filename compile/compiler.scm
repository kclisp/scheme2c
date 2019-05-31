;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

(define (compile-to-reg exp)
  (compile (macroexpand exp) 'val 'next (get-primitives-cenv)))

(define (primitive-file) "c/primitives.def")
(define (primitives)
  (map (lambda (line)
         (let ((par (string-find-next-char line #\())
               (com (string-find-next-char line #\,)))
           (if (or (not par) (not com))
               (error "PRIMITIVES - primitive-file incorrectly formatted")
               (symbol (substring line (+ 1 par) com)))))
       (read-lines (primitive-file))))

(define (get-primitives-cenv)
  (make-initial-cenv (primitives)))
