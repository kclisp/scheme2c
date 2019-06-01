;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

(define (compile-to-reg exp)
  (let* ((cenv (get-primitives-cenv))
         (init-length (cenv-num-bindings cenv))
         (exp (macroexpand exp)))
    (annotate-lambdas exp)
    (let* ((compiled (caddr (compile exp 'val 'next cenv)))
           (new-length (cenv-num-bindings cenv)))
      ;; the top level environment was extended
      (if (> new-length init-length)
          (cons `(perform (op increase-env-size)
                          (const ,(- new-length init-length)))
                compiled)
          compiled))))

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
