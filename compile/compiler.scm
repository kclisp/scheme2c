;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

(define (compile-to-reg exp)
  (let* ((cenv (get-primitives-cenv))
         (init-length (cenv-num-bindings cenv))
         (exp (macroexpand exp)))
    (annotate-cenv! exp cenv)
    ;; (annotate-lambdas! exp)
    (let ((new-length (cenv-num-bindings cenv))
          (compiled (caddr (compile exp 'val 'next cenv))))
      ;; the top level environment was extended
      (if (> new-length init-length)
          (cons `(perform (op increase-env-size)
                          (const ,(- new-length init-length)))
                compiled)
          compiled))))

(define (primitive-file) "c/primitives.def")
(define (primitive-pairs)
  (map (lambda (line)
         (let ((par1 (string-find-next-char line #\())
               (com (string-find-next-char line #\,))
               (par2 (string-find-next-char line #\))))
           (if (or (not par1) (not com) (not par2))
               (error "PRIMITIVES - primitive-file incorrectly formatted")
               ;; scheme, c
               (list (symbol (substring line (+ 1 par1) com))
                     (symbol (substring line (+ 2 com) par2))))))
       (read-lines (primitive-file))))

(define (get-primitives-cenv)
  (make-initial-cenv
   (make-frame
    (map (lambda (pair)
           ;;use the primitive symbol as the value
           ;;this is fine since if a (non prim) symbol is put in, the type becomes other
           (make-binding (car pair) (cadr pair) 'pproc))
         (primitive-pairs)))))
