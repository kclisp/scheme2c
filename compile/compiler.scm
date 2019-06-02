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

(define (get-primitives-cenv)
  (make-initial-cenv
   (make-frame
    (map (lambda (pair)
           ;;use the primitive symbol as the value
           ;;this is fine since if a (non prim) symbol is put in, the type becomes other
           (make-binding (car pair) (cadr pair) 'pproc))
         (primitive-pairs)))))
