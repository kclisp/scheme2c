(define (compile-to-c exp)
  (sanitize (ccompile-sequence (caddr (compile exp 'val 'next)))))

(define (line . args)
  (string-append* (append args (list "\n"))))
(define (statement . args)
  (string-append* (append args (list ";\n"))))

(define (sanitize string)
  (string-map sanitize-char string))

(define (sanitize-char char)
  (case char
    ((#\-) #\_)
    ((#\?) #\p)
    ((#\!) #\m)
    (else char)))
