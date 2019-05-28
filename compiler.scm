(define (compile-to-c exp)
  (sanitize (ccompile-sequence (caddr (compile exp 'val 'next)))))

(define (line . args)
  (string-append* (append args (list "\n"))))
(define (statement . args)
  (string-append* (append args (list ";\n"))))

(define (sanitize string)
  (define state                         ;don't sanitize in strings
    (let ((in-string #f))
      (lambda (char)
        (if (char=? char #\")
            (set! in-string (not in-string)))
        (if in-string char (sanitize-char char)))))
  (string-map state string))

(define (sanitize-char char)
  (case char
    ((#\-) #\_)
    ((#\?) #\p)
    ((#\!) #\m)
    (else char)))
