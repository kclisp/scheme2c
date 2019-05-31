(define (compile-to-file filename exp)
  (with-output-to-file filename
    (lambda ()
      (display (template-replace (compile-to-c exp))))))

(define (compile-to-c exp)
  (set! label-counter 0)
  (sanitize (ccompile-sequence (caddr (compile-to-reg exp)))))

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

