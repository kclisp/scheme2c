(define (compile-to-file filename exps)
  (with-output-to-file filename
    (lambda ()
      (display (template-replace (compile-to-c exps))))))

(define (compile-to-c exps)
  (set! label-counter 0)
  (sanitize (ccompile-sequence (compile-to-reg exps))))


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

