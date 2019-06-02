(define (template-file) "data/template.c")
(define (c-template)
  (read-file-to-string (template-file)))

;;replace //CODE in template with string
(define (template-replace str)
  (find-and-replace (c-template) "//CODE" str))


(define (primitive-file) "data/primitives.def")
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
