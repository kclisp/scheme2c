(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (read-delimited-string (char-set)))))

(define (read-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '()
            (cons line (loop (read-line))))))))
