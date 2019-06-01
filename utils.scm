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

(define (list-union . s)
  (apply lset-union eq? s))

(define (list-intersect . s)
  (apply lset-intersection eq? s))

(define (list-difference . s)
  (apply lset-difference eq? s))

(define (list-adjoin s . els)
  (apply lset-adjoin eq? s els))
