;;;READ
(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((obj (read)))
        (if (eof-object? obj)
            '()
            (cons obj (loop (read))))))))

(define (read-file-to-string filename)
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

;;;FIND AND REPLACE
;; assume no overlaps
(define (find-and-replace base replace with)
  (let ((index (string-search-forward replace base)))
    (if (not index)
        base
        (string-append (string-slice base 0 index)
                       with
                       (find-and-replace (string-slice base (+ index (string-length replace)))
                                         replace with)))))

;;LIST SETS
(define (list-union . s)
  (apply lset-union eq? s))

(define (list-intersect . s)
  (apply lset-intersection eq? s))

(define (list-difference . s)
  (apply lset-difference eq? s))

(define (list-adjoin s . els)
  (apply lset-adjoin eq? s els))
