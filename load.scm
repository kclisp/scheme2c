;;files should be based on base
(define base (pwd))
(define (on-base filename)
  (format #f "~a~a" (->namestring base) filename))

;;make sure dir doesn't change if an error happens
(define (cd-load dir file)
  (let ((here (pwd)))
    (dynamic-wind
      (lambda () (cd dir))
      (lambda () (load file))
      (lambda () (cd here)))))

(load "utils")
(cd-load "data" "data")
(cd-load "compile" "load")
(cd-load "ccompile" "load")
(cd-load "tests" "load")
