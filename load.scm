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
(load "tests")
(load "benchmark")
