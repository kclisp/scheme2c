;;; SCHEME LIBRARY - to be added to expressions, not evaluated directly

;; (define true #t)
;; (define false #f)

(define (apply proc . args)
  ;; put the args into the right form
  (proc args))
