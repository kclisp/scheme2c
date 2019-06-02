;;; SCHEME LIBRARY - to be added to expressions

;; (define true #t)
;; (define false #f)

(define (apply proc . args)
  ;; put the args into the right form
  (proc args))
