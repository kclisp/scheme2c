;;take sicp's mceval
;;primitives: apply
;; number? string? symbol? null? eq?
;; cxr cons list set-car! set-cdr!
;; = < length
;; newline display read 
;; error
;; true false
;; map

;;still needed: most of them

(define (mceval) "mceval/mceval.scm")

(apply make-test 'mceval
       (read-file (mceval)))
