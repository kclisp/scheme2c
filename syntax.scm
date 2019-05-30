(define label? symbol?)
(define label-value symbol->string)

(define (goto? exp)
  (tagged-list? exp 'goto))
(define goto-dest cadr)

(define (assign? exp)
  (tagged-list? exp 'assign))
(define (assign-dest dest)
  (symbol->string (cadr dest)))
(define assign-args cddr)

(define (test-branch? exp)
  (tagged-list? exp 'test-branch))

(define (save? exp)
  (tagged-list? exp 'save))
(define save-arg cadr)

(define (restore? exp)
  (tagged-list? exp 'restore))
(define restore-arg cadr)

(define (perform? exp)
  (tagged-list? exp 'perform))
(define perform-args cdr)

(define arg-type car)
(define (arg-val arg)
  (symbol->string (cadr arg)))
