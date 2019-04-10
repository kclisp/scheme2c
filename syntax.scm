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

(define arg-type car)
(define (arg-val arg)
  (symbol->string (cadr arg)))

(define (test? exp)
  (tagged-list? exp 'test))

(define (branch? exp)
  (tagged-list? exp 'branch))

(define (save? exp)
  (tagged-list? exp 'save))

(define (restore? exp)
  (tagged-list? exp 'restore))
