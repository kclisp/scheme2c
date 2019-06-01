;;macroexpanded
;;walk the expression tree, annotate lambdas with free vars and used-in-internal
;;put the annotation into an eq? hashtable
(define (annotate-lambdas exp)
  (var-usage exp))

(define (var-usage exp)
  (cond ((or (self-evaluating? exp)
             (quoted? exp))
         (empty-var-usage))
        ((variable? exp)
         (make-body-var-usage exp))
        ((assignment? exp)
         (add-free-body (assignment-variable exp)
                        (var-usage (assignment-value exp))))
        ((definition? exp)
         (add-free-body (definition-variable exp)
                        (var-usage (definition-value exp))))
        ((if? exp)
         (var-usages (if-predicate exp) (if-consequent exp) (if-alternative exp)))
        ((lambda? exp) (annotate-lambda exp))
        ((begin? exp)
         (apply var-usages (begin-actions exp)))
        ((application? exp)
         (apply var-usages (operator exp) (operands exp)))
        (else (error "Unknown expression - VAR-USAGE"exp))))

(define (annotate-lambda exp)
  (hash-table-ref lambda-var-usage exp
    (lambda ()
      (hash-table-intern! lambda-var-usage exp (lambda () (var-usage-lambda exp))))))

;;free vars imply closures --> lambdas that have free vars are closures
;;external lambdas bind their variables to free vars in closures

;;used in internal is the union of the free-lambda in internal lambdas
;; intersected with lambda-vars
;;free-body vars become free-lambda vars if not in the lambda
(define (var-usage-lambda exp)
  (let ((vars (lambda-vars exp))
        (body-usage (apply var-usages (lambda-body exp))))
    (make-var-usage '()
                    (list-difference (usage-free-vars body-usage)
                                     vars)
                    (list-intersect (usage-free-lambda body-usage)
                                    vars))))

;;annotates lambda expression with a list (free-vars used-in-internal)
(define lambda-var-usage (make-equal-hash-table)) ;could possibly be eq?, for our purposes

;;Details
(define (var-usages . exps)
  (apply union-var-usage
         (map var-usage exps)))

(define (make-var-usage free-body free-lambda used-in-internal)
  (list free-body free-lambda used-in-internal))
(define (empty-var-usage) (make-var-usage '() '() '()))
(define (usage-free-body obj) (car obj))
(define (usage-free-lambda obj) (cadr obj))
(define (usage-used-internal obj) (caddr obj))

(define (usage-free-vars obj)
  (list-union (usage-free-body obj) (usage-free-lambda obj)))

(define (make-body-var-usage var)
  (make-var-usage (list var) '() '()))

(define (add-free-body var obj)
  (union-var-usage (make-body-var-usage var) obj))

(define (union-var-usage . vus)
  (apply make-var-usage (apply map list-union vus)))


;; (lambda (x y)                           ;(() (+) ()) --> x and y can be retracted
;;   (+ x y))                              ;((+ x y) () ())

;; (lambda (x y)                           ;(() (+) ()) --> x and y can be retracted
;;   (lambda (y z)                         ;(() (+) (y)) --> z can be retracted
;;     (lambda (x z)                       ;(() (+ y) ()) --> x and z can be retracted
;;       (+ x y z))))                      ;((+ x y z) () ())

;; (lambda (x)                             ;(() (+) (x))
;;   (lambda (y)                           ;(() (+ x) (y))
;;     (lambda (z)                         ;(() (+ x y) ())
;;       (+ x y z))))                      ;((+ x y z) () ()))

;; (lambda (x)                             ;(() (+ *) (x))
;;                                         ;(() (+ * x) ())  -- unioned
;;   (lambda (y)                           ;(() (+) (y))
;;     (lambda (x z)                       ;(() (+ y) ())
;;       (+ x y z)))                       ;((+ x y z) () ())
;;   (lambda (w)                           ;(() (* x) ())
;;     (* 2 x w)))                         ;((* x w) () ())

;;todo: optimize by getting rid of used-internal. That's specific to lambdas, and only
;; that should be annotated

;;lambda-vars minus used-in-internal gets the variables that can be retracted

;;returns new-vars and how many should be retracted
;;new-vars should be ordered: (un-retracted retracted)
(define (lambda-annotation exp)
  (let* ((vars (lambda-vars exp))
         (retract-vars
          (list-difference vars
                           (usage-used-internal (hash-table-ref lambda-var-usage exp)))))
    (list (append (list-difference vars retract-vars) retract-vars)
          (num-retract vars retract-vars))))

;;possibly include retracting parent env
(define (num-retract vars retract-vars)
  (let ((num (length retract-vars)))
    (if (= num (length vars))
        (+ 1 num)
        num)))
