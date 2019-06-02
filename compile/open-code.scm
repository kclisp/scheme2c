;;closures use whatever operation the environment has, which can change during run-time
;; with define and set!
;;if a symbol is defined and set! with only one type of procedure,
;; then the check can be compiled away
;;if a primitive symbol is not changed, then they can be open-coded
;;  if a primitive symbol is set! to another primitive in the top-level, then, following the timeline, can be open-coded (before the change, first primitive, after, second)
;;    if changed not in top-level, can't do anything -- only know it's primitive

;;macroexpanded
;;top-level vs in-lambda aren't checked
;;things like if could be optimized if it were
(define (annotate-cenv! exp cenv)
  (cond ((or (self-evaluating? exp)
             (quoted? exp)
             (variable? exp)))
        ((assignment? exp)
         (update-type! (assignment-variable exp)
                       (type-of (assignment-value exp) cenv)
                       cenv))
        ((definition? exp)
         (cenv-define-var! (definition-variable exp)
                           (type-of (definition-value exp) cenv)
                           cenv))
        ;;no sense of branching... important?
        ((if? exp)
         (apply annotate-cenv!s cenv (if-exps exp)))
        ;;important to extend - lambda might bind over previous variables
        ((lambda? exp)
         (apply annotate-cenv!s (extend-cenv (lambda-vars exp) cenv) (lambda-body exp)))
        ((begin? exp)
         (apply annotate-cenv!s cenv (begin-actions exp)))
        ((application? exp)
         (apply annotate-cenv!s cenv (operator exp) (operands exp)))
        (else
         (error "Unknown expression type -- ANNOTATE-CENV!" exp))))

(define (annotate-cenv!s cenv . exps)
  (for-each (lambda (exp)
              (annotate-cenv! exp cenv))
            exps))

;;types are pproc, cproc, and other
(define (type-of exp cenv)
  (cond ((or (self-evaluating? exp)
             (quoted? exp)
             (assignment? exp)
             (definition? exp))
         'other)
        ((variable? exp)
         (let ((binding (cenv-get-binding exp cenv)))
           (if binding
               (binding-type binding)
               (error "Variable not in cenv -- TYPE-OF" exp))))
        ((if? exp)
         (type-ofs cenv (if-consequent exp) (if-alternative exp)))
        ((lambda? exp)
         'cproc)
        ((begin? exp)
         (apply type-ofs cenv (begin-actions exp)))
        ((application? exp)             ;sophisticated could check?
         'other)
        (else
         (error "Unknown expression type -- TYPE-OF" exp))))

(define (type-ofs cenv . exprs)
  (apply merge-types (map (lambda (exp)
                            (type-of exp cenv))
                          exprs)))

;;need to merge values
(define (update-type! var type cenv)
  (let ((binding (cenv-get-binding var cenv)))
    (set-binding-type! binding (merge-types type (binding-type binding)))))

;;current merge is simple -- if different values, set type to other
(define (merge-types-1 type1 type2)
  (if (not (eq? type1 type2))
      'other
      type1))
(define (merge-types . types)
  (reduce-left merge-types-1 'other types))

;; (+ 1 2)                                 ;+ should get open-coded
;; (pp (compile-to-reg '(+ 1 2)))
;; ((assign val (const 2))
;;  (assign argl (op cons) (reg val) (const ()))
;;  (assign val (const 1))
;;  (assign argl (op cons) (reg val) (reg argl))
;;  (assign proc (op lexical-address-lookup) (const 0) (const 0) (reg env))
;;  primitive-branch65
;;  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)))


;; (define + -)
;; (+ 1 2)                                 ;naively shouldn't get open-coded
;;                                         ;sophisticatedly, open-codes to -

;; (pp (compile-to-reg '(begin (define + -)
;;                                         (+ 1 2))))
;; ((assign val (op lexical-address-lookup) (const 0) (const 1) (reg env))
;;  (perform (op define-variable!) (const 0) (const 0) (reg val) (reg env))
;;  (assign val (const ok))
;;  (assign val (const 2))
;;  (assign argl (op cons) (reg val) (const ()))
;;  (assign val (const 1))
;;  (assign argl (op cons) (reg val) (reg argl))
;;  (assign proc (op lexical-address-lookup) (const 0) (const 0) (reg env))
;;  primitive-branch68
;;  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)))


;; (+ 1 2)                                 ;sophisticatedly, open-coded
;; (lambda (x)
;;   (+ x 2))                              ;same as previous section
;; (define + -)
;; (+ 1 2)                                 ;same as previous

;; (lambda (f)
;;   (f 2))                                ;nothing can be said about f

;; (define fact (lambda (x) x))            ;compiled
;; (lambda (x) (fact x))                   ;compiled
;; (lambda (fact) (fact 2))                ;nothing can be said


;; (define + (lambda (y) 2))               ;+ is a cproc
;; (set! + (lambda (y) 2))                 ;+ is naively an other
