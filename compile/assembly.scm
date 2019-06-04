;;Special (scheme-assembly ...) instructions
;;generate instruction sequences (no needs modifies) (for now)
;;currently: wrappers to compile instructions
(define (assemble exp cenv)
  (cond ((quoted? exp) (list (text-of-quotation exp))) ;quotes are only 1 statement
        ((compile-variable? exp) (assemble-variable exp cenv))
        ((compile-proc-call? exp) (assemble-proc-call exp cenv))
        ((compile-application? exp) (assemble-application exp cenv))
        ;; can add more
        (else (error "Unknown expression - ASSEMBLE" exp))))

(define (assemble-sequence exps cenv)
  (append-map (lambda (exp) (assemble exp cenv)) exps))

(define (assemble-arg arg cenv)
  (cond ((self-evaluating? arg) arg)
        ((quoted? arg) (text-of-quotation arg))
        ((this-cenv? arg) cenv)
        (else (error ("Unknown arg - ASSEMBLE-ARG" arg)))))

(define ((assemble-wrapper proc) exp cenv)
  (caddr (apply proc (map (lambda (arg) (assemble-arg arg cenv))
                          (function-args exp)))))

(define assemble-variable (assemble-wrapper compile-variable))
(define assemble-proc-call (assemble-wrapper compile-procedure-call))
(define assemble-application (assemble-wrapper compile-application))

;;DETAILS
(define (this-cenv? exp) (eq? exp 'this-cenv))

(define (compile-variable? exp) (tagged-list? exp 'compile-variable))
(define (compile-proc-call? exp) (tagged-list? exp 'compile-procedure-call))
(define (compile-application? exp) (tagged-list? exp 'compile-application))
(define (function-args exp) (cdr exp))
