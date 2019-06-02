;;after macroexpansion
(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage cenv))
        ((assignment? exp)
         (compile-assignment exp target linkage cenv))
        ((definition? exp)
         (compile-definition exp target linkage cenv))
        ((if? exp) (compile-if exp target linkage cenv))
        ((lambda? exp) (compile-lambda exp target linkage cenv))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           cenv))
        ((application? exp)
         (compile-application exp target linkage cenv))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;;;linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(cont) '()
          '((goto (reg cont)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        ((lambda-ending? linkage) ;retract environment
         (make-instruction-sequence '(cont) ()
          `(,(lambda-ending-sequence linkage)
            (goto (reg cont)))))
        ((symbol? linkage)
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))
        (else
         (error "Unknown linkage -- COMPILE-LINKAGE" linkage))))

(define (lambda-ending? linkage) (tagged-list? linkage 'lambda-ending))
(define (lambda-ending-sequence linkage)
  `(perform (op retract-environment) (const ,(cadr linkage))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(cont)
   instruction-sequence
   (compile-linkage linkage)))


;;;simple expressions

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage cenv)
  (let ((binding (cenv-get-binding exp cenv))
        (address (lexical-address-lookup exp cenv)))
    (let* ((open-code (var-pproc? (binding-var binding) cenv))
           (needs (if open-code '() '(env)))
           (assignment
            (if open-code
                `((primitive-op ,(binding-val binding)))
                `((op lexical-address-lookup)
                  (const ,(car address))
                  (const ,(cadr address))
                  (reg env)))))
      (end-with-linkage linkage
       (make-instruction-sequence needs (list target)
        `((assign ,target ,@assignment)))))))

(define (compile-assignment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next cenv)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)          ;change eventually
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage cenv)
  (let ((var (definition-variable exp)))
    (let ((address (lexical-address-lookup var cenv)))
      (end-with-linkage linkage
       (preserving '(env)
        (compile (definition-value exp) 'val 'next cenv)
        (make-instruction-sequence '(env val) (list target)
         `((perform (op define-variable!)
                    (const ,(car address))
                    (const ,(cadr address))
                    (reg val)
                    (reg env))
           (assign ,target (const ok)))))))))


;;;conditional expressions


(define (compile-if exp target linkage cenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next cenv))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage cenv))
            (a-code
             (compile (if-alternative exp) target linkage cenv)))
        (preserving '(env cont)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test-branch (op false?) (reg val) (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;;; sequences

(define (compile-sequence seq target linkage cenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage cenv)
      (let ((first (compile (first-exp seq) target 'next cenv))) ;force order
        (preserving '(env cont)
          first
          (compile-sequence (rest-exps seq) target linkage cenv)))))

;;;lambda expressions

(define (compile-lambda exp target linkage cenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry cenv))
       after-lambda))))

(define (compile-lambda-body exp proc-entry cenv)
  (let* ((vars (lambda-vars exp))
         ;; the end of a lambda should retract-environment
         ;; examine internal lambdas, order vars by retractability
         ;; (examined (lambda-annotation exp))
         ;; (vars (car examined))
         ;; (num-retract (cadr examined))
         ;; ;possibly include retracting parent env
         ;; (linkage `(lambda-ending ,num-retract))
         ;; (end-linkage
          ;; (if (zero? num-retract) 'return linkage))
    )
    ;;need to reorder argl to match new variable order
    ;;waiting until argl is an array
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,(length vars))
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return ;; end-linkage
                       (extend-cenv vars cenv)))))


;;;SECTION 5.5.3

;;;combinations

(define (compile-application exp target linkage cenv)
  (let ((operand-codes
         (map (lambda (operand) (compile operand 'val 'next cenv))
              (operands exp)))
        (proc-code (compile (operator exp) 'proc 'next cenv)))
    (preserving '(cont)
     (preserving '(env)
      (construct-arglist operand-codes)
      (preserving '(argl)
       proc-code
       (make-instruction-sequence '(argl) '() '()))) ;always save argl for procedure call
     (compile-procedure-call (operator exp) target linkage cenv))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op cons) (reg val) (const ())))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;;;applying procedures

(define (compile-procedure-call operator target linkage cenv)
  (define (prepend-label seq label)
    (append-instruction-sequences label seq))
  (define (append-label seq label)
    (append-instruction-sequences seq label))
  (let ((compiled-branch (make-label 'compiled-branch))
        (primitive-branch (make-label 'primitive-branch))
        (after-call (make-label 'after-call)))
    (let* ((compiled-linkage
             (if (eq? linkage 'next) after-call linkage))
           (compiled-proc-seq
            (compile-proc-appl target compiled-linkage))
           (primitive-seq
            (end-with-linkage linkage
             (make-instruction-sequence '(proc argl)
              (list target)
              `((assign ,target
                        (op apply-primitive-procedure)
                        (reg proc)
                        (reg argl)))))))
      (let ((either
             (append-instruction-sequences
              (make-instruction-sequence '(proc) '()
               `((test-branch (op primitive-procedure?) (reg proc) (label ,primitive-branch))))
              (parallel-instruction-sequences
               (prepend-label compiled-proc-seq compiled-branch)
               (prepend-label primitive-seq primitive-branch))
              after-call)))
        (if (variable? operator)
            (cond
             ((var-pproc? operator cenv) primitive-seq)
             ((var-cproc? operator cenv) (append-label compiled-proc-seq after-call))
             (else either))
            either)))))

;;;applying compiled procedures

;;linkage is either return, label, or lambda-ending
(define (compile-proc-appl target linkage)
  (define (label? obj) (not (or (eq? obj 'return) (lambda-ending? obj))))
  (let ((base `((assign val (op compiled-procedure-entry)
                            (reg proc))
                ;; ,@(if (lambda-ending? linkage)
                ;;       (list (lambda-ending-sequence linkage))
                ;;       '())
                (goto (reg val)))))
    (cond ((and (eq? target 'val) (label? linkage))
           (make-instruction-sequence '(proc) all-regs
            `((assign cont (label ,linkage))
              ,@base)))
          ((and (not (eq? target 'val)) (label? linkage))
           (let ((proc-return (make-label 'proc-return)))
             (make-instruction-sequence '(proc) all-regs
              `((assign cont (label ,proc-return))
                ,@base
                ,proc-return
                (assign ,target (reg val))
                (goto (label ,linkage))))))
          ((and (eq? target 'val) (not (label? linkage)))
           (make-instruction-sequence '(proc cont) all-regs base))
          ((and (not (eq? target 'val)) (not (label? linkage)))
           (error "return linkage, target not val -- COMPILE"
                  target))
          (else (error "Unknown target and linkage -- COMPILE-PROC-APPL" target linkage)))))
