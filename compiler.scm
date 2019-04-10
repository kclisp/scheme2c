(load "sicp-compiler/compiler.scm")
(load "syntax.scm")

(define (compile-to-c exp)
  (ccompile-sequence (caddr (compile exp 'val 'next))))

(define (ccompile-sequence seq)
  (string-append* (map ccompile seq)))

;;takes IR expression, returns a string
(define (ccompile exp)
  (cond
   ((label? exp) (ccompile-label exp))
   ((goto? exp) (ccompile-goto exp))
   ((assign? exp) (ccompile-assign exp))
   ((test? exp) (ccompile-test exp))
   ((branch? exp) (ccompile-branch exp))
   ((save? exp) (ccompile-save exp))
   ((restore? exp) (ccompile-restore exp))
   (else (error "Unknown expression -- CCOMPILE" exp))))

(define (ccompile-label exp)
  (string-append (label-value exp) ":"))

(define (ccompile-goto exp)
  (string-append "goto " (ccompile-dest (goto-dest exp)) ";"))

(define (ccompile-assign exp)
  (string-append (assign-dest exp)
                 " = "
                 (ccompile-args (assign-args exp))
                 ";"))

(define (ccompile-test exp)
  (ccompile `(assign flag ,@(cdr exp))))

(define (ccompile-branch exp)
  (string-append "if (flag) {" (ccompile-goto `(goto ,@(cdr exp))) "}"))

;;for save and restore, use temporary variables (as stack...)

(define (ccompile-dest dest)
  (case (arg-type dest)
    ((label) (arg-val dest))
    ((reg) (string-append "*" (arg-val dest)))
    (else (error "Unknown dest -- CCOMPILE-DEST" dest))))

(define (ccompile-args args)
  (let ((first-arg (car args)))
    (case (arg-type first-arg)
      ((const label) (ccompile-arg first-arg))
      ((op) (ccompile-op args))
      (else (error "Unknown arg-type -- CCOMPILE-ARGS" args)))))

(define (ccompile-op args)
  (string-append (arg-val (car args))
                 "("
                 (ccompile-arg (cadr args))
                 (string-append* (map (lambda (arg)
                                        (string-append ", " (ccompile-arg arg)))
                                      (cddr args)))
                 ")"))

(define (ccompile-arg arg)
  (case (arg-type arg)
    ((label) (string-append "&&" (arg-val arg)))
    ((reg) (arg-val arg))
    ((const) (ccompile-const arg))
    (else (error "Unknown arg-type -- CCOMPILE-ARG" arg))))

(define (ccompile-const const)
  ;;TODO: symbol, number, cons, string, vector
  "generic-const")
