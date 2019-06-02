;;;Ccompile
;;takes IR expression, returns a string
(define (ccompile exp)
  (cond
   ((label? exp) (ccompile-label exp))
   ((goto? exp) (ccompile-goto exp))
   ((assign? exp) (ccompile-assign exp))
   ((test-branch? exp) (ccompile-test-branch exp))
   ((save? exp) (ccompile-save exp))
   ((restore? exp) (ccompile-restore exp))
   ((perform? exp) (ccompile-perform exp))
   (else (error "Unknown expression -- CCOMPILE" exp))))

(define (ccompile-sequence seq)
  (string-append* (map ccompile seq)))

(define (ccompile-label exp)
  (format #f "~a:\n" (label-value exp)))

(define (ccompile-goto exp)
  (format #f "goto ~a;\n" (ccompile-dest (goto-dest exp))))

(define (ccompile-assign exp)
  (format #f "~a = ~a;\n" (assign-dest exp) (ccompile-args (assign-args exp))))

(define (ccompile-test-branch exp)
  (format #f "if (~a) ~a"
          (ccompile-args (except-last-pair (cdr exp)))
          (ccompile-goto `(goto ,(cadddr exp)))))

;;for save and restore, use temporary variables as stack
(define (ccompile-save exp)
  (ccompile `(perform (op save) (reg ,(save-arg exp)))))

(define (ccompile-restore exp)
  (ccompile `(assign ,(restore-arg exp) (op restore))))

(define (ccompile-perform exp)
  (format #f "~a;\n" (ccompile-args (perform-args exp))))

;;Details
(define (ccompile-dest dest)
  (case (arg-type dest)
    ((label) (arg-val dest))
    ((reg) (format #f "*obj_clear(~a)" (arg-val dest)))
    (else (error "Unknown dest -- CCOMPILE-DEST" dest))))

(define (ccompile-args args)
  (let ((first-arg (car args)))
    (case (arg-type first-arg)
      ((const label primitive-op reg) (ccompile-arg first-arg)) ;reg probably
      ((op) (ccompile-op args))
      (else (error "Unknown arg-type -- CCOMPILE-ARGS" args)))))

(define (ccompile-op args)
  (format #f "~a(~a)"
          (arg-val (car args))
          (if (null? (cdr args))
              ""
              (apply string-append
                     (ccompile-arg (cadr args))
                     (map (lambda (arg)
                            (format #f ", ~a" (ccompile-arg arg)))
                          (cddr args))))))

(define (ccompile-arg arg)
  (case (arg-type arg)
    ((label) (format #f "adr_to_obj(&&~a)" (arg-val arg)))
    ((reg) (arg-val arg))
    ((primitive-op) (format #f "make_primitive_procedure(~a)" (arg-val arg)))
    ((const) (ccompile-const (cadr arg)))
    (else (error "Unknown arg-type -- CCOMPILE-ARG" arg))))

(define (ccompile-const const)
  (cond
   ((number? const) (ccompile-number const))
   ((pair? const) (ccompile-cons const))
   ((symbol? const) (ccompile-symbol const))
   ((string? const) (ccompile-string const))
   ((vector? const) (ccompile-vector const))
   ((null? const) "nil")
   (else (error "Unknown type -- CCOMPILE-CONST" const))))
;; should be done statically at compile time...
(define (ccompile-number number)
  (cond
   ((integer? number) (format #f "int_to_obj(~a)" number))
   ((real? number) (format #f "dbl_to_obj(~a)" (inexact number)))
   (else (error "Unknown type -- CCOMPILE-NUMBER" number))))
(define (ccompile-cons pair)
  (format #f "cons(~a, ~a)" (ccompile-const (car pair)) (ccompile-const (cdr pair))))
(define (ccompile-symbol symbol)
  (format #f "sym_to_obj(~s)" (string-downcase (string symbol))))
(define (ccompile-string string)
  (format #f "str_to_obj(~s)" string))
(define (ccompile-vector vector)
  "generic-const")
