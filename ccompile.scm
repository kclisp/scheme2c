;;;Ccompile
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
   ((perform? exp) (ccompile-perform exp))
   (else (error "Unknown expression -- CCOMPILE" exp))))

(define (ccompile-sequence seq)
  (string-append* (map ccompile seq)))

(define (ccompile-label exp)
  (line (label-value exp) ":"))

(define (ccompile-goto exp)
  (statement "goto " (ccompile-dest (goto-dest exp))))

(define (ccompile-assign exp)
  (statement (assign-dest exp)
             " = "
             (ccompile-args (assign-args exp))))

(define (ccompile-test exp)
  (ccompile `(assign flag ,@(cdr exp))))

(define (ccompile-branch exp)
  (string-append "if (flag) " (ccompile-goto `(goto ,@(cdr exp)))))

;;for save and restore, use temporary variables as stack
(define (ccompile-save exp)
  (ccompile `(perform (op save) (reg ,(save-arg exp)))))

(define (ccompile-restore exp)
  (ccompile `(assign ,(restore-arg exp) (op restore))))

(define (ccompile-perform exp)
  (statement (ccompile-args (perform-args exp))))
;;Details

(define (ccompile-dest dest)
  (case (arg-type dest)
    ((label) (arg-val dest))
    ((reg) (string-append "*" (arg-val dest)))
    (else (error "Unknown dest -- CCOMPILE-DEST" dest))))

(define (ccompile-args args)
  (let ((first-arg (car args)))
    (case (arg-type first-arg)
      ((const label reg) (ccompile-arg first-arg)) ;reg probably
      ((op) (ccompile-op args))
      (else (error "Unknown arg-type -- CCOMPILE-ARGS" args)))))

(define (ccompile-op args)
  (string-append (arg-val (car args))
                 "("
                 (if (null? (cdr args))
                     ""
                     (string-append
                      (ccompile-arg (cadr args))
                      (if (null? (cddr args))
                          ""
                          (string-append* (map (lambda (arg)
                                                 (string-append ", " (ccompile-arg arg)))
                                               (cddr args))))))
                 ")"))

(define (ccompile-arg arg)
  (case (arg-type arg)
    ((label) (string-append "&&" (arg-val arg)))
    ((reg) (arg-val arg))
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
