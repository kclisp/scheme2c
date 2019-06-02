;;treat let, cond, define, internal definitions (lambda) as macros
;;eventally quasiquote too
(define (macroexpand exp)
  (cond
   ((quoted? exp) exp)
   ((cond? exp) (macroexpand (cond->if exp)))
   ((let? exp) (macroexpand (let->lambda exp)))
   ((edefine? exp) (macroexpand (edefine->lambda exp)))
   ((idefine? exp) (macroexpand (idefine->lambda exp)))
   ((list? exp) (map macroexpand exp))
   (else exp)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (let->lambda exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (map car bindings) (let-body exp))
          (map cadr bindings))))

;;extended define
(define (edefine? exp)
  (and (definition? exp)
       (pair? (cadr exp))))
(define (edefine->lambda exp)
  `(define ,(edefine-symbol (cadr exp)) ,@(edefine-expand (cadr exp) (cddr exp))))
(define (edefine-symbol nested)
  (if (pair? nested)
      (edefine-symbol (car nested))
      nested))
(define (edefine-expand nested body)
  (if (pair? nested)
      (edefine-expand (car nested)
                      `((lambda ,(cdr nested) ,@body)))
      body))

;;internal definition
(define (idefine? exp)
  (and (lambda? exp)
       (definition? (car (lambda-body exp)))))
;;return list of variable value pairs
(define (idefine-definitions exp)
  (map cdr (map edefine->lambda (filter definition? (lambda-body exp)))))
(define (idefine-body exp)
  (remove definition? (lambda-body exp)))

(define (idefine->lambda exp)
  (let ((definitions (idefine-definitions exp)))
    (let ((new-bindings (map (lambda (def) (list (car def) '*unassigned*))
                             definitions))
          (sets (map (lambda (def) (cons 'set! def))
                     definitions))
          (body (idefine-body exp)))
      `(lambda ,(lambda-parameters exp)
         (let (,@new-bindings)
           ,@sets
           ,@body)))))
