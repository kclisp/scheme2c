;;cenv - (frame next-cenv)
;;frame is a list of bindings - (binding ...)
;;binding is a list of variable, value, and
;; type (ccproc, pproc, other -- for top-level) - (var type)

(define (lexical-address-lookup var cenv)
  (let ((address (cenv-get-address var cenv)))
    (if address
        address
        (error "LEXICAL-ADDRESS-LOOKUP - No var in cenv - " var))))

(define (cenv-get-address var cenv)
  (let loop ((cenv cenv)
             (env-address 0))
    (if (null? cenv)
        #f
        (let ((frame-address (frame-get-address var (cenv-frame cenv))))
          (if frame-address
              (list env-address frame-address)
              (loop (cenv-next-cenv cenv) (+ 1 env-address)))))))
(define (frame-get-address var frame)
  (bindings-get-address var (frame-bindings frame)))
(define (bindings-get-address var bindings)
  (let loop ((bindings bindings)
             (address 0))
    (cond
     ((null? bindings) #f)
     ((eq? var (binding-var (car bindings))) address)
     (else (loop (cdr bindings) (+ 1 address))))))

(define (cenv-lookup var cenv if-found if-not-found)
  (let ((binding (cenv-get-binding var cenv)))
    (if binding
        (if-found binding)
        (if-not-found))))

(define (cenv-get-binding var cenv)
  (if (null? cenv)
      #f
      (let ((binding (frame-get-binding var (cenv-frame cenv))))
        (if binding
            binding
            (cenv-get-binding var (cenv-next-cenv cenv))))))
(define (frame-get-binding var frame)
  (bindings-get-binding var (frame-bindings frame)))
(define (bindings-get-binding var bindings)
  (find (lambda (binding) (eq? var (binding-var binding))) bindings))

(define (cenv-var-type var cenv)
  (cenv-lookup var cenv binding-type
               (lambda () (error "var not in cenv -- CENV-VAR-TYPE" var))))

(define (var-pproc? var cenv)
  (eq? (cenv-var-type var cenv) 'pproc))
(define (var-cproc? var cenv)
  (eq? (cenv-var-type var cenv) 'cproc))

;;define for current env frame
(define (cenv-define-var! var val type cenv)
  (frame-define-var! var val type (cenv-frame cenv)))
(define (frame-define-var! var val type frame)
  (let ((binding (frame-get-binding var frame)))
    (if binding
        (set-binding-type! binding type)
        (frame-extend! (make-binding var val type) frame))))

(define (extend-cenv vars cenv)
  (make-cenv (vars->frame vars) cenv))

(define (cenv-num-bindings cenv)
  (frame-num-bindings (cenv-frame cenv)))
(define (frame-num-bindings frame)
  (length (frame-bindings frame)))

(define (vars->frame vars)
  (make-frame (map make-default-binding vars)))
(define (frame-extend! binding frame)
  (append! (frame-bindings frame) (list binding)))

;;Details
(define (make-cenv frame next-cenv)
  (list frame next-cenv))
(define (cenv-frame cenv)
  (car cenv))
(define (cenv-next-cenv cenv)
  (cadr cenv))

(define (make-initial-cenv frame)
  (make-cenv frame '()))

(define (make-frame bindings)
  bindings)
(define (frame-bindings frame)
  frame)

(define (make-binding var val type)
  (list var val type))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cadr binding))
(define (binding-type binding)
  (caddr binding))
(define (set-binding-type! binding type)
  (set-car! (cddr binding) type))

(define (make-default-binding var)
  (make-binding var 'unbound 'other))
