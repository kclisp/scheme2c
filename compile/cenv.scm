;;cenv - (numframes frame next-cenv)
;;frame is a list of bindings (var . val)
;;bindings have no value (yet)
;; put values in eventually - open-code primitives
(define (make-cenv numframes frame next-cenv)
  (list numframes frame next-cenv))
(define (make-initial-cenv vars)
  (make-cenv 0 (vars->frame vars) '()))

(define (lexical-address-lookup var cenv)
  (let ((address (cenv-get-address var cenv)))
    (if address
        address
        (error "LEXICAL-ADDRESS-LOOKUP - No var in cenv - " var))))

(define (cenv-get-address var cenv)
  (let loop ((var var)
             (cenv cenv)
             (env-address (cenv-numframes cenv)))
    (if (null? cenv)
        #f
        (let ((frame-address (frame-get-address var (cenv-frame cenv))))
          (if frame-address
              (list env-address frame-address)
              (loop var (cenv-next-cenv cenv) (- env-address 1)))))))

(define (frame-get-address var frame)
  (bindings-get-address var (frame-bindings frame)))
(define (bindings-get-address var bindings)
  (let loop ((bindings bindings)
             (address 0))
    (cond
     ((null? bindings) #f)
     ((eq? var (binding-var (car bindings))) address)
     (else (loop (cdr bindings) (+ 1 address))))))

(define (cenv-define-var! var cenv)     ;no value yet
  (frame-define-var! var (cenv-frame cenv)))
(define (extend-cenv vars cenv)
  (make-cenv (+ 1 (cenv-numframes cenv))
             (vars->frame vars)
             cenv))

(define (vars->frame vars)
  (make-frame (map make-default-binding vars)))
(define (frame-define-var! var frame)
  (append! (frame-bindings frame) (list (make-default-binding var))))

;;Details
(define (cenv-numframes cenv)
  (car cenv))
(define (cenv-frame cenv)
  (cadr cenv))
(define (cenv-next-cenv cenv)
  (caddr cenv))
(define (extend-cenv-frame! cenv val)
  (set-car! (cdr cenv) val))

(define (make-frame bindings)
  bindings)
(define (frame-bindings frame)
  frame)
(define (frame-first-binding frame)
  (car frame))
(define (frame-next-bindings frame)
  (cdr frame))

(define (make-binding var val)
  (cons var val))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cdr binding))
(define (make-default-binding var)
  (cons var 'nothing))
