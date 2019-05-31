;;cenv - (frame next-cenv)
;;frame is a list of bindings (var . val)
;;bindings have no value (yet)
;; put values in eventually - open-code primitives
(define (make-cenv frame next-cenv)
  (list frame next-cenv))
(define (make-initial-cenv vars)
  (extend-cenv vars '()))

(define (lexical-address-lookup var cenv)
  (let ((address (cenv-get-address var cenv)))
    (if address
        address
        (error "LEXICAL-ADDRESS-LOOKUP - No var in cenv - " var))))

(define (cenv-get-address var cenv)
  (let loop ((var var)
             (cenv cenv)
             (env-address 0))
    (if (null? cenv)
        #f
        (let ((frame-address (frame-get-address var (cenv-frame cenv))))
          (if frame-address
              (list env-address frame-address)
              (loop var (cenv-next-cenv cenv) (+ 1 env-address)))))))

(define (frame-get-address var frame)
  (cond
   ((null? frame) #f)
   ((eq? var (binding-var (frame-first-binding frame))) 0)
   (else (+ 1 (frame-get-address var (frame-next-binding frame))))))

(define (cenv-define-var! var cenv)     ;no value yet
  (set-car! cenv (frame-add-binding (make-default-binding var)
                                    (cenv-frame cenv))))
(define (extend-cenv vars cenv)
  (make-cenv (make-frame (map make-default-binding vars))
             cenv))

;;Details
(define (cenv-frame cenv)
  (car cenv))
(define (cenv-next-cenv cenv)
  (cadr cenv))

(define (make-frame bindings)
  bindings)
(define (frame-first-binding frame)
  (car frame))
(define (frame-next-binding frame)
  (cdr frame))
(define (frame-add-binding binding frame)
  (cons binding frame))

(define (make-binding var val)
  (cons var val))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cdr binding))
(define (make-default-binding var)
  (cons var 'nothing))
