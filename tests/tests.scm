;;Tests
(define (compile-dir) "compiled")
(define (make-test name . exps)
  (compile-to-file (format #f "~a/~a.c" (compile-dir) name)
                   (cons 'begin exps)))

;;with val.u as return value
(make-test 'adding '(+ 1 2))
;; [kenny@dellArch example]$ make adding
;; ...
;; [kenny@dellArch example]$ ./adding.out 
;; [kenny@dellArch example]$ echo $?
;; 3

(make-test 'lambda '(lambda (x) (+ x 1)))
;; [kenny@dellArch example]$ ./lambda.out 
;; [kenny@dellArch example]$ echo $?
;; 4
;;probably because the return value restricts the output, so this is the cons index

(make-test 'lambda2 '((lambda (x) (+ x 1)) 20))
;; [kenny@dellArch example]$ ./lambda2.out 
;; [kenny@dellArch example]$ echo $?
;; 21

(make-test 'display '(display "Hello, world!\n"))
;; [kenny@dellArch example]$ ./display.out 
;; Hello, World!

(make-test 'display2 '(display '(1.3 2 hi "hello" +)))
;; [kenny@dellArch example]$ ./display2.out 
;; (1.300000 2 hi hello +)

(make-test 'display3 '(display +))
;; [kenny@dellArch example]$ ./display3.out 
;; #[primitive procedure 0x55d72e2b0d83]

(make-test 'add '(display (+ 1 2 3 4 5)))
;; [kenny@dellArch example]$ ./add.out 
;; 15

(make-test 'define
           '(define x 21)
           'x)
;; [kenny@dellArch example]$ ./define.out 
;; 21

(make-test 'fact
           '(define (fact n)
              (if (= n 0)
                  1
                  (* n (fact (- n 1)))))
           '(display (fact 4)))
;; [kenny@dellArch example]$ ./fact2.out 
;; 24

(make-test 'map1
           '(define (map1 f lst)
              (if (null? lst)
                  '()
                  (let ((head (f (car lst)))) ;force evaluation order
                    (cons head (map1 f (cdr lst))))))
           '(map1 display '(1.3 2 hi "hello")))
;; [kenny@dellArch example]$ ./map1.out 
;; 1.300000
;; 2
;; hi
;; hello

;;sicp 5.5.6
(make-test 'lexical
           '((let ((x 3) (y 4))
                (lambda (a b c d e)
                  (let ((y (* a b x)) (z (+ c d x)))
                    (* x y z))))
             1 2 3 4 5))
;;should be 180

(make-test 'closure
           '(define ((adder x) y) (+ x y)) ;only y gets collected after the second call
           '((adder 3) 2))

(make-test 'nested-lambdas
           '(define (foo x y)
              (if (= x 0)
                  (+ x y)               ;x should get collected, so the order is (y x)
                  (lambda (z) (+ y z)))) ;z and parent should get collected
           '(foo 0 2)
           '((foo 3 4) 5))

(make-test 'open-coded
           '(define (linear + * a b x y)
              (+ (* a x) (* b y)))
           '(display (linear * + 2 3 4 5)))
;; should be 48
