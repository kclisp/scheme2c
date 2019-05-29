;;Tests
(define (make-test name . exps)
  (compile-to-file (format #f "example/~a.c" name) (cons 'begin exps)))

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

(make-test 'display '(display 20))
;; [kenny@dellArch example]$ ./display.out 
;; 20

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

(make-test 'fact1
           '(define (fact n)
              (if (= n 0)
                  1
                  (* n (fact (- n 1))))))
;; [kenny@dellArch example]$ ./fact1.out 
;; [kenny@dellArch example]$ echo $?
;; 9
;;similarly, cons index

(make-test 'fact2
           '(define (fact n)
              (if (= n 0)
                  1
                  (* n (fact (- n 1)))))
           '(display (fact 4)))
;; [kenny@dellArch example]$ ./fact2.out 
;; 24

(make-test 'map1
           '(define (map1 f list)
              (if (null? list)
                  '()
                  (let ((head (f (car list)))) ;force evaluation order
                    (cons head (map1 f (cdr list))))))
           '(map1 display '(1.3 2 hi "hello")))
;; [kenny@dellArch example]$ ./map1.out 
;; 1.300000
;; 2
;; hi
;; hello
