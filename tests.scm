;;Tests
(define (make-test name exp)
  (compile-to-file (format #f "example/~a.c" name) exp))

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
