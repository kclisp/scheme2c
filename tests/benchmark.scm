(declare (usual-integrations))

(define (time thunk)
  (let ((start (real-time-clock)))
    (display (thunk))
    (let ((end (real-time-clock)))
      (format #t "time elapsed: ~a" (internal-time/ticks->seconds (- end start))))))
(define (memory thunk)
  (print-gc-statistics)
  (display (thunk))
  (print-gc-statistics))

(define (ack m n)
  (cond
   ((= m 0) (+ n 1))
   ((= n 0) (ack (- m 1) 1))
   (else (ack (- m 1) (ack m (- n 1))))))

;; (time (lambda () (ack 3 9)))
;;no optimizations, uncompiled
;; time elapsed: 16.945
;;compiled
;; time elapsed: .121
;;time elapsed: .087

(make-test 'benchmark
           ;; '(define (time thunk)
           ;;    (let ((start (clock)))
           ;;      (display (thunk))
           ;;      (let ((end (clock)))
           ;;        (display (- end start)))))
           '(define (ack m n)
              (cond
               ((= m 0) (+ n 1))
               ((= n 0) (ack (- m 1) 1))
               (else (ack (- m 1) (ack m (- n 1))))))
           ;; '(time (lambda () (ack 3 7)))
           '(let ((start (clock)))
              (display (ack 3 7))
              (let ((end (clock)))
                (display (- end start)))))
;;with debug
;;3.3 seconds for (ack 3 6)
;;13 seconds for (ack 3 7)

;;no debug
;;1.8 seconds, 6
;;7.3 seconds, 7

;;no debug, compile for speed
;;.6 seconds, 6
;;2.5 seconds, 7

;;(ack 3 7) uses about 2^24 words -- 16 million words -- 125MB

;; (memory (lambda () (ack 3 7)))
;;uncompiled scheme uses 2,800,000 words -- 22Mb
;;compiled scheme uses ~17,000 words -- 130Kb


;;pitifully slow.

;;AFTER:
;;inlined some functions, obarray is an array
;;no debug, compile for speed
;;.17, 6
;;.64, 7
;;after more:
;;currently 50 times slower than compiled scheme

;;c:
;;ack(3, 9) -- 25 ms
;;compiled scheme:
;;(ack 3 9) -- 86 ms
;;compiled to c:
;;a lot of memory

;;ack(3, 7) -- 2 ms
;;(ack 3 7) -- 10 ms
;;benchmark.out -- 540 ms

;;lexical address part 1
;;benchmark.out 274 ms

;;compile operands first
;;248 ms

;;primitive/compiled procedure data
;;225 ms

;;open-code primitive 2
;;202 ms

;;rest args slow it down by about 5%
