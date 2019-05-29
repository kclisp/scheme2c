(declare (usual-integrations))

(define (time thunk)
  (let ((start (real-time-clock)))
    (display (thunk))
    (let ((end (real-time-clock)))
      (format #t "time elapsed: ~a" (internal-time/ticks->seconds (- end start))))))

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
;;.7 seconds, 6
;;2.7 seconds, 7

;;c code takes 110 milliseconds


;;pitifully slow.
