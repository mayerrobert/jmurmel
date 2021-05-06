;;; Run Ackermann function for 10 minutes printing current speed
;;; Murmel version. See also ack_cl.lisp
;;;
;;; Run with
;;;   C:\> jm samples\ackermann_bm\ack_murmel.lisp
;;; or compile and run with
;;;   C:\> jm --jar samples\ackermann_bm\ack_murmel.lisp
;;;   C:\> java -jar a.jar

(defun Z (f)
  ((lambda (g)
     (g g))
   (lambda (g)
     (f (lambda args (apply (g g) args))))))

(defun ackermann (x y)
  ((Z (lambda (ackermann)
        (lambda (m n)
          (cond
            ((= m 0) (+ n 1))
            ((= n 0) (ackermann (- m 1) 1))
            (t (ackermann (- m 1) (ackermann m (- n 1))))))))
   x
   y))

(defun rate (n nanos)
  (/ n (/ nanos internal-time-units-per-second)))
  
(defun run (n tstart tend)
    (format t "%4.0f: %3.3g, %g ops/s%n" (floor n) (ackermann 3 6) (rate n (- (get-internal-real-time) tstart)))
    (if (< (get-internal-real-time) tend)
          (apply run (list (+ n 1) tstart tend))))

(defun time^ (msg now)
    (format t "%s: %d:%d:%d%n" msg (car(cdr(cdr now))) (car(cdr now)) (car now)))



; warmup the JVM
(run 1 (get-internal-real-time) (+ (get-internal-real-time) (* 10 internal-time-units-per-second)))

(time^ "starting" (get-decoded-time))
(run 1 (get-internal-real-time) (+ (get-internal-real-time) (* 600 internal-time-units-per-second)))
(time^ "done" (get-decoded-time))
