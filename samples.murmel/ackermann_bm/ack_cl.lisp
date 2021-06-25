;;; Run Ackermann function for 10 minutes printing current speed
;;; Common Lisp version. See also ack_murmel.lisp.
;;;
;;; Run with
;;;
;;;     C:\> abcl --load samples\ackermann_bm\ack_cl.lisp
;;;
;;; or within the abcl repl compile and run with
;;;
;;;     C:\> abcl
;;;     CL-USER(1): :cl samples\ackermann_bm\ack_cl.lisp
;;;
;;; or
;;;
;;;     C:\> sbcl --script samples\ackermann_bm\ack_cl.lisp

(defun Z (f)
  (funcall (lambda (g)
     (funcall g g))
   (lambda (g)
     (funcall f (lambda (&rest args) (apply (funcall g g) args))))))

(defun ackermann (x y)
  (funcall (Z (lambda (ackermann)
                (lambda (m n)
                  (cond
                    ((= m 0) (+ n 1))
                    ((= n 0) (funcall ackermann (- m 1) 1))
                    (t (funcall ackermann (- m 1) (funcall ackermann m (- n 1))))))))
           x
           y))

(defun rate (n nanos)
  (/ n (/ nanos internal-time-units-per-second)))
  
(defun run (n tstart tend)
    (format t "~D: ~3,0F, ~F ops/s~%" (floor n) (ackermann 3 6) (rate n (- (get-internal-real-time) tstart)))
    (if (< (get-internal-real-time) tend)
          (run (+ n 1) tstart tend)))

(defun time^ (msg now)
    (format t "~A: ~D:~D:~D~%" msg (car(cdr(cdr now))) (car(cdr now)) (car now)))



; warmup the JVM
(run 1 (get-internal-real-time) (+ (get-internal-real-time) (* 10 internal-time-units-per-second)))

(time^ "starting" (multiple-value-list (get-decoded-time)))
(run 1 (get-internal-real-time) (+ (get-internal-real-time) (* 600 internal-time-units-per-second)))
(time^ "done" (multiple-value-list (get-decoded-time)))
