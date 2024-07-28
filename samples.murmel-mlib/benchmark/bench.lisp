;;;; Simple benchmark utility.
;;;
;;; The macro `bench` will run a form for a given number of seconds
;;; and then print timing results. Benchmark overhead is not accounted for
;;; and will skew the results, so the results should be taken with a grain of salt.
;;;
;;; Timing results are based on wall time only.

#-murmel (defmacro define (n v) `(defparameter ,n ,v))

#+murmel
(defmacro compiletime-too forms
  `(progn ,@forms))

#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro compiletime-too (&body forms)
    `(eval-when (:compile-toplevel :load-toplevel :execute) ,@forms)))


;;; run benchmark for this many seconds
(compiletime-too (define *default-duration* 5))

;;; if > 0 then run the form this many seconds before the timed benchmark
;;; 0 for sbcl because sbcl doesn't need warmup
(define *warmup-duration*
        #+(or jvm abcl murmel) 5
        #-(or jvm abcl murmel) 0)

;;; if non-nil then sleep this many seconds before each timed run
;;; in order to let the CPU cool down
(define *cooldown-duration* nil)


;;; run the form under test this many times inside a progn to reduce benchmark overhead
(defmacro count-per-try () #+sbcl 300 #-sbcl 30)


;;; Macro: bench
;;;     (bench name form seconds
;;;
;;; Repeatedly run `form` for approx. `seconds` seconds and print timing results.
(defmacro bench (name form seconds)
  `(do-bench ,name (lambda () ,form) ,seconds))



(define *min* nil)
(define *max* nil)
(define *total* 0.0)


(defun adjust-minmax (duration)
  (if (> *min* duration) (setq *min* duration))
  (if (< *max* duration) (setq *max* duration)))

(define *count-per-try* (count-per-try))

(compiletime-too
(defun do-unroll (expr times)
  (if (<= times 0) nil
    (cons expr (do-unroll expr (1- times)))))

(defmacro unroll (expr)
  `(progn ,@(do-unroll expr (count-per-try))))
)

(defun do-run (f counter endtime tmp)
  (setq tmp (get-internal-real-time))
  (unroll (#-murmel funcall f))
  (adjust-minmax (- (get-internal-real-time) tmp))
  (if (< (get-internal-real-time) endtime)
        (do-run f (1+ counter) endtime nil)
    counter))

(defun do-bench (name f ref)
  (setq *min* 1e30 *max* 0)

  (format t "~A result: ~A~%" name (#-murmel funcall f))

  (if (> *warmup-duration* 0)
    (progn
      (do-run f 1 (+ (get-internal-real-time) (* *warmup-duration* internal-time-units-per-second)) nil)
      (princ "warmup done.")
      #+murmel (writeln)
      #-murmel (terpri)))

  #+sbcl (gc :full t)
  (if *cooldown-duration* (sleep *cooldown-duration*))

  (let* ((seconds *default-duration*)
         (internal-per-ms (/ internal-time-units-per-second 1000))
         (start (get-internal-real-time))
         (endtime (+ start (* seconds internal-time-units-per-second)))
         (count (do-run f 1 endtime nil))
         (end (get-internal-real-time))
         (count (truncate (* *count-per-try* count)))
         (elapsed-seconds (/ (- end start) internal-time-units-per-second))
         (iterations-per-second (/ count elapsed-seconds))
         (seconds-per-iteration (/ elapsed-seconds count))
         (weighted-avg (/ seconds-per-iteration ref 0.001)))
    (format t
            "~A: did ~D iterations in ~F seconds walltime, ~F iterations/second, avg/min/max ~F/~F/~F milliseconds/iteration~%"
            name count elapsed-seconds iterations-per-second (* 1000 seconds-per-iteration) (/ *min* internal-per-ms *count-per-try*) (/ *max* internal-per-ms *count-per-try*))
    (princ "---> weighted avg: ") (princ weighted-avg)
    (terpri)
    (incf *total* weighted-avg)
    nil))

(provide "bench")
