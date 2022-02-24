;;;; Simple benchmark utility.
;;;
;;; The macro `bench` will run a form for a given number of seconds
;;; and then print timing results. Benchmark overhead is not accounted for
;;; and will skew the results, so the results should be taken with a grain of salt.
;;;
;;; Timing results are based on wall time only.

;;; run the benchmark this many seconds
(#+murmel define
 #-murmel defparameter
  *default-duration* 5)

;;; if > 0 then run the form this many seconds before the timed benchmark
(#+murmel define
 #-murmel defparameter
  *warmup-duration* 5)


;;; Macro: bench
;;;     (bench name form seconds
;;;
;;; Repeatedly run `form` for approx. `seconds` seconds and print timing results.

(defmacro bench (name form seconds)
  `(do-bench ,name (lambda () ,form) ,seconds))


(defun do-run (f counter endtime)
  (#-murmel funcall f)
  (if (< (get-internal-real-time) endtime)
        (do-run f (1+ counter) endtime)
    counter))

(defun do-bench (name f seconds)
  (format t
          #+murmel "%s result: %s%n"
          #-murmel "~A result: ~A~%"
          name (#-murmel funcall f))

  (if (> *warmup-duration* 0)
    (do-run f 1 (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))

  (let* ((start (get-internal-real-time))
         (endtime (+ start (* seconds internal-time-units-per-second)))
         (count (do-run f 1 endtime))
         (end (get-internal-real-time))
         (elapsed-seconds (/ (- end start) internal-time-units-per-second))
         (iterations-per-second (/ count elapsed-seconds)))
    (format t
            #+murmel "%n%n%s: did %d iterations in %g seconds walltime, %g iterations / second%n"
            #-murmel "~%~%~A: did ~D iterations in ~F seconds walltime, ~F iterations / second~%"
            name count elapsed-seconds iterations-per-second)
    iterations-per-second))
