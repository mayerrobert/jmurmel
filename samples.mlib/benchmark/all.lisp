;;; Run all benchmarks from a single lisp file

#+murmel (progn

(require "mlib")

(defmacro eval-when (situations . body)
  `(progn ,@body))

; enable the two following functions to run all.lisp with JMurmel version < 1.4.5
#|
(defun lisp-implementation-type ()
  "JMurmel")

(defun lisp-implementation-version ()
  ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "getLanguageVersion") ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "new"))))
|#

)


(princ (lisp-implementation-type))
(princ #\ )
(princ (lisp-implementation-version))
(terpri)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))

(setq *cooldown-duration* 5)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "00_tak_apo.lisp")
  (load "3.01_tak.lisp")
  (load "3.02_stak.lisp")
  (load "3.03_ctak.lisp")
  (load "3.04_takL.lisp")
  (load "3.08_destru.lisp")
  (load "3.10_deriv.lisp")
  (load "array1.lisp")
   ;; dovector.lisp uses "dovector" or SBCL's sb-int:dovector, both of which are not Common Lisp.
  #+(or sbcl murmel)
  (load "dovector.lisp")
  (load "q.lisp")
  (load "qfloat.lisp")
)

;; reference sum of weighted avg.
;; The individual weights of each benchmark were adjusted
;; so that each benchmark has approx. 1.0 as a weighted result
;; with Windows 10/ SBCL 2.1.8 on my i5-1135G7 @ 2.40GHz with turbo disabled.
;;
;; You may want to run all benchmarks with your "reference system",
;; use the total of that run as *ref*,
;; and then run the "system under test".
;;
;; Or leave *ref* as is and adjust the individual weights
;; so that each benchmark when run on the "reference system"
;; will give an adjusted weight of approx. 1.0.
(define *ref* #+(or sbcl murmel) 15.0
              #-(or sbcl murmel) 14.0)


(terpri)
(princ "Sum of weighted avg: ") (princ *total*) (terpri)

(let* ((change (/ (- *total* *ref*) *ref*)))
  (princ "Relative change to reference result of ") (princ *ref*) (princ ": ")
  (princ (* 100.0 change)) (princ " %")
  (terpri)
  (princ "This run took ") (princ (/ *total* *ref*)) (princ " times as long as the reference result")
  (terpri))
