;;; Run all benchmarks from a single lisp file

#+murmel (progn

(require "mlib")

(defmacro eval-when (sit . body)
  `(progn ,@body))

(defun lisp-implementation-type ()
  ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "getEngineName") ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "new"))))

(defun lisp-implementation-version ()
  ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "getLanguageVersion") ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "new"))))

)


(princ (lisp-implementation-type))
(princ #\ )
(princ (lisp-implementation-version))
(terpri)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "00_tak_apo.lisp")
  (load "3.01_tak.lisp")
  (load "3.02_stak.lisp")
  (load "3.03_ctak.lisp")
  (load "3.04_takL.lisp")
  (load "3.08_destru.lisp")
  (load "3.10_deriv.lisp")
  (load "array1.lisp")
  (load "dovector.lisp")
  (load "q.lisp")
)


(princ "Sum of weighted avg: ") (princ *total*) (terpri)

;; reference sum of weighted avg.
;; The weights were adjusted so that each benchmark has approx. 1.0 as a weighted result
;; with SBCL 2.1.8 on my i5-1135G7 @ 2.40GHz with turbo disabled.
;;
;; You may want to run all benchmarks with your "reference system", use the total of that run as *ref*,
;; and then run the "system under test".
;;
;; Or leave *ref* as is and adjust the individual weights so that each benchmark when run on the "reference system"
;; will give an adjusted weight of approx. 1.0.
(define *ref* 12.0)

(let* ((change (/ (- *total* *ref*) *ref*)))
  (princ "Change to reference result of ") (princ *ref*) (princ ": ")
  (princ (* 100.0 change)) (princ "%") (terpri)
  (princ "Run took ") (princ (/ *total* *ref*)) (princ "x as long as the reference result") (terpri))
