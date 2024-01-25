;;; Run all benchmarks from a single lisp file
;;;
;;; See also the JMH benchmark lambdajbm/src/main/io/github/jmurmel/GabrielBenchmark.java

#+murmel (load "000_lib.lisp")

#-murmel
(defmacro define (sym &optional (val nil))
  `(defparameter ,sym ,val))


(load "00_tak_apo.lisp")
(load "3.01_tak.lisp")
(load "3.01_takeuchi.lisp")
#+murmel (load "3.02_stak.lisp")      #-murmel (load "3.02_stak_CL.lisp")
(load "3.04_takl.lisp")
#+murmel (load "3.10_deriv.lisp")     #-murmel (load "3.10_deriv_CL.lisp")
