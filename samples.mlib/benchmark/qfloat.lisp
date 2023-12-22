;;; from http://www.ulisp.com/show?1EO1 but with floating point
;;; Murmel does math as double by default so there's not much of a difference compared to q.lisp,
;;; for e.g. sbcl the difference is quite large especially if the type declarations were removed.

(declaim (optimize (speed 3)))

#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float))


#+murmel (require "mlib")
#+murmel (require "bench" "bench.lisp")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


; Hofstadter Q sequence
; From Douglas Hofstadter's book "Goedel, Escher, Bach: an Eternal Golden Braid":
(declaim (ftype (function (double-float) double-float) q-float))  ; ignored by JMurmel but helps sbcl
(defun q-float (n)
  (if (<= n 2) 1.0
    (+
     (q-float (- n (q-float (- n 1.0))))
     (q-float (- n (q-float (- n 2.0)))))))

(bench "q-float" (q-float 21.0) 1.06) ; ==> 12


; Two-dimensional recursive function Q2
(declaim (ftype (function (double-float double-float) double-float) q2-float))
(defun q2-float (x y)
  (if (or (< x 1.0) (< y 1.0)) 1.0
    (+ (q2-float (- x (q2-float (1- x) y)) y)
       (q2-float x (- y (q2-float x (1- y)))))))

(bench "q2-float" (q2-float 7.0 8.0) 0.978) ; ==> 31


; Same as Q2 but faster because Mlib's "or" is inefficient because it expands into a let
; (and/ or jmurmel's let is inefficient)
(declaim (ftype (function (double-float double-float) double-float) q2*-float))
(defun q2*-float (x y)
  (if (< x 1) 1.0
    (if (< y 1) 1.0
      (+ (q2*-float (- x (q2*-float (1- x) y)) y)
         (q2*-float x (- y (q2*-float x (1- y))))))))

(bench "q2*-float" (q2*-float 7.0 8.0) 0.978) ; ==> 31


#+ignore
(progn

(require :sb-sprof)

(sb-sprof:with-profiling (:mode :cpu :loop t :max-samples 1000 :report :flat :sample-interval 0.1)
  (loop repeat 10
        do (q2*-float 7.0 8.0)))

(disassemble 'q2*-float)

)
