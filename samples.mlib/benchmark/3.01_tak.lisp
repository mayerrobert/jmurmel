;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 82

#+murmel (require "mlib")
#+murmel (require "bench" "bench.lisp")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


(defun tak (x y z)
 (if (not (< y x))
       z
  (tak (tak (1- x) y z)
       (tak (1- y) z x)
       (tak (1- z) x y))))

(bench "tak" (tak 18 12 6) 0.32)  ; => 7