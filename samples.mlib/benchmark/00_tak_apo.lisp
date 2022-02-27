;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 24

#+murmel (require "mlib")
(load "bench.lisp")


(defun  tak´  (x  y  z)
  (cond  ((null  (<  y  x))  z) ; original had "not", null is faster in jmurmel
         (t  (tak´  (tak´  (1-  x)  y  z)
                    (tak´  (1-  y)  z  x)
                    (tak´  (1-  z)  x  y)))))

(bench "tak´" (tak´ 18 12 6) *default-duration*) ; result: 7