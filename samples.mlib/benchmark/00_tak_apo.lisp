;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 24

#+murmel (require "mlib")
(require "bench" "bench.lisp")


(defun  tak´  (x  y  z)
  (cond  ((not  (<  y  x))  z)
         (t  (tak´  (tak´  (1-  x)  y  z)
                    (tak´  (1-  y)  z  x)
                    (tak´  (1-  z)  x  y)))))

(bench "tak´" (tak´ 18 12 6) *default-duration*) ; result: 7