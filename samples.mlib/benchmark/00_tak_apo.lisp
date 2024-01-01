;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 24

#+murmel (require "mlib")
#+murmel (require "bench" "bench")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench"))


(defun  tak´  (x  y  z)
  (cond  ((not  (<  y  x))  z)
         (t  (tak´  (tak´  (1-  x)  y  z)
                    (tak´  (1-  y)  z  x)
                    (tak´  (1-  z)  x  y)))))

(bench "tak´" (tak´ 18 12 6) 0.325) ; result: 7