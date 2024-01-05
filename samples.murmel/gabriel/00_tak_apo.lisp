;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 24


(defun  tak´  (x  y  z)
  (cond  ((not  (<  y  x))  z)
         (t  (tak´  (tak´  (1-  x)  y  z)
                    (tak´  (1-  y)  z  x)
                    (tak´  (1-  z)  x  y)))))

(tak´ 18 12 6)