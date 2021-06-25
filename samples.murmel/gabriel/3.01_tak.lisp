;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 82


(defun tak (x y z)
 (if (not (< y x))
       z
  (tak (tak (1- x) y z)
       (tak (1- y) z x)
       (tak (1- z) x y))))

(write (tak 18 12 6))  ; => 7