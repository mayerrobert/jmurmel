;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 84


(defun takeuchi (x y z)
 (cond ((> x y)
        (takeuchi (takeuchi (1- x) y z)
                  (takeuchi (1- y) z x)
                  (takeuchi (1- z) x y) ))
       (t y)))

(write (takeuchi 18 12 6))  ; => 18