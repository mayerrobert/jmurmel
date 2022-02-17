;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 93


(defvar x)
(defvar y)
(defvar z)

(defun stak (x y z)
  (stak-aux))

(defun stak-aux ()
  (if (not (< y x))
        z
    (let
      ((x (let ((x (1- x))
                (y y)
                (z z))
            (stak-aux)))
       (y (let ((x (1- y))
                (y z)
                (z x))
            (stak-aux)))
       (z (let ((x (1- z))
                (y x)
                (z y))
            (stak-aux))))
      (stak-aux))))

(write (stak 18 12 6)) ; ==> 7