;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 93


(define x)
(define y)
(define z)

(defun stak (x y z)
  (let* dynamic ((x x) (y y) (z z))
    (stak-aux)))

(defun stak-aux ()
  (if (not (< y x))
        z
    (let dynamic
      ((x (let dynamic ((x (1- x))
                (y y)
                (z z))
            (stak-aux)))
       (y (let dynamic ((x (1- y))
                (y z)
                (z x))
            (stak-aux)))
       (z (let dynamic ((x (1- z))
                (y x)
                (z y))
            (stak-aux))))
      (stak-aux))))

(stak 18 12 6) ; ==> 7