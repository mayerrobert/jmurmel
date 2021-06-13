;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 93


(define x nil)
(define y nil)
(define z nil)

(defun stak (x y z)
  (let* dynamic ((x x) (y y) (z z))
    (stak-aux)))

(defun stak-aux ()
  ;(write x) (write y) (writeln z)
  (if (not (< y x))
        z
    (let ((oldx x) (oldy y) (oldz z))
      (let* dynamic
        ((x (let* dynamic ((x (1- x))
                          (y y)
                          (z z))
             (stak-aux)))
         (y (let* dynamic ((x (1- y))
                          (y z)
                          (z oldx))
             (stak-aux)))
         (z (let* dynamic ((x (1- z))
                          (y oldx)
                          (z oldy))
             (stak-aux))))
        (stak-aux)))))

;(write (stak 3 2 1)) ; ==> 2
(write (stak 18 12 6)) ; ==> 7