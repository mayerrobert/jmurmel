;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 93

#+murmel (require "mlib")
#+murmel (require "bench" "bench.lisp")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


(define *x* nil)
(define *y* nil)
(define *z* nil)

#+murmel
(defun stak (*x* *y* *z*)
  (let* dynamic ((*x* *x*) (*y* *y*) (*z* *z*))
    (stak-aux)))

#-murmel
(defun stak (*x* *y* *z*)
    (stak-aux))

(defun stak-aux ()
  (if (not (< *y* *x*))
        *z*
    (let #+murmel dynamic
      ((*x* (let #+murmel dynamic ((*x* (1- *x*))
                (*y* *y*)
                (*z* *z*))
            (stak-aux)))
       (*y* (let #+murmel dynamic ((*x* (1- *y*))
                (*y* *z*)
                (*z* *x*))
            (stak-aux)))
       (*z* (let #+murmel dynamic ((*x* (1- *z*))
                (*y* *x*)
                (*z* *y*))
            (stak-aux))))
      (stak-aux))))

(bench "stak" (stak 18 12 6) 1.2)  ; => 7