;;; Homegrown sample for Murmel's dovector

#+murmel (require "mlib")
#+murmel (require "bench" "bench")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench"))


(defun mk-vector (n)
  (let ((vec (make-array n)))
    (dotimes (i (length vec) vec)
      (setf (svref vec i) i))))

(defun sum (vec)
  (let ((result 0))
    (#+sbcl sb-int:dovector
     #+murmel dovector (x vec result)
       (incf result x))))

(defun run-sum ()
  (let ((v (mk-vector 10000)))
    (sum v)
    (sum v)
    (sum v)
    (sum v)
    (sum v)))

(bench "sum (5x)" (run-sum) 0.309) ; ==> 4999500
