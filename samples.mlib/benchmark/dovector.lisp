(require "bench" "bench.lisp")

#+murmel
(require "mlib")

(defun mk-vector (n)
  (let ((vec (make-array n)))
    (dotimes (i (length vec))
      (setf (svref vec i) i))
    vec))


(defun sum (vec)
  (let ((result 0))
    (#+sbcl sb-int:dovector #+murmel dovector (x vec) (incf result x))
    result))


(defun run ()
  (let ((v (mk-vector 10000)))
    (sum v)
    (sum v)
    (sum v)
    (sum v)
    (sum v)))


(bench "sum (5x)" (run) *default-duration*) ; ==> 4999500
