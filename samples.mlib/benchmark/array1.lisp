;;; Based on https://github.com/ecraven/r7rs-benchmarks/blob/master/src/array1.scm which says it is based on
;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

(require "bench" "bench.lisp")

#+murmel
(require "mlib")


#+murmel
(defun vector-length (v)
  (svlength v))

#-murmel
(defun vector-length (v)
  (length v))


(defun create-x (n)
  (let ((result (make-array n)))
    (do ((i 0 (1+ i)))
        ((>= i n) result)
      (setf (svref result i) i))))

(defun create-y (x)
  (let* ((n (vector-length x))
         (result (make-array n)))
    (do ((i (1- n) (1- i)))
        ((< i 0) result)
      (setf (svref result i) (svref x i)))))

(defun my-try (n)
  (vector-length (create-y (create-x n))))

(defun run (m n)
  (do ((i 0 (1+ i))
       (result nil (my-try n)))
      ((= i m) result)))



(bench "array1 10 10000" (run 10 10000) *default-duration*) ; ==> 10000