;;; Based on https://github.com/ecraven/r7rs-benchmarks/blob/master/src/array1.scm which says it is based on
;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

#+murmel (require "mlib")
#+murmel (require "bench" "bench.lisp")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


(defun create-x (n)
  (let ((result (make-array n)))
    (do ((i 0 (1+ i)))
        ((>= i n) result)
      (setf (svref result i) i))))

(defun create-y (x)
  (let* ((n (length x))
         (result (make-array n)))
    (do ((i (1- n) (1- i)))
        ((< i 0) result)
      (setf (svref result i) (svref x i)))))

(defun my-try (n)
  (length (create-y (create-x n))))

(defun run-array1 (m n)
  (do ((i 0 (1+ i))
       (result nil (my-try n)))
      ((= i m) result)))

(bench "array1 10 10000" (run-array1 10 10000) 0.556) ; ==> 10000