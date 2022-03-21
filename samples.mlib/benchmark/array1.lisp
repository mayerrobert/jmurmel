;;; Based on https://github.com/ecraven/r7rs-benchmarks/blob/master/src/array1.scm which says it is based on
;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.
;;; i5-1135G7:
;;; Murmel interpreted:  16 iterations in 5,31676  seconds walltime,   3,00935 iterations/second, 332,297     milliseconds/iteration
;;; Murmel compiled:    137 iterations in 5,03487  seconds walltime,  27,2103  iterations/second,  36,7508    milliseconds/iteration
;;; SBCL:              3741 iterations in 5.000085 seconds walltime, 748.18726 iterations/second,   1.3365637 milliseconds/iteration
(load "bench.lisp")

#+murmel (progn
(require "mlib")

(define m%long->int    (:: "java.lang.Number"  "intValue"))
(define m%number->int  (:: "java.lang.Long"    "intValue"))

(define m%int->long    (:: "java.lang.Integer" "longValue"))
(define m%number->long (:: "java.lang.Number"  "longValue"))

(define m%svref (:: "java.util.ArrayList" "get"    "int"))
(define m%svset (:: "java.util.ArrayList" "set"    "int" "java.lang.Object"))
(define m%svlen (:: "java.util.ArrayList" "size"))
(define m%svadd (:: "java.util.ArrayList" "add"    "java.lang.Object"))
(define m%svrem (:: "java.util.ArrayList" "remove" "int"))
(define m%svmak (:: "java.util.ArrayList" "new"))

(defun make-vector (size)
  (let ((v (m%svmak)))
    (let loop ((n size))
      (if (zerop n) v
        (progn (m%svadd v nil)
               (loop (1- n)))))))

(defun vector-set! (v idx elem)
  (m%svset v (m%long->int idx) elem))

(defun vector-ref (v idx)
  (m%svref v (m%long->int idx)))

(defun vector-length (v)
  (m%int->long (m%svlen v)))
)

#-murmel (progn
(defun make-vector (size)
  (make-array size))

(defun vector-set! (v idx elem)
  (setf (svref v idx) elem))

(defun vector-ref (v idx)
  (svref v idx))

(defun vector-length (v)
  (length v))
)



(defun create-x (n)
  (let ((result (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((>= i n) result)
      (vector-set! result i i))))

(defun create-y (x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(defun my-try (n)
  (vector-length (create-y (create-x n))))

(defun run (m n)
  (do ((i 0 (1+ i))
       (result nil (my-try n)))
      ((= i m) result)))



;(run 10 10000)

(bench "array1 10 10000" (run 10 10000) *default-duration*) ; ==> 10000