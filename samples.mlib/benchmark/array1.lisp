;;; Based on https://github.com/ecraven/r7rs-benchmarks/blob/master/src/array1.scm which says it is based on
;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.
;;; i5-1135G7:
;;; Murmel interpreted:  16 iterations in 5,31676  seconds walltime,   3,00935 iterations/second, 332,297     milliseconds/iteration
;;; Murmel compiled:    137 iterations in 5,03487  seconds walltime,  27,2103  iterations/second,  36,7508    milliseconds/iteration
;;; SBCL:              3741 iterations in 5.000085 seconds walltime, 748.18726 iterations/second,   1.3365637 milliseconds/iteration
(load "bench.lisp")

#+murmel (progn
(require "mlib")


(defmacro m%svref (obj idx)
  `((jmethod "java.util.ArrayList" "get" "int") ,obj ,idx))

(defmacro m%svset (obj idx val)
  `((jmethod "java.util.ArrayList" "set" "int" "java.lang.Object") ,obj ,idx ,val))

(defmacro m%svlen (obj)
  `((jmethod "java.util.ArrayList" "size") ,obj))

(defmacro m%svadd (obj val)
  `((jmethod "java.util.ArrayList" "add" "java.lang.Object") ,obj ,val))

(defmacro m%svmak-capacity (capacity)
  `((jmethod "java.util.ArrayList" "new" "int") ,capacity))


(defun make-vector (size)
  (let ((v (m%svmak-capacity size)))
    (let loop ((n size))
      (if (= 0 n) v
        (progn (m%svadd v nil)
               (loop (1- n)))))))


(defun vector-set! (v idx elem)
  (m%svset v idx elem))

(defmacro vector-set! (v idx elem)
  `(m%svset ,v ,idx ,elem))


(defun vector-ref (v idx)
  (m%svref v idx))

(defmacro vector-ref (v idx)
  `(m%svref ,v ,idx))


(defun vector-length (v)
  (m%svlen v))

;(defmacro vector-length (v)
;  `(m%int->long (m%svlen ,v)))
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
    (do ((i 0 (1+ i)))
        ((>= i n) result)
      (vector-set! result i i))))

(defun create-y (x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (1- n) (1- i)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(defun my-try (n)
  (vector-length (create-y (create-x n))))

(defun run (m n)
  (do ((i 0 (1+ i))
       (result nil (my-try n)))
      ((= i m) result)))



(bench "array1 10 10000" (run 10 10000) *default-duration*) ; ==> 10000