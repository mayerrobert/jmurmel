;;; Based on https://github.com/ecraven/r7rs-benchmarks/blob/master/src/array1.scm which says it is based on
;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.
;;; i5-1135G7:
;;; Murmel interpreted (w/o macros):  11 iterations in 5,33380  seconds walltime,   2,06232 iterations/second, 484,891     milliseconds/iteration
;;; Murmel compiled (w/o macros):    461 iterations in 5,00629  seconds walltime,  92,0841  iterations/second,  10,8596    milliseconds/iteration
;;; Murmel compiled (with macros):   662 iterations in 5,00115  seconds walltime, 132,370   iterations/second,   7,55460   milliseconds/iteration
;;; SBCL:                           3741 iterations in 5.000085 seconds walltime, 748.18726 iterations/second,   1.3365637 milliseconds/iteration
(load "bench.lisp")

#+murmel (progn
(require "mlib")


(define m%svref
  (jmethod "java.util.ArrayList" "get" "int"))

(define m%svset
  (jmethod "java.util.ArrayList" "set" "int" "java.lang.Object"))

(define m%svlen
  (jmethod "java.util.ArrayList" "size"))

(define m%svadd
  (jmethod "java.util.ArrayList" "add" "java.lang.Object"))

(define m%svmak-capacity
  (jmethod "java.util.ArrayList" "new" "int"))


;#+(or)  ; compiled Murmel is faster with macros enabled because jmethod+call will be opencoded, interpreted Murmel is faster with macros disabled
(progn
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
)


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