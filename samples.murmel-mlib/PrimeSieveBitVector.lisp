;;;; Murmel solution for Dave Plummer's Software Drag Race,
;;;; loosely based on mikehw's Common Lisp solution
;;;
;;; run as:
;;;     java -jar jmurmel.jar --run PrimeSieve.lisp
;;; or
;;;     sbcl --script PrimeSieve.lisp
;;; or
;;;     abcl --batch --load PrimeSieve.lisp
;;;
;;; see https://github.com/PlummersSoftwareLLC/Primes
;;; see https://github.com/mayerrobert/jmurmel


#+murmel (require "mlib")
#-murmel (defmacro define (s v) `(defparameter ,s ,v))


(define *rawbits* ())
(define *sieve-size* 0)


(defun prime-sieve (limit)
  (setq *sieve-size*  limit)
  (setq *rawbits* (make-array (ceiling limit 2) #-murmel :element-type 'bit))

  (dotimes (i (length *rawbits*))
    (setf (bit *rawbits* i) 1)))


(defmacro get-bit (rawbits index)
  `(bit ,rawbits (floor ,index 2)))


(defmacro clear-bit (rawbits index)
  `(setf (bit ,rawbits (floor ,index 2)) 0))


(defun run-sieve ()
  (do* ((rawbits *rawbits*)
        (factor 3)
        (sieve-size *sieve-size*)
        (q (floor (sqrt sieve-size))))
       ((>= factor q))

    (do* ((num factor))
         ((or (>= num sieve-size)
              (= (get-bit rawbits num) 1))
          (setq factor num))
        (incf num 2))

    (do* ((num (* factor 3))
          (increment (* factor 2)))
         ((>= num sieve-size))
        #-murmel (declare (type fixnum num increment sieve-size factor)) ; approx 2.5x speedup for sbcl

        (clear-bit rawbits num)
        (incf num increment))

    (incf factor 2)))


(defun count-primes ()
  (reduce #'+ *rawbits*))


(define *passes* 0)
(define *start* 0)


;; two runs: warmup and timed run
(dotimes (i 2)
  (setq *passes* 0)
  (setq *start*  (get-internal-real-time))

  (do ((runtime (* internal-time-units-per-second 5)))
      ((> (- (get-internal-real-time) *start*) runtime))
    (prime-sieve 1000000)
    (run-sieve)
    (incf *passes*)))


;#|
(princ "2, ")
(do ((n 3))
    ((>= n 100))
  (when (= 1 (get-bit *rawbits* n))
    (write n) (princ ", "))
  (setq n (1+ (1+ n))))
(terpri)
;|#


(let* ((duration (/ (- (get-internal-real-time) *start*) internal-time-units-per-second))
       (avg (/ duration *passes*)))
  (format t "passes: ~d, count: ~d, avg: ~f~%" *passes* (count-primes) avg)
  (format t "bitvector;~d;~f;1;algorithm=base,faithful=no,bits=unknown~%" *passes* duration))
