;;;; from https://github.com/WillSewell/gc-latency-experiment/

#+murmel (require "mlib")

(declaim (optimize (speed 3) (safety 0)))

#-murmel
(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defmacro measure #+murmel body #-murmel (&body body)
  ;"Eval body and returns two values: the time it took in seconds as a rational and the result"
  (with-gensyms (start-sym result-sym elapsed-sym)
    `(let* ((,start-sym (get-internal-real-time))
            (,result-sym (progn ,@body))
            (,elapsed-sym (/ (- (get-internal-real-time) ,start-sym)
                             internal-time-units-per-second)))
       (values ,elapsed-sym ,result-sym))))


;(defconstant +winSize+ 200000)
(defmacro +winSize+ ()  20000)

;(defconstant +msgCount+ 1000000)
(defmacro +msgCount+ () 1000000)

;(defconstant +msgSize+ 1024)
(defmacro +msgSize+ () 1024)

(#+murmel define #-murmel defvar *worst* 0)

(defun make-message (n)
#+sbcl
  (make-array (+msgSize+) :element-type '(unsigned-byte 8)
                          :initial-element (mod n 256))
#+murmel
  (vector-fill (make-array (+msgSize+)) (mod n 256))
)

(defun push-message (store id)
  (let ((elapsed
         (measure
           (setf (svref store (mod id (+winSize+))) (make-message id)))))
    (if (> elapsed *worst*)
        (setf *worst* elapsed))))

(time
(let ((store (make-array (+winSize+))))
  (dotimes (i (+msgCount+)) (push-message store i))
  #+murmel (format t "Worst push time: %.3f ms%n" (* *worst* 1e3))
  #-murmel (format t "Worst push time: ~,3F ms~%" (* *worst* 1e3))
)
)
