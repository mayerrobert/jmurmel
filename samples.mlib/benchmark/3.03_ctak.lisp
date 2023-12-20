;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 99

#+murmel (require "mlib")
#+murmel (require "bench" "bench.lisp")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


(defun ctak (x y z)
  (catch 'ctak (ctak-aux x y z)))

(defun ctak-aux (x y z)
  (cond ((not (< y x))
         (throw 'ctak z))
        (t (ctak-aux
             (catch 'ctak
               (ctak-aux (1- x)
                         y
                         z))
             (catch 'ctak
               (ctak-aux (1- y)
                         z
                         x))
             (catch 'ctak
               (ctak-aux (1- z)
                         x
                         y))))))

(bench "ctak" (ctak 18 12 6) *default-duration*)  ; => 7