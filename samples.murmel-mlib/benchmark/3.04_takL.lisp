;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 105

#+murmel (require "mlib")
#+murmel (require "bench" "bench")
#-murmel
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench"))


(defun listn (n)
  (if (not (= 0 n))
        (cons n (listn (1- n)))))

(define 18l (listn 18)) ;note that these are potential numbers
(define 12l (listn 12))
(define  6l (listn 6))

(defun mas (x y z)
  (if (not (shorterp y x))
        z
    (mas (mas (cdr x)
              y z)
         (mas (cdr y)
              z x)
         (mas (cdr z)
              x y))))

(defun shorterp (x y)
  (and y (or (null x)
             (shorterp (cdr x)
                       (cdr y)))))

(bench "takL" (mas 18l 12l 6l) 1.825)  ; => (7 6 5 4 3 2 1)