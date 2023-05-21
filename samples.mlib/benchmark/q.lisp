;;; from http://www.ulisp.com/show?1EO1

#+murmel (require "mlib")
(require "bench" "bench.lisp")


; Hofstadter Q sequence
; From Douglas Hofstadter's book "Goedel, Escher, Bach: an Eternal Golden Braid":
(defun q (n)
  (if (<= n 2) 1
    (+
     (q (- n (q (- n 1))))
     (q (- n (q (- n 2)))))))

(bench "q" (q 21) *default-duration*) ; ==> 12


; Two-dimensional recursive function Q2
(defun q2 (x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (1- x) y)) y)
       (q2 x (- y (q2 x (1- y)))))))

(bench "q2" (q2 7 8) *default-duration*) ; ==> 31


; Same as Q2 but faster because Mlib's "or" is inefficient because it expands into a let
; (and/ or jmurmel's let is inefficient)
(defun q2* (x y)
  (if (< x 1) 1
    (if (< y 1) 1
      (+ (q2* (- x (q2* (1- x) y)) y)
         (q2* x (- y (q2* x (1- y))))))))

(bench "q2*" (q2* 7 8) *default-duration*) ; ==> 31 