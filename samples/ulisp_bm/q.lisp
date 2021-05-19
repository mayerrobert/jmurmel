;;; from http://www.ulisp.com/show?1EO1

(defmacro or args
   (if (null args)
         nil
     (if (null (cdr args))
           (car args)
       (let ((temp (gensym)))
         `(let ((,temp ,(car args)))
             (if ,temp
                   ,temp
               (or ,@(cdr args))))))))

(defun 1- (n) (- n 1))


; Hofstadter Q sequence
; From Douglas Hofstadter's book "Goedel, Escher, Bach: an Eternal Golden Braid":
(defun q (n)
  (if (<= n 2) 1
    (+
     (q (- n (q (- n 1))))
     (q (- n (q (- n 2)))))))

(q 21) ; ==> 12


; Two-dimensional recursive function Q2
(defun q2 (x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (1- x) y)) y)
       (q2 x (- y (q2 x (1- y)))))))

(q2 7 8) ; ==> 31 