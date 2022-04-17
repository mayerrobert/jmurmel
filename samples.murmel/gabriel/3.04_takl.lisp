;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 105


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
  (if y (if (null x) t
          (shorterp (cdr x)
                    (cdr y)))))

(write (mas 18l 12l 6l))  ; => (7 6 5 4 3 2 1)