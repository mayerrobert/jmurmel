;;; Some CL functions that are not available in Murmel
;;; Use like this:
;;; jm 000_lib.lisp 3.01_tak.lisp

(defun not (e)
  (null e))

(defun zerop (n)
  (= n 0))


(defun mapcar (f l)
  (if l (cons (f (car l)) (mapcar f (cdr l)))
    nil))

(defun  cadr (l) (car (cdr l)))
(defun caddr (l) (car (cdr (cdr l))))
