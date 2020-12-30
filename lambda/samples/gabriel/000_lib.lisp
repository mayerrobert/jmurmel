(defun not (e)
  (null e))

(defun zerop (n)
  (= n 0))


(defun 1- (n)
  (- n 1))

(defun 1+ (n)
  (+ n 1))


(defun mapcar (f l)
  (if l (cons (f (car l)) (mapcar f (cdr l)))
    nil))

