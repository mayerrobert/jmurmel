;;; Simple test for expand

(require "expand")

(defmacro def (name body)
  `(defun ,name () ,body))


(define form
        '(def f (+ 1 (-> 1 1+ 1+ (expt 2)) 2))) ; contains an embedded macro
(define expansion 
        '(defun f () (+ 1 (expt (1+ (1+ 1)) 2) 2)))

(unless (equal (expand form)
               expansion)
  (fatal "equal check failed")) 