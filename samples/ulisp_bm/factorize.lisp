;;; from http://www.ulisp.com/show?1EO1

(defun zerop (n) (= 0 n))

; Factor
; This function takes a simple approach to finding the least prime factor of a number:
(defun factor (n)
  (let loop ((d 2) (i 1))
    (if (> (* d d) n) n
      (if (zerop (mod n d)) d
        (loop (+ d i) 2)))))


; To find the least prime factor of 2146654199 (46327 x 46337):
(factor 2146654199) ; ==> 46327.0


; Factorize
; You can use the above factor function as the basis for a simple recursive routine
; to factorize a number into a list of its prime factors:
(defun factorize (n)
  (let ((f (factor n)))
    (if (= n f) (list n) (cons f (factorize (/ n f))))))

; For example:
(factorize 731731731) ; ==> (3 17 43 333667)