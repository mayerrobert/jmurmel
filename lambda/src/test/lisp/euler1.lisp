;;; https://projecteuler.net/problem=1
;;; 
;;; Multiples of 3 and 5
;;; Problem 1
;;; 
;;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;; 
;;; Find the sum of all the multiples of 3 or 5 below 1000.

; result: nil
; output: The sum of all the multiples of 3 or 5 below 1000 is 266333.\n

;(trace sum)

(defun sum (acc add incr max)
  (if (>= add max)
        acc
    (sum (+ acc add) (+ add incr) incr max)))

(defun euler-1 (max)
  (+ (sum 0 0 3 max) (sum 0 0 5 max)))

(format t "The sum of all the multiples of 3 or 5 below 1000 is %g.%n" (euler-1 1000))