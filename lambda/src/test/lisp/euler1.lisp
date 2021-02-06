;;; https://projecteuler.net/problem=1
;;; 
;;; Multiples of 3 and 5
;;; Problem 1
;;; 
;;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
;;; The sum of these multiples is 23.
;;; 
;;; Find the sum of all the multiples of 3 or 5 below 1000.

; result: 233168.0
; output: (empty)
; correct according to euler

;(trace sum)

; when adding multiples of 5 we dont want to add numbers that are also multiples of 3
; return "add" if incr == 3 or if add is not a multiple of 3
(defun maybeadd (add incr)
  (if (= incr 3)
        add
    (if (= 0 (mod add 3))
          0
      add)))

(defun sum (acc add incr max)
  (if (>= add max)
        acc
    (sum (+ acc (maybeadd add incr)) (+ add incr) incr max)))

(defun euler-1 (max)
  (+ (sum 0 0 3 max) (sum 0 0 5 max)))



; alternative: lots more modulo operations
(defun sum2 (n acc max)
  (if (>= n max)
        acc
    (let ((x (if (= 0 (mod n 3))
                   n 
               (if (= 0 (mod n 5)) n 0))))
      (sum2 (+ n 1) (+ acc x) max))))

(defun euler-12 (max) (sum2 0 0 max))



(defun sum3 (acc add incr max)
  (if (>= add max)
        acc
    (sum3 (+ acc add) (+ add incr) incr max)))

(defun euler-13 (max)
  (+ (sum3 0 0 3 max) (sum3 0 0 5 max) (- (sum3 0 0 15 max))))



(euler-1  1000)
(euler-12 1000)
(euler-13 1000)
