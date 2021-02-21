;;; See https://en.wikipedia.org/wiki/Gosper_curve
;;; and https://www.reddit.com/r/lisp/comments/ldgjbv/gosper_curve/

(defun 1- (n) (- n 1))

(make-frame "Gosper Curve")

;(defun gosper-curve (order size flag)
;  (if (= 0 order)
;        (forward size)
;    (if flag
;          (gosper-map (1- order) size '(1 8 2 8 8 2 9 1 9 9 1 1 9 2 8))
;      (gosper-map (1- order) size '(9 1 8 2 2 8 8 2 8 1 9 9 1 9 2)))))

(defun gosper-curve (order size flag)
  (if (= 0 order)
        (forward size)
    (gosper-map (1- order)
                size
                (if flag '(1 8 2 8 8 2 9 1 9 9 1 1 9 2 8)
                  '(9 1 8 2 2 8 8 2 8 1 9 9 1 9 2)))))

(defun gosper-map (order size objlist)
  (let loop ((item objlist))
    (cond ((= (car item) 1) (gosper-curve order size t))
          ((= (car item) 2) (gosper-curve order size nil))
          ((= (car item) 8) (right 60))
          ((= (car item) 9) (left 60)))
    (if (cdr item) (loop (cdr item)))))

(right 90)
(gosper-curve 3 20 nil)
(open-frame)