;;;; N-Queens
;;;
;;; From: https://gist.github.com/kyontan/9d3b437b4675c420813d74cf5e603a41

#+murmel (require "mlib")
#+murmel (define rec nil)

#-murmel (defparameter rec nil)

(defun can-place (x y queens)
  (cond
    ((member t (mapcar #'(lambda (xy) (or (= x (car xy)) (= y (cadr xy)))) queens)) nil)
    ((member t (mapcar #'(lambda (xy) (= 1 (abs (/ (- x (car xy)) (- y (cadr xy)))))) queens)) nil)
    (t)))

; try putting a queen to (x y);
;   if possible, put it and try putting recursively
;   otherwise, trying putting a queen in order: (1 1) ~ (max max)
(defun solve-loop (x y queens maxval)
  (cond
    ((= maxval (length queens)) (print (list 'answer queens)) (cdr queens)) ; print answer and backtrack (try to solve more than 1 answer)
    ; ((= maxval (length queens)) queens)
    ((or (> x maxval) (> y maxval)) (cdr queens)) ; backtrack
    ((can-place x y queens)
      (setq rec (solve-loop (1+ x) 1 (append (list (list x y)) queens) maxval))
      (cond
        ((equal queens rec) (solve-loop x (1+ y) queens maxval)) ; backtracked
        (t rec)))
    (t (solve-loop x (1+ y) queens maxval))))

; (trace 'solve-loop)
; (trace 'can-place)

(defun solve (maxval queens)
  (solve-loop 1 1 queens maxval))

(print (list 'answer (solve 8 '()))) ; try 8 queens, hit RET to run
