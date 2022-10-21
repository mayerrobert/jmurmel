; output: 1.02.03.02.01.00.0-1.00.0
; result: 0.0

; inc and dec close over the same variable n
(let ((n 0))
  (defun inc ()
    (write (setq n (1+ n))))
  (defun dec ()
    (write (setq n (1- n)))))

(inc) (inc) (inc) (dec) (dec) (dec) (dec) (inc)