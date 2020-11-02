;; see also namedlet.lisp

; output: (empty)
; result: (1 3 3)

;;; Murmel using letrec and lambda
(defun number->list (n)
  (letrec ((acc '())
           (loop (lambda (n acc)
                         (if (< n 10)
                             (cons (floor n) acc)
                             (loop (/ n 10)
                                   (cons (floor (mod n 10)) acc))))))
                                   (loop n acc)))

(number->list 133)