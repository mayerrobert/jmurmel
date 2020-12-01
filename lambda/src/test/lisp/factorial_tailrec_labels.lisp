; tail recursive version of factorial

; result: 24.0
; output: (empty)

(labels ((factTR (n a)
                 (labels ((mult (lhs rhs) (* lhs rhs)))
                   (cond ((= n 0) a)
                       (t (factTR (- n 1) (mult n a)))))))

; A wrapper over factTR 
 (factTR 4 1))