; tail recursive version of factorial

; result: 24.0
; output: (empty)

(labels ((factTR (n a)
                 (cond ((= n 0) a)
                       (t (factTR (- n 1) (* n a))))))

; A wrapper over factTR 
 (factTR 4 1))