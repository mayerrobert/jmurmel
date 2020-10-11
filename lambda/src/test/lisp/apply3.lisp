; die fehlermeldung ist komplett sinnlos
; eigentlich sollte das gehen

; error: apply: not a function: string-format.
; output: (empty)
; result: Factorial of 50 is 3,04141e+64

(labels ((factTR (n a)
                 (cond ((= n 0) a)
                       (t (factTR (- n 1) (* n a))))))
 (apply 'string-format '("Factorial of 50 is %g" (factTR 50 1))))