; result: 24.0
; output: (empty)

(labels ((factorial (n)
                    (cond ((= n 1) 1)
                          ((quote t) (* n (factorial (- n 1)))))))
  (factorial 4))
