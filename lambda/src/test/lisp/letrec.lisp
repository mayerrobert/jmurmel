; output: "f"4.0"f"3.0"f"2.0"f"1.0"body 1""body 2"
; result: t

(letrec ((x (+ 1 1))
         (y (+ x 2))
         (f (lambda (n)
                    (if (= n 0)
                        nil
                        (progn (write "f")
                               (write n)
                               (f (- n 1)))))))
        (f y)
        (write "body 1")
        (write "body 2"))