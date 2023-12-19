; output: "f""globval"4.0"f""globval"3.0"f""globval"2.0"f""globval"1.0"body 1""body 2"
; result: "body 2"

(define *global* "globval")

(letrec ((x (+ 1 1))
         (y (+ x 2))
         (f (lambda (n)
                    (if (= n 0)
                        nil
                        (progn (write "f")
                               (write *global*)
                               (write n)
                               (f (- n 1)))))))
        (f y)
        (write "body 1")
        (write "body 2"))