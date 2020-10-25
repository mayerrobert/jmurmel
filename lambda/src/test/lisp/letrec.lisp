; output: "f""globval"4.0"f""globval"3.0"f""globval"2.0"f""globval"1.0"body 1""body 2"
; result: t

(letrec ((x (+ 1 1))
         (y 42)
         (y (+ x 2)) ; later bindings hide earlier ones
         (f (lambda (n)
                    (if (= n 0)
                        nil
                        (progn (write "f")
                               (write *global*) ; global environment is dynamic
                               (write n)
                               (f (- n 1)))))))
        (define *global* "globval")
        (f y)
        (write "body 1")
        (write "body 2"))