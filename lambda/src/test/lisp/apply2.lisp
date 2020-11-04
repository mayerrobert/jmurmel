; from https://github.com/carld/micro-lisp/blob/master/examples/reverse.lisp

; output: (fn (a b c d))
; result: t

((lambda (exp)
   ((lambda (expa)
      ((lambda (fn)
         (cond
           ((listp fn)
              (write (quote (cons fn (cdr expa)))))
           ((quote t)
              ((lambda (args)
                ((lambda (evlist)
                    (write (cons fn (evlist evlist args (quote ())))))
                  (lambda (evlist^ ea evargs)
                    (cond
                      ((not ea)  (quote ()))
                      ((quote t) (cons (car ea)
                                    (evlist^ evlist^ (cdr ea) evargs)))))))
              (cdr expa)))))
       (car expa)))
    (cond
      ((eq (car exp) (quote apply))  (cdr exp))
      ((quote t) exp))))

  (quote (apply fn (a b c d))))
