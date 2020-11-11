; from https://github.com/carld/micro-lisp/blob/master/examples/reverse.lisp

; output: (empty)
; result: (9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0)

((lambda (reverse)
    (reverse (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 nil)))))))))))

 (lambda (list)
   ((lambda (rev)
      (rev rev nil list))
    (lambda (rev_ a l)
      (if (not l)
            a
        (rev_ rev_ (cons (car l) a) (cdr l)))))))