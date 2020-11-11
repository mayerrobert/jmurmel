; from https://github.com/carld/micro-lisp/blob/master/examples/reverse.lisp

; output: (empty)
; result: (9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0)

((lambda (reverse)
    (reverse (quote (1 2 3 4 5 6 7 8 9))))

 (lambda (list)
   ((lambda (rev)
      (rev rev (quote ()) list))
    (lambda (rev_ a l)
      (if (not l)
            a
        (rev_ rev_ (cons (car l) a) (cdr l)))))))