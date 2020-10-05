; from https://github.com/carld/micro-lisp/blob/master/examples/reverse.lisp

; stuerzt mit CCE ab
; outputt: t
; result: (9 8 7 6 5 4 3 2 1)

((lambda (reverse)
    (reverse (quote (1 2 3 4 5 6 7 8 9))))

 (lambda (list)
   ((lambda (rev)
      (rev rev (quote ()) list))
    (lambda (rev^ a l)
      (cond
        ((null? l) a)
        ((quote t) (rev^ rev^ (cons (car l) a) (cdr l ))))))))