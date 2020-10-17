; outputt: onetwo
; resultt: t

;;; das geht nur mit lex closures
(((lambda (a)
          (lambda (b)
                  (write a)
                  (write b))) 'one)
    'two)