;;; this would fail in --dyn mode with "eval: 'a' is undefined"

; output: onetwo
; result: t

(((lambda (a)
       (lambda (b)
            (write a)
            (write b)))
          'one)
    'two)