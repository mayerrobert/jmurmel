;;; this would fail in --dyn mode with "eval: 'a' is undefined"

; output: onetwo
; result: two

(((lambda (a)
       (lambda (b)
            (write a)
            (write b)))
          'one)
    'two)