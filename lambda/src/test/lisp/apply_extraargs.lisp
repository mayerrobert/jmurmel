; makeshift variadic functions: pass a list
;
; result: t
; output: 1.0 (2.0 3.0)

(apply (lambda (a b)
               (write a)
               (write '\ )
               (write b))
       '(1 '(2 3)))