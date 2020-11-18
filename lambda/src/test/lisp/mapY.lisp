;;; implement map using Y-combinator and apply it to "car '(the rain in spain)"

; result: (t r i s)
; output: (empty)

((lambda (f n) ; this lambda is defining MAP
   ((lambda (map) (map map f n))
      (lambda (map f n)
        (if (not n)
              '()
          (cons (f (car n)) (map map f (cdr n))) )) ))
   car ; here are the arguments to MAP
   '(the rain in spain)) ; ==> (t r i s)