;;; implement map using Y-combinator

;; after car/cdr were changed to no longer accept strings this gives an error
; error: expected a list or string argument but got "the"
; _result: (#\t #\r #\i #\s)
; _output: (empty)

((lambda (f n) ; this lambda is defining MAP
   ((lambda (map) (map map f n))
      (lambda (map f n)
        (if (null n)
              '()
          (cons (f (car n)) (map map f (cdr n))) )) ))
   car ; here are the arguments to MAP
   '("the" "rain" "in" "spain")) ; ==> (t r i s)