; In lexical closure mode my interpreter will return (2.0 0.0), in dynamic mode (2.0 2.0)




(define *test* 0)

(defun foo (x *test*)
  (cons *test* (cons (bar) nil))) ; *test* is bound to the second argument

(defun bar ()
  *test*)   ; in lexical mode *test* will be bound to the global variable,
            ; in dynamic mode *test* will be bound later at the time of execution

((lambda (*test*)
         (foo 1 2))
    1)