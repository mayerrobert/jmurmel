;(write (lambda () (lambda () (quote (1 2 3))))) ; geht

;(write ((lambda () (lambda () (quote (1 2 3)))))) ; geht

;(write (((lambda () (lambda () (quote (1 2 3))))))) ; geht nur in scheme



; mit apply gehts in lisp und scheme, aber nicht in LambdaJ, weil das kann apply nur fuer builtins
(write (apply ((lambda () (lambda () (quote (1 2 3))))) ()))