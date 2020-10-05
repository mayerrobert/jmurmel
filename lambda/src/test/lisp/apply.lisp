; output: (1 2 3)
; result: t

;(write (lambda () (lambda () (quote (1 2 3))))) ; geht

;(write ((lambda () (lambda () (quote (1 2 3)))))) ; geht

;(write (((lambda () (lambda () (quote (1 2 3))))))) ; geht nur in scheme



; mit apply gehts in lisp und scheme
(write (apply ((lambda () (lambda () (quote (1 2 3))))) ()))