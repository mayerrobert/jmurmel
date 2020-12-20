; output: (a b c)
; result: t

;(write (lambda () (lambda () (quote (a b c))))) ; geht

;(write ((lambda () (lambda () (quote (a b c)))))) ; geht

;(write (((lambda () (lambda () (quote (a b c))))))) ; geht nur in scheme
;(write (funcall ((lambda () (lambda () (quote (1 2 3))))))) ; geht nur in CL



; mit apply gehts in lisp und scheme
(write (apply ((lambda () (lambda () (quote (a b c))))) ()))