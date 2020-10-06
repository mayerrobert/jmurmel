; output: (a b c)
; result: t

;(write (lambda () (lambda () (quote (a b c))))) ; geht

;(write ((lambda () (lambda () (quote (a b c)))))) ; geht

;(write (((lambda () (lambda () (quote (a b c))))))) ; geht nur in scheme



; mit apply gehts in lisp und scheme
(write (apply ((lambda () (lambda () (quote (a b c))))) ()))