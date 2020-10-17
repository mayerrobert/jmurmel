; output: (empty)
; result: (one . two)

;;; das geht dynamisch und lexical
((lambda (a)
         ((lambda (b)
                  (cons a b))
             'two))
    'one)