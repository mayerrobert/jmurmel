; very simple tailrecursive program

; output: 3.0
; result: t

(labels ((print-last (list)
              (if (null? (cdr list))
                  (write (car list))
                  (print-last (cdr list)))))
    (print-last (quote (1 2 3))))