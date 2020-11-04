; very simple tailrecursive program

; output: 9.0
; result: t

(labels ((print-last (list)
              (if (not (cdr list))
                  (write (car list))
                  (print-last (cdr list)))))
    (print-last (quote (0 1 2 3 4 5 6 7 8 9))))