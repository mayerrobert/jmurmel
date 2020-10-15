; result: t
; output: "42"

(define *answer* 42)

(define print-answer (lambda () (write (string-format "%2.2g" *answer*))))

(print-answer)