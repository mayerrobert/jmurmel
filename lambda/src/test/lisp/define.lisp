; result: "42"
; output: "42"

(define *answer* 42)

(define print-answer (lambda () (write (format nil "%2.2g" *answer*))))

(print-answer)