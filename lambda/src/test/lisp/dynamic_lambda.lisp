; output: "in dynamic-lambda""param""dynamic variable"
; result: t

; "a" will be found in the dynamic environment, wouldn't work if "dynamic-lambda" was a lexical closure
(define dynamic-lambda
  (lambda dynamic (s)
    (write "in dynamic-lambda")
    (write s)
    (write a)))

(let* ((a "dynamic variable"))
  (dynamic-lambda "param"))