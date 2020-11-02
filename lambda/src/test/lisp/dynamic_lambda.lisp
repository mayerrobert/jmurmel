; output: "in dynamic-lambda""param""dynamic variable"
; result: t


(define dynamic-lambda
  (lambda dynamic (s)
    (write "in dynamic-lambda")
    (write s)
    (write a)))    ; "a" will be found in the dynamic environment, wouldn't work if "dynamic-lambda" was a lexical closure

(let* ((a "dynamic variable"))
  (dynamic-lambda "param"))