; output: onetwo
; result: two

(if t
    (if nil
        nil
        (progn (write 'one) (write 'two)))
    nil)