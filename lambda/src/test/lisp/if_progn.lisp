; output: onetwo
; result: t

(if t
    (if nil
        nil
        (progn (write 'one) (write 'two)))
    nil)