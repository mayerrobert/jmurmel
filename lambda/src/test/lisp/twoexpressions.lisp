; this file contains two expressions
; should both be executed, result will be the result of the last expression

; output: Hello, World!
; result: (quote t)

(write (quote Hello,\ ))
(write (quote World!))