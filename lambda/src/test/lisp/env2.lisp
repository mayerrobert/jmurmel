; result: t
; output: "in f""vorher ""global a""global b"\n"in f""im let ""let a   ""global b"\n"in f""nachher""let a   ""global b"\n

(define a "global a")
(defun f (s) (write "in f") (write s) (write a) (writeln b))

(progn

  (define b "global b")
  (f "vorher ")
  (let* ((a "let a   ")) (f "im let "))
  (f "nachher")

)
