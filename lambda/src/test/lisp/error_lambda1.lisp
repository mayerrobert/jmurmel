; no formal parameter list

; error: lambda: expected a symbol or a list of symbols but got ((quote hello))

((lambda (write (quote hello)))
   (quote aaa))
