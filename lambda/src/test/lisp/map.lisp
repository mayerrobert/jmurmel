; this doesn't work on GNU clisp nor Gauche v0.9.3.3 (scheme), don't know why

; output: map called with element:amap called with element:bmap called with element:cmap called with element:dmap called with element:e
; result: (t t t t t)

(labels ((map (fn rest)
              (cond ((eq nil rest) rest)
                    ((quote t) (cons (fn (car rest)) (map fn (cdr rest)))))))
	 
  (map (lambda (e) (write (quote map\ called\ with\ element:))(write e))
       (quote (a b c d e))))
