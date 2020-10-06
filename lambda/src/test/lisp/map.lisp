; this doesn't work on GNU clisp nor Gauche v0.9.3.3 (scheme), don't know why

; output: map called with element:1map called with element:2map called with element:3map called with element:4map called with element:5
; result: (t t t t t)

(labels ((map (fn rest)
              (cond ((eq nil rest) rest)
                    ((quote t) (cons (fn (car rest)) (map fn (cdr rest)))))))
	 
  (map (lambda (e) (write (quote map\ called\ with\ element:))(write e))
       (quote (1 2 3 4 5))))
