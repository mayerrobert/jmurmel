; this doesn't work on GNU clisp nor Gauche v0.9.3.3 (scheme), don't know why

; output: |map called with element:|a|map called with element:|b|map called with element:|c|map called with element:|d|map called with element:|e
; result: (a b c d e)

(labels ((map (fn rest)
              (cond ((eq nil rest) rest)
                    ((quote t) (cons (fn (car rest)) (map fn (cdr rest)))))))

  (map (lambda (e) (write (quote map\ called\ with\ element:))(write e))
       (quote (a b c d e))))
