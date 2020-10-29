
; gibt cce

(define env-letrec 
    (letrec let-body (f (lambda ()
                            (write "f calls let-body")
                            (let-body)))
        (write "let-body")))

(env-letrec)