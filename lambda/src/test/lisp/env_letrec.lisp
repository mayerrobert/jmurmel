
; ist komplett anders als CL oder Scheme

; output: "in let-body, calling f: ""in f, displaying contents of let-body: "(lambda (f) (write "in let-body, calling f: ") (f))
; result: t

(define env-letrec 
    (lambda () 
        (let* let-body 
            ((f (lambda ()
                    (write "in f, displaying contents of let-body: ")
                    (write let-body))))
        (write "in let-body, calling f: ")
        (f))))

(env-letrec)