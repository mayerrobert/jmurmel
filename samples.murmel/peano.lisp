;;;; Numbers represented as lists, see https://en.wikipedia.org/wiki/Peano_axioms

(define zero ())             ; -> zero =  ()

(define succ (lambda (a) (cons zero a)))

(define one   (succ zero))   ; -> one =   (NIL)
(define two   (succ one))    ; -> two =   (NIL NIL)
(define three (succ two))    ; -> three = (NIL NIL NIL)
(define four  (succ three))  ; -> four =  (NIL NIL NIL NIL)


(define add (lambda (a b) (if (eq b zero)
                              a
                              (add (succ a) (cdr b)))))

(define mul (lambda (a b) (if (eq b zero)
                              zero
                              (add a (mul a (cdr b))))))


(mul three four)
; -> (nil nil nil nil nil nil nil nil nil nil nil nil)
