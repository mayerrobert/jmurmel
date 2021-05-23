; output: (empty)
; result: (1 3 3)

;;; scheme
; (define (number->list n)
  ; (let loop ((n n)
             ; (acc '()))
    ; (if (< n 10)
        ; (cons n acc)
        ; (loop (quotient n 10)
              ; (cons (remainder n 10) acc)))))

;;; Murmel using letrec and lambda
; (defun number->list (n)
  ; (letrec ((acc '())
           ; (loop (lambda (n acc)
                         ; (if (< n 10)
                             ; (cons n acc)
                             ; (loop (/ n 10)
                                   ; (cons (mod n 10) acc))))))
                                   ; (loop n acc)))

;;; Murmel named let
(defun number->list (n)
  (let loop ((n n)
              (acc '()))
             (if (< n 10)
                 (cons (round n) acc)
                 (loop (/ n 10)
                       (cons (round (mod n 10)) acc)))))

(number->list 133)