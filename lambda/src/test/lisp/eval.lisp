;;; eval implemented in LISP
;;; based on https://www.reddit.com/r/lisp/comments/aklz0x/what_is_the_smallest_definition_of_lisp/
;;;
;;; see also Lisp challenge (Lisp in Lisp): https://github.com/jart/sectorlisp/blob/main/lisp.lisp


; needs a LISP 1 (dyn or lex env) with symbols and cons, car, cdr, eq, atom, quote, cond, lambda (and S-expressions i.e. a reader)
; implements a LISP 1 (dyn env)   with symbols and cons, car, cdr, eq, atom, quote, cond, lambda and label

;;; result of the test function:
; result: (AA BB CC DD EE FF)
; output: (empty)


((lambda (eval evcon evlis assoc pairlis)

    ;;; test eval: use the provided append to append two lists
    (eval (quote ((label append
                    (lambda (a b)
                      (cond ((eq () a) b)
                            (t (cons (car a)
                                     (append (cdr a) b))))))
                  (quote (AA BB CC))  
                  (quote (DD EE FF))))
          (quote ((t t)))))


  ;;; eval
  (quote (lambda (x e)
     (cond ((atom x)                                    ;;; symbol
             (assoc x e))
           ((atom (car x))                              ;;; special forms und forms
             (cond ((eq (car x) (quote quote))          ; quote
                     (car (cdr x)))
                   ((eq (car x) (quote atom))           ; atom
                     (atom (eval (car (cdr x)) e)))
                   ((eq (car x) (quote eq))             ; eq
                     (eq (eval (car (cdr x)) e)
                         (eval (car (cdr (cdr x))) e)))
                   ((eq (car x) (quote car))            ; car
                     (car (eval (car (cdr x)) e)))
                   ((eq (car x) (quote cdr))            ; cdr
                     (cdr (eval (car (cdr x)) e)))
                   ((eq (car x) (quote cons))           ; cons
                     (cons (eval (car (cdr x)) e)
                           (eval (car (cdr (cdr x))) e)))
                   ((eq (car x) (quote cond))           ; cond
                     (evcon (cdr x) e))
                   ((quote t)                           ; function application
                     (eval (cons (eval (car x) e)
                                 (cdr x))
                           e))))
           ((eq (car (car x)) (quote lambda))           ; lambda
             (eval (car (cdr (cdr (car x))))
                   (pairlis (car (cdr (car x)))
                            (evlis (cdr x) e)
                            e)))
           ((eq (car (car x)) (quote label))            ; label
             (eval (cons (car (cdr (cdr (car x)))) (cdr x))  
                   (cons (cons (car (cdr (car x))) (car x))
                         e))))))

   ;;; evcon
   (quote (lambda (c e)
     (cond ((eval (car (car c)) e)
            (eval (car (cdr (car c))) e))
           ((quote t) (evcon (cdr c) e)))))

   ;;; evlis
   (quote (lambda (a e)
     (cond ((eq () a) ())
           ((quote t) (cons (eval (car a) e)
                            (evlis (cdr a) e))))))

   ;;; assoc
   (quote (lambda (x a)
     (cond ((eq () a) ())
           ((eq x (car (car a))) (cdr (car a)))
           ((quote t) (assoc x (cdr a))))))

   ;;; pairlis
   (quote (lambda (a b e)
     (cond ((eq () a) e)
           ((quote t) (cons (cons (car a) (car b))
                            (pairlis (cdr a) (cdr b) e)))))))
