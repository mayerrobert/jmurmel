;;; eval implemented in LISP
;;; based on https://www.reddit.com/r/lisp/comments/aklz0x/what_is_the_smallest_definition_of_lisp/
;;;
;;; see also Lisp challenge (Lisp in Lisp): https://github.com/jart/sectorlisp/blob/main/lisp.lisp


; needs a LISP 1 (dyn or lex env) with symbols and cons, car, cdr, eq, atom, quote, cond, lambda (and S-expressions i.e. a reader)
; implements a LISP 1 (dyn env)   with symbols and cons, car, cdr, eq, atom, quote, cond, lambda and label

;;; result of the test function:
; result: (AA BB CC DD EE FF)
; output: (empty)


((lambda (caar cadr cadar caddr caddar append eval evcon evlis assoc pair)

    ;;; test eval: use the provided append to append two lists
    (eval (quote ((label append
                       (lambda (a b)
                         (cond ((eq () a) b)
                               (t (cons (car a)
                                        (append (cdr a) b))))))
                  (quote (AA BB CC))  
                  (quote (DD EE FF))))
          (quote ((t t)))))

;    (eval (quote ((lambda (reverse)
;                     (reverse (quote (1 2 3 4 5 6 7 8 9))))
;                 
;                  (lambda (lst)
;                    ((lambda (rev)
;                       (rev rev (quote ()) lst))
;                     (lambda (rev^ a l)
;                       (cond
;                         ((eq () l) a)
;                         (t (rev^ rev^ (cons (car l) a) (cdr l)))))))))
;          (quote ((t t)))))


  ;;; caar..caddar
  (lambda (l) (car (car l)))             ; caar
  (lambda (l) (car (cdr l)))             ; cadr
  (lambda (l) (car (cdr (car l))))       ; cadar
  (lambda (l) (car (cdr (cdr l))))       ; caddr
  (lambda (l) (car (cdr (cdr (car l))))) ; caddar

  ;;; append
  (quote (lambda (a b)
           (cond ((eq () a) b)
                  ((quote t) (cons (car a) (append (cdr a) b))))))

  ;;; eval
  (quote (lambda (x e)
     (cond ((atom x)                                    ;;; symbol
             (assoc x e))
           ((atom (car x))                              ;;; special forms und forms
             (cond ((eq (car x) (quote quote))          ; quote
                     (cadr x))
                   ((eq (car x) (quote atom))           ; atom
                     (atom (eval (cadr x) e)))
                   ((eq (car x) (quote eq))             ; eq
                     (eq (eval (cadr x) e)
                         (eval (caddr x) e)))
                   ((eq (car x) (quote car))            ; car
                     (car (eval (cadr x) e)))
                   ((eq (car x) (quote cdr))            ; cdr
                     (cdr (eval (cadr x) e)))
                   ((eq (car x) (quote cons))           ; cons
                     (cons (eval (cadr x) e)
                           (eval (caddr x) e)))
                   ((eq (car x) (quote cond))           ; cond
                     (evcon (cdr x) e))
                   ((quote t)                           ; function application
                     (eval (cons (eval (car x) e)
                                 (cdr x)) e))))
           ((eq (caar x) (quote lambda))                ; lambda
             (eval (caddar x)
                    (append (pair (cadar x)
                                  (evlis (cdr x) e))
                            e)))
           ((eq (caar x) (quote label))                 ; label
             (eval (cons (caddar x) (cdr x))  
                   (cons (cons (cadar x) (cons (car x) ()))
                         e))))))

   ;;; evcon
   (quote (lambda (c e)
     (cond ((eval (caar c) e)
            (eval (cadar c) e))
           ((quote t) (evcon (cdr c) e)))))

   ;;; evlis
   (quote (lambda (a e)
     (cond ((eq () a) ())
           ((quote t) (cons (eval (car a) e)
                            (evlis (cdr a) e))))))

   ;;; assoc
   (quote (lambda (x a)
     (cond ((eq () a) ())
           ((eq x (caar a)) (cadar a))
           ((quote t) (assoc x (cdr a))))))

   ;;; pair
   (quote (lambda (a b)
     (cond ((eq () a) ())
           ((eq () b) ())
           ((quote t) (cons (cons (car a) (cons (car b) ()))
                            (pair (cdr a) (cdr b))))))))
