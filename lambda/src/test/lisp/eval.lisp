;;; eval implemented in LISP
;;; based on https://www.reddit.com/r/lisp/comments/aklz0x/what_is_the_smallest_definition_of_lisp/

; needs a LISP with   S-expressions, symbols and cons, car, cdr, eq, atom, quote, cond, labels, defun
; implements a LISP 1 (dyn env) with symbols and cons, car, cdr, eq, atom, quote, cond, label, lambda

;;; result of the test function:
; result: (AA BB CC DD EE FF)
; output: (empty)

(defun caar   (l) (car (car l)))
(defun cadr   (l) (car (cdr l)))
(defun cadar  (l) (car (cdr (car l))))
(defun caddr  (l) (car (cdr (cdr l))))
(defun caddar (l) (car (cdr (cdr (car l)))))

(defun list (a b) (cond ((eq  a ()) (cond ((atom b) (cons b ()))
                                           (t b)))
                         (t (cons a (cons b ())))))

(defun append (a b)
       (cond ((eq () a) b)
              (t (cons (car a) (append (cdr a) b)))))
              
              
(labels

  ;;; eval
  ((eval (x e)
     (cond ((atom x)                                    ;;; symbol
             (assoc x e))
           ((atom (car x))                              ;;; special forms und forms
             (cond ((eq (car x) (quote quote))			; quote
                     (cadr x))
                   ((eq (car x) (quote atom))			; atom
                     (atom (eval (cadr x) e)))
                   ((eq (car x) (quote eq))				; eq
                     (eq (eval (cadr x) e)
                         (eval (caddr x) e)))
                   ((eq (car x) (quote car))			; car
                     (car (eval (cadr x) e)))
                   ((eq (car x) (quote cdr))  			; cdr
                     (cdr (eval (cadr x) e)))
                   ((eq (car x) (quote cons))			; cons
                     (cons (eval (cadr x) e)
                           (eval (caddr x) e)))
                   ((eq (car x) (quote cond))			; cond
                     (evcon (cdr x) e))
                   (t                                   ; function application
                     (eval (cons (assoc (car x) e)
                                 (cdr x)) e))))
           ((eq (caar x) (quote lambda))				; lambda
             (eval (caddar x)
                    (append (pair (cadar x)
                                  (evlis (cdr x) e))
                            e)))
           ((eq (caar x) (quote label))					; label
             (eval (cons (caddar x) (cdr x))  
                   (cons (list (cadar x) (car x))
                         e)))))

   ;;; evcon
   (evcon (c e)          
     (cond ((eval (caar c) e)
             (eval (cadar c) e))
           (t      
             (evcon (cdr c) e))))

   ;;; evlis
   (evlis (a e)
     (cond ((eq () a)
             ())          
           (t      
             (cons (eval (car a) e) 
                   (evlis (cdr a) e)))))

   ;;; assoc
   (assoc (x a)      
     (cond ((eq () a) ())      
           ((eq x (caar a)) (cadar a))  
           (t (assoc x (cdr a)))))

   ;;; pair
   (pair (a b)
     (cond ((eq () a) ())
           ((eq () b) ())
           (t
             (cons (list (car a) (car b))
                   (pair (cdr a) (cdr b)))))))
				   
  ;;; test eval: use the provided append to append two lists
  (eval (quote ((label append  
                  (lambda (a b)
                    (cond ((eq () a) b)
                          (t (cons (car a)
                                   (append (cdr a) b))))))
                (quote (AA BB CC))  
                (quote (DD EE FF))))
        (quote ((t t)))))
