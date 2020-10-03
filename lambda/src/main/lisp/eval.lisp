;;; eval in lisp programmiert
;;; siehe https://www.reddit.com/r/lisp/comments/aklz0x/what_is_the_smallest_definition_of_lisp/

;;; caddr, cadar usw. verkehrt lesen:
; car... wert des paares
; cdr... next
;cadar
; |||__ wert als paar interpretieren
; ||___ davon das naechste paar
; |____ davon den wert
 
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
                   (t                                   ; funktionsaufruf
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
     (cond ((eq nil a)
             nil)          
           (t      
             (cons (eval (car a) e) 
                   (evlis (cdr a) e)))))

   ;;; assoc
   (assoc (x a)      
     (cond ((eq nil a) nil)      
           ((eq x (caar a)) (cadar a))  
           (t (assoc x (cdr a)))))

   ;;; pair
   (pair (a b)      
     (cond ((eq nil a) nil)
           ((eq nil b) nil)
           (t
             (cons (list (car a) (car b))
                   (pair (cdr a) (cdr b)))))))
				   
  ;;; test eval
  (eval (quote ((label append  
                  (lambda (a b)
                    (cond ((eq nil a) b)
                          (t (cons (car a)
                                   (append (cdr a) b))))))
                (quote (a b c))  
                (quote (d e f))))
        (quote ((t t)))))