;;;; Standard library for Murmel

;;; Short-circuiting macros for logical and and or

(defmacro or args
   (if (null args)
         nil
     (if (null (cdr args))
           (car args)
       (let ((temp (gensym)))
         `(let ((,temp ,(car args)))
             (if ,temp
                   ,temp
               (or ,@(cdr args))))))))

(defmacro and args
   (if (null args)
         t
     (if (null (cdr args))
           (car args)
       (let ((temp (gensym)))
         `(let ((,temp ,(car args)))
             (if ,temp
                   (and ,@(cdr args))))))))


;;; Logical not
(defun not (e)
  (null e))


;;; Return t if all of the arguments are the same character
(defun char= (c . more)
  (if more
        (let loop ((code (char-code c)) (l more))
          (if (= code (char-code (car l)))
                (if (cdr l)
                      (loop code (cdr l))
                  t)
            nil))
   t))


;;; Return t if one of the following is true
;;; a and b are eq
;;; a and b are numbers of the same type and have the same value
;;; a and b are the same characters
(defun eql (a b)
  (or (eq a b)
      (and (integerp a) (integerp b)     (= a b))
      (and (floatp a)   (floatp b)       (= a b))
      (and (characterp a) (characterp b) (char= a b))))


;;; Return t if one of the following is true
;;; a and b are eql
;;; a and b are strings, characters or symbols and have the same text value
;;; a and b are conses whose car and cdr are equal respectively
(defun equal (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (consp a)   (consp b)   (equal (car a) (car b)) (equal (cdr a) (cdr b)))))


;;; Is this number zero?
(defun zerop (n)
  (= n 0))


(defmacro when (condition . body)
  (list 'if condition (cons 'progn body) nil))

(defmacro unless (condition . body)
  (list 'if condition nil (cons 'progn body)))


; use like this:
; (member 1 '(a b c 1 2 3))
; (member 1 '(a b c 1 2 3) eq)
; (member 1 '(a b c 1 2 3) (lambda (a b) (eq a b))
(defun member (obj l . test)
  (let* ((tst (car test))
         (pred (if tst
                     (if (symbolp tst)
                           (lambda (a b) (apply tst (list a b)))
                       tst)
                 (lambda (a b) (eql a b)))))
    (if l
          (if (pred obj (car l))
                l
            (member obj (cdr l) pred))
      nil)))


(defun mapcar (f l)
  (if l (cons (f (car l)) (mapcar f (cdr l)))
    nil))

(defun remove-if (pred l)
  (if l
        (let ((obj (car l)))
          (if (pred obj)
                (remove-if pred (cdr l))
            (cons obj (remove-if pred (cdr l)))))
    nil))

(defun remove-if-not (pred l)
  (if l
        (let ((obj (car l)))
          (if (pred obj)
                (cons obj (remove-if-not pred (cdr l)))
            (remove-if-not pred (cdr l))))
    nil))


; similar to CL dotimes http://clhs.lisp.se/Body/m_dotime.htm
; dotimes (var count-form [result-form]) statement* => result
(defmacro dotimes (exp . body)
  (let ((var (car exp))
        (countform (car (cdr exp)))
        (count (gensym))
        (result (car (cdr (cdr exp)))))
    `(let ((,count ,countform))
       (if (<= ,count 0)
             (let ((,var 0)) ,result)
         (let loop ((,var 0))
           (if (>= ,var ,count) ,result
             (progn
               ,@body
               (loop (1+ ,var)))))))))


; similar to CL dolist http://clhs.lisp.se/Body/m_dolist.htm
; dolist (var list-form [result-form]) statement* => result*
(defmacro dolist (exp . body)
  (let ((var (car exp))
        (listform (car (cdr exp)))
        (lst (gensym))
        (result (car (cdr (cdr exp)))))
    `(let loop ((,lst ,listform))
       (let ((,var (car ,lst)))
         (if (null ,lst) ,result
           (progn
             ,@body
             (loop (cdr ,lst))))))))
