;;;; Default library for Murmel
; Provides:
; not, and, or
; zerop, abs
; char=
; eql, equal
; when, unless, dotimes, dolist
; member
; mapcar, maplist, mapc, mapl
; remove-if, remove-if-not


;;; Logical not
(defun not (e)
  (null e))


;;; Short-circuiting macros for logical and and or
(defmacro and args
   (if (null args)
         t
     (if (null (cdr args))
           (car args)
       `(if ,(car args)
         (and ,@(cdr args))))))

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


;;; Is this number zero?
(defun zerop (n)
  (= n 0))

(defun abs (n)
  (if (< n 0) (- n) (+ n)))


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


;;; Return t if any of the following is true
;;; a and b are eq
;;; a and b are numbers of the same type and have the same value
;;; a and b are the same characters
(defun eql (a b)
  (or (eq a b)
      (and (integerp a) (integerp b)     (= a b))
      (and (floatp a)   (floatp b)       (= a b))
      (and (characterp a) (characterp b) (char= a b))))


;;; Return t if any of the following is true
;;; a and b are eql
;;; a and b are strings, characters or symbols and have the same text value
;;; a and b are conses whose car and cdr are equal respectively
(defun equal (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (consp a)   (consp b)   (equal (car a) (car b)) (equal (cdr a) (cdr b)))))


(defmacro when (condition . body)
  (list 'if condition (cons 'progn body) nil))

(defmacro unless (condition . body)
  (list 'if condition nil (cons 'progn body)))


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


; helper macro to generate defuns for the various maxXX functions
(defmacro mapx (name comb acc accn return-list)
  (append
    (list 'defun name '(f l . more))
    (list `(if more
                 (labels ((none-nil (lists)
                            (if lists
                                  (and (car lists) (none-nil (cdr lists)))
                              t))
                          (cdrs (lists)
                            (if lists
                                  (cons (cdr (car lists)) (cdrs (cdr lists)))
                              nil))
                          (cars (lists)
                            (if lists
                                  (cons (car (car lists)) (cars (cdr lists)))
                              nil)))
                   (let loop ((args (cons l more)))
                     (if (none-nil args)
                           (,comb (apply f (,accn args)) (loop (cdrs args)))
                       nil)))
             (if l (,comb (f (,acc l)) (,name f (cdr l)))
               nil)))
    (when return-list '(l))))

; (mapcar function list [more-lists]) -> list
(mapx mapcar  cons  car cars nil)
; (maplist function list [more-lists]) -> list
(mapx maplist cons  (lambda (l) l) (lambda (l) l) nil)
; (mapc function list [more-lists]) -> first-arg-list
(mapx mapc    progn car cars t)
; (mapl function list [more-lists]) -> first-arg-list
(mapx mapl    progn (lambda (l) l) (lambda (l) l) t)


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
