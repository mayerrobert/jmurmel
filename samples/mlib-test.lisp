;;;; Tests for Murmel's default library "mlib".

(load "mlib")

(define *success-count* 0)
(define *error-count* 0)


;;; check whether "form" eval's to "expected"
(defmacro assert-equal (expected form . msg)
  `(do-assert-equal ,expected ,form (if ,msg ,(car msg) '(equal ,expected ,form))))

(defun do-assert-equal (expected actual msg)
  (setq *success-count* (1+ *success-count*))
  (unless (equal expected actual)
    (format t "%nassert-equal failed: ") (writeln msg)
    (format t "expected: ") (writeln expected)
    (format t "actual:   ") (writeln actual)
    (setq *error-count* (1+ *error-count*))
    nil))


(defmacro assert-true (condition . msg)
  `(do-assert ,condition ',condition (if ,msg ,(car msg) "expected condition to be true")))

(defmacro assert-false (condition . msg)
  `(do-assert (not ,condition) ',condition (if ,msg ,(car msg) "expected condition to be false")))

(defun do-assert (condition quoted-condition msg)
  (setq *success-count* (1+ *success-count*))
  (unless condition
    (format t "%nassertion failed:%n")
    (writeln quoted-condition)
    (when msg (format t "%s%n" msg))
    (setq *error-count* (1+ *error-count*))
    nil))


(defun caddr (l) (car (cdr (cdr l))))
(defun cdddr (l) (cdr (cdr (cdr l))))

(defmacro tests l
  (if l
    `(append (assert-equal ',(caddr l) ,(car l))
             (tests ,@(cdddr l)))))


;;; test acons
(define alist '())
(tests
  (setq alist '()) => NIL
  (acons 1 "one" alist) =>  ((1 . "one"))
  alist =>  NIL
  (setq alist (acons 1 "one" (acons 2 "two" alist))) =>  ((1 . "one") (2 . "two"))
  (assoc 1 alist) =>  (1 . "one")
  (setq alist (acons 1 "uno" alist)) =>  ((1 . "uno") (1 . "one") (2 . "two"))
  (assoc 1 alist) =>  (1 . "uno")
)


;;; test not
(tests
  (not nil) => t
  (not '()) => t
  (not (integerp 'sss)) => t
  (not (integerp 1)) => nil
  (not 3.7) => nil
  (not 'apple) => nil
)

;;; test logical and/ or macros
(assert-true
  (and (= 1 1)
       (or (< 1 2)
           (> 1 2))
       (and (<= 1 2 3 4)
            (> 5 3 1))))

(defmacro inc-var (var) `(setq ,var (1+ ,var)))
(defmacro dec-var (var) `(setq ,var (1- ,var)))
(define temp0 nil) (define temp1 1) (define temp2 1) (define temp3 1)

(assert-equal 2 (and (inc-var temp1) (inc-var temp2) (inc-var temp3)))
(assert-true (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)))
(assert-equal 1 (dec-var temp3))
(assert-false (and (dec-var temp1) (dec-var temp2) (eq temp3 'nil) (dec-var temp3)))
(assert-true (and (eql temp1 temp2) (eql temp2 temp3)))
(assert-true (and))

(assert-false (or))
(assert-equal 30 (setq temp0 nil temp1 10 temp2 20 temp3 30))
(assert-equal 10 (or temp0 temp1 (setq temp2 37)))
(assert-equal 20 temp2)
(assert-equal 11 (or (inc-var temp1) (inc-var temp2) (inc-var temp3)))
(assert-equal 11 temp1)
(assert-equal 20 temp2)
(assert-equal 30 temp3)
; (or (values) temp1) =>  11
; (or (values temp1 temp2) temp3) =>  11
; (or temp0 (values temp1 temp2)) =>  11, 20
; (or (values temp0 temp1) (values temp2 temp3)) =>  20, 30


; todo abs


; todo zerop, evenp, oddp


; todo char=


;;; test eql
(assert-false (eql 'a 'b))
(assert-true  (eql 'a 'a))
(assert-true  (eql 3 3))
(assert-false (eql 3 3.0))
(assert-true  (eql 3.0 3.0))
;(assert-true  (eql #c(3 -4) #c(3 -4)))
; (eql #c(3 -4.0) #c(3 -4)) =>  false
(assert-false (eql (cons 'a 'b) (cons 'a 'c)))
(assert-false (eql (cons 'a 'b) (cons 'a 'b)))
(assert-false (eql '(a . b) '(a . b)))
(define x nil)
(assert-true  (progn (setq x (cons 'a 'b)) (eql x x)))
(assert-true  (progn (setq x '(a . b)) (eql x x)))
; (eql #\A #\A) =>  true
(assert-false (eql "Foo" "Foo"))
; (eql "Foo" (copy-seq "Foo")) =>  false
(assert-false (eql "FOO" "foo"))


;;; test equal
(assert-false (equal 'a 'b))
(assert-true  (equal 'a 'a))
(assert-true  (equal 3 3))
(assert-false (equal 3 3.0))
(assert-true  (equal 3.0 3.0))
;(assert-true  (equal #c(3 -4) #c(3 -4)))
;(assert-false (equal #c(3 -4.0) #c(3 -4)))
(assert-false (equal (cons 'a 'b) (cons 'a 'c)))
(assert-true  (equal (cons 'a 'b) (cons 'a 'b)))
;(assert-true  (equal #\A #\A))
;(assert-false (equal #\A #\a))
(assert-true  (equal "Foo" "Foo"))
;(assert-true  (equal "Foo" (copy-seq "Foo")))
(assert-false (equal "FOO" "foo"))
(assert-true  (equal "This-string" "This-string"))
(assert-false (equal "This-string" "this-string"))


; test when, unless
(labels ((prin1 (form) (write form) form))
  (assert-equal 'hello (when t 'hello))
  (assert-equal nil    (unless t 'hello))
  (assert-equal nil    (when nil 'hello))
  (assert-equal 'hello (unless nil 'hello))
  (assert-equal nil    (when t))
  (assert-equal nil    (unless nil))
  (assert-equal 3      (when t (prin1 1) (prin1 2) (prin1 3))) ; >>  123, =>  3
  (assert-equal nil    (unless t (prin1 1) (prin1 2) (prin1 3)))
  (assert-equal nil    (when nil (prin1 1) (prin1 2) (prin1 3)))
  (assert-equal 3      (unless nil (prin1 1) (prin1 2) (prin1 3))) ; >>  123, =>  3
  (assert-equal
    '((4) NIL (5) NIL 6 (6) 7 (7))
    (let ((x 3))
      (list (when (oddp x) (inc-var x) (list x))
            (when (oddp x) (inc-var x) (list x))
            (unless (oddp x) (inc-var x) (list x))
            (unless (oddp x) (inc-var x) (list x))
            (if (oddp x) (inc-var x) (list x))
            (if (oddp x) (inc-var x) (list x))
            (if (not (oddp x)) (inc-var x) (list x))
            (if (not (oddp x)) (inc-var x) (list x))))))


; test dotimes
(assert-equal 10 (dotimes (temp-one 10 temp-one)))
(define temp-two 0)
(assert-true (dotimes (temp-one 10 t) (inc-var temp-two)))
(assert-equal 10 temp-two)


; test dolist
(defmacro prepend (elem l) `(setq ,l (cons ,elem ,l)))
(define temp-two '())
(assert-equal '(4 3 2 1) (dolist (temp-one '(1 2 3 4) temp-two) (prepend temp-one temp-two)))

(define temp-two 0)
(assert-equal nil (dolist (temp-one '(1 2 3 4)) (inc-var temp-two)))
(assert-equal 4 temp-two)

(assert-equal nil (dolist (x '(a b c d)) (write x) (format t " "))) ; >>  A B C D , =>  NIL



; test member
(assert-equal '(2 3) (member 2 '(1 2 3)))
(assert-equal NIL (member 'e '(a b c d)))


; test mapcar
(assert-equal '(1 2 3)
              (mapcar car '((1 a) (2 b) (3 c))))
(assert-equal '(3.0 4.0 2.0 5.0 6.0)
              (mapcar abs '(3 -4 2 -5 -6)))
(assert-equal '((A . 1) (B . 2) (C . 3))
              (mapcar cons '(a b c) '(1 2 3)))


; test maplist
(assert-equal '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))
              (maplist append '(1 2 3 4) '(1 2) '(1 2 3)) )
(assert-equal '((FOO A B C D) (FOO B C D) (FOO C D) (FOO D))
              (maplist (lambda (x) (cons 'foo x)) '(a b c d)))
(assert-equal '(0 0 1 0 1 1 1)
              (maplist (lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c)))
;An entry is 1 if the corresponding element of the input
;  list was the last instance of that element in the input list.


; test mapc
(define dummy nil)
(assert-equal
  '(1 2 3 4)
  (mapc (lambda x (setq dummy (append dummy x)))
        '(1 2 3 4)
        '(a b c d e)
        '(x y z)))
(assert-equal '(1 A X 2 B Y 3 C Z) dummy)


; test mapl
(setq dummy nil)
(assert-equal '(1 2 3 4)
              (mapl (lambda (x) (prepend x dummy)) '(1 2 3 4)))
(assert-equal '((4) (3 4) (2 3 4) (1 2 3 4)) dummy)


; test remove-if, remove-if-not, remove
(assert-equal '(2 4 4) (remove-if oddp '(1 2 4 1 3 4 5)))
(assert-equal '(2 4 4) (remove-if-not evenp '(1 2 4 1 3 4 5)))

(assert-equal '(1 3 5 9) (remove 4 '(1 3 4 5 9)))
(assert-equal '(1 2 1 3 5) (remove 4 '(1 2 4 1 3 4 5)))


; Summary
; print succeeded and failed tests if any
(writeln) (writeln)
(if (zerop *error-count*)
      (format t "mlib-test.lisp: %d asserts succeeded. Yay!%n" *success-count*)
  (fatal (format nil "mlib-test.lisp: %d/%d asserts failed. D'oh!%n" *error-count* *success-count*)))
