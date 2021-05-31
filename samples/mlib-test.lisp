(load "mlib")

(define *error-count* 0)


;;; check whether "form" eval's to "expected"
(defmacro assert-equal (expected form . msg)
  `(do-assert-equal ,expected ,form (if ,msg ,(car msg) '(equal ,expected ,form))))

(defun do-assert-equal (expected actual msg)
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
  (unless condition
    (format t "%nassertion failed:%n")
    (writeln quoted-condition)
    (when msg (format t "%s%n" msg))
    (setq *error-count* (1+ *error-count*))
    nil))


;;; test not
(assert-true  (not nil))
(assert-true  (not '()))
(assert-true  (not (integerp 'sss)))
(assert-false (not (integerp 1)))
(assert-false (not 3.7))
(assert-false (not 'apple))
 

;;; test logical and/ or macros
(assert-true
  (and (= 1 1)
       (or (< 1 2)
           (> 1 2))
       (and (<= 1 2 3 4)
            (> 5 3 1))))

(defmacro inc-var (var) `(setq ,var (1+ ,var)))
(defmacro dec-var (var) `(setq ,var (1- ,var)))
(define temp1 1) (define temp2 1) (define temp3 1)
(assert-equal 2 (and (inc-var temp1) (inc-var temp2) (inc-var temp3)))
(assert-true (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)))
(assert-equal 1 (dec-var temp3))
(assert-false (and (dec-var temp1) (dec-var temp2) (eq temp3 'nil) (dec-var temp3)))
(assert-true (and (eql temp1 temp2) (eql temp2 temp3)))
(assert-true (and))


; todo zerop, char=


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


; todo when, unless, dotimes, dolist
; todo member, mapcar, remove-if, remove-if-not



; Summary
; print failed tests if any
(unless (zerop *error-count*)
  (fatal (format nil "%n%d asserts failed%n" *error-count*)))
