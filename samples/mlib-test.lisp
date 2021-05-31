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



;(defmacro run-form (e) `(progn (write ',e) (format t " ; ==> ") (writeln ,e)))
;(run-form (char-code (car "la")))
;(run-form (code-char 108))
;(run-form (integerp 1))
;(run-form (integerp 1.0))
;(run-form (floatp 1))
;(run-form (floatp 1.0))



;;; test logical and/ or macros
(assert-true
  (and (= 1 1)
       (or (< 1 2)
           (> 1 2))
       (and (<= 1 2 3 4)
            (> 5 3 1))))


;;; test not
(assert-true (not nil))


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
;=>  true
;OR=>  false
(define x nil)
(assert-true  (progn (setq x (cons 'a 'b)) (eql x x)))
(assert-true  (progn (setq x '(a . b)) (eql x x)))
; (eql #\A #\A) =>  true
(assert-false (eql "Foo" "Foo"))
;=>  true
;OR=>  false
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



; Summary
; print failed tests if any
(unless (zerop *error-count*)
  (fatal (format nil "%n%d asserts failed%n" *error-count*)))
