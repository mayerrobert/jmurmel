;;;; Tests for core Murmel

;;; This file is valid Common Lisp as well as Murmel
;;; (doesn't need mlib). Some #+/#- feature expressions
;;; are needed, tough.
;;;
;;; It can be run with e.g. sbcl or abcl to test the tests
;;; and after that with jmurmel to test jmurmel.
;;;
;;; Usage:
;;;
;;;     scbl --script murmel-test.lisp
;;;     abcl --batch --load samples\murmel-test.lisp
;;;
;;;     java -jar jmurmel.jar murmel-test.lisp
;;;
;;; or using the compiler:
;;;
;;;     java -jar jmurmel.jar --jar murmel-test.lisp
;;;     java -cp jmurmel.jar;a.jar MurmelProgram
;;;

#+murmel
(defun terpri () (writeln))


;;; Test "framework":
;;;
;;; - A list to hold the failure count
;;;   (a list is used to hold results because compiled Murmel
;;;    doesn't support setq yet).
;;; - The macro "deftest" to define test-name, test-form and expected result.

#+murmel (define result '(0 . 0))
#-murmel (defparameter result '(0 . 0))

(defun inc-failed ()
  (rplaca result (1+ (car result))))

(defun inc-count ()
  (rplacd result (1+ (cdr result))))

(defun tequal (a b)
  (if (eql a b)
        t
    (if (stringp a)
          (if (stringp b)
                (string= a b)
            nil)
      (if (consp a)
            (if (consp b)
                  (if (tequal (car a) (car b))
                        (tequal (cdr a) (cdr b))
                    nil)
              nil)
        nil))))


(defmacro deftest (name form expected-result)
  (let ((result (gensym)))
    `(let ((,result ,form))
       (inc-count)
       #-murmel
       (if (equal ,result ,expected-result) nil
         (progn (inc-failed) (write ',name) (format t " equal test failed") (terpri)))
       (if (tequal ,result ,expected-result) nil
         (progn (inc-failed) (write ',name) (format t " tequal test failed") (terpri))))))



;;; Test the test-framework

(deftest tequal.1 0 0)
(deftest tequal.2 1 1)
(deftest tequal.3 -1 -1)

(deftest tequal.4 0.0 0.0)
(deftest tequal.5 -0.0 -0.0)
(deftest tequal.6 3.4 3.4)

(deftest tequal.7 nil nil)
(deftest tequal.8 '(a) '(a))
(deftest tequal.9 '(a (b)) '(a (b)))
(deftest tequal.10
  (tequal '(a (b)) '(a (c)))
  nil
)


;;; Tests

;;; test eql
(deftest eql.1 (eql 'a 'b)  nil)
(deftest eql.2 (eql 'a 'a)  t)
(deftest eql.3 (eql 3 3)  t)
(deftest eql.4 (eql 3 3.0)  nil)
(deftest eql.5 (eql 3.0 3.0)  t)
;(deftest eql.6 (eql #c(3 -4) #c(3 -4))  t)
;(deftest eql.7 (eql #c(3 -4.0) #c(3 -4))  false)
(deftest eql.8 (eql (cons 'a 'b) (cons 'a 'c))  nil)
(deftest eql.9 (eql (cons 'a 'b) (cons 'a 'b))  nil)
(deftest eql.10 (eql '(a . b) '(a . b))  nil)
(#+murmel define #-murmel defparameter x nil)
(deftest eql.11 (progn (setq x (cons 'a 'b)) (eql x x))  t)
(deftest eql.12 (progn (setq x '(a . b)) (eql x x))  t)
;(deftest eql.13 (eql #\A #\A)  true)
(deftest eql.14 (eql "Foo" "Foo")  nil)
;(deftest eql.15 (eql "Foo" (copy-seq "Foo"))  false)
(deftest eql.16 (eql "FOO" "foo")  nil)


;;; test number comparison operators
(deftest test-numbereq.1
  (= 2 2)
  t
)

(deftest test-numbereq.2
  (= 2 3)
  nil
)

(deftest test-numbereq.3
  (= 1e20 1e20)
  t
)


;;; test +
(deftest test-add
  (+ -0.0 -0.0)
  -0.0
)


;;; Print summary
(write (car result)) (format t "/") (write (cdr result)) (format t " test(s) failed")
(terpri)
(if (= 0 (car result))
      (format t "Success.")
  (format t "Failure."))
(terpri)

#+murmel (if (> (car result) 0) (fatal (format nil "%d/%d errors" (car result) (cdr result))))