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
;;;     sbcl --script murmel-test.lisp
;;;     abcl --batch --load murmel-test.lisp
;;;
;;;     java -jar jmurmel.jar murmel-test.lisp
;;;
;;; or using the compiler:
;;;
;;;     java -jar jmurmel.jar --run murmel-test.lisp
;;;

#-murmel (defun writeln () (terpri))
#-murmel (defmacro define (n v) `(defparameter ,n ,v))


;;; Test "framework":
;;;
;;; - Global variables to hold test count and failure count.
;;; - The macro "deftest" to define test-name, test-form and expected result.

(define *failed* 0)
(define *count* 0)

(defun inc-failed ()
  (setq *failed* (1+ *failed*)))

(defun inc-count ()
  (setq *count* (1+ *count*)))

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
         (progn (inc-failed) (write ',name) (format t " equal test failed") (writeln)))
       (if (tequal ,result ,expected-result) nil
         (progn (inc-failed) (write ',name) (format t " tequal test failed") (writeln))))))



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

(define *a* nil)
(define *b* nil)
(define *c* nil)


;;; test setq
(deftest setq.global (setq *a* 1) 1)
(deftest setq.param (#-murmel funcall (lambda (a) (setq a 3)) 1) 3)
(deftest setq.local (let ((a 1)) (setq a 3)) 3)


;;; test lambda
#+murmel (deftest lambda.1 ((lambda nil)) nil)


;;; test letXX
(deftest let.1 (let () 1) 1)
(deftest let.2 (let* () 2) 2)
#+murmel (deftest let.3 (letrec () 3) 3)


;;; test named letXX
#+murmel (deftest namedlet.1 (let loop () (if nil (loop)) 2) 2)
#+murmel (deftest namedlet.2 (let* loop () (if nil (loop)) 2) 2)
#+murmel (deftest namedlet.3 (letrec loop () (if nil (loop)) 2) 2)


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
; todo (deftest eql.11 (progn (setq x (cons 'a 'b)) (eql x x))  t)
; todo (deftest eql.12 (progn (setq x '(a . b)) (eql x x))  t)
;(deftest eql.13 (eql #\A #\A)  true)
#+murmel (deftest eql.14 (eql "Foo" "Foo")  t) ; sbcl murmel-test.lisp -> nil, sbcl murmel-test.fasl -> t
;(deftest eql.15 (eql "Foo" (copy-seq "Foo"))  false)
(deftest eql.16 (eql "FOO" "foo")  nil)

(deftest eql.17 (eql -0 -0) t)
(deftest eql.18 (eql -0 0) t)
(deftest eql.18 (eql -0.0 -0) nil)

(deftest eql.17 (eql -0.0 -0.0) t)
(deftest eql.18 (eql -0.0 0.0) nil)


;;; test number comparison operators
(deftest test-numbereq.1  (= 2 2)       t)
(deftest test-numbereq.2  (= 2 3)       nil)
(deftest test-numbereq.3  (= 1e20 1e20) t)

(deftest test-numbereq.4 (= -0 -0)     t)
(deftest test-numbereq.5 (= -0 0)      t)
(deftest test-numbereq.6 (= -0.0 -0)   t)

(deftest test-numbereq.7 (= -0.0 -0.0) t)
(deftest test-numbereq.8 (= -0.0 0.0)  t)


;;; test +
(deftest test-add-minus-zero
  (+ -0.0 -0.0)
  -0.0
)


;;; Murmel-only tests for various not-a-numbers.
;;; In Common division by zero is signalled as a condition.
#+murmel
(let ((nan (/ 0 0))     ; NaN, not-a-number
      (ninf (/ -1 0))   ; -Infinity, negative infinity
      (pinf (/ 1 0)))   ; Infinity, positive infinity

  (deftest inf.lt (< ninf -1.0 0.0 pinf) t)
  
  (deftest inf.add1  (+ ninf ninf) ninf)
  (deftest inf.add2  (+ ninf -1.0) ninf)
  (deftest inf.add3  (+ ninf  0.0) ninf)
  (deftest inf.add4  (+ ninf  1.0) ninf)
  (deftest inf.add5  (+ ninf pinf)  nan)
  (deftest inf.add6  (+ ninf  nan)  nan)

  (deftest inf.add7  (+ pinf ninf)  nan)
  (deftest inf.add8  (+ pinf -1.0) pinf)
  (deftest inf.add9  (+ pinf  0.0) pinf)
  (deftest inf.add10 (+ pinf  1.0) pinf)
  (deftest inf.add11 (+ pinf pinf) pinf)
  (deftest inf.add11 (+ pinf  nan)  nan)

  (deftest inf.sub1  (- ninf ninf)  nan)
  (deftest inf.sub2  (- ninf -1.0) ninf)
  (deftest inf.sub3  (- ninf  0.0) ninf)
  (deftest inf.sub4  (- ninf  1.0) ninf)
  (deftest inf.sub5  (- ninf pinf) ninf)
  (deftest inf.sub6  (- ninf  nan)  nan)

  (deftest inf.sub7  (- pinf ninf) pinf)
  (deftest inf.sub8  (- pinf -1.0) pinf)
  (deftest inf.sub9  (- pinf  0.0) pinf)
  (deftest inf.sub10 (- pinf  1.0) pinf)
  (deftest inf.sub11 (- pinf pinf)  nan)
  (deftest inf.sub12 (- pinf  nan)  nan)

  (deftest nan.1 (= nan nan) nil)
  (deftest nan.2 (< nan nan) nil)
  (deftest nan.3 (> nan nan) nil)
  (deftest nan.4 (/= nan nan) t)
)


;;; Print summary
(write *failed*) (format t "/") (write *count*) (format t " test(s) failed")
(writeln)
(if (= 0 *failed*)
      (format t "Success.")
  (format t "Failure."))

#+murmel (if (> *failed* 0) (fatal (format nil "%n%d/%d errors" *failed* *count*)))