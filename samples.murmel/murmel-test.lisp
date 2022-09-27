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
#-murmel (defun assq (key alist) (assoc key alist :test #'eq))


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


(defun assert-equal (expected-result result msg)
  (inc-count)
  #-murmel
  (if (equal result expected-result) nil
    (progn
      (write msg)
      (format t " equal test failed, expected '~A', got unexpected result '~A'~%" expected-result result)))

  (if (tequal result expected-result) nil
    (progn
      (inc-failed)
      (write msg)
      #+murmel (format t " tequal test failed, expected '%s', got unexpected result '%s'%n" expected-result result)
      #-murmel (format t " tequal test failed, expected '~A', got unexpected result '~A'~%" expected-result result))))


(defmacro deftest (name form expected-result)
  (let ((result (gensym)))
    `(let ((,result ,form))
       (assert-equal ,expected-result ,result ',name))))


; a varargs function that echoes all arguments as a list.
; Useful to check for invalid Java code emission related to varargs.
(defun echo #+murmel x
            #-murmel (&rest x)
  x)


;;; Test the test-framework

(deftest tequal.1 0 0)
(deftest tequal.2 1 1)
(deftest tequal.3 -1 -1)

(deftest tequal.4 0.0 0.0)
(deftest tequal.5 -0.0 -0.0)
(deftest tequal.6 3.4 3.4)

(deftest tequal.7 nil  nil)
(deftest tequal.8 '(a)  '(a))
(deftest tequal.9 '(a (b))  '(a (b)))
(deftest tequal.10 (tequal '(a (b)) '(a (c)))  nil)

(deftest test-echo.1 (echo) nil)
(deftest test-echo.2 (echo nil) '(nil))
(deftest test-echo.3 (echo 1 2 3) '(1 2 3))


;;; test reader
(deftest reader.1 1 1)
(deftest reader.2 '(1 . 2) '(1 . 2))
(deftest reader.3 '(1 2 3 4 5) '(1 2 3 4 5))
(deftest reader.4 '(1 2 3 4 . 5) '(1 2 3 4 . 5))
(deftest reader.5 (echo '()) '(nil))

(deftest reader.6 '\+1                    '|+1|)
(deftest reader.7 '+\1                    '|+1|)
(deftest reader.8 'APL\\360               '|APL\\360|)
(deftest reader.9 '|APL\360|              'APL360)
(deftest reader.10 '\(\b^2\)\ -\ 4*\a*\c  '|(b^2) - 4*a*c|)

(deftest readermacro.1 #\a #\a)
(deftest readermacro.2 (char-code #\Nul) 0)
#+murmel (deftest readermacro.3 (char-code #\200) 200)

#+murmel
(deftest feature.1 #+(and murmel jvm) 'murmel-jvm 'murmel-jvm)
(deftest feature.2 #+(or sbcl (and murmel jvm)) 'sbcl-or-murmel-jvm 'sbcl-or-murmel-jvm)
(deftest feature.3 #+(not (or abcl sbcl murmel (not murmel))) 'should-ignore 'abcl-sbcl-or-murmel-or-notmurmel 'abcl-sbcl-or-murmel-or-notmurmel)


;;; Tests for core Murmel w/o mlib


;;; basic special forms: quote, lambda

;;; test lambda
(deftest lambda.1 (#-murmel funcall (lambda nil)) nil)


;;; Additional special forms: define, defun, defmacro, setq, let, multiple-value-bind, multiple-value-call, if, progn, cond, labels, load, require, provide
;;; todo labels, load, require, provide

;;; test define
(define *a* nil)
(define *b* nil)
(define *c* nil)


;;; test setq
(deftest setq.global   (setq *a* 1) 1)
(deftest setq.global.2 (setq *a* 11 *b* 22 *c* 33) 33)
(deftest setq.param    (#-murmel funcall (lambda (a) (setq a 3)) 1) 3)
(deftest setq.local    (let ((a 1)) (setq a 3)) 3)


;;; test let, let*, letrec
; no bindings, in CL a let w/o bindings is malformed
#+murmel (deftest let.1 (echo (let)) '(nil))
#+murmel (deftest let.2 (echo (let*)) '(nil))
#+murmel (deftest let.3 (echo (letrec)) '(nil))

(deftest let.4 (echo (let nil)) '(nil))
(deftest let.5 (echo (let* nil)) '(nil))
#+murmel (deftest let.6 (echo (letrec nil)) '(nil))

(deftest let.7 (let () (1+ 1)) 2)
(deftest let.8 (let* () (1+ 2)) 3)
#+murmel (deftest let.9 (letrec () (1+ 3)) 4)

(deftest let.10 (let (a) (list a)) '(nil))
(deftest let.11 (let* (a) (list a)) '(nil))
#+murmel (deftest let.12 (letrec (a) (list a)) '(nil))

(deftest let.13 (let ((a 1) b) (list b a)) '(nil 1))
(deftest let.14 (let* ((a 1) b) (list b a)) '(nil 1))
#+murmel (deftest let.15 (letrec ((a 1) b) (list b a)) '(nil 1))


;;; test named let, let*, letrec
#+murmel
(progn
(deftest namedlet.1 (let loop () (if nil (loop)) (1+ 1)) 2)
(deftest namedlet.2 (let* loop () (if nil (loop)) (1+ 1)) 2)
(deftest namedlet.3 (letrec loop () (if nil (loop)) (1+ 1)) 2)
(deftest namedlet.4 (letrec loop ((aaa 3) bbb)
                      (if (> aaa 1)
                            (loop (1- aaa) 1)
                        (+ aaa bbb)))
                    2.0)

(deftest namedlet.5
         (let loop ((a 3) (b 1)) (list a (if (= 0 a) b (loop (1- a) (1+ b))) a b))
         '(3 (2 (1 (0 4 0 4) 1 3) 2 2) 3 1))

(deftest namedlet.6
         (let* loop ((a 3) (b 1)) (list a (if (= 0 a) b (loop (1- a) (1+ b))) a b))
         '(3 (2 (1 (0 4 0 4) 1 3) 2 2) 3 1))

(deftest namedlet.7
         (letrec loop ((a 3) (b 1)) (list a (if (= 0 a) b (loop (1- a) (1+ b))) a b))
         '(3 (2 (1 (0 4 0 4) 1 3) 2 2) 3 1))
)


;;; test let dynamic
(setq *a* 1 *b* 2 *c* 3)
(defun globals-as-list ()
  (list *a* *b* *c*)) 

(deftest letdynamic.1
  (append (let #+murmel dynamic ((*a* 123) (*b* *a*) (*c* (1+ *c*))) (globals-as-list))
          (list *a* *b* *c*))
  '(123 1 4 1 2 3))

; changes to globals in a let dynamic form will be undone
(deftest letdynamic.2
  (append (let #+murmel dynamic ((*a* 123) (*b* *a*) (*c* (1+ *c*)))
            (append (globals-as-list)
                    (setq *a* 1111 *b* 2222 *c* nil)))
          (list *a* *b* *c*))
  '(123 1 4 1 2 3))

; bind globals and locals
(deftest letdynamic.3
  (let #+murmel dynamic ((*a* 11) (*b* 22) (*c* 33) (b 2)) (append (globals-as-list) b))
  '(11 22 33 . 2))


;;; test let* dynamic
(deftest let*dynamic.1
  (append (let* #+murmel dynamic ((*a* 123) (*b* 456) (*c* 789)) (globals-as-list))
          (list *a* *b* *c*))
  '(123 456 789 1 2 3))

(deftest let*dynamic.2
  (append (let* #+murmel dynamic ((*a* 123) (*a* 456) (*a* 789)) (globals-as-list))
            (list *a* *b* *c*))
    '(789 2 3 1 2 3))

(deftest let*dynamic.3
  (append (let* #+murmel dynamic ((*a* 123) (*a* 456) (*b* *a*)) (globals-as-list))
            (list *a* *b* *c*))
    '(456 456 3 1 2 3))


;;; test if
(deftest if-number.1
  (if 1 'yes)  'yes)

(deftest if-number.2
  (if 1.0 'yes)  'yes)

(deftest if-char.1
  (if #\1 'yes)  'yes)


;;; test cond
(deftest cond.1
  (cond) nil)

(deftest cond.2
  (cond ((null nil) 'yes)) 'yes)

(deftest cond.2
  (cond ((null (null nil)) 'yes)) nil)

(deftest cond.2
  (cond ((null 1) 'yes)) nil)

(deftest cond.2
  (cond ((null (null 1)) 'yes)) 'yes)


;;; let over lambda
#+murmel
(progn
  (define f (let ((ctr 0)) (lambda () (setq ctr (1+ ctr)))))
  (deftest closure.1 (list (f) (f) (f)) '(1 2 3))
)


;;; catch, throw
(deftest catch.1 (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4)  3)
(deftest catch.2 (catch 'dummy-tag 1 2 3 4)                     4)
(defun throw-back (tag) (throw tag t)) ; =>  THROW-BACK
(deftest catch.3 (catch 'dummy-tag (throw-back 'dummy-tag) 2)   t)
(deftest catch.4 (catch 'c
                   (labels ((c1 () (throw 'c 1)))
                     (catch 'c (c1) (write 'unreachable))
                     2))  2)


;;; unwind-protect
(setq *a* 0)
(deftest unwind-protect.1 (unwind-protect 1 2)          1)
(deftest unwind-protect.2 (unwind-protect 1 2 3 4 5)    1)
(deftest unwind-protect.3 (unwind-protect 1 (setq *a* (1+ *a*)) (setq *a* (1+ *a*)))    1)
(deftest unwind-protect.3a *a* 2)
(deftest unwind-protect.4 (catch 'tag (unwind-protect
                                        (unwind-protect
                                          (unwind-protect
                                            'result
                                            (throw 'tag "inner"))
                                          (throw 'tag "middle"))
                                        (throw 'tag "outer")))
                          "outer")

(deftest unwind-protect.5  (catch nil 
                             (unwind-protect (throw nil 1)
                               (throw nil 2)))  2)


;;; values
(deftest values.1 (values) nil)                   ; primary value is nil
(deftest values.2 (values 1 2 3) 1)               ; secondary values are discarded
(deftest values.3 (values (values 1 2 3) 4 5) 1)  ; secondary values are discarded


;;; multiple-value-bind
(deftest mvb.1  (multiple-value-bind nil nil) nil)
(deftest mvb.2  (multiple-value-bind (a b c) nil) nil)
(deftest mvb.3  (multiple-value-bind (a b c) (values 1 2 3)) nil)
(deftest mvb.4  (multiple-value-bind (a b) (values 1 2) (echo a b)) '(1 2))
(deftest mvb.5  (multiple-value-bind (a b) (values 1 2 3 4) (echo a b)) '(1 2))
(deftest mvb.6  (multiple-value-bind (a b c d) (values 1 2) (echo a b c d)) '(1 2 nil nil))
(deftest mvb.7  (multiple-value-bind (a b c d) 11 (echo a b c d)) '(11 nil nil nil))

#+murmel (deftest mvb.8  (multiple-value-bind (a b . c) (values 1 2 3 4 5) (echo a b c)) '(1 2 (3 4 5)))
#+murmel (deftest mvb.9  (multiple-value-bind (a b . c) (values 1) (echo a b c)) '(1 nil nil))


;;; multiple-value-call
(deftest mvc.1 (multiple-value-call #'+) #+murmel 0.0 #-murmel 0)
(deftest mvc.2 (multiple-value-call #'+ 1.0 2.0 3.0) 6.0)
(deftest mvc.3 (multiple-value-call #'+ (values 1.0 2.0 3.0)) 6.0)
(deftest mvc.4 (multiple-value-call #'+ (values 1 2) 3.0 (values 4 5)) 15.0)
(deftest mvc.5 (multiple-value-call (lambda (a b #+murmel . #-murmel &rest c) (list* a b c)) 1 (values 2 3 4 5)) '(1 2 3 4 5))
(deftest mvc.6 (multiple-value-call #'+ 1.0 2 3 (values) 4) 10.0)


;;; test higher order functions
; from https://norvig.com/lispy2.html
(defun combine (f)
  (lambda (x y)
    (if (null x) nil
      (#-murmel funcall f (list (car x) (car y))
         (#-murmel funcall (combine f) (cdr x) (cdr y))))))

#+murmel (define zip (combine cons))
#-murmel (setf (symbol-function 'zip) (combine #'cons))

(deftest higher-order.1
  (zip (list 1 2 3 4) (list 5 6 7 8))   '((1 5) (2 6) (3 7) (4 8)))


;;; Primitives
;;; todo remaining primitives

;;; test rplaca
#+murmel (progn  ; sbcl stackoverflows on these
(define *l* (list 1 2 3 4 5))
(deftest rplaca.1 (format nil "%s" (rplaca (cdr *l*) *l*)) "((1 #<this list> 3 4 5) 3 4 5)")
(deftest rplaca.2 (format nil "%s" *l*) "(1 #<this list> 3 4 5)")

; test modifying the varargs parameter which in compiled code is different from a regular ConsCell based list
(defun func l
  (rplaca (cdr l) l)
  (format nil "%s" l))
(deftest rplaca.3 (func 11 22 33 44 55) "(11 #<this list> 33 44 55)")
)


;;; test eval
; when running compiled murmel `eval` starts the embedded interpreter,
; and e.g. `(eval '(lambda () ...` returns an interpreted closure.
; So these tests additionally check if compiled code can run interpreted lambdas.
(define x (eval '(lambda () '|hello from interpreter|)))

#+murmel
(deftest eval.1 (x) '|hello from interpreter|)
(deftest eval.2 (#-murmel funcall x) '|hello from interpreter|)

#+murmel
(deftest eval.3 ((eval '(lambda (x) (format nil "%s" x))) '|interpreted format|) "interpreted format")
(deftest eval.4 (#-murmel funcall (eval '(lambda (x) (format nil #+murmel "%s" #-murmel "~A" x))) '|interpreted format|) "interpreted format")

; invoke x in the tailposition. This used to break the compiler.
(deftest eval.5
  (let (a) (#-murmel funcall x)) '|hello from interpreter|)


;;; test apply
#+murmel
(deftest apply.1 (apply + '(1.0 2.0))           3.0)
(deftest apply.2 (apply #'+ '(1.0 2.0))         3.0)
(deftest apply.3 (apply '+ '(1.0 2.0))          3.0)

#+murmel
(deftest apply.4 (apply apply '(+ (1.0 2.0)))   3.0)
(deftest apply.5 (apply 'apply '(+ (1.0 2.0)))  3.0)
(deftest apply.6 (apply #'apply '(+ (1.0 2.0))) 3.0)

#+murmel
(deftest apply.7 (apply apply '(apply (+ (1.0 2.0))))   3.0)
(deftest apply.8 (apply #'apply '(apply (+ (1.0 2.0)))) 3.0)
(deftest apply.9 (apply 'apply '(apply (+ (1.0 2.0))))  3.0)

#+murmel
(deftest apply.10 (apply ((lambda () +)) '(1.0 2.0))    3.0)
(deftest apply.11 (apply ((lambda () '+)) '(1.0 2.0))   3.0)
(deftest apply.12 (apply ((lambda () #'+)) '(1.0 2.0))    3.0)

; the following not valid CL, doesn't work in Murmel either
;(setq *a* 1.0 *b* 2.0 *c* '(*a* *b*))
;(deftest apply.13 (apply '+ *c*)   3.0)


;;; test null
(deftest null.1 (null nil) t)
(deftest null.2 (null 'a-symbol) nil)
(deftest null.3 (null 3) nil)


;;; test all predicates
(define *predicates*
  '(("n/a"   "null"    "atom"      "symbolp"   "consp"     "listp"     "numberp"   "integerp"  "floatp"   "characterp" "vectorp"    "stringp"   "simple-bit-vector-p")
    (value    null      atom        symbolp     consp       listp       numberp     integerp    floatp     characterp   vectorp     stringp     simple-bit-vector-p)
    (nil      t         t           t           nil         t           nil         nil         nil        nil          nil         nil         nil)
    ((a . b)  nil       nil         nil         t           t           nil         nil         nil        nil          nil         nil         nil)

    (a        nil       t           t           nil         nil         nil         nil         nil        nil          nil         nil         nil)
    (\123     nil       t           t           nil         nil         nil         nil         nil        nil          nil         nil         nil)
    (1\23     nil       t           t           nil         nil         nil         nil         nil        nil          nil         nil         nil)
    ; sbcl chokes on the next line even when it's prepended with #+murmel
    ;(1\23"    nil       t           t           nil         nil         nil         nil         nil        nil          nil         nil        nil)

    (0        nil       t           nil         nil         nil         t           t           nil        nil          nil         nil         nil)
    (2.3      nil       t           nil         nil         nil         t           nil         t          nil          nil         nil         nil)
    (3.2e15   nil       t           nil         nil         nil         t           nil         t          nil          nil         nil         nil)
    (#\a      nil       t           nil         nil         nil         nil         nil         nil        t            nil         nil         nil)
    (\#\a     nil       t           t           nil         nil         nil         nil         nil        nil          nil         nil         nil)
    (\#a      nil       t           t           nil         nil         nil         nil         nil        nil          nil         nil         nil)
    ("hi"     nil       t           nil         nil         nil         nil         nil         nil        nil          t           t           nil)
    (#()      nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         nil)
    (#*       nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         t)
))


(let ((predicate-names (car *predicates*))
      (predicates (car (cdr *predicates*))))
  (labels ((do-one-test (predicate-names predicates value expected-results)
             (let* ((name (car predicate-names))
                    (predicate (car predicates))
                    (expected (car expected-results))
                    (actual (apply predicate (list value))))
               #+murmel (assert-equal expected actual (format nil "(%s %s)" name value))

               #-murmel (assert-equal expected actual (format nil "(~A ~A)" name value))

               (if (cdr predicate-names)
                 (do-one-test (cdr predicate-names) (cdr predicates) value (cdr expected-results)))))

           (do-all-tests (test-descriptors)
             (do-one-test (cdr predicate-names) (cdr predicates) (car (car test-descriptors)) (cdr (car test-descriptors)))
             (if (cdr test-descriptors)
               (do-all-tests (cdr test-descriptors)))))
    (do-all-tests (cdr (cdr *predicates*)))))


;;; test eq
(deftest eq.1 (eq 'a 'a)   t)
(deftest eq.2 (eq nil nil) t)
(deftest eq.3 (eq 'a 1)    nil)
(deftest eq.3 (eq 1 1.0)   nil)


;;; test eql
(deftest eql.1 (eql 'a 'b)  nil)
(deftest eql.2 (eql 'a 'a)  t)
(deftest eql.3 (eql 3 3)  t)
(deftest eql.4 (eql 3 3.0)  nil)
(deftest eql.5 (eql 3.0 3.0)  t)
;(deftest eql.6 (eql #c(3 -4) #c(3 -4))  t)
;(deftest eql.7 (eql #c(3 -4.0) #c(3 -4))  nil)
(deftest eql.8 (eql (cons 'a 'b) (cons 'a 'c))  nil)
(deftest eql.9 (eql (cons 'a 'b) (cons 'a 'b))  nil)

;;; Murmel: interpreted eql.10 is nil, compiled eql.10 is t because quoted cons cells are coalesced
;;; SBCL: in the repl eql.10 is nil, when compiled eql.10 is t
;;; See: "Issue QUOTE-SEMANTICS Writeup" http://www.lispworks.com/documentation/HyperSpec/Issues/iss282_w.htm
#+(or)
(deftest eql.10 (eql '(a . b) '(a . b))  nil)

(deftest eql.11 (progn (setq x (cons 'a 'b)) (eql x x))  t)
(deftest eql.12 (progn (setq x '(a . b)) (eql x x))  t)
(deftest eql.13 (eql #\A #\A)  t)
#+murmel (deftest eql.14 (eql "Foo" "Foo")  t) ; sbcl murmel-test.lisp -> nil, sbcl murmel-test.fasl -> t
;(deftest eql.15 (eql "Foo" (copy-seq "Foo"))  nil)
(deftest eql.16 (eql "FOO" "foo")  nil)

(deftest eql.17 (eql -0 -0) t)
(deftest eql.18 (eql -0 0) t)
(deftest eql.18 (eql -0.0 -0) nil)

(deftest eql.17 (eql -0.0 -0.0) t)
(deftest eql.18 (eql -0.0 0.0) nil)


;;; test list
(deftest list.1 (list) nil)
(deftest list.2 (list (list))  '(nil))
(deftest list.3 (echo (list))  '(nil))
(deftest list.4 (list 1 2 3)   '(1 2 3))


;;; test list*
(deftest list*.1 (list* nil)        nil)
(deftest list*.1 (list* 1)          1)
(deftest list*.2 (list* (list))     nil)
(deftest list*.3 (list* 1 2 )       '(1 . 2))
(deftest list*.3 (list* 1 2 3 4 5)  '(1 2 3 4 . 5))


;;; test append
(deftest append.1 (append)                   nil)
(deftest append.2 (append nil)               nil)
(deftest append.3 (append nil '(1 2 3))      '(1 2 3))
(deftest append.4 (append nil '(1 2 3) nil)  '(1 2 3))
(deftest append.5 (append nil '(1 2 3) nil '(4 5 6))  '(1 2 3 4 5 6))
(deftest append.6 (append nil '(1 2 3) nil '(4 5 6))  '(1 2 3 4 5 6))


;;; test assq
(deftest assq.1 (assq 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4)))  '(a-key 3))
(deftest assq.2 (assq nil '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))   '(nil 2))


;;; test assoc
(deftest assoc.1 (assoc 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4)))  '(a-key 3))
(deftest assoc.2 (assoc nil '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))   '(nil 2))


;;; test number comparison operators
(deftest test-numbereq.1  (= 2 2)       t)
(deftest test-numbereq.2  (= 2 3)       nil)
(deftest test-numbereq.3  (= 1e20 1e20) t)

(deftest test-numbereq.4 (= -0 -0)     t)
(deftest test-numbereq.5 (= -0 0)      t)
(deftest test-numbereq.6 (= -0.0 -0)   t)

(deftest test-numbereq.7 (= -0.0 -0.0) t)
(deftest test-numbereq.8 (= -0.0 0.0)  t)

(deftest test-numbereq.9  (= 1 1 1 1 1)  t)
(deftest test-numbereq.10 (= 1 1 1 1 2)  nil)
(deftest test-numbereq.11 (= 1 1.0 1e0)  t)
(deftest test-numbereq.12 (= 1 1.0 1e1)  nil)

(deftest test-numbereq.13 (= 1)  t)
(deftest test-numbereq.14 (= -0.0)  t)

(deftest test-ne.1  (/= 1 2 3)  t)
(deftest test-ne.2  (/= 1 2 2)  nil)
(deftest test-ne.2  (/= 1 2 2.0)  nil)


;;; test +
(deftest test-add-minus-zero (+ -0.0 -0.0)  -0.0)


;;; test mod, rem
(deftest test-mod-rem
  (list (mod -3.0 -2) (rem -3 -2.0) (mod 3.0 -2) (rem 3 -2.0))
    '(-1.0 -1.0 -1.0 1.0))


;;; test floor
(deftest floor.1 (floor 5.3) 5)
(deftest floor.2 (floor 5.3 2) 2)


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



#+murmel
(let (
      (byte          ((jmethod "Byte"                    "new" "String") "1"))
      (short         ((jmethod "Short"                   "new" "String") "1"))
      (integer       ((jmethod "Integer"                 "new" "String") "1"))
      (long          ((jmethod "Long"                    "new" "String") "1"))
      (bigInteger    ((jmethod "java.math.BigInteger"    "new" "String") "1"))

      (float         ((jmethod "Float"                   "new" "String") "1"))
      (double        ((jmethod "Double"                  "new" "String") "1"))
      (bigDecimal    ((jmethod "java.math.BigDecimal"    "new" "String") "1"))

      (arrayList     ((jmethod "java.util.ArrayList"     "new")))
      (bitSet        ((jmethod "java.util.BitSet"        "new")))

      (string        ((jmethod "java.lang.String"        "new")))
      (stringBuffer  ((jmethod "java.lang.StringBuffer"  "new")))
      (stringBuilder ((jmethod "java.lang.StringBuilder" "new")))
     )

  (deftest ffi.number.1 (numberp byte) t)
  (deftest ffi.number.2 (numberp short) t)
  (deftest ffi.number.3 (numberp integer) t)
  (deftest ffi.number.4 (numberp long) t)
  (deftest ffi.number.5 (numberp bigInteger) t)

  (deftest ffi.number.6 (numberp float) t)
  (deftest ffi.number.7 (numberp double) t)
  (deftest ffi.number.8 (numberp bigDecimal) t)

  (deftest ffi.coll.1 (vectorp arrayList) t)
  ; todo (deftest ffi.coll.2 (vectorp bitSet) t)

  (deftest ffi.string.1 (vectorp string) t)
  (deftest ffi.string.2 (vectorp stringBuffer) t)
  (deftest ffi.string.3 (vectorp stringBuilder) t)
)


;;; Print summary
(write *failed*) (format t "/") (write *count*) (format t " test(s) failed")
(writeln)
(if (= 0 *failed*)
      (format t "Success.")
  (format t "Failure."))

#+murmel (if (> *failed* 0) (fatal (format nil "%n%d/%d errors" *failed* *count*)))