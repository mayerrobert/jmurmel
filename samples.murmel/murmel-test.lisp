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


; *******************************************************************
;;; Emulate some stuff so that this file can be run with e.g. sbcl

#-murmel (progn

(defmacro define (n v) `(defparameter ,n ,v))
(defun assq (key alist) (assoc key alist :test #'eq))
(defun writeln () (terpri))

(defun seqref (seq idx)
  (typecase seq
    (cons
     (if (> idx 0)
           (let ((c (nthcdr (1- idx) seq)))
             (cond ((consp (cdr c)) (cadr c))
                   (c (cdr c))))
       (if (= idx 0)
             (car seq)
         (error "idx must be >= 0")))) 

    (vector
     (elt seq idx))
    
    (t (error "not a nonempty sequence"))))

(defun seqset (seq idx val)
  (typecase seq
    (cons
     (if (> idx 0)
           (let ((c (nthcdr (1- idx) seq)))
             (cond ((consp (cdr c)) (cadr c))
                   (c (rplacd c val))))
       (if (= idx 0)
             (rplaca seq val)
         (error "idx must be >= 0")))) 
    (vector
     (setf (elt seq idx) val)))
     
  val)

(defun vector-copy (vec)
  (check-type vec vector)
  (copy-seq vec))

(defun vector-fill (vec item &optional start end)
  (check-type vec vector)
  (if end (fill vec item :start start :end end)
    (if start (fill vec item :start start)
      (fill vec item))))

(defmacro try (form &optional errorobj)
  (let ((ex (gensym)))
    `(handler-case ,form
                   (condition (,ex) (values ,errorobj ,ex)))))
)


; *******************************************************************
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
        (if (bit-vector-p a)
              (if (bit-vector-p b)
                    #+murmel (bv= a b)
                    #-murmel (equal a b)
                nil)
          (if (vectorp a)
                (if (vectorp b)
                      #+murmel (let ((lena (vector-length a)) (lenb (vector-length b)))
                                 (if (= lena lenb)
                                       (let loop ((i 0))
                                         (cond ((= i lena) t)
                                               ((null (tequal (seqref a i) (seqref b i))) nil)
                                               (t (loop (1+ i)))))
                                   nil)) 
                                   
                      #-murmel (equalp a b)
                  nil)
            nil))))))


(defun assert-equal (expected-result result msg)
  (inc-count)
  #-murmel
  (if (equalp result expected-result) nil
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


; *******************************************************************
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


; *******************************************************************
;;; Tests for core Murmel w/o mlib

; *******************************************************************
;;; S-Expressions, reader
#|
This is a
multiline comment
|#

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
(deftest feature.2 #+(or sbcl abcl (and murmel jvm)) 'sbcl-or-abcl-or-murmel-jvm 'sbcl-or-abcl-or-murmel-jvm)
(deftest feature.3 #+(not (or abcl sbcl murmel (not murmel))) 'should-ignore 'abcl-sbcl-or-murmel-or-notmurmel 'abcl-sbcl-or-murmel-or-notmurmel)


(deftest read.binary #b0101 5)
(deftest read.octal  #o0101 65)
(deftest read.hex    #xcafebabe 3405691582)
(deftest read.bitvector #*0101
                        #+murmel (list->bit-vector '(0 1 0 1))
                        #-murmel (make-array 4 :element-type 'bit :initial-contents '(0 1 0 1))
)

;; some of the tests "fail" on sbcl: jmurmel expands backquoted forms all at once
;; sbcl apparently defers (part of) the expansion to when it's actually needed
;; and/ or does double backticks in a different way
(deftest backquote.1  `nil nil)
(deftest backquote.2  `1 1)
#+murmel
(deftest backquote.3  ``1 ''1)
(deftest backquote.4  `x 'x)
(deftest backquote.5  (let ((x 1)) `,x) 1)

(deftest backquote.6  `(nil) '(nil))
(deftest backquote.6  `(1) '(1))
(deftest backquote.7  `(x) '(x))
(deftest backquote.8  (let* ((x 1)) `(,x) ) '(1)) 
(deftest backquote.9  (let* ((x '(1))) `(,@x) ) '(1)) 
#+murmel (deftest backquote.10  (let* ((x '(1))) `(`,,@x) ) '(1)) 
#+murmel (deftest backquote.11  (let* ((x '(1)) (y '(2))) `(`,,@x ,@y) ) '(1 2)) 
#+murmel (deftest backquote.12 (let* ((x '(1)) (y '(2))) `(`,,@x ,y) ) '(1 (2))) 

(deftest backquote
  (let ((a "A") (c "C") (d '("D" "DD")))
    `((,a b) ,c ,@d))
  '(("A" b) "C" "D" "DD"))


; *******************************************************************
;;; basic special forms: quote, lambda

;;; test lambda
(deftest lambda.1 (#-murmel funcall (lambda nil)) nil)

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


; *******************************************************************
;;; Additional special forms: define, defun, defmacro, setq, let, multiple-value-bind, multiple-value-call, if, progn, cond, labels, catch, throw, load, require, provide
;;; todo load, require, provide

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


(deftest labels
  (labels () 1) 1)


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


(defun fail #+murmel datum #-murmel (&rest datum) (apply #'error datum))
(defun fail1 () (fail "test"))

(deftest try.1 (try (values 1 2 3)) 1)
(deftest try.2 (try (fail1)) nil)
(deftest try.3 (try (fail1) 'err) 'err)
(deftest try.4 (multiple-value-bind (ret ex) (try (fail 'file-error #-murmel :pathname "xyxxy") 'err)
                 (list ret (typep ex 'file-error)))
               '(err t))


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


; *******************************************************************
;;; Primitives
;;; - basic primitives: apply and eval

;;; test eval
; when running compiled murmel `eval` starts the embedded interpreter,
; and e.g. `(eval '(lambda () ...` returns an interpreted closure.
; So these tests additionally check if compiled code can run interpreted lambdas.
(define intp (eval '(lambda () '|hello from interpreter|)))

#+murmel
(deftest eval.1 (intp) '|hello from interpreter|)
(deftest eval.2 (#-murmel funcall intp) '|hello from interpreter|)

#+murmel
(deftest eval.3 ((eval '(lambda (x) (format nil "%s" x))) '|interpreted format|) "interpreted format")
(deftest eval.4 (#-murmel funcall (eval '(lambda (x) (format nil #+murmel "%s" #-murmel "~A" x))) '|interpreted format|) "interpreted format")

; invoke x in the tailposition. This used to break the compiler.
(deftest eval.5
  (let (a) (#-murmel funcall intp)) '|hello from interpreter|)

(defun eval-helper ()
  (values 1 2))

#+murmel
(deftest eval.6
  (eval '(multiple-value-bind (a b) (f) (list a b)) (cons (cons 'f eval-helper) nil)) '(1 2))


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


; *******************************************************************
;;; - logic, predicates, program structure

;;; test null
(deftest null.1 (null nil) t)
(deftest null.2 (null 'a-symbol) nil)
(deftest null.3 (null 3) nil)


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

(define *x* nil)
(deftest eql.11 (progn (setq *x* (cons 'a 'b)) (eql *x* *x*))  t)
(deftest eql.12 (progn (setq *x* '(a . b)) (eql *x* *x*))  t)
(deftest eql.13 (eql #\A #\A)  t)
#+murmel (deftest eql.14 (eql "Foo" "Foo")  t) ; sbcl murmel-test.lisp -> nil, sbcl murmel-test.fasl -> t
;(deftest eql.15 (eql "Foo" (copy-seq "Foo"))  nil)
(deftest eql.16 (eql "FOO" "foo")  nil)

(deftest eql.17 (eql -0 -0) t)
(deftest eql.18 (eql -0 0) t)
(deftest eql.19 (eql -0.0 -0) nil)

(deftest eql.20 (eql -0.0 -0.0) t)
(deftest eql.21 (eql -0.0 0.0) nil)

#+murmel (deftest eql.22 (eql 1 ((jmethod "Integer" "valueOf" "String") "1")) t)
#+murmel (deftest eql.23 (eql 1 ((jmethod "java.math.BigInteger" "new" "String") "1")) t)


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
    (#(0 1)   nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         nil)
    (#*       nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         t)
    (#*01     nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         t)
))

(define *predicates-for-vector*
  (list
    (list "n/a"   "null"    "atom"      "symbolp"   "consp"     "listp"     "numberp"   "integerp"  "floatp"   "characterp" "vectorp"    "stringp"   "simple-bit-vector-p"   "adjustable-array-p")
    (list 'value   #'null   #'atom      #'symbolp   #'consp     #'listp     #'numberp   #'integerp  #'floatp   #'characterp #'vectorp    #'stringp   #'simple-bit-vector-p   #'adjustable-array-p)
    (list "hi"     nil       t           nil         nil         nil         nil         nil         nil        nil          t           t           nil                     nil)
    (list #()      nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         nil                     nil)
    (list #(0 1)   nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         nil                     nil)
    (list #*       nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         t                       nil)
    (list #*01     nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         t                       nil)

    (list (make-array 3)
                   nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         nil                     nil)
    (list (make-array 3 #-murmel :element-type t #-murmel :adjustable t)
                   nil       t           nil         nil         nil         nil         nil         nil        nil          t           nil         nil                     t)
))

(defun test-predicates (preds)
  (let ((predicate-names (car preds))
        (predicates (car (cdr preds))))
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
      (do-all-tests (cdr (cdr preds))))))

(test-predicates *predicates*)
(test-predicates *predicates-for-vector*)

(deftest functionp.1 (functionp #'write) t)
(deftest functionp.1 (functionp 1) nil)


; *******************************************************************
;;; - conses and lists

;;; test car, cdr
#+murmel (progn
(deftest car.string (car "123") #\1)
(deftest cdr.string (cdr "123") "23")
)


;;; test rplaca, rplacd
(define *some-list* (list* 'one 'two 'three 'four)) ; =>  *some-list*
(deftest rplac.1 *some-list*                             '(ONE TWO THREE . FOUR))
(deftest rplac.2 (rplaca *some-list* 'uno)               '(UNO TWO THREE . FOUR))
(deftest rplac.3 *some-list*                             '(UNO TWO THREE . FOUR))
(deftest rplac.4 (rplacd (cdr (cdr *some-list*))
                         (list 'IV))                     '(THREE IV))
(deftest rplac.5 *some-list*                             '(UNO TWO THREE IV))

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
(deftest assq.1 (assq 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4)))     '(a-key 3))
(deftest assq.2 (assq nil    '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))   '(nil 2))
(deftest assq.3 (assq 'key-5 '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))   nil)


;;; test assoc
(deftest assoc.1 (assoc 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4)))     '(a-key 3))
(deftest assoc.2 (assoc nil    '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))   '(nil 2))
(deftest assoc.3 (assoc 'key-5 '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))   nil)


; *******************************************************************
;;; - iteration: n/a


; *******************************************************************
;;; - places: n/a


; *******************************************************************
;;; - numbers, characters

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

(deftest test-ne.1  (/= 1 2 3)    t)
(deftest test-ne.2  (/= 1 2 2)    nil)
(deftest test-ne.3  (/= 1 2 2.0)  nil)

(deftest test-lt.1  (< 1 2 3)    t)
(deftest test-lt.2  (< 1 2 2)    nil)
(deftest test-lt.3  (< 1 2 2.0)  nil)

(deftest test-le.1  (<= 1 2 3)   t)
(deftest test-le.2  (<= 1 2 2)   t)
(deftest test-le.3  (<= 1 2 2.0) t)
(deftest test-le.4  (<= 1 2 1.9) nil)

(deftest test-ge.1  (>= 3 2 1)  t)
(deftest test-ge.2  (>= 3 2 2)  t)
(deftest test-ge.3  (>= 3 2 3)  nil)


;;; test +, *
(deftest test-add-minus-zero (+ -0.0 -0.0)  -0.0)
#+murmel (deftest test-mul.1 (*)          1.0)
#+murmel (deftest test-mul.2 (* 1)        1.0)
#+murmel (deftest test-mul.2 (* 1 2 3.0)  6.0)


;;; test mod, rem
(deftest test-mod-rem
  (list (mod -3.0 -2) (rem -3 -2.0) (mod 3.0 -2) (rem 3 -2.0))
    '(-1.0 -1.0 -1.0 1.0))

(deftest test-mod-rem.1 (* 1.0 (mod 13 4))       1.0)
(deftest test-mod-rem.2 (* 1.0 (rem 13 4))       1.0)
(deftest test-mod-rem.3 (* 1.0 (mod -13 4))      3.0)
(deftest test-mod-rem.4 (* 1.0 (rem -13 4))     -1.0)
(deftest test-mod-rem.5 (* 1.0 (mod 13 -4))     -3.0)
(deftest test-mod-rem.6 (* 1.0 (rem 13 -4))      1.0)
(deftest test-mod-rem.7 (* 1.0 (mod -13 -4))    -1.0)
(deftest test-mod-rem.8 (* 1.0 (rem -13 -4))    -1.0)


;;; test floor, truncate
(deftest floor.1 (floor    5.3) 5)
(deftest trunc.1 (truncate 5.3) 5)
(deftest floor.2 (floor    5.3 2) 2)
(deftest trunc.2 (truncate 5.3 2) 2)

(deftest floor.3 (floor -5.3)      -6)
(deftest trunc.3 (truncate -5.3)   -5)
(deftest floor.4 (floor -5.3 2)    -3)
(deftest trunc.4 (truncate -5.3 2) -2)


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


; *******************************************************************
;;; - vectors, sequences

;;; test vector-copy
(let* ((vec (vector-fill (make-array 3 #-murmel :element-type t #-murmel :adjustable t) 1))
       (copy (vector-copy vec)))
  (deftest vector-copy.1 (adjustable-array-p vec) t)
  (deftest vector-copy.2 (adjustable-array-p copy) nil))


;;; test make-array, vector-fill
(deftest vector-fill.1  (vector-fill (vector 0 0 0) 1) #(1 1 1))
(deftest vector-fill.2  (vector-fill (make-array 3) 1) #(1 1 1))
(deftest vector-fill.3  (vector-fill (make-array 3 #-murmel :element-type 'bit) 1) #*111)
(deftest vector-fill.4  (vector-fill (make-array 3 #-murmel :element-type 'character) #\1) "111")

(deftest vector-fill.5  (vector-fill (make-array 3 #-murmel :element-type t #-murmel :adjustable t) 1) #(1 1 1))
(deftest vector-fill.6  (vector-fill (make-array 3 #-murmel :element-type 'bit #-murmel :adjustable t) 1) #*111)
(deftest vector-fill.7  (vector-fill (make-array 3 #-murmel :element-type 'character #-murmel :adjustable t) #\1) "111")

(deftest vector-fill.8  (vector-fill (make-array 3 #-murmel :element-type t #-murmel :adjustable t) 1 0 3) #(1 1 1))
(deftest vector-fill.9  (vector-fill (make-array 3 #-murmel :element-type 'bit #-murmel :adjustable t) 1 0 3) #*111)
(deftest vector-fill.10 (vector-fill (make-array 3 #-murmel :element-type 'character #-murmel :adjustable t) #\1 0 3) "111")


;;; test operations on simple-vector
#+murmel
(let ((sv (vector-copy #(0 1 2))))
  (deftest simple-vector.1 (vectorp sv)              t)
  (deftest simple-vector.2 (vector-length sv)        3)
  (deftest simple-vector.3 (vector->list sv)         '(0 1 2))
  (deftest simple-vector.4 (simple-vector-p sv)      t)

  (deftest simple-vector.5 (seqref #(0 1 2 3) 2)           2)
  (deftest simple-vector.6 (seqref #(0 1 2 3) 3)           3)
  (deftest simple-vector.7 (seqset (vector 0 1 2 3) 2 22)  22)
  (deftest simple-vector.8 (seqset (vector 0 1 2 3) 3 33)  33)

  (deftest simple-vector.9  (svref sv 0)              0)
  (deftest simple-vector.10 (svset sv 0 1)            1)
  (deftest simple-vector.11 sv                        #(1 1 2))
  (deftest simple-vector.12 (svlength sv)             3)
  (deftest simple-vector.13 (simple-vector->list sv)  '(1 1 2))
)


;;; test operations on adjustable vector
#+murmel
(let ((av (vector-fill (make-array 3 t t) 1)))
  (deftest adjustable-vector.1  (vectorp av)              t)
  (deftest adjustable-vector.2  (simple-vector-p av)      nil)
  (deftest adjustable-vector.3  (vector-length av)        3)
  (deftest adjustable-vector.4  (vector->list av)         '(1 1 1))

  (deftest adjustable-vector.5  (seqref av 0)             1)
  (deftest adjustable-vector.6  (seqset av 0 0)           0)
  (deftest adjustable-vector.7  av                        #(0 1 1))

  (deftest adjustable-vector.8  (vector-add av 2)         3)
  (deftest adjustable-vector.9  av                        #(0 1 1 2))
)


;;; test operations on simple bitvector
#+murmel
(let ((sbv (vector-copy #*0101)))
  (deftest simple-bit-vector.1 (vectorp sbv)                      t)
  (deftest simple-bit-vector.2 (bit-vector-p sbv)                 t)
  (deftest simple-bit-vector.3 (simple-bit-vector-p sbv)          t)
  (deftest simple-bit-vector.4 (vector-length sbv)                4)
  (deftest simple-bit-vector.5 (vector->list sbv)                 '(0 1 0 1))

  (deftest simple-bit-vector.6 (seqref #*0101 2)                  0)
  (deftest simple-bit-vector.7 (seqref #*0101 3)                  1)
  (deftest simple-bit-vector.8 (seqset (vector-copy #*0101) 2 0)  0)
  (deftest simple-bit-vector.9 (seqset (vector-copy #*0101) 3 0)  0)

  (deftest simple-bit-vector.10 (bvref sbv 0)                      0)
  (deftest simple-bit-vector.11 (bvset sbv 0 1)                    1)
  (deftest simple-bit-vector.12 sbv                                #*1101)
  (deftest simple-bit-vector.13 (bvlength sbv)                     4)
  (deftest simple-bit-vector.14 (bv= sbv #*1101)                   t)
  (deftest simple-bit-vector.15 (bit-vector->list sbv)             '(1 1 0 1))
)


;;; test operations on adjustable bitvector
#+murmel
(let ((abv (vector-fill (make-array 3 'bit t) 1)))
  (deftest adjustable-bit-vector.1  (vectorp abv)                  t)
  (deftest adjustable-bit-vector.2  (bit-vector-p abv)             t)
  (deftest adjustable-bit-vector.3  (simple-bit-vector-p abv)      nil)
  (deftest adjustable-bit-vector.4  (vector-length abv)            3)
  (deftest adjustable-bit-vector.5  (vector->list abv)             '(1 1 1))

  (deftest adjustable-bit-vector.6  (seqref abv 0)                 1)
  (deftest adjustable-bit-vector.7  (seqset abv 1 0)               0)
  (deftest adjustable-bit-vector.8  abv                            #*101)

  (deftest adjustable-bit-vector.9  (bv= abv #*101)                t)

  (deftest adjustable-bit-vector.10 (vector-add abv 0)             3)
  (deftest adjustable-bit-vector.11 abv                            #*1010)
)


;;; test operations on simple string
#+murmel
(let ((sstring (vector-copy "123")))
  (deftest simple-string.1 (vectorp sstring)          t)
  (deftest simple-string.2 (stringp sstring)          t)
  (deftest simple-string.3 (simple-string-p sstring)  t)
  (deftest simple-string.4 (vector-length sstring)    3)
  (deftest simple-string.5 (vector->list sstring)     '(#\1 #\2 #\3))

  (deftest simple-string.6 (seqref "0123" 3)                    #\3)
  (deftest simple-string.6 (seqref "0123" 2)                    #\2)
  (deftest simple-string.7 (seqset (vector-copy "0123") 2 #\a)  #\a)
  (deftest simple-string.8 (seqset (vector-copy "0123") 3 #\b)  #\b)

  (deftest simple-string.10 (sref sstring 0)          #\1)
  (deftest simple-string.11 (sset sstring 0 #\a)      #\a)
  (deftest simple-string.12 sstring                   "a23")
  (deftest simple-string.13 (slength sstring)         3)
  (deftest simple-string.14 (string= sstring "a23")   t)
  (deftest simple-string.15 (string->list sstring)    '(#\a #\2 #\3))
)


;;; test operations on adjustable string
#+murmel
(let ((astring (vector-fill (make-array 3 'character t) #\1)))
  (deftest adjustable-string.1  (vectorp astring)                 t)
  (deftest adjustable-string.2  (stringp astring)                 t)
  (deftest adjustable-string.3  (simple-string-p astring)         nil)
  (deftest adjustable-string.4  (vector-length astring)           3)
  (deftest adjustable-string.5  (vector->list astring)            '(#\1 #\1 #\1))

  (deftest adjustable-string.6  (sref astring 0)                  #\1)
  (deftest adjustable-string.7  (sset astring 0 #\a)              #\a)
  (deftest adjustable-string.8  astring                           "a11")
  (deftest adjustable-string.9  (slength astring)                 3)
  (deftest adjustable-string.10 (string= astring "a11")           t)
  (deftest adjustable-string.11 (string->list astring)            '(#\a #\1 #\1))

  (deftest adjustable-string.12 (vector-add astring #\b)          3)
  (deftest adjustable-string.13 astring                           "a11b")
)


;;; test seqref, seqset on proper and dotted lists
(deftest seqref.1 (seqref '(0 1 2 3) 2)    2)
(deftest seqref.2 (seqref '(0 1 2 3) 3)    3)
(deftest seqref.3 (seqref '(0 1 2 . 3) 3)  3)

(deftest seqset.1 (seqset (list 0 1 2 3)  2 22)   22)
(deftest seqset.2 (seqset (list 0 1 2 3)  3 33)   33)
(deftest seqset.3 (seqset (list* 0 1 2 3) 2 22)   22)


; *******************************************************************
;;; - higher order: n/a


; *******************************************************************
;;; - I/O


; *******************************************************************
;;; - misc


;;; test macroexpand-1
(defmacro alpha (x y) `(beta ,x ,y))   ; =>  ALPHA
(defmacro beta (x y) `(gamma ,x ,y))   ; =>  BETA
(defmacro delta (x y) `(gamma ,x ,y))  ; =>  EPSILON

;; Murmel doesn't have macroexpand, only macroexpand-1, so we emulate it here.
;; Note: (macroexpand-1 form) only works in the interpreter, so we can't
;;       (defun macroexpand...) but implement macroexpand as a macro
;;       (and as a local function inside "(defmacro expand...")
#+murmel
(defmacro macroexpand (form)
  (let loop ((form (eval form)) (x nil))
    (multiple-value-bind (expanded expanded-p) (macroexpand-1 form)
      (if expanded-p (loop expanded t)
        `(values ',form ,x)))))

(defmacro expand (form #|&environment env|#)
  (labels (#-murmel (macroexp (form) (macroexpand form))
           #+murmel (macroexp (form)
                      (let loop ((form form) (x nil))
                        (multiple-value-bind (expanded expanded-p) (macroexpand-1 form)
                          (if expanded-p (loop expanded t)
                            (values form x))))))

    (multiple-value-bind (expansion expanded-p)
        (macroexp form #|env|#)
      `(values ',expansion ',expanded-p))))       ; =>  EXPAND

(defmacro expand-1 (form #|&environment env|#)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form #|env|#)
    `(values ',expansion ',expanded-p)))       ; =>  EXPAND-1

;; Simple examples involving just the global environment
(deftest macroexpand-1.0 (macroexpand-1 '(nil))            '(nil))
(deftest macroexpand-1.1 (macroexpand-1 '(alpha a b))      '(BETA A B))         ; =>  (BETA A B), true
(deftest macroexpand-1.2 (expand-1 (alpha a b))            '(BETA A B))         ; =>  (BETA A B), true
(deftest macroexpand-1.3 (macroexpand '(alpha a b))        '(GAMMA A B))        ; =>  (GAMMA A B), true
(deftest macroexpand-1.4 (expand (alpha a b))              '(GAMMA A B))        ; =>  (GAMMA A B), true
(deftest macroexpand-1.5 (macroexpand-1 'not-a-macro)      'NOT-A-MACRO)        ; =>  NOT-A-MACRO, false
(deftest macroexpand-1.6 (expand-1 not-a-macro)            'NOT-A-MACRO)        ; =>  NOT-A-MACRO, false
(deftest macroexpand-1.7 (macroexpand '(not-a-macro a b))  '(NOT-A-MACRO A B))  ; =>  (NOT-A-MACRO A B), false
(deftest macroexpand-1.8 (expand (not-a-macro a b))        '(NOT-A-MACRO A B))  ; =>  (NOT-A-MACRO A B), false


;;; test time functions
(deftest time.1 (> (get-internal-real-time) 0)  t)  ; wall time
(deftest time.2 (> (get-internal-run-time) 0)  t)   ; user time
#+murmel
(deftest time.3 (> (get-internal-cpu-time) 0)  t)   ; cpu time

(deftest time.4 (> (get-universal-time) 0)  t)

#+murmel
(deftest time.5 (consp (get-decoded-time))  t)

(deftest time.6 (sleep 0.1) nil)


;;; test conditions, error and typep
(define *all-murmel-conditions* '(condition
                                  error simple-error
                                  cell-error unbound-variable undefined-function
                                  control-error program-error parse-error
                                  arithmetic-error type-error simple-type-error
                                  file-error
                                  stream-error end-of-file reader-error))

(defmacro get-condition (form)
  ; invoke "form" and return the condition it throws
  (let ((ret (gensym))
        (cnd (gensym)))
  `(multiple-value-bind (,ret ,cnd) (try ,form) ,cnd)))

(labels ((looop (cnd-types) ; can't use "loop" because of CL's "Lock on package COMMON-LISP"
           (if cnd-types
                 (let ((cnd-type (car cnd-types)))
                   ;(format t "condition %s%n" cnd-type)
                   (deftest cnd.X (typep (get-condition (error cnd-type)) cnd-type) t)
                   (looop (cdr cnd-types))))))
  (looop *all-murmel-conditions*))


;;; Java FFI: tests some functions with objects Java classes that are not normally used in Murmel
#+murmel
(let (
      (byte          ((jmethod "Byte"                    "valueOf" "String") "1"))
      (short         ((jmethod "Short"                   "valueOf" "String") "1"))
      (integer       ((jmethod "Integer"                 "valueOf" "String") "1"))
      (long          ((jmethod "Long"                    "valueOf" "String") "1"))
      (bigInteger    ((jmethod "java.math.BigInteger"    "new"     "String") "1"))

      (float         ((jmethod "Float"                   "valueOf" "String") "1"))
      (double        ((jmethod "Double"                  "valueOf" "String") "1"))
      (bigDecimal    ((jmethod "java.math.BigDecimal"    "new"     "String") "1"))

      (arrayList     ((jmethod "java.util.ArrayList"     "new")))

      (string        ((jmethod "java.lang.String"        "new" "String") "stringvalue"))
      (stringBuffer  ((jmethod "java.lang.StringBuffer"  "new" "String") "stringvalue"))
      (stringBuilder ((jmethod "java.lang.StringBuilder" "new" "String") "stringvalue"))
     )

  (deftest ffi.number.1 (numberp byte) t)
  (deftest ffi.number.2 (numberp short) t)
  (deftest ffi.number.3 (numberp integer) t)
  (deftest ffi.number.4 (numberp long) t)
  (deftest ffi.number.5 (numberp bigInteger) t)
  (deftest ffi.number.6 (numberp float) t)
  (deftest ffi.number.7 (numberp double) t)
  (deftest ffi.number.8 (numberp bigDecimal) t)

  (deftest ffi.integerp.1 (integerp byte) t)
  (deftest ffi.integerp.2 (integerp short) t)
  (deftest ffi.integerp.3 (integerp integer) t)
  (deftest ffi.integerp.4 (integerp long) t)
  (deftest ffi.integerp.5 (integerp bigInteger) t)
  (deftest ffi.integerp.6 (integerp float) nil)
  (deftest ffi.integerp.7 (integerp double) nil)
  (deftest ffi.integerp.8 (integerp bigDecimal) nil)

  (deftest ffi.eql.1 (eql 1 byte) t)
  (deftest ffi.eql.2 (eql 1 short) t)
  (deftest ffi.eql.3 (eql 1 integer) t)
  (deftest ffi.eql.4 (eql 1 long) t)
  (deftest ffi.eql.5 (eql 1 bigInteger) t)
  (deftest ffi.eql.6 (eql 1 float) nil)
  (deftest ffi.eql.7 (eql 1 double) nil)
  (deftest ffi.eql.8 (eql 1 bigDecimal) nil)

  (deftest ffi.eql10.1 (eql 1.0 byte) nil)
  (deftest ffi.eql10.2 (eql 1.0 short) nil)
  (deftest ffi.eql10.3 (eql 1.0 integer) nil)
  (deftest ffi.eql10.4 (eql 1.0 long) nil)
  (deftest ffi.eql10.5 (eql 1.0 bigInteger) nil)
  (deftest ffi.eql10.6 (eql 1.0 float) nil)
  (deftest ffi.eql10.7 (eql 1.0 double) t)
  (deftest ffi.eql10.8 (eql 1.0 bigDecimal) nil)

  (deftest ffi.floatp.1 (floatp byte) nil)
  (deftest ffi.floatp.2 (floatp short) nil)
  (deftest ffi.floatp.3 (floatp integer) nil)
  (deftest ffi.floatp.4 (floatp long) nil)
  (deftest ffi.floatp.5 (floatp bigInteger) nil)
  (deftest ffi.floatp.6 (floatp float) t)
  (deftest ffi.floatp.7 (floatp double) t)
  (deftest ffi.floatp.8 (floatp bigDecimal) t)

  (deftest ffi.inc.1 (1+ byte) 2)
  (deftest ffi.inc.2 (1+ short) 2)
  (deftest ffi.inc.3 (1+ integer) 2)
  (deftest ffi.inc.4 (1+ long) 2)
  (deftest ffi.inc.5 (1+ bigInteger) 2)
  (deftest ffi.inc.6 (1+ float) 2.0)
  (deftest ffi.inc.7 (1+ double) 2.0)
  (deftest ffi.inc.8 (1+ bigDecimal) 2.0)

  (deftest ffi.dec.1 (1- byte) 0)
  (deftest ffi.dec.2 (1- short) 0)
  (deftest ffi.dec.3 (1- integer) 0)
  (deftest ffi.dec.4 (1- long) 0)
  (deftest ffi.dec.5 (1- bigInteger) 0)
  (deftest ffi.dec.6 (1- float) 0.0)
  (deftest ffi.dec.7 (1- double) 0.0)
  (deftest ffi.dec.8 (1- bigDecimal) 0.0)

  (deftest ffi.signum.1 (signum byte) 1)
  (deftest ffi.signum.2 (signum short) 1)
  (deftest ffi.signum.3 (signum integer) 1)
  (deftest ffi.signum.4 (signum long) 1)
  (deftest ffi.signum.5 (signum bigInteger) 1)
  (deftest ffi.signum.6 (signum float) 1.0)
  (deftest ffi.signum.7 (signum double) 1.0)
  (deftest ffi.signum.8 (signum bigDecimal) 1.0)

  (deftest ffi.compare.1 (= 1 bigInteger) t)
  (deftest ffi.compare.2 (eql 1 bigInteger) t)
  (deftest ffi.compare.3 (eql byte bigInteger) t)
  (deftest ffi.compare.4 (= bigInteger bigDecimal) t)
  (deftest ffi.compare.5 (eql bigInteger bigDecimal) nil)

  (deftest ffi.vectorp.1 (vectorp arrayList)     t)
  (deftest ffi.vectorp.2 (vectorp string)        t)
  (deftest ffi.vectorp.3 (vectorp stringBuffer)  t)
  (deftest ffi.vectorp.4 (vectorp stringBuilder) t)

  (deftest ffi.len.1 (vector-length arrayList)     0)
  (deftest ffi.len.2 (vector-length string)        11)
  (deftest ffi.len.3 (vector-length stringBuffer)  11)
  (deftest ffi.len.4 (vector-length stringBuilder) 11)


  (let loop ((i 0))
    (if (< i 10)
      (progn (vector-add arrayList i)
             (loop (1+ i)))))

  (deftest ffi.adjustable.1 (adjustable-array-p arrayList)     t)
  (deftest ffi.adjustable.2 (adjustable-array-p string)        nil)
  (deftest ffi.adjustable.3 (adjustable-array-p stringBuffer)  t)
  (deftest ffi.adjustable.4 (adjustable-array-p stringBuilder) t)

  (deftest ffi.seqref.1 (seqref arrayList 6)     6)
  (deftest ffi.seqref.2 (seqref string 6)        #\v)
  (deftest ffi.seqref.3 (seqref stringBuffer 6)  #\v)
  (deftest ffi.seqref.4 (seqref stringBuilder 6) #\v)


  (deftest ffi.seqset.1 (list (seqset arrayList     6 66 ) (seqref arrayList 6))  '(66 66))
  (deftest ffi.seqset.1 (list (seqset stringBuffer  6 #\a) stringBuffer)  '(#\a "stringaalue"))
  (deftest ffi.seqset.2 (list (seqset stringBuilder 6 #\a) stringBuilder)  '(#\a "stringaalue"))
)


;;; test jmethod, jproxy
#+murmel
(deftest ffi.jproxy
  (let* (value
         (apply-runnable (jmethod "java.lang.Runnable" "run"))
         (runnable (jproxy "java.lang.Runnable" "run" (lambda () (setq value 123)))))
  
        (apply-runnable runnable)
        value)
  123)


; *******************************************************************
;;; Print tests summary
(write *failed*) (format t "/") (write *count*) (format t " test(s) failed")
(writeln)
(if (= 0 *failed*)
      (format t "Success.")
  (format t "Failure."))

#+murmel (if (> *failed* 0) (error "%n%d/%d errors" *failed* *count*))
