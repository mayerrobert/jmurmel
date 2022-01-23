;;;; Tests for Murmel's default library "mlib".

;;; This file is valid Common Lisp as well as Murmel
;;; with mlib.lisp. Some #+/#- feature expressions
;;; are needed, tough, and some tests e.g. for threading
;;; macros are murmel-only.
;;;
;;; It can be run with e.g. sbcl or abcl to test the tests
;;; and after that with jmurmel to test mlib/jmurmel.
;;;
;;; Usage:
;;;
;;;     sbcl --script mlib-test.lisp
;;;     abcl --batch --load mlib-test.lisp
;;;
;;;     java -jar jmurmel.jar mlib-test.lisp
;;;
;;; Notes:
;;;
;;; - Doesn't yet work with the compiler.
;;; - Contains some occurrences of #'. These are needed
;;;   for CL compatibility, #' is ignored by murmel.

#+murmel (require "mlib")
#-murmel (defmacro define (n v) `(defparameter ,n ,v))
#-murmel (defun writeln (&optional (o nil)) (when o (princ o)) (terpri))

(define *success-count* 0)
(define *error-count* 0)


;;; Macro to check whether "form" eval's to "expected".
;;; Comparison is done using "equal".
(defmacro assert-equal (expected form . msg)
  `(do-assert-equal
     ,expected
     ,form
     ',(if (car msg)
             (car msg)
         (append (list 'equal) (list expected) (list form)))))

; helper function for assert-equal macro
(defun do-assert-equal (expected actual msg)
  (setq *success-count* (1+ *success-count*))
  (unless (equal expected actual)
    (writeln)
    (format t "assert-equal failed: ") (writeln msg)
    (format t "expected: ") (writeln expected)
    (format t "actual:   ") (writeln actual)
    (setq *error-count* (1+ *error-count*))
    nil))


;;; Macro to run some tests
;;; usage:
;;; (tests form1 => expected-result1
;;;        form2 => expected-result2
;;;        ...)
(defmacro tests (name . l)
  (if l
    `(append (assert-equal ',(caddr l) ,(car l) ,name)
             (tests ,name ,@(cdddr l)))))


;;; test destructuring-bind
(tests destructuring-bind
  (destructuring-bind (a b c) '(1.0 2 3) (+ a b c)) => 6.0
)


(define ctr nil)
(defun place (l) (setq ctr (1+ ctr)) l) ; return arg, incr number of invocations

;;; test setf
(define x nil)
(define y nil)
(tests setf
  (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3) 
  (setf (car x) 'x (cadr y) (car x) (cdr x) y) =>  (1 X 3) 
  x =>  (X 1 X 3) 
  y =>  (1 X 3) 
  (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3) 

  (setq x '(1 2 3)) => (1 2 3)
  (setf (car x) 11) => 11

  (setq x '(0 1 2 3)) => (0 1 2 3)
  (setq ctr 0) => 0
  (setf (nth 2 (place x)) 222) => 222
  x => (0 1 222 3)
  ctr => 1
)


;;; test incf, decf, todo: *f, /f, +f and -f
(define n 0)
(tests inplace
  (incf n) =>  1
  n =>  1
  (decf n 3.0) =>  -2.0
  n =>  -2.0
  (decf n -5.0) =>  3.0
  (decf n) =>  2.0
  (incf n 0.5) =>  2.5
  (decf n) =>  1.5
  n =>  1.5
  
  (setq x '(0)) => (0)
  (setq ctr 0) => 0
  (incf (car (place x))) => 1
  ctr => 1

  (setq ctr 0) => 0
  (incf (car (place x)) 2.0) => 3.0
  ctr => 1
)


;;; test push, pop
(define llst nil)

(tests push-pop
  (setq llst '(nil)) =>  (NIL)
  (push 1 (car llst)) =>  (1)
  llst =>  ((1))
  (push 1 (car llst)) =>  (1 1)
  llst =>  ((1 1))
  (setq x '(a (b c) d)) =>  (A (B C) D)
  (push 5 (cadr x)) =>  (5 B C)  
  x =>  (A (5 B C) D)

  (setq llst '(1 2 3)) => (1 2 3)
  (setq ctr 0) => 0
  (push 11 (cdr (place llst))) => (11 2 3)
  llst => (1 11 2 3)
  ctr => 1

  (setq llst '((1 11) 2 3)) => ((1 11) 2 3)
  (setq ctr 0) => 0
  (pop (car (place llst))) => 1
  llst => ((11) 2 3)
  ctr => 1

  (setq llst '(((1 11) 22) 2 3)) => (((1 11) 22) 2 3)
  (setq ctr 0) => 0
  (pop (caar (place llst))) => 1
  llst => (((11) 22) 2 3)
  ctr => 1

  (setq llst '(1 (2 22) 3)) => (1 (2 22) 3)
  (setq ctr 0) => 0
  (pop (cadr (place llst))) => 2
  llst => (1 (22) 3)
  ctr => 1

  (setq llst '((((1 11))) 2 3)) => ((((1 11))) 2 3)
  (pop (caaar (place llst))) => 1
  llst => ((((11))) 2 3)

  (setq llst '((0 (1 11)) 2 3)) => ((0 (1 11)) 2 3)
  (pop (cadar (place llst))) => 1
  llst => ((0 (11)) 2 3)

  (setq llst '(-1 0 (1 11) 2 3)) => (-1 0 (1 11) 2 3)
  (pop (caddr (place llst))) => 1
  llst => (-1 0 (11) 2 3)

  (setq llst '(1 11 2 3)) => (1 11 2 3)
  (setq ctr 0) => 0
  (pop (cdr (place llst))) => 11
  llst => (1 2 3)
  ctr => 1

  (setq llst '((1 11) 2 3)) => ((1 11) 2 3)
  (setq ctr 0) => 0
  (pop (cdar (place llst))) => 11
  llst => ((1) 2 3)
  ctr => 1

  (setq llst '(1 11 2 3)) => (1 11 2 3)
  (setq ctr 0) => 0
  (pop (cddr (place llst))) => 2
  llst => (1 11 3)
  ctr => 1
)


(tests push-pop.2
  (setq llst '((1))) => ((1))
  (setq ctr 0) => 0
  (push 11 (cdr (car (place llst)))) => (11)
  llst => ((1 11))
  ctr => 1
)


;;; test acons
(tests acons
  (define alist '()) => alist
  (acons 1 "one" alist) => ((1 . "one"))
  alist => NIL
  (setq alist (acons 1 "one" (acons 2 "two" alist))) => ((1 . "one") (2 . "two"))
  (assoc 1 alist) => (1 . "one")
  (setq alist (acons 1 "uno" alist)) => ((1 . "uno") (1 . "one") (2 . "two"))
  (assoc 1 alist) => (1 . "uno")
)


;;; test not
(tests not
  (not nil) => t
  (not '()) => t
  (not (integerp 'sss)) => t
  (not (integerp 1)) => nil
  (not 3.7) => nil
  (not 'apple) => nil
)


;;; test logical and/ or macros
(tests and
  (and (= 1 1)
       (or (< 1 2)
           (> 1 2))
       (and (<= 1 2 3 4)
            (> 5 3 1))) => t
)

(defmacro inc-var (var) `(setq ,var (1+ ,var)))
(defmacro dec-var (var) `(setq ,var (1- ,var)))
(define temp0 nil) (define temp1 1) (define temp2 1) (define temp3 1)

(tests and.2
  (and (inc-var temp1) (inc-var temp2) (inc-var temp3)) => 2
  (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)) => t
  (dec-var temp3) => 1
  (and (dec-var temp1) (dec-var temp2) (eq temp3 'nil) (dec-var temp3)) => nil
  (and (eql temp1 temp2) (eql temp2 temp3)) => t
  (and) => t
)

(tests or
  (or) => nil
  (setq temp0 nil temp1 10 temp2 20 temp3 30) => 30
  (or temp0 temp1 (setq temp2 37)) => 10
  temp2 => 20
  (or (inc-var temp1) (inc-var temp2) (inc-var temp3)) => 11
  temp1 => 11
  temp2 => 20
  temp3 => 30
; (or (values) temp1) => 11
; (or (values temp1 temp2) temp3) => 11
; (or temp0 (values temp1 temp2)) => 11, 20
; (or (values temp0 temp1) (values temp2 temp3)) => 20, 30
)

; todo abs


; todo zerop, evenp, oddp


; todo char=


;;; test equal
(tests equal
  (equal 'a 'b) => nil
  (equal 'a 'a) => t
  (equal 3 3) => t
  (equal 3 3.0) => nil
  (equal 3.0 3.0) => t
  ;(equal #c(3 -4) #c(3 -4)) => t
  ;(equal #c(3 -4.0) #c(3 -4)) => nil
  (equal (cons 'a 'b) (cons 'a 'c)) => nil
  (equal (cons 'a 'b) (cons 'a 'b)) => t
  (equal #\A #\A) => t
  (equal #\A #\a) => nil
  (equal "Foo" "Foo") => t
  ;(equal "Foo" (copy-seq "Foo")) => t
  (equal "FOO" "foo") => nil
  (equal "This-string" "This-string") => t
  (equal "This-string" "this-string") => nil
)


; test prog1, prog2
(tests prog
  (define temp 1) =>  temp
  (prog1 temp (print temp) (incf temp) (print temp))
  ; >>  1
  ; >>  2
  =>  1
  (prog1 temp (setq temp nil)) =>  2
  temp =>  NIL
  ;(prog1 (values 1 2 3) 4) =>  1 
  (setq temp (list 'a 'b 'c)) => (a b c)
  (prog1 (car temp) (setf (car temp) 'alpha)) =>  A
  temp =>  (ALPHA B C)
  ;(flet ((swap-symbol-values (x y)
  ;         (setf (symbol-value x) 
  ;               (prog1 (symbol-value y)
  ;                      (setf (symbol-value y) (symbol-value x))))))
  ;  (let ((*foo* 1) (*bar* 2))
  ;    (declare (special *foo* *bar*))
  ;    (swap-symbol-values '*foo* '*bar*)
  ;    (values *foo* *bar*)))
  ;=>  2, 1
  (setq temp 1) =>  1
  (prog2 (incf temp) (incf temp) (incf temp)) =>  3
  temp =>  4
  ;(prog2 1 (values 2 3 4) 5) =>  2
)


; test when, unless
(tests when-unless
  (when t 'hello) => hello
  (unless t 'hello) => nil
  (when nil 'hello) => nil
  (unless nil 'hello) => hello
  (when t) => nil
  (unless nil) => nil
  (when t (prin1 1) (prin1 2) (prin1 3)) => 3 ; >>  123, => 3
  (unless t (prin1 1) (prin1 2) (prin1 3)) => nil
  (when nil (prin1 1) (prin1 2) (prin1 3)) => nil
  (unless nil (prin1 1) (prin1 2) (prin1 3)) => 3 ; >>  123, => 3
  (let ((x 3))
    (list (when (oddp x) (inc-var x) (list x))
          (when (oddp x) (inc-var x) (list x))
          (unless (oddp x) (inc-var x) (list x))
          (unless (oddp x) (inc-var x) (list x))
          (if (oddp x) (inc-var x) (list x))
          (if (oddp x) (inc-var x) (list x))
          (if (not (oddp x)) (inc-var x) (list x))
          (if (not (oddp x)) (inc-var x) (list x)))) => ((4) NIL (5) NIL 6 (6) 7 (7))
)


; test case
(define res nil)
(tests case
  (dolist (k '(1 2 3 :four #\v () t 'other))
    (setq res (append res (list
       (case k ((1 2) 'clause1)
               (3 'clause2)
               (nil 'no-keys-so-never-seen)
               ((nil) 'nilslot)
               ((:four #\v) 'clause4)
               ((t) 'tslot)
               (t 'others))
       ))))
  =>  NIL
  res => (CLAUSE1 CLAUSE1 CLAUSE2 CLAUSE4 CLAUSE4 NILSLOT TSLOT OTHERS)
   (defun add-em (x) (apply #'+ (mapcar #'decode x)))
  =>  ADD-EM
   (defun decode (x)
     (case x
       ((i uno) 1.0)
       ((ii dos) 2.0)
       ((iii tres) 3.0)
       ((iv cuatro) 4.0)))
  =>  DECODE
   (add-em '(uno iii)) =>  4.0
)


; test do, do*
(tests do
  (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
      ((> (- temp-one temp-two) 5) temp-one)) =>  4
 
  (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1+ temp-one)))
      ((= 3 temp-two) temp-one)) =>  3

  (do* ((temp-one 1 (1+ temp-one))
        (temp-two 0 (1+ temp-one)))
       ((= 3 temp-two) temp-one)) =>  2
)


; test dotimes
(tests dotimes
  (dotimes (temp-one 10 temp-one)) => 10
  (define temp-two 0) => temp-two
  (dotimes (temp-one 10 t) (inc-var temp-two)) => t
  temp-two => 10
  (let ((loop "loop") (result nil)) (dotimes (i 3 result) (setq result (cons loop result))))
    => ("loop" "loop" "loop")
)


; test dolist
(tests dolist
  (define temp-two '()) => temp-two
  (dolist (temp-one '(1 2 3 4) temp-two) (push temp-one temp-two)) => (4 3 2 1)

  (setq temp-two 0) => 0
  (dolist (temp-one '(1 2 3 4)) #-murmel (declare (ignore temp-one)) (inc-var temp-two)) => nil
  temp-two => 4

  (dolist (x '(a b c d)) (write x) (format t " ")) => nil ; >>  A B C D , => NIL
)


; test identity
(tests identity
  (identity 101) =>  101
  (mapcan #'identity (list (list 1 2 3) '(4 5 6))) =>  (1 2 3 4 5 6)
)


; test constantly
(tests constantly
  (mapcar (constantly 3) '(a b c d)) =>  (3 3 3 3)

  (defmacro with-vars (vars . forms)
    `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars))) => WITH-VARS

  (macroexpand-1 '(with-vars (a b) (setq a 3 b (* a a)) (list a b)))
    => ((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL)
)


; test complement
(tests complement
  (#-murmel funcall (complement #'zerop) 1) => t
  (#-murmel funcall (complement #'characterp) #\a) => nil
  (#-murmel funcall (complement #'member) 'a '(a b c)) =>  nil
  (#-murmel funcall (complement #'member) 'd '(a b c)) =>  t
)


; test member
(tests member
  (member 2 '(1 2 3)) => (2 3)
  (member 'e '(a b c d)) => NIL
  (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3)) #-murmel :test #'equal) => ((1 . 1) (2 . 2) (3 . 3))
  (member 'c '(a b c 1 2 3) #-murmel :test #'eq) => (c 1 2 3)
  (member 'b '(a b c 1 2 3) #-murmel :test (lambda (a b) (eq a b))) => (b c 1 2 3)
)


; test reverse
(let ((str nil) (l nil))
  (tests reverse
    (setq str "abc") =>  "abc"
    (reverse str) => "cba"
    str =>  "abc"
    (setq l (list 1 2 3)) =>  (1 2 3)
    (reverse l) =>  (3 2 1)
    l => (1 2 3)
  ))


; test map-into
(let ((l (list 0 0 0 0 0)) (k '(one two three)) (n 0))
  (tests map-into
    (map-into l #'1+ l) => (1 1 1 1 1)
    l => (1 1 1 1 1)
    (map-into l #'+ l '(10.0 20.0 30.0)) => (11.0 21.0 31.0 1 1)
    (map-into l #'truncate l) => (11 21 31 1 1)
    (map-into l #'cons k l) => ((one . 11) (two . 21) (three . 31) 1 1)
    k => (one two three)
    (map-into l (lambda () (setq n (1+ n)))) => (1 2 3 4 5)
    n => 5
  ))


; test mapcar
(tests mapcar
  (mapcar #'car '((1 a) (2 b) (3 c))) => (1 2 3)
  (mapcar #'abs '(3.0 -4.0 2.0 -5.0 -6.0)) => (3.0 4.0 2.0 5.0 6.0)
  (mapcar #'cons '(a b c) '(1 2 3)) => ((A . 1) (B . 2) (C . 3))
)


; test maplist
(tests maplist
  (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) => ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))
  (maplist (lambda (x) (cons 'foo x)) '(a b c d)) => ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D))
  (maplist (lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c)) => (0 0 1 0 1 1 1)
  ;An entry is 1 if the corresponding element of the input
  ;  list was the last instance of that element in the input list.
)

; test mapc
(tests mapc
  (define dummy nil) => dummy
  (mapc (lambda #+murmel x #-murmel (&rest x) (setq dummy (append dummy x)))
        '(1 2 3 4)
        '(a b c d e)
        '(x y z)) => (1 2 3 4)
  dummy => (1 A X 2 B Y 3 C Z)
)


; test mapl
(tests mapl
  (setq dummy nil) => nil
  (mapl (lambda (x) (push x dummy)) '(1 2 3 4)) => (1 2 3 4)
  dummy => ((4) (3 4) (2 3 4) (1 2 3 4))
)


; test mapcan
(tests mapcan
  (mapcan (lambda (x y) (if (null x) nil (list x y)))
          '(nil nil nil d e)
          '(1 2 3 4 5 6))
    =>  (D 4 E 5) 
  (mapcan (lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))
    =>  (1 3 4 5)
)


; test mapcon
(tests mapcon
  (mapcon #'list '(1 2 3 4)) =>  ((1 2 3 4) (2 3 4) (3 4) (4))
)


; test every, some, notevery, notany
(tests predicates
  (every #'characterp "abc") =>  t
  (every #'char= "abcdefg" '(#\a #\b)) => t
  (some     #'= '(1 2 3 4 5) '(5 4 3 2 1)) =>  t
  (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  nil
  (notany   #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  t
)


; test remove-if, remove-if-not, remove
(tests remove
  (remove-if #'oddp '(1 2 4 1 3 4 5)) => (2 4 4)
  (remove-if (complement #'evenp) '(1 2 4 1 3 4 5)) => (2 4 4)

  (remove 4 '(1 3 4 5 9)) => (1 3 5 9)
  (remove 4 '(1 2 4 1 3 4 5)) => (1 2 1 3 5)
)


; test reduce
(tests reduce
  (reduce #'* '(1.0 2 3 4 5)) =>  120.0

  ;(reduce append '((1) (2)) :initial-value '(i n i t)) =>  (I N I T 1 2)
  (reduce #'append (cons '(i n i t) '((1) (2)))) =>  (I N I T 1 2)

  ;(reduce append '((1) (2)) :from-end t :initial-value '(i n i t)) =>  (1 2 I N I T) 
  (reduce #'append (append '((1) (2)) (list '(i n i t))) #-murmel :from-end t) =>  (1 2 I N I T) 

  (reduce #'- '(1.0 2 3 4)) ;==  (- (- (- 1 2) 3) 4)
    =>  -8.0
  (reduce #'- '(1.0 2 3 4) #-murmel :from-end t)    ;Alternating sum: ==  (- 1 (- 2 (- 3 4)))
    =>  -2.0
  (reduce #'+ '()) =>  #+murmel 0.0 #-murmel 0
  (reduce #'+ '(3)) =>  3
  (reduce #'+ '(foo)) =>  FOO
  (reduce #'list '(1 2 3 4)) =>  (((1 2) 3) 4)
  (reduce #'list '(1 2 3 4) #-murmel :from-end t) =>  (1 (2 (3 4)))

  ;(reduce list '(1 2 3 4) :initial-value 'foo) =>  ((((foo 1) 2) 3) 4)
  (reduce #'list (cons 'foo '(1 2 3 4))) =>  ((((foo 1) 2) 3) 4)

  ;(reduce #'list '(1 2 3 4)
  ;     :from-end t :initial-value 'foo) =>  (1 (2 (3 (4 foo))))
  (reduce #'list (append '(1 2 3 4) (list 'foo)) #-murmel :from-end t) =>  (1 (2 (3 (4 foo))))
)


; test compose
#+murmel
(tests compose
  ((compose - sqrt) 10) => -3.1622776601683795
  ((compose 1+ 1+ truncate +) 1 2 3) => 8
)


; test with-gensyms
; define a "non-shortcircuiting logical and" as a macro
; uses "with-gensyms" so that the macro expansion does NOT contain a variable "result"
#+murmel
(defmacro logical-and-3 (a b c)
  (with-gensyms (result)
    `(let ((,result t))
       (if ,a nil (setq ,result nil))
       (if ,b nil (setq ,result nil))
       (if ,c nil (setq ,result nil))
       ,result)))

#+murmel
(tests with-gensyms
  (define result 1) ==> result; the symbol "result" is used in the macro, name-capturing must be avoided
  (logical-and-3 result 2 3) ==> t
  result ==> 1 ; global variable is not affected by the macro
)


; test thread-first
#+murmel
(tests thread-first
  (->) => nil
  (-> 200 (/ 2) (+ 7)) => 107.0
  (macroexpand-1 '(-> 200 (/ 2) (+ 7)))
    ==> (+ (/ 200 2) 7)
  (-> 107 code-char char-code) => 107
)


; test thread-last
#+murmel
(tests thread-last
  (->>) => nil
  (->> 200 (/ 2) (+ 7)) => 7.01
  (macroexpand-1 '(->> 200 (/ 2) (+ 7)))
    ==> (+ 7 (/ 2 200))
  (->> 107 code-char char-code) => 107
  (->> '(1 2 3) (mapcar (lambda (n) (expt n 2))) (reduce +)) => 14.0
  (->> '(1 2 3) (mapcar 1+) (reduce +)) => 9.0
  (->> '(1 2 3 4 5) (remove-if evenp) (mapcar 1+) (reduce +)) => 12.0
)


; test short-circuiting thread first
#+murmel
(let* ((mk-nil-args nil)
       (mk-nil (lambda args (setq mk-nil-args args) nil))
       (fail (lambda (args) (assert-equal t nil "function fail should not be called!"))))
  (tests short-circuiting-thread-first
    (and-> 1 1+ (+ 2 3) (mk-nil 'a 'b 'c) fail) => nil
    mk-nil-args => (7.0 a b c)
  ))


; test short-circuiting thread last
#+murmel
(tests short-circuiting-thread-last
  (and->> '(1 3 5) (mapcar 1+) (remove-if evenp) (reduce -)) => nil
    ; ->> would throw an error: "-" needs at least one arg
)


; more threading macros tests
#+murmel
(tests more-threading-macros-tests
  (->) => nil
  (->>) => nil
  (and->) => nil
  (and->>) => nil

  (-> 1) => 1
  (->> 1) => 1
  (and-> 1) => 1
  (and->> 1) => 1

  (-> 1 identity) => 1
  (->> 1 identity) => 1
  (and-> 1 identity) => 1
  (and->> 1 identity) => 1

  (-> 1 1+) => 2
  (->> 1 1+) => 2
  (and-> 1 1+) => 2
  (and->> 1 1+) => 2
)

#+murmel
(let* ((f-args nil)
       (f (lambda (a1 a2 a3) (setq f-args (list a1 a2 a3)) a1)) ; f passes 1st arg and records args
       (l-args nil)
       (l (lambda (a1 a2 a3) (setq l-args (list a1 a2 a3)) a3))) ; l passes last arg and records args
  (tests more-threading-macros-tests.2
    (-> 11 (f 1 2)) => 11
    f-args => (11 1 2)
    (setq f-args nil) => nil

    (and-> 11 (f 1 2)) => 11
    f-args => (11 1 2)
    (setq f-args nil) => nil

    (->> 11 (l 1 2)) => 11
    l-args => (1 2 11)
    (setq l-args nil) => nil

    (and->> 11 (l 1 2)) => 11
    l-args => (1 2 11)
    (setq l-args nil) => nil
  ))


; Summary
; print succeeded and failed tests if any

(writeln) (writeln)
(write *error-count*) (format t "/") (write *success-count*) (format t " test(s) failed")
(writeln)
(if (= 0 *error-count*)
      (format t "Success.")
  (format t "Failure."))
(writeln)


#+murmel
(unless (zerop *error-count*)
  (fatal (format nil "mlib-test.lisp: %d/%d asserts failed. D'oh!%n" *error-count* *success-count*)))
