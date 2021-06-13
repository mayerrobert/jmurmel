;;;; Tests for Murmel's default library "mlib".

(require "mlib")

(define *success-count* 0)
(define *error-count* 0)


;;; Macro to check whether "form" eval's to "expected".
;;; Comparison is done using "equal".
(defmacro assert-equal (expected form . msg)
  `(do-assert-equal ,expected ,form (if ,(car msg) ',(car msg) '(equal ,expected ,form))))

; helper function for assert-equal macro
(defun do-assert-equal (expected actual msg)
  (setq *success-count* (1+ *success-count*))
  (unless (equal expected actual)
    (format t "%nassert-equal failed: ") (writeln msg)
    (format t "expected: ") (writeln expected)
    (format t "actual:   ") (writeln actual)
    (setq *error-count* (1+ *error-count*))
    nil))


;;; Macro to run some tests
;;; usage:
;;; (tests form1 => expected-result1
;;;        form2 => expected-result2
;;;        ...)
(defmacro tests l
  (if l
    `(append (assert-equal ',(caddr l) ,(car l))
             (tests ,@(cdddr l)))))


;;; test setf
(define x nil)
(define y nil)
(tests
 (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3) 
 (setf (car x) 'x (cadr y) (car x) (cdr x) y) =>  (1 X 3) 
 x =>  (X 1 X 3) 
 y =>  (1 X 3) 
 (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3) 
; (psetf (car x) 'x (cadr y) (car x) (cdr x) y) =>  NIL 
; x =>  (X 1 A 3) 
; y =>  (1 A 3)
)


;;; test acons
(tests
  (define alist '()) => alist
  (acons 1 "one" alist) => ((1 . "one"))
  alist => NIL
  (setq alist (acons 1 "one" (acons 2 "two" alist))) => ((1 . "one") (2 . "two"))
  (assoc 1 alist) => (1 . "one")
  (setq alist (acons 1 "uno" alist)) => ((1 . "uno") (1 . "one") (2 . "two"))
  (assoc 1 alist) => (1 . "uno")
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
(tests
  (and (= 1 1)
       (or (< 1 2)
           (> 1 2))
       (and (<= 1 2 3 4)
            (> 5 3 1))) => t
)

(defmacro inc-var (var) `(setq ,var (1+ ,var)))
(defmacro dec-var (var) `(setq ,var (1- ,var)))
(define temp0 nil) (define temp1 1) (define temp2 1) (define temp3 1)

(tests
  (and (inc-var temp1) (inc-var temp2) (inc-var temp3)) => 2
  (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)) => t
  (dec-var temp3) => 1
  (and (dec-var temp1) (dec-var temp2) (eq temp3 'nil) (dec-var temp3)) => nil
  (and (eql temp1 temp2) (eql temp2 temp3)) => t
  (and) => t
)

(tests
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
(tests
  (equal 'a 'b) => nil
  (equal 'a 'a) => t
  (equal 3 3) => t
  (equal 3 3.0) => nil
  (equal 3.0 3.0) => t
  ;(equal #c(3 -4) #c(3 -4)) => t
  ;(equal #c(3 -4.0) #c(3 -4)) => nil
  (equal (cons 'a 'b) (cons 'a 'c)) => nil
  (equal (cons 'a 'b) (cons 'a 'b)) => t
  ;(equal #\A #\A) => t
  ;(equal #\A #\a) => nil
  (equal "Foo" "Foo") => t
  ;(equal "Foo" (copy-seq "Foo")) => t
  (equal "FOO" "foo") => nil
  (equal "This-string" "This-string") => t
  (equal "This-string" "this-string") => nil
)

; test when, unless
(labels ((prin1 (form) (write form) form))
  (tests
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
))


; test dotimes
(tests
  (dotimes (temp-one 10 temp-one)) => 10
  (define temp-two 0) => temp-two
  (dotimes (temp-one 10 t) (inc-var temp-two)) => t
  temp-two => 10
  (let ((loop "loop") (result nil)) (dotimes (i 3 result) (setq result (cons loop result))))
    => ("loop" "loop" "loop")
)


; test dolist
(defmacro prepend (elem l) `(setq ,l (cons ,elem ,l)))
(tests
  (define temp-two '()) => temp-two
  (dolist (temp-one '(1 2 3 4) temp-two) (prepend temp-one temp-two)) => (4 3 2 1)

  (setq temp-two 0) => 0
  (dolist (temp-one '(1 2 3 4)) (inc-var temp-two)) => nil
  temp-two => 4

  (dolist (x '(a b c d)) (write x) (format t " ")) => nil ; >>  A B C D , => NIL
)


; test identity
(tests
  (identity 101) =>  101
  (mapcan identity (list (list 1 2 3) '(4 5 6))) =>  (1 2 3 4 5 6)
)


; test constantly
(tests
  (mapcar (constantly 3) '(a b c d)) =>  (3 3 3 3)

  (defmacro with-vars (vars . forms)
    `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars))) => WITH-VARS

  (macroexpand-1 '(with-vars (a b) (setq a 3 b (* a a)) (list a b)))
    => ((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL)
)


; test complement
(tests
  ((complement zerop) 1) => t
  ((complement characterp) #\a) => nil
  ((complement member) 'a '(a b c)) =>  nil
  ((complement member) 'd '(a b c)) =>  t
)


; test member
(tests
  (member 2 '(1 2 3)) => (2 3)
  (member 'e '(a b c d)) => NIL
  (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3)) equal) => ((1 . 1) (2 . 2) (3 . 3))
  (member 'c '(a b c 1 2 3) eq) => (c 1 2 3)
  (member 'b '(a b c 1 2 3) (lambda (a b) (eq a b))) => (b c 1 2 3)
)


; test reverse
(let ((str nil) (l nil))
  (tests
    (setq str "abc") =>  "abc"
    (reverse str) => "cba"
    str =>  "abc"
    (setq l (list 1 2 3)) =>  (1 2 3)
    (reverse l) =>  (3 2 1)
    l => (1 2 3)
  ))


; test mapcar
(tests
  (mapcar car '((1 a) (2 b) (3 c))) => (1 2 3)
  (mapcar abs '(3 -4 2 -5 -6)) => (3.0 4.0 2.0 5.0 6.0)
  (mapcar cons '(a b c) '(1 2 3)) => ((A . 1) (B . 2) (C . 3))
)


; test maplist
(tests
  (maplist append '(1 2 3 4) '(1 2) '(1 2 3)) => ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))
  (maplist (lambda (x) (cons 'foo x)) '(a b c d)) => ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D))
  (maplist (lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c)) => (0 0 1 0 1 1 1)
  ;An entry is 1 if the corresponding element of the input
  ;  list was the last instance of that element in the input list.
)

; test mapc
(tests
  (define dummy nil) => dummy
  (mapc (lambda x (setq dummy (append dummy x)))
        '(1 2 3 4)
        '(a b c d e)
        '(x y z)) => (1 2 3 4)
  dummy => (1 A X 2 B Y 3 C Z)
)


; test mapl
(tests
  (setq dummy nil) => nil
  (mapl (lambda (x) (prepend x dummy)) '(1 2 3 4)) => (1 2 3 4)
  dummy => ((4) (3 4) (2 3 4) (1 2 3 4))
)


; test mapcan
(tests
  (mapcan (lambda (x y) (if (null x) nil (list x y)))
          '(nil nil nil d e)
          '(1 2 3 4 5 6))
    =>  (D 4 E 5) 
  (mapcan (lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))
    =>  (1 3 4 5)
)


; test mapcon
(tests
  (mapcon list '(1 2 3 4)) =>  ((1 2 3 4) (2 3 4) (3 4) (4))
)


; test every, some, notevery, notany
(tests
  (every characterp "abc") =>  t
  (every char= "abcdefg" '(#\a #\b)) => t
  (some     = '(1 2 3 4 5) '(5 4 3 2 1)) =>  t
  (notevery < '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  nil
  (notany   > '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  t
)


; test remove-if, remove-if-not, remove
(tests
  (remove-if oddp '(1 2 4 1 3 4 5)) => (2 4 4)
  (remove-if (complement evenp) '(1 2 4 1 3 4 5)) => (2 4 4)

  (remove 4 '(1 3 4 5 9)) => (1 3 5 9)
  (remove 4 '(1 2 4 1 3 4 5)) => (1 2 1 3 5)
)


; test reduce
(tests
  (reduce * '(1 2 3 4 5)) =>  120.0

  ;(reduce append '((1) (2)) :initial-value '(i n i t)) =>  (I N I T 1 2)
  (reduce append (cons '(i n i t) '((1) (2)))) =>  (I N I T 1 2)

  ;(reduce append '((1) (2)) :from-end t :initial-value '(i n i t)) =>  (1 2 I N I T) 
  (reduce append (append '((1) (2)) (list '(i n i t))) t) =>  (1 2 I N I T) 

  (reduce - '(1 2 3 4)) ;==  (- (- (- 1 2) 3) 4)
    =>  -8.0
  (reduce - '(1 2 3 4) t)    ;Alternating sum: ==  (- 1 (- 2 (- 3 4)))
    =>  -2.0
  (reduce + '()) =>  0.0
  (reduce + '(3)) =>  3
  (reduce + '(foo)) =>  FOO
  (reduce list '(1 2 3 4)) =>  (((1 2) 3) 4)
  (reduce list '(1 2 3 4) t) =>  (1 (2 (3 4)))

  ;(reduce list '(1 2 3 4) :initial-value 'foo) =>  ((((foo 1) 2) 3) 4)
  (reduce list (cons 'foo '(1 2 3 4))) =>  ((((foo 1) 2) 3) 4)
  
  ;(reduce #'list '(1 2 3 4)
  ;     :from-end t :initial-value 'foo) =>  (1 (2 (3 (4 foo))))
  (reduce list (append '(1 2 3 4) (list 'foo)) t) =>  (1 (2 (3 (4 foo))))
)


; test with-gensyms
; define a "non-shortcircuiting logical and" as a macro
; uses "with-gensyms" so that the macro expansion does NOT contain a variable "result"
(defmacro logical-and-3 (a b c)
  (with-gensyms (result)
    `(let ((,result t))
       (if ,a nil (setq ,result nil))
       (if ,b nil (setq ,result nil))
       (if ,c nil (setq ,result nil))
       ,result)))

(tests
  (define result 1) ==> result; the symbol "result" is used in the macro, name-capturing must be avoided
  (logical-and-3 result 2 3) ==> t
  result ==> 1 ; global variable is not affected by the macro
)


; test thread-first
(tests
  (->) => nil
  (-> 200 (/ 2) (+ 7)) => 107.0
  (macroexpand-1 '(-> 200 (/ 2) (+ 7)))
    ==> (+ (/ 200 2) 7)
  (-> 107 code-char char-code) => 107
)


; test thread-last
(tests
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
(let* ((mk-nil-args nil)
       (mk-nil (lambda args (setq mk-nil-args args) nil))
       (fail (lambda (args) (assert-equal t nil "function fail should not be called!"))))
  (tests
    (and-> 1 1+ (+ 2 3) (mk-nil 'a 'b 'c) fail) => nil
    mk-nil-args => (7.0 a b c)
  ))


; test short-circuiting thread last
(tests
  (and->> '(1 3 5) (mapcar 1+) (remove-if evenp) (reduce -)) => nil
    ; ->> would throw an error: "-" needs at least one arg
)


; more threading macros tests
(tests
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

(let* ((f-args nil)
       (f (lambda (a1 a2 a3) (setq f-args (list a1 a2 a3)) a1)) ; f passes 1st arg and records args
       (l-args nil)
       (l (lambda (a1 a2 a3) (setq l-args (list a1 a2 a3)) a3))) ; l passes last arg and records args
  (tests
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
(if (zerop *error-count*)
      (format t "mlib-test.lisp: %d asserts succeeded. Yay!%n" *success-count*)
  (fatal (format nil "mlib-test.lisp: %d/%d asserts failed. D'oh!%n" *error-count* *success-count*)))
