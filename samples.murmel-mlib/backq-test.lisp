;;;; More backquote tests.

;;; This file is valid Common Lisp as well as Murmel
;;; with mlib.lisp. Some #+/#- feature expressions
;;; are needed, tough.
;;;
;;; It can be run with e.g. sbcl or abcl to test the tests
;;; and after that with jmurmel to test mlib/jmurmel.
;;;
;;; Usage:
;;;
;;;     ecl  --shell  backq-test.lisp
;;;     sbcl --script backq-test.lisp
;;;     abcl --batch --load backq-test.lisp
;;;
;;;     java -jar jmurmel.jar backq-test.lisp
;;;
;;; Notes:
;;;
;;; - won't work with compiled Murmel because the compiler only has a limited eval
;;; - contains some occurrences of #'. These are needed
;;;   for CL compatibility, #' is ignored by murmel.
;;; - may contain some mutation of quoted forms which will break some tests
;;;   when compiling with SBCL (and mutating quoted forms i.e. constants
;;;   is bad style...), I think I fixed all, though.

#+murmel (require "mlib")


#-murmel (progn

(defmacro define (n v) `(defparameter ,n ,v))

(defun writeln (&optional (o nil) (escape t))
  (when o (funcall (if escape #'print #'princ) o))
  (terpri)
  o)

(defmacro sref (str idx) `(aref ,str ,idx))

(defmacro try (form &optional errorobj)
  (let ((ex (gensym)))
    `(handler-case ,form
                   (condition (,ex) (values ,errorobj ,ex)))))

(defun circular-list (&rest elements)
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

;; from alexandria
(defun mappend (function &rest lists)
  "Applies FUNCTION to respective element(s) of each LIST, appending
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'mapcar function lists)
        append results))

(defun mappend-tails (function &rest lists)
  "Applies FUNCTION to respective tail(s) of each LIST, appending
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'maplist function lists)
        append results))
)


(define *success-count* 0)
(define *error-count* 0)


;;; Macro to check whether "form" eval's to "expected".
;;; Comparison is done using "equal".
(defmacro assert-equal (expected form . msg)
  `(do-assert-equal
     ,expected
     ,form
     ,(if (car msg)
          (car msg)
          (append (list 'equal) (list expected) (list form)))))

; helper function for assert-equal macro
(defun do-assert-equal (expected actual msg)
  (setq *success-count* (1+ *success-count*))
  (unless (equal expected actual)
    (writeln)
    (format t "assert-equal failed: ") (writeln msg)
    (format t "expected: ") (writeln expected t)
    (format t "actual:   ") (writeln actual t)
    (setq *error-count* (1+ *error-count*))
    nil))


;;; Macro to run some tests
;;;
;;; The format is so that examples form the CLHS can be copy&pasted.
;;; `expected-result`s will not be evaluated.
;;;
;;; Usage:
;;;     (tests test-name
;;;       form1 => expected-result1
;;;       form2 => expected-result2
;;;       ...)
(defmacro tests (name . l)
  (when l
    `(append (assert-equal ',(caddr l) ,(car l) ',name)
             (tests ,name ,@(cdddr l)))))


;;; Macro to run one test
;;;
;;; Modeled after https://github.com/pfdietz/ansi-test.
;;; `expected-result` will not be evaluated.
;;;
;;; Usage:
;;;     (deftest test-name
;;;       form1 expected-result)
(defmacro deftest (name form expected-result)
  `(assert-equal ',expected-result ,form ',name))


;;; Macro to check if given condition is thrown
;;;
;;; Modeled after https://github.com/pfdietz/ansi-test.
;;;
;;; Usage:
;;;     (tests test-name
;;;       (signals-error (/) program-error) => t
;;;       (signals-error (read-from-string "") end-of-file) => t
;;;       ...)
;;;
(defmacro signals-error (form cnd)
  (let ((v (gensym))
        (e (gensym)))
    `(multiple-value-bind (,v ,e) (try ,form) (typep ,e ',cnd))))



;;; Begin of the actual tests.


;;; more backquote tests from sbcl backq.pure.lisp
(define *qq* '(*rr* *ss*))
(define *qqq* '(*rrr* *ss*))
(define *rrr* '(3.0 5.0))
(define *ss* '(4.0 6.0))

(defun *rr* (x)
  (reduce #'* x))

(define *x* '(a b))
(define *y* '(c))
(define *p* '(append *x* *y*))
(define *q* '((append *x* *y*) (list 'sqrt 9)))
(define *r* '(append *x* *y*))
(define *s* '((append *x* *y*)))

(defun test-double-backquote (expression value)
  #+murmel (format t "Testing: %s... " expression)
  #-murmel (format t "~&Testing: ~A... " expression)

  (assert-equal value
                (eval (eval (read-from-string expression))))

  #+murmel (format t "Ok. Look at PRINTed version: %n%s%n" (read-from-string expression))
  #-murmel (progn
             (format t "Ok. Look at PPRINTed version: ")
             (pprint (read-from-string expression))))

(define *backquote-tests*
  '(("``(,,*QQ*)" . (24.0))
    ("``(,@,*QQ*)" . 24.0)
    ("``(,,@*QQQ*)" . ((3.0 5.0) (4.0 6.0)))
    ("``(FOO ,,*P*)" . (foo (a b c)))
    ("``(FOO ,,@*Q*)" . (foo (a b c) (sqrt 9)))
    ("``(FOO ,',*R*)" . (foo (append *x* *y*)))
    ("``(FOO ,',@*S*)" . (foo (append *x* *y*)))
    ("``(FOO ,@,*P*)" . (foo a b c))
    ("``(FOO ,@',*R*)" . (foo append *x* *y*))
    ;; The following expression produces different result under LW.
    ("``(FOO . ,,@*Q*)" . (foo a b c sqrt 9))    ; fails on ecl
    ;; These three did not work.
    #-murmel
    ("``(FOO ,@',@*S*)" . (foo append *x* *y*))  ; todo currently broken: can't splice here
    ("``(FOO ,@,@*Q*)" . (foo a b c sqrt 9))     ; fails on ecl
    #-murmel
    ("``(,@,@*QQQ*)" . (3.0 5.0 4.0 6.0))        ; fails on ecl, todo currently broken: can't splice here
    ))

(mapc (lambda (test)
        (test-double-backquote (car test) (cdr test)))
      *backquote-tests*)



;;; Summary
;;; print succeeded and failed tests if any

(writeln) (writeln)
(write *error-count*) (format t "/") (write *success-count*) (format t " test(s) failed")
(writeln)
(if (= 0 *error-count*)
      (format t "Success.")
  (format t "Failure."))
(writeln)


#+murmel
(unless (zerop *error-count*)
  (error "backq-test.lisp: %d/%d asserts failed.%n" *error-count* *success-count*))
