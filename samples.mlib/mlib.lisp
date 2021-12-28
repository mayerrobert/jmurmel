;;;; === Mlib - Default library for Murmel
;;;
;;; mlib adds commonly used Lisp functions and macros to the
;;; [core Murmel language (see murmel-langref.md)](murmel-langref.md).
;;;
;;; Most of mlib's functions and macros are modeled after Common Lisp,
;;; (often with reduced functionality) plus some additional macros and functions.
;;;
;;; = Usage:
;;;
;;; Copy mlib.lisp into the directory containing jmurmel.jar
;;; or into the directory specified with --libdir
;;; and begin your source file with
;;;
;;;     (require "mlib")
;;;
;;; = Provides:
;;; mlib provides the following Common Lisp-like functions and macros:
;;;
;;; - [caar..cdddr](#caarcdddr), [nthcdr, nth](#nthcdr-nth)
;;; - [destructuring-bind](#destructuring-bind)
;;; - [get-setf-expansion](#get-setf-expansion)
;;; - [setf](#setf), [incf, decf](#incf-decf)
;;; - [push](#push), [pop](#pop)
;;; - [acons](#acons)
;;; - [not](#not), [and](#and), [or](#or)
;;; - [abs](#abs), [zerop](#zerop), [evenp](#evenp), [oddp](#oddp)
;;; - [char=](#char), [char](#char-1)
;;; - [equal](#equal)
;;; - [prog1, prog2](#prog1-prog2)
;;; - [when](#when), [unless](#unless), [case](#case), [do, do*](#do-do), [dotimes](#dotimes), [dolist](#dolist)
;;; - [identity](#identity), [constantly](#constantly), [complement](#complement)
;;; - [member](#member)
;;; - [mapcar](#mapcar), [maplist](#maplist), [mapc](#mapc), [mapl](#mapl), [mapcan](#mapcan), [mapcon](#mapcon)
;;; - [every](#every), [some](#some), [notevery](#notevery), [notany](#notany)
;;; - [remove-if](#remove-if), [remove](#remove)
;;; - [reduce](#reduce)
;;; - [write-char](#write-char)
;;; - [terpri, prin1, princ, print](#terpri-prin1-princ-print), [pprint](#pprint)
;;; - [list-length](#list-length), [length](#length)
;;; - [time](#time)
;;;
;;; as well as the following additional functions and macros:
;;;
;;; - [*f, /f, +f, -f](#f-f)
;;; - [with-gensyms](#with-gensyms)
;;; - [->](#-), [->>](#--1), [and->](#and-), [and->>](#and--1)


;;; = caar..cdddr
;;;     (c..r lst) -> result
;;;
;;; c..r repeatedly apply car and/ or cdr as the name suggests.
(defun  caar (l) (car (car l)))
(defun  cadr (l) (car (cdr l)))
(defun  cdar (l) (cdr (car l)))
(defun  cddr (l) (cdr (cdr l)))

(defun caaar (l) (car (caar l)))
(defun caadr (l) (car (cadr l)))
(defun cadar (l) (car (cdar l)))
(defun caddr (l) (car (cddr l)))

(defun cdaar (l) (cdr (caar l)))
(defun cdadr (l) (cdr (cadr l)))
(defun cddar (l) (cdr (cdar l)))
(defun cdddr (l) (cdr (cddr l)))


;;; = nthcdr, nth
;;;     (nthcdr n lst) -> nth-tail
;;;     (nth n lst) -> nth-element
;;;
;;; nthcdr applies cdr n times and returns the result.
;;; nth works as if `(car (nth n lst))` was invoked.
(defun nthcdr (n l)
  (let loop ((n n) (l l))
    (if (<= n 0) l
      (loop (1- n) (cdr l)))))

(defun nth (n l)
  (car (nthcdr n l)))


; m%rplaca
;     (m%rplaca lst value) -> value
;
; Replace the car of lst by value and return value (as opposed to rplaca which returns lst).
; Used in setf-expansions.
(defun m%rplaca (l v) (rplaca l v) v)


; m%rplacd
;     (m%rplacd lst value) -> value
;
; Replace the cdr of lst by value and return value (as opposed to rplacd which returns lst).
; Used in setf-expansions.
(defun m%rplacd (l v) (rplacd l v) v)


;;; = destructuring-bind
;;;     (destructuring-bind (vars*) (expressions*) forms*)
;;;
;;; Murmel's destructuring-bind is a subset of CL's destructuring-bind,
;;; trees are not supported, only lists are.
;;;
;;; destructuring-bind binds the variables specified in vars
;;; to the corresponding values in the list resulting from the evaluation
;;; of expression; then destructuring-bind evaluates forms. 
(defmacro destructuring-bind (vars expression . forms)
  `(apply (lambda ,vars ,@forms) ,expression))


;;; = get-setf-expansion
;;;     (get-setf-expansion place) -> vars, vals, store-vars, writer-form, reader-form
(defun get-setf-expansion (place)
  (let ((read-var (gensym)) (store-var (gensym)))
    (if (symbolp place) `(nil nil (,read-var) (setq ,place ,read-var) ,place)
      (let ((op (car place))
            (args (cdr place)))
        (cond ((eq   'car op)  `((,read-var) (        ,@args ) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))
              ((eq  'caar op)  `((,read-var) ((   car ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))
              ((eq  'cadr op)  `((,read-var) ((   cdr ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))
              ((eq 'caaar op)  `((,read-var) ((  caar ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))
              ((eq 'caadr op)  `((,read-var) ((  cadr ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))
              ((eq 'cadar op)  `((,read-var) ((  cdar ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))
              ((eq 'caddr op)  `((,read-var) ((  cddr ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))

              ((eq   'cdr op)  `((,read-var) (        ,@args ) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))
              ((eq  'cdar op)  `((,read-var) ((   car ,@args)) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))
              ((eq  'cddr op)  `((,read-var) ((   cdr ,@args)) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))
              ((eq 'cdaar op)  `((,read-var) ((  caar ,@args)) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))
              ((eq 'cdadr op)  `((,read-var) ((  cadr ,@args)) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))
              ((eq 'cddar op)  `((,read-var) ((  cdar ,@args)) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))
              ((eq 'cdddr op)  `((,read-var) ((  cddr ,@args)) (,store-var) (m%rplacd ,read-var ,store-var) (cdr ,read-var)))

              ((eq 'nth op)    `((,read-var) ((nthcdr ,@args)) (,store-var) (m%rplaca ,read-var ,store-var) (car ,read-var)))

              (t (fatal "only symbols, car..cdddr and nth are supported for 'place'")))))))


;;; = setf
;;;     (setf pair*) -> result
;;;
;;; Takes pairs of arguments like SETQ. The first is a place and the second
;;; is the value that is supposed to go into that place. Returns the last
;;; value. The place argument may be any of the access forms for which SETF
;;; knows a corresponding setting form, which currently are:
;;;
;;; - symbols
;;; - car..cdddr
(defmacro setf args
  (if args
        (if (cdr args)
              (if (cddr args)
                    (cons 'progn
                      (let loop ((args args))
                        (if (cdr args)
                              (cons `(setf ,(car args) ,(cadr args))
                                    (if (cddr args)
                                          (loop (cddr args))))
                          (fatal "odd number of arguments to setf"))))

                (if (symbolp (car args))
                      `(setq ,(car args) ,(cadr args))
                  (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion (car args))
                    `(let* (,@(mapcar list vars vals)
                            (,(car store-vars) ,@(cdr args)))
                       ,writer-form))))
          (fatal "odd number of arguments to setf"))))


; Helper macro to generate defmacro's for inplace modification macros.
(defmacro m%inplace (name noarg arg)
  `(defmacro ,name (place . delta-form)
    (if (symbolp place)
          `(setq ,place ,(if delta-form `(,,@arg ,place ,(car delta-form)) `(,,@noarg ,place)))
      (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion place)
          `(let* (,@(mapcar list vars vals)
                 (,(car store-vars) ,(if delta-form
                                           `(,,@arg ,reader-form ,(car delta-form))
                                       `(,,@noarg ,reader-form))))
             ,writer-form)))))


;;; = incf, decf
;;;     (incf place [delta-form]) -> new-value
;;;     (decf place [delta-form]) -> new-value
;;;
;;; incf and decf are used for incrementing and decrementing
;;; the value of place, respectively.
;;;
;;; The delta is added to (in the case of incf) or subtracted
;;; from (in the case of decf) the number in place and the result
;;; is stored in place.
;;;
;;; Without delta-form the return type of incf and decf will be
;;; the type of the number in place, otherwise the return type will be float.
(m%inplace incf ('1+) ('+))
(m%inplace decf ('1-) ('-))


;;; = *f, /f
;;;     (*f place [delta-form]) -> new-value
;;;     (/f place [delta-form]) -> new-value
;;;
;;; *f and /f are used for multiplying and dividing
;;; the value of place, respectively.
;;;
;;; The number in place is multiplied (in the case of *f) by delta
;;; or divided (in the case of /f) by delta and the result
;;; is stored in place.
;;;
;;; Without delta /f will return the reciprocal of the number in place,
;;; *f will return the number in place.
;;;
;;; Without delta-form the return type of *f will be
;;; the type of the number in place, otherwise the return type will be float.
(m%inplace *f (identity) (*))
(m%inplace /f (/) (/))


;;; = +f, -f
;;;     (+f place [delta-form]) -> new-value
;;;     (-f place [delta-form]) -> new-value
;;;
;;; +f and +f are used for adding and subtracting
;;; to/ from the value of place, respectively.
;;;
;;; The delta is added (in the case of *f) to
;;; or subtracted (in the case of /f) from the number in place
;;; and the result is stored in place.
;;;
;;; Without delta -f will return the negation of the number in place,
;;; +f will return the number in place.
;;;
;;; Without delta-form the return type of +f will be
;;; the type of the number in place, otherwise the return type will be float.
(m%inplace +f (identity) (+))
(m%inplace -f (-) (-))

; undef m%inplace
(defmacro m%inplace)


;;; = push
;;;     (push item place) -> new-place-value
;;;
;;; push prepends item to the list that is stored in place,
;;; stores the resulting list in place, and returns the list.
(defmacro push (item place)
  (if (symbolp place)
        `(setq ,place (cons ,item ,place))
    (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion place)
      `(let* (,@(mapcar list vars vals)
              (,(car store-vars) (cons ,item ,reader-form)))
         ,writer-form))))


;;; = pop
;;;     (pop place) -> element
;;;
;;; pop reads the value of place, remembers the car of the list which
;;; was retrieved, writes the cdr of the list back into the place,
;;; and finally yields the car of the originally retrieved list.
(defmacro pop (place)
  (let ((result (gensym)))
    (if (symbolp place)
          `(let ((,result (car ,place)))
             (setq ,place (cdr ,place))
             ,result)
      (destructuring-bind (vars vals new writer-form reader-form) (get-setf-expansion place)
        `(let* (,@(mapcar list vars vals)
                (,@new (cdr ,reader-form))
                (,result (car ,reader-form)))
           ,writer-form
           ,result)))))


;;; = acons
;;;     (acons key datum alist) -> new-alist
;;;
;;; Prepends alist with a new (key . datum) tuple
;;; and returns the modified list.
(defun acons (key datum alist)
  (cons (cons key datum) alist))


;;; = not
;;;     (not form) -> boolean
;;;
;;; Logical not.
(defun not (e)
  (null e))


;;; = and
;;;     (and forms*) -> boolean
;;;
;;; Short-circuiting logical and.
;;; Return T unless any of the forms evaluate to NIL,
;;; NIL otherwise.
(defmacro and args
   (if args
         (if (cdr args)
               `(if ,(car args)
                 (and ,@(cdr args)))
           (car args))
     t))


;;; = or
;;;     (or forms*) -> result
;;;
;;; Short-circuiting logical or.
;;; Return NIL unless any of the forms evaluate to non-NIL,
;;; the result of the first form returning non-NIL otherwise.
(defmacro or args
   (if args
         (if (cdr args)
               (let ((temp (gensym)))
                 `(let ((,temp ,(car args)))
                     (if ,temp
                           ,temp
                       (or ,@(cdr args)))))
           (car args))
     nil))


;;; = abs
;;;     (abs n) -> result
;;;
;;; Return the absoute value of a number.
(defun abs (n)
  (if (< n 0) (- n) (+ n)))


;;; = zerop
;;;     (zerop number) -> boolean
;;;
;;; Is this number zero?
(defun zerop (n) (= n 0))


;;; = evenp
;;;     (evenp number) -> boolean
;;;
;;; Is this number even?
(defun evenp (n) (= 0.0 (mod n 2)))


;;; = oddp
;;;     (oddp number) -> boolean
;;;
;;; Is this number odd?
(defun oddp (n) (= 1.0 (mod n 2)))


;;; = char=
;;;     (char= characters+) -> boolean
;;;
;;; Return t if all of the arguments are the same character
(defun char= (c . more)
  (if more
        (let loop ((code (char-code c)) (l more))
          (if (= code (char-code (car l)))
                (if (cdr l)
                      (loop code (cdr l))
                  t)
            nil))
    t))


;;; = char
;;;     (char string n) -> nth-character
(defun char (str n)
  (nth n str))


;;; = equal
;;;     (equal x y) -> boolean
;;;
;;; Return t if any of the following is true
;;; a and b are eql
;;; a and b are strings, characters or symbols and have the same text value
;;; a and b are conses whose car and cdr are equal respectively
(defun equal (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (consp a)   (consp b)   (equal (car a) (car b)) (equal (cdr a) (cdr b)))))


;;; = prog1, prog2
;;;     (prog1 first-form forms*) -> result-1
;;;     (prog2 first-form second-form forms*) -> result-2
(defmacro prog1 (first-form . forms)
  (if forms
        (let ((result (gensym)))
          `(let ((,result ,first-form))
             ,@forms
             ,result))
    `,first-form))

(defmacro prog2 (first-form second-form . forms)
  (if forms
        (let ((ignore (gensym))
              (result (gensym)))
          `(let ((,ignore ,first-form)
                 (,result ,second-form))
             ,@forms
             ,result))
    `(progn ,first-form ,second-form)))


;;; = when
;;;     (when condition forms*) -> result
;;;
;;; Execute forms if condition evaluates to true
;;; and return the result of the last form if any
;;; Orherwise f condition evaluates to false,
;;; the forms are not evaluated and the return value
;;; of the when-form is nil.
(defmacro when (condition . body)
  (list 'if
        condition
        (if (cdr body)
              (cons 'progn body)
          (car body))))


;;; = unless
;;;     (unless condition forms*) -> result
;;;
;;; Execute forms if condition evaluates to false
;;; and return the result of the last form if any
;;; Orherwise f condition evaluates to true,
;;; the forms are not evaluated and the return value
;;; of the unless-form is nil.
(defmacro unless (condition . body)
  (list 'if
        condition
        nil
        (if (cdr body)
              (cons 'progn body)
          (car body))))


;;; = case
;;;      (case keyform (keys forms*)* (t forms*)?) -> result
(defmacro case (keyform . clauses)
  (labels ((do-clause (tmp clause)
             (let ((keydesignator (car clause))
                   (forms (cdr clause)))
               (if keydesignator
                     (if (consp keydesignator)
                           `((member ,tmp ',keydesignator eq) ,@forms)
                       (if (eq 't keydesignator)
                             `(t ,@forms)
                         `((eq ,tmp ,keydesignator) ,@forms)))))))
    (if (atom keyform)
          `(cond ,@(mapcar (lambda (clause) (do-clause keyform clause)) clauses))
      (let ((tmp (gensym)))
        `(let ((,tmp ,keyform))
           (cond ,@(mapcar (lambda (clause) (do-clause tmp clause)) clauses)))))))


;;; = do, do*
;;;     (do ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result
;;;     (do* ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result
;;;
;;; do and do* iterate over a group of statements while "end-test-form" returns nil.
(defmacro do (var-defs test-and-result . forms)
  (labels ((init-form (l)
             (if (symbolp l) (list l nil)
               (list (car l) (cadr l))))

           (step-form (l)
             (if (symbolp l) l
               (if (caddr l) (caddr l)
                 (car l)))))

    (let ((loop (gensym)))
      `(let ,loop (,@(mapcar init-form var-defs))
         (if ,(car test-and-result)
               (progn ,@(cdr test-and-result))
           (progn
             ,@forms
             (,loop ,@(mapcar step-form var-defs))))))))

(defmacro do* (var-defs test-and-result . forms)
  (labels ((init-form (l)
             (if (symbolp l) (list l nil)
               (list (car l) (cadr l))))

           (step-form (l)
             (if (symbolp l) nil
               (if (caddr l) `((setq ,(car l) ,(caddr l)))))))

    (let ((loop (gensym)))
      `(let* (,@(mapcar init-form var-defs))
         (let ,loop ()
           (if ,(car test-and-result)
                 (progn ,@(cdr test-and-result))
             (progn
               ,@forms
               ,@(mapcan step-form var-defs)
               (,loop))))))))


;;; = dotimes
;;;     (dotimes (var count-form [result-form]) statement*) -> result
;;;
;;; Similar to CL dotimes http://clhs.lisp.se/Body/m_dotime.htm
(defmacro dotimes (exp . body)
  (let ((var (car exp))
        (countform (cadr exp))
        (count (gensym))
        (loop (gensym))
        (result (caddr exp)))
    `(let ((,count ,countform))
       (if (<= ,count 0)
             (let ((,var 0)) ,result)
         (let ,loop ((,var 0))
           (if (>= ,var ,count) ,result
             (progn
               ,@body
               (,loop (1+ ,var)))))))))


;;; = dolist
;;;     (dolist (var list-form [result-form]) statement*) -> result
;;;
;;; Similar to CL dolist http://clhs.lisp.se/Body/m_dolist.htm
(defmacro dolist (exp . body)
  (let ((var (car exp))
        (listform (cadr exp))
        (lst (gensym))
        (loop (gensym))
        (result (caddr exp)))
    `(let ,loop ((,lst ,listform))
       (let ((,var (car ,lst)))
         (if ,lst
               (progn
                 ,@body
                 (,loop (cdr ,lst)))
           ,result)))))


;(defmacro while (expr . body)
;  `(let loop ()
;     (when ,expr
;        ,@body
;        (loop))))


;;; = identity
;;;     (identity object) -> object
;;;
;;; Returns its argument object.
(defun identity (x) x)


;;; = constantly
;;;     (constantly value) -> function
;;;
;;; constantly returns a function that accepts any number of arguments,
;;; that has no side-effects, and that always returns value. 
(defun constantly (value)
  (lambda arguments value))


;;; = complement
;;;     (complement function) -> complement-function
;;;
;;; complement returns a function that takes the same arguments as function,
;;; and has the same side-effect behavior as function, but returns only
;;; a single value: a boolean with the opposite truth value of that
;;; which would be returned as the value of function.
(defun complement (f)
  (lambda arguments
    (null (apply f arguments))))


;;; = member
;;;     (member item list [test]) -> tail
;;;
;;; member searches list for item or for a top-level element that
;;; satisfies the test.
;;;
;;; "test" if given must be a function that takes to arguments.
;;; If "test" was omitted or nil then "eql" will be used.
;;;
;;; Example usage:
;;;
;;;     (member 2 '(1 2 3))
;;;         ; => (2 3)
;;;     (member 'e '(a b c d))
;;;         ; => NIL
;;;     (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3)) equal)
;;;         ; => ((1 . 1) (2 . 2) (3 . 3))
;;;     (member 'c '(a b c 1 2 3) eq)
;;;         ; => (c 1 2 3)
;;;     (member 'b '(a b c 1 2 3) (lambda (a b) (eq a b)))
;;;         ; => (b c 1 2 3)
(defun member (obj l . test)
  (let* ((tst (car test))
         (pred (if tst tst eql)))
    (if l
          (if (pred obj (car l))
                l
            (member obj (cdr l) pred))
      nil)))


;;; = reverse
;;;     (reverse sequence) -> reversed-sequence
;;;
;;; If sequence is a list then return a fresh list
;;; with elements in reversed order, if sequence
;;; is a string then return a fresh reversed string.
(defun reverse (l)
  (labels ((rev (l lp)
             (if l (rev (cdr l) (cons (car l) lp))
               lp)))
    (if (stringp l) (list->string (rev l nil))
      (rev l nil))))


; Helper macro to generate defuns for the various maxXX functions
(defmacro m%mapx (name comb acc accn return-list lastelem)
  `(defun ,name (f l . more)
     (if more
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t))
                    (cdrs (lists)
                      (if lists (cons (cdar lists) (cdrs (cdr lists)))
                        nil))
                    (cars (lists)
                      (if lists (cons (caar lists) (cars (cdr lists)))
                        nil)))
             (let loop ((args (cons l more)))
               (if (none-nil args)
                     (,comb (apply f ,(if accn (list accn 'args) 'args)) (loop (cdrs args)))
                 ,lastelem)))
       (let loop ((f f) (l l))
         (if l (,comb (f ,(if acc (list acc 'l) 'l)) (loop f (cdr l)))
           ,lastelem)))
    ,@(when return-list '(l))))


;;; = mapcar
;;;     (mapcar function sequence+) -> list
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent items of the given sequences.
;;; All function application results will be combined into a list
;;; which is the return value of mapcar.
(m%mapx mapcar  cons    car cars nil nil)


;;; = maplist
;;;     (maplist function sequence+) -> list
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent tails of the given sequences.
;;;
;;; All function application results will be combined into a list
;;; which is the return value of maplist.
(m%mapx maplist cons    nil nil nil nil)


;;; = mapc
;;;     (mapc function sequence+) -> first-arg
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent cars items of the given sequences.
(m%mapx mapc    progn   car cars t nil)


;;; = mapl
;;;     (mapl function sequence+) -> first-arg
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent tails of the given sequences.
(m%mapx mapl    progn   nil nil t nil)


;;; = mapcan
;;;     (mapcan function sequence+) -> concatenated-results
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent items of the given sequences.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of mapcan.
(m%mapx mapcan  append  car cars nil nil)


;;; = mapcon
;;;     (mapcon function sequence+) -> concatenated-results
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent tails of the given sequences.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of mapcon.
(m%mapx mapcon  append  nil nil nil nil)


;;; = every
;;;     (every function sequence+) -> boolean
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; Immediately return nil if an application of function returns nil,
;;; t otherwise.
(m%mapx every and car cars nil t)


;;; = some
;;;     (some function sequence+) -> result
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; Immediately return the first non-nil-value of an application of function,
;;; or nil if no applications yield non-nil.
(m%mapx some or car cars nil nil)

; undef m%mapx
(defmacro m%mapx)


;;; = notevery
;;;     (notevery function sequence+) -> boolean
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; (notevery predicate sequence+) == (not (every predicate sequence+))
(defun notevery (f seq . more)
  (not (apply some (cons f (cons seq more)))))


;;; = notany
;;;     (notany function sequence+) -> boolean
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; (notany predicate sequence+) == (not (some predicate sequence+))
(defun notany (f seq . more)
  (not (apply every (cons f (cons seq more)))))


;;; = remove-if
;;;     (remove-if pred list) -> list
;;;
;;; Return a fresh list without the elements for which pred
;;; evaluates to non-nil.
(defun remove-if (pred l)
  (if l
        (let ((obj (car l)))
          (if (pred obj)
                (remove-if pred (cdr l))
            (cons obj (remove-if pred (cdr l)))))
    nil))


;;; = remove
;;;     (remove elem list) -> list
;;;
;;; Return a fresh list without occurrences of elem.
;;; An occurrence is determined by eql.
(defun remove (elem l)
  (if l
        (let ((obj (car l)))
          (if (eql elem obj)
                (remove elem (cdr l))
            (cons obj (remove elem (cdr l)))))
    nil))


;;; = reduce
;;;     (reduce func sequence [from-end-p]) -> result
;;;
;;; If sequence is empty then "reduce" will return (f).
;;;
;;; Otherwise if sequence contains one element then "reduce will
;;; return this element.
;;;
;;; Otherwise if from-end is omitted or nil then
;;; f will be called with the first two elements
;;; of the sequence and subsequently with the previous result
;;; and the next element, and "reduce" will return the last
;;; result from f.
;;;
;;; Otherwise if from-end is given and non-nil then
;;; f will be called with the last two elements
;;; of the sequence and subsequently with the previous result
;;; and the previous element, and "reduce" will return the last
;;; result from f.
(defun reduce (f seq . from-end)
  (let ((from-end-p (car from-end)))
    (if seq
          (if (cdr seq)
                (let loop ((elem (car seq))
                           (tail (cdr seq)))
                  (if (cdr tail)
                        (if from-end-p
                              (f elem (loop (car tail) (cdr tail)))
                          (loop (f elem (car tail)) (cdr tail)))
                    (f elem (car tail))))
            (car seq))
      (f))))


;;; = write-char
(defun write-char (c) (format t "%s" c))


;;; = terpri, prin1, princ, print
(defun terpri () (writeln) nil)

(defun prin1 (o) (write o) o)

(defun princ (o) (format t "%s" o) o)

(defun print (o) (lnwrite o) o)


;;; = pprint
;;;     (pprint object) -> t
;;;
;;; Simple pretty printer,
;;; based on https://picolisp.com/wiki/?prettyPrint .
(defun pprint (x)
  (labels
    ((size (l)
       (if l
             (if (consp l)
                   (+ (size (car l)) (size (cdr l)))
               1)
         1))

     (pp (x l)
        (dotimes (ign (* l 3)) (write-char #\ ))
        (if (< (size x) 6)
              (write x)
          (progn
            (write-char #\()
            (let loop ()
              (when
                (and
                   (member
                      (princ (pop x))
                      '(lambda let let* letrec defun define defmacro setq setf if when unless dotimes dolist))
                   (< (size (car x)) 7))
                (write-char #\ )
                (loop)))
            (let loop ()
              (when x
                (write-char #\Newline)
                (if (atom x)
                      (pp x (1+ l)))
                  (progn (pp (pop x) (1+ l)) (loop))))
            (write-char #\))
            t))))

    (writeln)
    (pp x 0)))


;;; = list-length
;;;     (list-length list-or-string) -> length
;;;
;;; Returns the length of list-or-string if it is a string or proper list.
;;; Returns nil if list is a circular list.
;;;
;;; See http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html
(defun list-length (x) 
  (let loop ((n 0)         ; Counter 
             (fast x)      ; Fast pointer: leaps by 2 
             (slow x))     ; Slow pointer: leaps by 1 
    ;; If fast pointer hits the end, return the count. 
    (if (null fast) n
      (if (null (cdr fast)) (1+ n)
        ;; If fast pointer eventually equals slow pointer, 
        ;;  then we must be stuck in a circular list. 
        ;; (A deeper property is the converse: if we are 
        ;;  stuck in a circular list, then eventually the 
        ;;  fast pointer will equal the slow pointer. 
        ;;  That fact justifies this implementation.) 
        (if (and (eq fast slow) (> n 0)) nil
          (loop (1+ (1+ n)) (cddr fast) (cdr slow)))))))


;;; = length
;;;     (length sequence) -> length
;;;
;;; Same as list-length.
(defun length (s)
  (list-length s))


; helper function for time
(defun call-with-timing (exp . args)
  (let* ((tstart-real (get-internal-real-time))
         (tstart-run  (get-internal-run-time))
         (result (apply exp args))
         (secs-real (/ (- (get-internal-real-time) tstart-real) internal-time-units-per-second))
         (secs-run  (/ (- (get-internal-run-time)  tstart-run) internal-time-units-per-second)))
    (format t "Evaluation took:%n  %g seconds of real time%n  %g seconds of total run time%n" secs-real secs-run)
    result))


;;; = time
;;;     (time form) -> result
;;;
;;; time evaluates form and prints various timing data.
(defmacro time (expr)
  `(call-with-timing (lambda () ,expr)))



;;; = with-gensyms
;;;     (with-gensyms (names*) forms*) -> result
;;;
;;; "with-gensyms" is a macro commonly used by Common Lispers
;;; to help with avoiding name capture when writing macros.
;;; See "Practical Common Lisp, Peter Seibel"
;;; (http://www.gigamonkeys.com/book/macros-defining-your-own.html)
(defmacro with-gensyms (names . body)
  `(let ,(let loop ((names names))
           (if names (cons (list (car names) '(gensym)) (loop (cdr names)))))
     ,@body))


;;; = ->
;;;     (-> forms*) -> result
;;;
;;; thread-first, inspired by https://github.com/amirgamil/lispy/blob/master/lib/library.lpy
;;;
;;; Inserts first form as the first argument of the second form, and so forth.
;;;
;;; Usage is illustrated by:
;;;
;;;     (macroexpand-1 '(-> 1 f g h))
;;;       ; ==> (h (g (f 1)))
;;;     (macroexpand-1 '(-> 1 (f farg) (g garg) (h harg)))
;;;       ; ==> (h (g (f 1 farg) garg) harg)
;;;
(defmacro -> terms
  (labels ((apply-partials (partials expr)
             (if partials
                   (if (symbolp (car partials))
                         (list (car partials) (apply-partials (cdr partials) expr))
                     ; if it's a list with other parameters, insert expr (recursive call) 
                     ; as second parameter into partial (note need to use cons to ensure same list for func args)
                     (cons (caar partials) (cons (apply-partials (cdr partials) expr) (cdar partials))))
               expr)))
    (apply-partials (reverse (cdr terms)) (car terms))))


;;; = ->>
;;;     (->> forms*) -> result
;;;
;;; thread-last
;;;
;;; Same as -> but inserts first form as last argument of the second form, and so forth.
;;;
;;; Usage is illustrated by:
;;;
;;;     (macroexpand-1 '(->> 1 f g h))
;;;       ; ==> (h (g (f 1)))
;;;     (macroexpand-1 '(->> 1 (f farg) (g garg) (h harg)))
;;;       ; ==> (h harg (g garg (f farg 1)))
;;;
(defmacro ->> terms
  (labels ((apply-partials (partials expr)
             (if partials
                   (if (symbolp (car partials))
                         (list (car partials) (apply-partials (cdr partials) expr))
                     ; if it's a list with other parameters, insert expr (recursive call) 
                     ; as last form 
                     (cons (caar partials) (append (cdar partials) (list (apply-partials (cdr partials) expr)))))
               expr)))
    (apply-partials (reverse (cdr terms)) (car terms))))


;;; = and->
;;;     (and-> forms*) -> result
;;;
;;; Short-circuiting thread-first
;;;
;;; Same as -> but if one function returns nil then the remaining
;;; functions are not called and the overall result is nil.
;;;
(defmacro and-> terms
  (if (cdr terms)
        (let* ((temp (gensym))
               (init (car terms))
               (forms (let loop ((tail (cdr terms)))
                         (if (symbolp (car tail))
                               (if (cdr tail)
                                     (cons (list 'setq temp (list (car tail) temp)) (loop (cdr tail)))
                                 (list (list (car tail) temp)))
                           (if (cdr tail)
                                 (cons (list 'setq temp (cons (caar tail) (cons temp (cdar tail)))) (loop (cdr tail)))
                             (list (cons (caar tail) (cons temp (cdar tail)))))))))
          `(let ((,temp ,init))
             (and ,@forms)))
    (car terms)))


;;; = and->>
;;;     (and->> forms*) -> result
;;;
;;; Short circuiting thread-last
;;;
;;; Same as ->> but if one function returns nil then the remaining
;;; functions are not called and the overall result is nil.
;;;
(defmacro and->> terms
  (if (cdr terms)
        (let* ((temp (gensym))
               (init (car terms))
               (forms (let loop ((tail (cdr terms)))
                         (if (symbolp (car tail))
                               (if (cdr tail)
                                     (cons (list 'setq temp (list (car tail) temp)) (loop (cdr tail)))
                                 (list (list (car tail) temp)))
                           (if (cdr tail)
                                 (cons (list 'setq temp (cons (caar tail) (append (cdar tail) (list temp)))) (loop (cdr tail)))
                             (list (cons (caar tail) (append (cdar tail) (list temp)))))
                             ))))
          `(let ((,temp ,init))
             (and ,@forms)))
    (car terms)))