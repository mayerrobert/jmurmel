;;;; === Mlib - Default library for Murmel
;;;
;;; mlib adds commonly used Lisp functions and macros to the
;;; [core Murmel language (see murmel-langref.md)](murmel-langref.md).
;;;
;;; Most of mlib's functions and macros are modeled after Common Lisp,
;;; (often with reduced functionality) plus some additional macros and functions.

;;; == Usage
;;;
;;; Copy `mlib.lisp` into the directory containing `jmurmel.jar`
;;; or into the directory specified with `--libdir`
;;; and begin your source file with
;;;
;;;     (require "mlib")

;;; == `mlib` functions and macros
;;;
;;; mlib provides the following Common Lisp-like functions and macros:
;;;
;;; - [caar..cdddr](#function-caarcdddr), [nthcdr, nth](#function-nthcdr-nth), [last](#function-last), [nconc](#function-nconc)
;;; - [destructuring-bind](#macro-destructuring-bind)
;;; - [get-setf-expansion](#function-get-setf-expansion)
;;; - [setf](#macro-setf), [incf, decf](#macro-incf-decf)
;;; - [push](#macro-push), [pop](#macro-pop)
;;; - [acons](#function-acons)
;;; - [not](#function-not), [and](#macro-and), [or](#macro-or)
;;; - [abs](#function-abs), [zerop](#function-zerop), [evenp](#function-evenp), [oddp](#function-oddp)
;;; - [char=](#function-char), [char](#function-char-1)
;;; - [equal](#function-equal)
;;; - [prog1, prog2](#macro-prog1-prog2)
;;; - [when](#macro-when), [unless](#macro-unless), [case](#macro-case), [do, do*](#macro-do-do), [dotimes](#macro-dotimes), [dolist](#macro-dolist)
;;; - [identity](#function-identity), [constantly](#function-constantly), [complement](#function-complement)
;;; - [member](#function-member), [reverse](#function-reverse)
;;; - [map-into](#function-map-into), [mapcar](#function-mapcar), [maplist](#function-maplist), [mapc](#function-mapc), [mapl](#function-mapl), [mapcan](#function-mapcan), [mapcon](#function-mapcon)
;;; - [every](#function-every), [some](#function-some), [notevery](#function-notevery), [notany](#function-notany)
;;; - [remove-if](#function-remove-if), [remove](#function-remove)
;;; - [reduce](#function-reduce)
;;; - [write-char](#function-write-char)
;;; - [terpri, prin1, princ, print](#function-terpri-prin1-princ-print), [pprint](#function-pprint)
;;; - [list-length](#function-list-length), [length](#function-length)
;;; - [time](#macro-time)
;;;
;;; functions and macros inspired by [Alexandria](https://alexandria.common-lisp.dev):
;;;
;;; - [circular-list](#function-circular-list)
;;; - [compose](#function-compose), [multiple-value-compose](#function-multiple-value-compose)
;;; - [conjoin](#function-conjoin), [disjoin](#function-disjoin)
;;; - [curry](#function-curry), [rcurry](#function-rcurry)
;;; - [doplist](#macro-doplist)
;;; - [with-gensyms](#macro-with-gensyms)
;;;
;;; functions inspired by [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html)
;;;
;;; - [unzip](#function-unzip)
;;;
;;; functions and macros inspired by [serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md)
;;;
;;; - [with-accumulator](#macro-with-accumulator), [summing](#macro-summing), [collecting](#macro-collecting), [reverse-collecting](#macro-reverse-collecting)
;;; - [plist-keys](#function-plist-keys), [plist-values](#function-plist-values)
;;;
;;; as well as the following additional functions and macros:
;;;
;;; - [unzip-tails](#function-unzip-tails)
;;; - [*f, /f, +f, -f](#macro-f-f)
;;; - [->](#macro), [->>](#macro-1), [and->](#macro-and-1), [and->>](#macro-and-2)


(provide "mlib")


;;; = Function: caar..cdddr
;;;     (c..r lst) -> result
;;;
;;; Since: 1.1
;;;
;;; `c..r` repeatedly apply `car` and/ or `cdr` as the name suggests.
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


(defmacro caar (l)  `(car (car ,l)))
(defmacro cadr (l)  `(car (cdr ,l)))
(defmacro cdar (l)  `(cdr (car ,l)))
(defmacro cddr (l)  `(cdr (cdr ,l)))

(defmacro caaar (l) `(car (car (car ,l))))
(defmacro caadr (l) `(car (car (cdr ,l))))
(defmacro cadar (l) `(car (cdr (car ,l))))
(defmacro caddr (l) `(car (cdr (cdr ,l))))

(defmacro cdaar (l) `(cdr (car (car ,l))))
(defmacro cdadr (l) `(cdr (car (cdr ,l))))
(defmacro cddar (l) `(cdr (cdr (car ,l))))
(defmacro cdddr (l) `(cdr (cdr (cdr ,l))))


;;; = Function: nthcdr, nth
;;;     (nthcdr n lst) -> nth-tail
;;;     (nth n lst) -> nth-element
;;;
;;; Since: 1.1
;;;
;;; `nthcdr` applies `cdr` n times and returns the result.
;;; `nth` works as if `(car (nthcdr n lst))` was invoked.
(defun nthcdr (n l)
  (let loop ((n n) (l l))
    (if (<= n 0) l
      (loop (1- n) (cdr l)))))

(defun nth (n l)
  (car (nthcdr n l)))
(defmacro nth (n l)
  `(car (nthcdr ,n ,l)))


;;; = Function: last
;;;     (last lst) -> last-cons-or-nil
;;;
;;; Since: 1.2
;;;
;;; `last` returns the last cons of a proper or dotted list
;;; or `nil` for the empty list.
(defun last (lst)
  (if (consp (cdr lst)) (last (cdr lst))
    lst))


;;; = Function: nconc
;;;     (nconc lists*) -> concatenated-list
;;;
;;; Since: 1.2
;;;
;;; `nconc` concatenates lists, each list but the last is modified.
;;; If no lists are supplied, `nconc` returns `nil`.
;;; Each argument but the last must be a proper or dotted list.
(defun nconc lists
  (if lists
        (if (car lists)
              (progn
                (if (cdr lists)
                      (let loop ((append-to (car lists))
                                 (lists (cdr lists)))
                        (if (car lists)
                              (progn
                                (if (cdr lists)
                                      (rplacd (last append-to) (loop (car lists) (cdr lists)))
                                  (rplacd (last append-to) (car lists)))
                                append-to)
                          (if (cdr lists)
                                (loop append-to (cdr lists))
                            append-to))))
                (car lists))
          (apply nconc (cdr lists)))
    nil))


; m%rplaca
;     (m%rplaca lst value) -> value
;
; Replace the car of `lst` by `value` and return `value` (as opposed to `rplaca` which returns `lst`).
; Used in setf-expansions.
(defun m%rplaca (l v) (rplaca l v) v)


; m%rplacd
;     (m%rplacd lst value) -> value
;
; Replace the cdr of `lst` by `value` and return `value` (as opposed to `rplacd` which returns `lst`).
; Used in setf-expansions.
(defun m%rplacd (l v) (rplacd l v) v)


;;; = Macro: destructuring-bind
;;;     (destructuring-bind (vars*) (expressions*) forms*)
;;;
;;; Since: 1.1
;;;
;;; Murmel's `destructuring-bind` is a subset of CL's `destructuring-bind`,
;;; trees are not supported, only lists are.
;;;
;;; `destructuring-bind` binds the variables specified in `vars`
;;; to the corresponding values in the list resulting from the evaluation
;;; of `expressions`; then `destructuring-bind` evaluates `forms`. 
(defmacro destructuring-bind (vars expression . forms)
  `(apply (lambda ,vars ,@forms) ,expression))


;;; = Function: get-setf-expansion
;;;     (get-setf-expansion place) -> vars, vals, store-vars, writer-form, reader-form
;;;
;;; Since: 1.1
(defun get-setf-expansion (place)
  (let ((read-var (gensym))
        (tmp1 (gensym))
        (tmp2 (gensym))
        (store-var (gensym)))
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

              ((eq 'svref op)
               `((,tmp1 ,tmp2 ,read-var)
                 (,(car args) ,(cadr args) (svref ,tmp1 ,tmp2))
                 (,read-var)
                 (svset ,tmp1 ,tmp2 ,read-var)
                 (svref ,tmp1 ,tmp2)))

              (t (fatal "only symbols, car..cdddr, nth and svref are supported for 'place'")))))))


;;; = Macro: setf
;;;     (setf pair*) -> result
;;;
;;; Since: 1.1
;;;
;;; Takes pairs of arguments like `setq`. The first is a place and the second
;;; is the value that is supposed to go into that place. Returns the last
;;; value. The place argument may be any of the access forms for which `setf`
;;; knows a corresponding setting form, which currently are:
;;;
;;; - symbols
;;; - car..cdddr
;;; - nth
;;; - svref
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
                  (if (eq 'svref (caar args))
                        `(svset ,@(cdar args) ,(cadr args))
                    (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion (car args))
                      `(let* (,@(mapcar list vars vals)
                              (,(car store-vars) ,@(cdr args)))
                         ,writer-form)))))
          (fatal "odd number of arguments to setf"))))


; Helper macro to generate defmacro's for inplace modification macros.
(defmacro m%inplace (name noarg arg)
  `(defmacro ,name (place . delta-form)
    (if (symbolp place)
          `(setq ,place ,(if delta-form `(,,@arg ,place ,@delta-form) `(,,@noarg ,place)))
      (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion place)
          `(let* (,@(mapcar list vars vals)
                 (,(car store-vars) ,(if delta-form
                                           `(,,@arg ,reader-form ,@delta-form)
                                       `(,,@noarg ,reader-form))))
             ,writer-form)))))


;;; = Macro: incf, decf
;;;     (incf place delta-form*) -> new-value
;;;     (decf place delta-form*) -> new-value
;;;
;;; Since: 1.1
;;;
;;; `incf` and `decf` are used for incrementing and decrementing
;;; the value of `place`, respectively.
;;;
;;; The delta is added to (in the case of `incf`) or subtracted
;;; from (in the case of `decf`) the number in `place` and the result
;;; is stored in `place`.
;;;
;;; Without `delta-form` the return type of `incf` and `decf` will be
;;; the type of the number in `place`, otherwise the return type will be float.
(m%inplace incf ('1+) ('+))
(m%inplace decf ('1-) ('-))


;;; = Macro: *f, /f
;;;     (*f place delta-form*) -> new-value
;;;     (/f place delta-form*) -> new-value
;;;
;;; Since: 1.1
;;;
;;; `*f` and `/f` are used for multiplying and dividing
;;; the value of `place`, respectively.
;;;
;;; The number in `place` is multiplied (in the case of `*f`) by delta
;;; or divided (in the case of `/f`) by delta and the result
;;; is stored in `place`.
;;;
;;; Without `delta-form` `/f` will return the reciprocal of the number in `place`,
;;; `*f` will return the number in `place`.
;;;
;;; Without `delta-form` the return type of `*f` will be
;;; the type of the number in `place`, otherwise the return type will be float.
(m%inplace *f (identity) (*))
(m%inplace /f (/) (/))


;;; = Macro: +f, -f
;;;     (+f place delta-form*) -> new-value
;;;     (-f place delta-form*) -> new-value
;;;
;;; Since: 1.1
;;;
;;; `+f` and `+f` are used for adding and subtracting
;;; to/ from the value of `place`, respectively.
;;;
;;; The delta is added (in the case of `*f`) to
;;; or subtracted (in the case of `/f`) from the number in `place`
;;; and the result is stored in `place`.
;;;
;;; Without `delta-form` `-f` will return the negation of the number in `place`,
;;; `+f` will return the number in `place`.
;;;
;;; Without `delta-form` the return type of `+f` will be
;;; the type of the number in `place`, otherwise the return type will be float.
(m%inplace +f (identity) (+))
(m%inplace -f (-) (-))

; undef m%inplace
(defmacro m%inplace)


;;; = Macro: push
;;;     (push item place) -> new-place-value
;;;
;;; Since: 1.1
;;;
;;; `push` prepends `item` to the list that is stored in `place`,
;;; stores the resulting list in `place`, and returns the list.
(defmacro push (item place)
  (if (symbolp place)
        `(setq ,place (cons ,item ,place))
    (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion place)
      `(let* (,@(mapcar list vars vals)
              (,(car store-vars) (cons ,item ,reader-form)))
         ,writer-form))))


;;; = Macro: pop
;;;     (pop place) -> element
;;;
;;; Since: 1.1
;;;
;;; `pop` reads the value of `place`, remembers the car of the list which
;;; was retrieved, writes the cdr of the list back into the `place`,
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


;;; = Function: acons
;;;     (acons key datum alist) -> new-alist
;;;
;;; Since: 1.1
;;;
;;; Prepends `alist` with a new `(key . datum)` tuple
;;; and returns the modified list.
(defun acons (key datum alist)
  (cons (cons key datum) alist))


;;; = Function: not
;;;     (not form) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Logical not.
(define not null) ; this should be faster than (defun not (e) (null e))
(defmacro not (form)
  `(null ,form))


;;; = Macro: and
;;;     (and forms*) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Short-circuiting logical and.
;;; Return `t` unless any of the `forms` evaluate to `nil`,
;;; `nil` otherwise.
(defmacro and args
   (if args
         (if (cdr args)
               `(if ,(car args)
                 (and ,@(cdr args)))
           (car args))
     t))


;;; = Macro: or
;;;     (or forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Short-circuiting logical or.
;;; Return `nil` unless any of the `forms` evaluate to non-nil,
;;; the result of the first form returning non-nil otherwise.
(defmacro or args
  (labels ((m%or (tmp args)
             (if args
                   (if (cdr args)
                         `(if (setq ,tmp ,(car args))
                                ,tmp
                            ,(m%or tmp (cdr args)))
                     (car args))
               nil)))

    (if args
          (if (cdr args)
                (let ((temp (gensym)))
                  `(let ((,temp ,(car args)))
                      (if ,temp
                            ,temp
                        ,(m%or temp (cdr args)))))
            (car args))
      nil)))


;;; = Function: abs
;;;     (abs n) -> result
;;;
;;; Since: 1.1
;;;
;;; Return the absoute value of a number.
(defun abs (n)
  (if (< n 0) (- n) (+ n)))


;;; = Function: zerop
;;;     (zerop number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number zero?
(defun zerop (n) (= n 0))


;;; = Function: evenp
;;;     (evenp number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number even?
(defun evenp (n) (= 0.0 (mod n 2)))


;;; = Function: oddp
;;;     (oddp number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number odd?
(defun oddp (n) (= 1.0 (mod n 2)))


;;; = Function: char=
;;;     (char= characters+) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Return `t` if all of the arguments are the same character.
(defun char= (c . more)
  (if more
        (let loop ((code (char-code c)) (l more))
          (if (= code (char-code (car l)))
                (if (cdr l)
                      (loop code (cdr l))
                  t)
            nil))
    t))


;;; = Function: char
;;;     (char string n) -> nth-character
;;;
;;; Since: 1.1
;;;
;;; Return the n-th character of the string `string`, `n` is 0-based.
(defun char (str n)
  (nth n str))


;;; = Function: equal
;;;     (equal a b) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Return `t` if any of the following is true:
;;;
;;; - `a` and `b` are `eql`
;;; - `a` and `b` are strings, characters or symbols and have the same text value
;;; - `a` and `b` are conses whose car and cdr are `equal` respectively
(defun equal (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (consp a)   (consp b)   (equal (car a) (car b)) (equal (cdr a) (cdr b)))))


;;; = Macro: prog1, prog2
;;;     (prog1 first-form forms*) -> result-1
;;;     (prog2 first-form second-form forms*) -> result-2
;;;
;;; Since: 1.1
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


;;; = Macro: when
;;;     (when condition forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Execute `forms` if `condition` evaluates to true
;;; and return the result of the last form if any.
;;; Otherwise if `condition` evaluates to false,
;;; the forms are not evaluated and the return value
;;; of the `when`-form is `nil`.
(defmacro when (condition . body)
  (list 'if
        condition
        (if (cdr body)
              (cons 'progn body)
          (car body))))


;;; = Macro: unless
;;;     (unless condition forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Execute `forms` if `condition` evaluates to false
;;; and return the result of the last form if any.
;;; Otherwise if `condition` evaluates to true,
;;; the forms are not evaluated and the return value
;;; of the `unless`-form is `nil`.
(defmacro unless (condition . body)
  (list 'if
        condition
        nil
        (if (cdr body)
              (cons 'progn body)
          (car body))))


;;; = Macro: case
;;;      (case keyform (keys forms*)* (t forms*)?) -> result
;;;
;;; Since: 1.1
;;;
;;; `keys` can be a single key or a list of keys, keys will not be evaluated.
;;; `keyform` will be matched against `keys` using `eql`, the `forms` of the
;;; matching clause will be eval'd and the last form determines the result.
;;; Subsequent clauses will be ignored.
;;;
;;; A clause with a key that is a single `t` is used as the default clause
;;; if no key matches.
(defmacro case (keyform . clauses)
  (labels ((do-clause (tmp clause)
             (let ((keydesignator (car clause))
                   (forms (cdr clause)))
               (if keydesignator
                     (if (consp keydesignator)
                           (if (cdr keydesignator)
                                 `((member ,tmp ',keydesignator eql) ,@forms)
                             `((eql ,tmp ',(car keydesignator)) ,@forms))
                       (if (eq 't keydesignator)
                             `(t ,@forms)
                         `((eql ,tmp ',keydesignator) ,@forms)))))))
    (if (atom keyform)
          `(cond ,@(remove nil (mapcar (lambda (clause) (do-clause keyform clause)) clauses)))
      (let ((tmp (gensym)))
        `(let ((,tmp ,keyform))
           (cond ,@(remove nil (mapcar (lambda (clause) (do-clause tmp clause)) clauses))))))))


;;; = Macro: do, do*
;;;     (do ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result
;;;     (do* ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result
;;;
;;; Since: 1.1
;;;
;;; `do` and `do*` iterate over a group of statements while `end-test-form` returns `nil`.
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


;;; = Macro: dotimes
;;;     (dotimes (var count-form result-form*) statement*) -> result
;;;
;;; Since: 1.1
;;;
;;; Similar to CL `dotimes`, see http://clhs.lisp.se/Body/m_dotime.htm,
;;; Murmel however supports multiple result-forms which will be eval'd in an
;;; implicit `progn`, similar to `do` and `do*`;
;;;
;;; Sample usage:
;;;
;;;     (let (l)
;;;       (dotimes (i 10 l)
;;;         (push i l))) ; ==> (9 8 7 6 5 4 3 2 1 0)
(defmacro dotimes (exp . body)
  (let ((var (car exp))
        (countform (cadr exp))
        (count (gensym))
        (loop (gensym))
        (resultform (cddr exp)))
    `(let ((,var 0)
           (,count ,countform))
       (if (<= ,count 0) (progn ,@resultform)
         (if (>= ,var ,count) (progn ,@resultform)
           (let ,loop ()
             ,@body
             (incf ,var)
             (if (>= ,var ,count) (progn ,@resultform)
               (,loop))))))))


;;; = Macro: dolist
;;;     (dolist (var list-form result-form*) statement*) -> result
;;;
;;; Since: 1.1
;;;
;;; Similar to CL `dolist`, see http://clhs.lisp.se/Body/m_dolist.htm
;;; Murmel however supports multiple result-forms which will be eval'd in an
;;; implicit `progn`, similar to `do` and `do*`;
(defmacro dolist (exp . body)
  (let ((var (car exp))
        (listform (cadr exp))
        (lst (gensym))
        (loop (gensym))
        (result (cddr exp)))
    `(let ,loop ((,lst ,listform))
       (if ,lst
             (let ((,var (car ,lst)))
               ,@body
               (,loop (cdr ,lst)))
           ,(if result `(let ((,var nil)) ,@result) nil)))))


;;; = Macro: doplist
;;;     (doplist (key-var value-var plist-form result-form*) statement*) -> result
;;;
;;; Since: 1.2
;;;
;;; Iterates over key-value pairs of `plist-form`.
;;; Similar to Alexandria `doplist`, see https://alexandria.common-lisp.dev/draft/alexandria.html.
(defmacro doplist (exp . body)
  (let ((key-var (car exp))
        (value-var (cadr exp))
        (listform (caddr exp))
        (lst (gensym))
        (loop (gensym))
        (result (cdddr exp)))
    `(let ,loop ((,lst ,listform))
       (if ,lst
             (if (cdr ,lst)
                   (let ((,key-var (car ,lst))
                         (,value-var (cadr ,lst)))
                     ,@body
                     (,loop (cddr ,lst)))
               (fatal "doplist: odd number of elements in plist"))
         (progn ,@result)))))


;(defmacro while (expr . body)
;  `(let loop ()
;     (when ,expr
;        ,@body
;        (loop))))


;;; = Function: identity
;;;     (identity object) -> object
;;;
;;; Since: 1.1
;;;
;;; `identity` returns its argument `object`.
(defun identity (x) x)


;;; = Function: constantly
;;;     (constantly value) -> function
;;;
;;; Since: 1.1
;;;
;;; `constantly` returns a function that accepts any number of arguments,
;;; that has no side-effects, and that always returns `value`.
(defun constantly (value)
  (lambda arguments value))


;;; = Function: complement
;;;     (complement function) -> complement-function
;;;
;;; Since: 1.1
;;;
;;; `complement` returns a function that takes the same arguments as `function`,
;;; and has the same side-effect behavior as `function`, but returns only
;;; a single value: a boolean with the opposite truth value of that
;;; which would be returned as the value of `function`.
(defun complement (f)
  (lambda arguments
    (null (apply f arguments))))


;;; = Function: member
;;;     (member item list [test]) -> tail
;;;
;;; Since: 1.1
;;;
;;; `member` searches list for `item` or for a top-level element that
;;; satisfies the `test`.
;;;
;;; `test` if given must be a function that takes two arguments.
;;; If `test` was omitted or `nil` then `eql` will be used.
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


;;; = Function: reverse
;;;     (reverse sequence) -> reversed-sequence
;;;
;;; Since: 1.1
;;;
;;; If `sequence` is a list then return a fresh list
;;; with elements in reversed order, if `sequence`
;;; is a string then return a fresh reversed string.
(defun reverse (l)
  (labels ((rev (l lp)
             (if l (rev (cdr l) (cons (car l) lp))
               lp)))
    (cond ((null l) nil)
          ((consp l) (rev l nil))
          ((stringp l) (list->string (rev (string->list l) nil)))
          ((simple-vector-p l) (list->simple-vector (rev (simple-vector->list l) nil)))
          (t (fatal "not a sequence")))))


;;; = Function: unzip
;;;     (unzip lst) -> result-list
;;;
;;; Since: 1.2
;;;
;;; `unzip` takes a list of lists, and returns a list
;;; containing the initial element of each such list,
;;; e.g.:
;;;
;;;     (unzip '((1 2) (11 22) (111 222))) ; ==> (1 11 111)
;;;     (unzip '(nil nil nil)) ; ==> (nil nil nil)
;;;     (unzip nil) ; ==> nil
;;;
;;; Similar to SRFI-1 `unzip1`, see https://srfi.schemers.org/srfi-1/srfi-1.html#unzip1.
;;;
;;; See also: [unzip-tails](#function-unzip-tails).
(defun unzip (lists)
  (if lists (cons (caar lists) (unzip (cdr lists)))
    nil))


;;; = Function: unzip-tails
;;;     (unzip-tails lst) -> result-list
;;;
;;; Since: 1.2
;;;
;;; `unzip-tails` takes a list of lists, and returns a list
;;; containing the `cdr`s of each such list.
;;;
;;; See also: [unzip](#function-unzip).
(defun unzip-tails (lists)
  (if lists (cons (cdar lists) (unzip-tails (cdr lists)))
    nil))


;;; = Function: map-into
;;;     (map-into result-list function list*) -> result-list
;;;
;;; Since: 1.2
;;;
;;; Destructively modifies `result-list` to contain the results
;;; of applying `function` to each element in the argument lists in turn.
;;; The iteration terminates when the shortest list (of any of
;;; the lists or the result-list) is exhausted.
;;;
;;; If `result-list` is `nil`, `map-into` returns `nil`.
;;;
;;; Similar to CL `map-into`, see http://clhs.lisp.se/Body/f_map_in.htm.
(defun map-into (result func . lists)
  (when result

     (if (cdr lists)
           ; 2 or more lists given
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t)))
             (let loop ((r result) (l lists))
               (when (and r (none-nil l))
                 (rplaca r (apply func (unzip l)))
                 (loop (cdr r) (unzip-tails l)))))

      (if lists
            ; 1 list given
            (let loop ((r result) (l (car lists)))
              (when (and r l)
                (rplaca r (func (car l)))
                (loop (cdr r) (cdr l))))

        ; 0 lists given
        (let loop ((r result))
          (when r
            (rplaca r (func))
            (loop (cdr r))))))

  result))


; Helper macro to generate defuns for the various maxXX functions
(defmacro m%mapx (name comb acc accn return-list lastelem)
  `(defun ,name (f l . more)
     (if more
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t)))
             (let loop ((args (cons l more)))
               (if (none-nil args)
                     (,comb (apply f ,(if accn (list accn 'args) 'args)) (loop (unzip-tails args)))
                 ,lastelem)))
       (let loop ((l l))
         (if l (,comb (f ,(if acc (list acc 'l) 'l)) (loop (cdr l)))
           ,lastelem)))
    ,@(when return-list '(l))))


;;; = Function: mapcar
;;;     (mapcar function list+) -> list
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent items of the given lists.
;;; All `function` application results will be combined into a list
;;; which is the return value of `mapcar`.
(m%mapx mapcar  cons    car unzip nil nil)


;;; = Function: maplist
;;;     (maplist function list+) -> list
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent tails of the given lists.
;;;
;;; All `function` application results will be combined into a list
;;; which is the return value of `maplist`.
(m%mapx maplist cons    nil nil nil nil)


;;; = Function: mapc
;;;     (mapc function list+) -> first-arg
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent cars items of the given lists.
(m%mapx mapc    progn   car unzip t nil)


;;; = Function: mapl
;;;     (mapl function list+) -> first-arg
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent tails of the given lists.
(m%mapx mapl    progn   nil nil t nil)


;;; = Function: mapcan
;;;     (mapcan function list+) -> concatenated-results
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent items of the given lists.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of `mapcan`.
(m%mapx mapcan  append  car unzip nil nil)


;;; = Function: mapcon
;;;     (mapcon function list+) -> concatenated-results
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent tails of the given lists.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of `mapcon`.
(m%mapx mapcon  append  nil nil nil nil)

; undef m%mapx
(defmacro m%mapx)


; coerce a sequence to a list
(defun m%s2l (seq)
  (cond ((null seq) nil)
        ((consp seq) seq)
        ((stringp seq) (string->list seq))
        ((simple-vector-p seq) (simple-vector->list seq))
        (t (fatal "not a sequence"))))

; Helper macro to generate defuns for every and some
(defmacro m%mapxx (name comb lastelem)
  `(defun ,name (f l . more)
     (if more
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t)))
             (let loop ((args (mapcar m%s2l (cons l more))))
               (if (none-nil args)
                     (,comb (apply f (unzip args)) (loop (unzip-tails args)))
                 ,lastelem)))
       (let loop ((l (m%s2l l)))
         (if l (,comb (f (car l)) (loop (cdr l)))
           ,lastelem)))))


;;; = Function: every
;;;     (every function sequence+) -> boolean
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; Immediately return `nil` if an application of function returns `nil`,
;;; `t` otherwise.
(m%mapxx every and t)


;;; = Function: some
;;;     (some function sequence+) -> result
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; Immediately return the first non-nil-value of an application of `function`,
;;; or `nil` if no applications yield non-nil.
(m%mapxx some or nil)

(defmacro m%mapxx)


;;; = Function: notevery
;;;     (notevery function sequence+) -> boolean
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;;     (notevery predicate sequence+) == (not (every predicate sequence+))
(defun notevery (f seq . more)
  (null (apply some (cons f (cons seq more)))))


;;; = Function: notany
;;;     (notany function sequence+) -> boolean
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;;     (notany predicate sequence+) == (not (some predicate sequence+))
(defun notany (f seq . more)
  (null (apply every (cons f (cons seq more)))))


;;; = Function: remove-if
;;;     (remove-if pred sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without the elements for which `pred`
;;; evaluates to non-nil.
(defun remove-if (pred seq)
  (labels ((remove-if/list (l)
              (if l
                   (let ((obj (car l)))
                     (if (pred obj)
                           (remove-if/list (cdr l))
                       (cons obj (remove-if/list (cdr l)))))
                nil)))

    (cond ((null seq)             nil)
          ((consp seq)            (remove-if/list seq))
          ((stringp seq)          (list->string (remove-if/list (string->list seq))))
          ((simple-vector-p seq)  (list->simple-vector (remove-if/list (simple-vector->list seq))))
          (t (fatal "not a sequence")))))


;;; = Function: remove
;;;     (remove elem sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without occurrences of `elem`.
;;; An occurrence is determined by `eql`.
(defun remove (elem seq)
  (labels ((remove/list (l)
              (if l
                   (let ((obj (car l)))
                     (if (eql elem obj)
                           (remove/list (cdr l))
                       (cons obj (remove/list (cdr l)))))
                nil)))

    (cond ((null seq)             nil)
          ((consp seq)            (remove/list seq))
          ((stringp seq)          (list->string (remove/list (string->list seq))))
          ((simple-vector-p seq)  (list->simple-vector (remove/list (simple-vector->list seq))))
          (t (fatal "not a sequence")))))


;;; = Function: reduce
;;;     (reduce func sequence [from-end-p]) -> result
;;;
;;; Since: 1.1
;;;
;;; If `sequence` is empty then `reduce` will return `(func)`.
;;;
;;; Otherwise if `sequence` contains one element then `reduce` will
;;; return this element.
;;;
;;; Otherwise if `from-end-p` is omitted or `nil` then
;;; `func` will be called with the first two elements
;;; of the `sequence` and subsequently with the previous result
;;; and the next element, and `reduce` will return the last
;;; result from `func`.
;;;
;;; Otherwise if `from-end-p` is given and non-nil then
;;; `func` will be called with the last two elements
;;; of the `sequence` and subsequently with the previous result
;;; and the previous element, and `reduce` will return the last
;;; result from `func`.
(defun reduce (f seq . from-end)
  (let ((from-end-p (car from-end)))
    (labels ((reduce/list (lst)
               (if (cdr lst)
                     (let loop ((elem (car lst))
                                (tail (cdr lst)))
                       (if (cdr tail)
                             (if from-end-p
                                   (f elem (loop (car tail) (cdr tail)))
                               (loop (f elem (car tail)) (cdr tail)))
                         (f elem (car tail))))
                 (car lst))))

      (cond ((null seq)             (f))
            ((consp seq)            (reduce/list seq))
            ((stringp seq)          (reduce/list (string->list seq)))
            ((simple-vector-p seq)  (reduce/list (simple-vector->list seq)))
            (t (fatal "not a sequence"))))))


;;; = Function: write-char
;;;     (write-char c) -> c
;;;
;;; Since: 1.1
;;;
;;; `write-char` outputs `c` to stdout.
(defun write-char (c)
  (if (characterp c)
        (write c nil)
    (fatal "not a character")) c)


;;; = Function: terpri, prin1, princ, print
;;;
;;; Since: 1.1
(defun terpri () (writeln) nil)

(defun prin1 (o) (write o) o)

(defun princ (o) (write o nil) o)

(defun print (o) (lnwrite o) o)


;;; = Function: pprint
;;;     (pprint object) -> t
;;;
;;; Since: 1.1
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


;;; = Function: list-length
;;;     (list-length list-or-string) -> length
;;;
;;; Since: 1.1
;;;
;;; Returns the length of `list-or-string` if it is a string or proper list.
;;; Returns `nil` if `list-or-string` is a circular list.
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


;;; = Function: length
;;;     (length sequence) -> length
;;;
;;; Since: 1.1
;;;
;;; Returns the length of `sequence`.
(defun length (seq)
  (cond ((null seq) 0)
        ((listp seq) (list-length seq))
        ((vectorp seq) (vector-length seq))
        (t (fatal "not a sequence"))))


; helper function for time
(defun call-with-timing (expr . args)
  (let* ((tstart-real (get-internal-real-time))
         (tstart-run  (get-internal-run-time))
         (result (apply expr args))
         (secs-real (/ (- (get-internal-real-time) tstart-real) internal-time-units-per-second))
         (secs-run  (/ (- (get-internal-run-time)  tstart-run) internal-time-units-per-second)))
    (format t "Evaluation took:%n  %g seconds of real time%n  %g seconds of total run time%n" secs-real secs-run)
    result))


;;; = Macro: time
;;;     (time form) -> result
;;;
;;; Since: 1.1
;;;
;;; `time` evaluates `form` and prints various timing data.
(defmacro time (expr)
  `(call-with-timing (lambda () ,expr)))


;;; = Function: circular-list
;;;     (circular-list elems*) -> circular-list
;;;
;;; Since: 1.2
;;;
;;; Creates a circular list of elements.
(defun circular-list elems
  (if elems
        (let ((start (let loop ((elem elems))
                       (if elem (cons (car elem) (loop (cdr elem)))
                         nil))))
          (rplacd (last start) start)
          start)
   nil))


;;; = Function: compose
;;;     (compose func1 funcs*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that composes the given functions, applying the last function first
;;; and the first function last. The compose function allows the last function to consume
;;; any number of values, internal value passing is a single value.
;;;
;;; The input arity of the last function is unrestricted, and it becomes the corresponding arity
;;; of the resulting composition.
;;;
;;; When exactly one function is given, it is returned.
(defun compose (f . more)
  (if more
        (let ((g (apply compose more)))
          (lambda args (f (apply g args))))
    f))


;;; = Function: multiple-value-compose
;;;     (multiple-value-compose func1 funcs*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that composes the given functions, applying the last function first
;;; and the first function last. The compose function allows the last function to consume
;;; any number of values, internal value passing is all return values of the previous function.
;;;
;;; The input arity of the last function is unrestricted, and it becomes the corresponding arity
;;; of the resulting composition.
;;;
;;; When exactly one function is given, it is returned.
(defun multiple-value-compose (f . more)
  (if more
        (let ((g (apply multiple-value-compose more)))
          (lambda args (multiple-value-call f (apply g args))))
    f))


;;; = Function: conjoin
;;;     (conjoin predicate more-predicates*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that applies each of `predicate` and `more-predicates`
;;; functions in turn to its arguments, returning `nil` if any of the predicates
;;; returns false, without calling the remaining predicates. If none of the
;;; predicates returns false, returns the value of the last predicate.
(defun conjoin (predicate . more-predicates)
  (if more-predicates

        (lambda arguments
          (and (apply predicate arguments)
               (let loop ((tail (cdr more-predicates))
                          (head (car more-predicates)))
                 (if tail
                       (if (apply head arguments)
                             (loop (cdr tail) (car tail))
                         nil)
                   (apply head arguments)))))

    predicate))


;;; = Function: disjoin
;;;     (disjoin predicate more-predicates*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that applies each of `predicate` and `more-predicates`
;;; functions in turn to its arguments, returning the value of the first
;;; predicate that returns true, without calling the remaining predicates.
;;; If none of the predicates returns true, `nil` is returned.
(defun disjoin (predicate . more-predicates)
  (lambda arguments
    (or (apply predicate arguments)
        (some (lambda (p)
                (apply p arguments))
          more-predicates))))


;;; = Function: curry
;;;     (curry func args*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that applies `args` and the arguments it is called with to `func`.
(defun curry (func . args)
  (lambda callargs (apply func (append args callargs))))

 
;;; = Function: rcurry
;;;     (rcurry func args*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that applies the arguments it is called with and `args` to `func`.
(defun rcurry (func . args)
  (lambda callargs (apply func (append callargs args))))

 
;;; = Macro: with-gensyms
;;;     (with-gensyms (names*) forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; `with-gensyms` is a macro commonly used by Common Lispers
;;; to help with avoiding name capture when writing macros.
;;; See "Practical Common Lisp, Peter Seibel"
;;; (http://www.gigamonkeys.com/book/macros-defining-your-own.html)
(defmacro with-gensyms (names . body)
  `(let ,(let loop ((names names))
           (if names (cons (list (car names) '(gensym)) (loop (cdr names)))))
     ,@body))


;;; = Macro: ->
;;;     (-> forms*) -> result
;;;
;;; Since: 1.1
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


;;; = Macro: ->>
;;;     (->> forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; thread-last
;;;
;;; Same as `->` but inserts first form as last argument of the second form, and so forth.
;;;
;;; Usage is illustrated by:
;;;
;;;     (macroexpand-1 '(->> 1 f g h))
;;;       ; ==> (h (g (f 1)))
;;;     (macroexpand-1 '(->> 1 (f farg) (g garg) (h harg)))
;;;       ; ==> (h harg (g garg (f farg 1)))
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


;;; = Macro: and->
;;;     (and-> forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Short-circuiting thread-first
;;;
;;; Same as `->` but if one function returns `nil` then the remaining
;;; functions are not called and the overall result is `nil`.
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


;;; = Macro: and->>
;;;     (and->> forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Short circuiting thread-last
;;;
;;; Same as `->>` but if one function returns nil then the remaining
;;; functions are not called and the overall result is `nil`.
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


;;; = Macro: with-accumulator
;;;     (with-accumulator accumulator-name accumulator start-value-form forms*) -> result
;;;
;;; Since: 1.2
;;;
;;; Within `forms`, bind the symbol given by `accumulator-name` to an accumulator-function of one argument
;;; that "accumulates" the arguments of all invocations.
;;; This accumulator-function will be constructed from the two-argument-function `accumulator`
;;; which will be invoked with two arguments: "accumulated value so far" and "argument to `accumulator-name`".
;;; "accumulated-value so far" will be initialized from `start-value-form`.
;;;
;;; Sample usage:
;;;
;;;     (defun factorial (n)
;;;       (with-accumulator mult * 1
;;;         (dotimes (i n)
;;;           (mult (1+ i)))))
;;;
;;;     (factorial 50) ; ==> 3.0414093201713376E64
(defmacro with-accumulator (name accum start-value-form . body)
  (let ((result (gensym))
        (delta (gensym)))
    `(let* ((,result ,start-value-form)
            (,name (lambda (,delta) (setq ,result (,accum ,result ,delta)))))
        ,@body
        ,result)))


;;; = Macro: summing
;;;     (summing forms*) -> result-sum
;;;
;;; Since: 1.2
;;;
;;; Within `forms`, bind `sum` to a function of one argument that sums the arguments
;;; of all invocations.
;;;
;;; Sample usage:
;;;
;;;     (summing (dotimes (i 10) (sum i))) ; ==> 45.0
(defmacro summing body
  `(with-accumulator sum + 0 ,@body))


;;; = Macro: reverse-collecting
;;;     (reverse-collecting forms*) -> result-list
;;;
;;; Since: 1.2
;;;
;;; Within `forms`, bind `collect` to a function of one argument that accumulates
;;; all the arguments it has been called with in reverse order.
;;;
;;; Sample usage:
;;;
;;;     (reverse-collecting (dotimes (i 10) (collect i)))
;;;     ; ==> (9 8 7 6 5 4 3 2 1 0)
(defmacro reverse-collecting body
  `(with-accumulator collect (lambda (l r) (cons r l)) nil ,@body))


;;; = Macro: collecting
;;;     (collecting forms*) -> result-list
;;;
;;; Since: 1.2
;;;
;;; Within `forms`, bind `collect` to a function of one argument that accumulates
;;; all the arguments it has been called with in order.
;;;
;;; Sample usage:
;;;
;;;     (collecting (dotimes (i 10) (collect i)))
;;;     ; ==> (0 1 2 3 4 5 6 7 8 9)
(defmacro collecting body
  (let ((result (gensym))
        (append-to (gensym))
        (delta (gensym)))
    `(let* ((,result (cons nil nil))
            (,append-to ,result)
            (collect (lambda (,delta)
                       (setq ,append-to (cdr (rplacd ,append-to (cons ,delta nil)))))))
        ,@body
        (cdr ,result))))


;;; = Function: plist-keys
;;;     (plist-keys plist) -> result-list
;;;
;;; Since: 1.2
;;;
;;; Return the keys of a plist.
;;;
;;; Sample usage:
;;;
;;;     (plist-keys '(a 1 b 2 c 3)) ; ==> (a b c)
(defun plist-keys (plist)
  (collecting
    (doplist (k v plist)
      (collect k))))


;;; = Function: plist-values
;;;     (plist-values plist) -> result-list
;;;
;;; Since: 1.2
;;;
;;; Return the values of a plist.
;;;
;;; Sample usage:
;;;
;;;     (plist-values '(a 1 b 2 c 3)) ; ==> (1 2 3)
(defun plist-values (plist)
  (collecting
    (doplist (k v plist)
      (collect v))))