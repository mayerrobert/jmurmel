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
;;; - [map](#function-map), [map-into](#function-map-into)
;;; - [mapcar](#function-mapcar), [maplist](#function-maplist), [mapc](#function-mapc), [mapl](#function-mapl), [mapcan](#function-mapcan), [mapcon](#function-mapcon)
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
;;; - [scan](#function-scan), [scan-multiple](#function-scan-multiple), [scan-concat](#function-scan-concat), [dogenerator](#macro-dogenerator)


;;; = Function: caar..cdddr
;;;     (c..r lst) -> result
;;;
;;; Since: 1.1
;;;
;;; `c..r` repeatedly apply `car` and/ or `cdr` as the name suggests.
(defun  caar (lst) (car (car lst)))
(defun  cadr (lst) (car (cdr lst)))
(defun  cdar (lst) (cdr (car lst)))
(defun  cddr (lst) (cdr (cdr lst)))

(defun caaar (lst) (car (caar lst)))
(defun caadr (lst) (car (cadr lst)))
(defun cadar (lst) (car (cdar lst)))
(defun caddr (lst) (car (cddr lst)))

(defun cdaar (lst) (cdr (caar lst)))
(defun cdadr (lst) (cdr (cadr lst)))
(defun cddar (lst) (cdr (cdar lst)))
(defun cdddr (lst) (cdr (cddr lst)))


(defmacro caar (lst)  `(car (car ,lst)))
(defmacro cadr (lst)  `(car (cdr ,lst)))
(defmacro cdar (lst)  `(cdr (car ,lst)))
(defmacro cddr (lst)  `(cdr (cdr ,lst)))

(defmacro caaar (lst) `(car (car (car ,lst))))
(defmacro caadr (lst) `(car (car (cdr ,lst))))
(defmacro cadar (lst) `(car (cdr (car ,lst))))
(defmacro caddr (lst) `(car (cdr (cdr ,lst))))

(defmacro cdaar (lst) `(cdr (car (car ,lst))))
(defmacro cdadr (lst) `(cdr (car (cdr ,lst))))
(defmacro cddar (lst) `(cdr (cdr (car ,lst))))
(defmacro cdddr (lst) `(cdr (cdr (cdr ,lst))))


;;; = Function: nthcdr, nth
;;;     (nthcdr n lst) -> nth-tail
;;;     (nth n lst) -> nth-element
;;;
;;; Since: 1.1
;;;
;;; `nthcdr` applies `cdr` n times and returns the result.
;;; `nth` works as if `(car (nthcdr n lst))` was invoked.
(defun nthcdr (n lst)
  (let loop ((n n) (lst lst))
    (if (<= n 0) lst
      (loop (1- n) (cdr lst)))))

(defun nth (n lst)
  (car (nthcdr n lst)))
(defmacro nth (n lst)
  `(car (nthcdr ,n ,lst)))


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
(defun m%rplaca (lst v) (rplaca lst v) v)


; m%rplacd
;     (m%rplacd lst value) -> value
;
; Replace the cdr of `lst` by `value` and return `value` (as opposed to `rplacd` which returns `lst`).
; Used in setf-expansions.
(defun m%rplacd (lst value) (rplacd lst value) value)


;;; = Macro: destructuring-bind
;;;     (destructuring-bind (vars*) expression forms*)
;;;
;;; Since: 1.1
;;;
;;; Murmel's `destructuring-bind` is a subset of CL's `destructuring-bind`,
;;; trees are not supported, only lists are.
;;;
;;; `destructuring-bind` binds the variables specified in `vars`
;;; to the corresponding values in the list resulting from the evaluation
;;; of `expression`; then `destructuring-bind` evaluates `forms`. 
(defmacro destructuring-bind (vars expr . forms)
  `(apply (lambda ,vars ,@forms) ,expr))


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

              ((eq 'sbit op)
               `((,tmp1 ,tmp2 ,read-var)
                 (,(car args) ,(cadr args) (sbit ,tmp1 ,tmp2))
                 (,read-var)
                 (bvset ,tmp1 ,tmp2 ,read-var)
                 (sbit ,tmp1 ,tmp2)))

              (t (fatal "only symbols, car..cdddr, nth, svref and sbit are supported for 'place'")))))))


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
(define not null)
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
(defmacro and forms
   (if forms
         (if (cdr forms)
               `(if ,(car forms)
                 (and ,@(cdr forms)))
           (car forms))
     t))


;;; = Macro: or
;;;     (or forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Short-circuiting logical or.
;;; Return `nil` unless any of the `forms` evaluate to non-nil,
;;; the result of the first form returning non-nil otherwise.
(defmacro or forms
  (labels ((m%or (tmp forms)
             (if forms
                   (if (cdr forms)
                         `(if (setq ,tmp ,(car forms))
                                ,tmp
                            ,(m%or tmp (cdr forms)))
                     (car forms))
               nil)))

    (if forms
          (if (cdr forms)
                (let ((temp (gensym)))
                  `(let ((,temp ,(car forms)))
                      (if ,temp
                            ,temp
                        ,(m%or temp (cdr forms)))))
            (car forms))
      nil)))


;;; = Function: abs
;;;     (abs number) -> result
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
;;;     (char str n) -> nth-character
;;;
;;; Since: 1.1
;;;
;;; Return the n-th character of the string `str`, `n` is 0-based.
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
;;;     (prog1 first-form more-forms*) -> result-1
;;;     (prog2 first-form second-form more-forms*) -> result-2
;;;
;;; Since: 1.1
(defmacro prog1 (first-form . more-forms)
  (if more-forms
        (let ((result (gensym)))
          `(let ((,result ,first-form))
             ,@more-forms
             ,result))
    `(values ,first-form)))

(defmacro prog2 (first-form second-form . more-forms)
  (if more-forms
        (let ((ignore (gensym))
              (result (gensym)))
          `(let ((,ignore ,first-form)
                 (,result ,second-form))
             ,@more-forms
             ,result))
    `(progn ,first-form (values ,second-form))))


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
(defmacro dotimes (loop-def . body)
  (let ((var (car loop-def))
        (countform (cadr loop-def))
        (count (gensym))
        (loop (gensym))
        (resultform (cddr loop-def)))
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
(defmacro dolist (loop-def . body)
  (let ((var (car loop-def))
        (listform (cadr loop-def))
        (lst (gensym))
        (loop (gensym))
        (result (cddr loop-def)))
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
(defmacro doplist (loop-def . body)
  (let ((key-var (car loop-def))
        (value-var (cadr loop-def))
        (listform (caddr loop-def))
        (lst (gensym))
        (loop (gensym))
        (result (cdddr loop-def)))
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
(defun identity (value) value)


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
(defun complement (func)
  (lambda arguments
    (null (apply func arguments))))


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
(defun member (item lst . test)
  (let* ((tst (car test))
         (pred (if tst tst eql)))
    (if lst
          (if (pred item (car lst))
                lst
            (member item (cdr lst) pred))
      nil)))


;;; = Function: reverse
;;;     (reverse sequence) -> reversed-sequence
;;;
;;; Since: 1.1
;;;
;;; If `sequence` is a list then return a fresh list
;;; with elements in reversed order, if `sequence`
;;; is a vector then return a fresh reversed vector.
(defun reverse (seq)
  (labels ((rev (l lp)
             (if l (rev (cdr l) (cons (car l) lp))
               lp)))
    (cond ((null seq) nil)
          ((consp seq) (rev seq nil))
          ((stringp seq) (list->string (rev (string->list seq) nil)))
          ((simple-vector-p seq) (list->simple-vector (rev (simple-vector->list seq) nil)))
          ((simple-bit-vector-p seq) (list->simple-bit-vector (rev (simple-bit-vector->list seq) nil)))
          (t (fatal "not a sequence")))))


;;; = Function: unzip
;;;     (unzip lists) -> result-list
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
;;;     (unzip-tails lists) -> result-list
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


(defun m%sequence->list (seq)
  (cond ((listp seq) seq)
        ((simple-vector-p seq) (simple-vector->list seq))
        ((simple-bit-vector-p seq) (simple-bit-vector->list seq))
        ((stringp seq) (string->list seq))
        (t (fatal "not a sequence"))))

(defun m%sequences->lists (sequences)
  (let loop ((s sequences))
    (if s (cons (m%sequence->list (car s))
                (loop (cdr s)))
      nil)))

(defun m%list->sequence (lst result-type)
  (cond ((null result-type)                  nil)
        ((eq result-type 'list)              lst)
        ((eq result-type 'cons)              (if lst lst (fatal "not of type cons: nil")))
        ((eq result-type 'vector)            (list->simple-vector lst))
        ((eq result-type 'simple-vector)     (list->simple-vector lst))
        ((eq result-type 'simple-bit-vector) (list->simple-bit-vector lst))
        ((eq result-type 'string)            (list->string lst))
        ((eq result-type 'simple-string)     (list->string lst))
        (t (fatal "not a sequence type"))))


;;; = Function: map
;;;     (map result-type function sequences+) -> result
;;;
;;; Since 1.3
;;;
;;; Applies `function` to successive sets of arguments in which one argument
;;; is obtained from each sequence. The function is called first on all the elements
;;; with index 0, then on all those with index 1, and so on.
;;; The result-type specifies the type of the resulting sequence.
;;;
;;; `map` returns `nil` if `result-type` is `nil`. Otherwise, `map` returns a sequence
;;; such that element j is the result of applying `function` to element j of each
;;; of the sequences. The result sequence is as long as the shortest of the sequences.
;;;
;;; Similar to CL `map`, see http://clhs.lisp.se/Body/f_map.htm.
(defun map (result-type func seq . more-sequences)
  (setq seq (if more-sequences

                  (labels ((none-nil (lists)
                             (if lists (and (car lists) (none-nil (cdr lists)))
                               t)))
                    (if result-type
                            (let* ((result (cons nil nil))
                                   (append-to result))
                              (let loop ((l (m%sequences->lists (cons seq more-sequences))))
                                (when (none-nil l)
                                  (setq append-to (cdr (rplacd append-to (cons (apply func (unzip l)) nil))))
                                  (loop (unzip-tails l)))
                              (cdr result)))
                      (let loop ((l (m%sequences->lists (cons seq more-sequences))))
                        (when (none-nil l)
                          (apply func (unzip l))
                          (loop (unzip-tails l))))))

              (if result-type
                      (let* ((result (cons nil nil))
                             (append-to result))
                        (let loop ((l (m%sequence->list seq)))
                          (when l
                            (setq append-to (cdr (rplacd append-to (cons (func (car l)) nil))))
                            (loop (cdr l))))
                        (cdr result))
                (let loop ((l (m%sequence->list seq)))
                  (when l
                    (func (car l))
                    (loop (cdr l)))))))

  (cond ((null result-type) nil)
        ((eq result-type 'list) seq)
        (t (m%list->sequence seq result-type))))


;;; = Function: map-into
;;;     (map-into result-list function sequence*) -> result-list
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
;;; Similar to CL `map-into`, see http://clhs.lisp.se/Body/f_map_in.htm,
;;; only lists are supported as result-list, tough.
(defun map-into (result func . sequences)
  (when result

    (if (cdr sequences)
          ; 2 or more sequences given
          (labels ((none-nil (lists)
                     (if lists (and (car lists) (none-nil (cdr lists)))
                       t)))
            (let loop ((r result) (l (m%sequences->lists sequences)))
              (when (and r (none-nil l))
                (rplaca r (apply func (unzip l)))
                (loop (cdr r) (unzip-tails l)))))

      (if sequences
            ; 1 list given
            (let loop ((r result) (l (m%sequence->list (car sequences))))
              (when (and r l)
                (rplaca r (func (car l)))
                (loop (cdr r) (cdr l))))

        ; 0 sequences given
        (let loop ((r result))
          (when r
            (rplaca r (func))
            (loop (cdr r))))))

  result))


; Helper macros to generate defuns for the various maxXX functions
(defmacro m%mapx (name acc accn)
  `(defun ,name (func lst . more-lists)
     (if more-lists
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t)))
             (let loop ((args (cons lst more-lists)))
               (when (none-nil args)
                 (apply func ,(if accn (list accn 'args) 'args))
                 (loop (unzip-tails args)))))
       (let loop ((lst lst))
         (when lst
           (func ,(if acc (list acc 'lst) 'lst))
           (loop (cdr lst)))))
    lst))

(defmacro m%mapx-cons (name acc accn)
  `(defun ,name (func lst . more-lists)
     (let* ((result (cons nil nil)) (append-to result))
       (if more-lists
             (labels ((none-nil (lists)
                        (if lists (and (car lists) (none-nil (cdr lists)))
                          t)))
               (let loop ((args (cons lst more-lists)))
                 (when (none-nil args)
                   (setq append-to (cdr (rplacd append-to (cons (apply func ,(if accn (list accn 'args) 'args)) nil))))
                   (loop (unzip-tails args)))))
         (let loop ((lst lst))
           (when lst
             (setq append-to (cdr (rplacd append-to (cons (func ,(if acc (list acc 'lst) 'lst)) nil))))
             (loop (cdr lst)))))

       (cdr result))))

(defmacro m%mapx-nconc (name acc accn)
  `(defun ,name (func lst . more-lists)
     (let* ((result (cons nil nil)) (append-to result) tmp)
       (if more-lists
             (labels ((none-nil (lists)
                        (if lists (and (car lists) (none-nil (cdr lists)))
                          t)))
               (let loop ((args (cons lst more-lists)))
                 (when (none-nil args)
                   (setq tmp (apply func ,(if accn (list accn 'args) 'args)))
                   (nconc append-to tmp)
                   (when tmp (setq append-to tmp))
                   (loop (unzip-tails args)))))
         (let loop ((lst lst))
           (when lst
             (setq tmp (func ,(if acc (list acc 'lst) 'lst)))
             (nconc append-to tmp)
             (when tmp (setq append-to tmp))
             (loop (cdr lst)))))

       (cdr result))))


;;; = Function: mapcar
;;;     (mapcar function list+) -> list
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent items of the given lists.
;;; All `function` application results will be combined into a list
;;; which is the return value of `mapcar`.
(m%mapx-cons mapcar car unzip)


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
(m%mapx-cons maplist nil nil)


;;; = Function: mapc
;;;     (mapc function list+) -> first-arg
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent cars items of the given lists.
(m%mapx mapc car unzip)


;;; = Function: mapl
;;;     (mapl function list+) -> first-arg
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent tails of the given lists.
(m%mapx mapl nil nil)


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
(m%mapx-nconc mapcan car unzip)


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
(m%mapx-nconc mapcon nil nil)

; undef m%mapx and friends
(defmacro m%mapx)
(defmacro m%mapx-cons)
(defmacro m%mapx-combine)


;;; = Function: scan
;;;     (scan start [step [endincl]])                -> generator-function that returns subsequent numbers starting from `start` incrementing by `step` (default: 1)
;;;     (scan sequence [start-idx [stop-idx-excl]])  -> generator-function that returns subsequent elements of the given sequence (list or vector)
;;;
;;; Since: 1.3
;;;
;;; `scan` creates a generator function that on subsequent calls produces subsequent values.
;;; A generator function takes no arguments and on subsequent applications returns `(values <next-value> t)`
;;; or `(values <undefined-value> nil)` to indicate "all values are exhausted".
(defun scan (arg . more-args)
  (cond ((numberp arg)
         (let* ((start arg)
                (step (if more-args (car more-args) 1))
                (endincl (cadr more-args)))

           (if endincl

                 (cond ((= step 1)
                        (lambda ()
                          (if (<= start endincl)
                                (values start
                                        (progn (incf start) t))
                            (values nil nil))))

                       ((= step -1)
                        (lambda ()
                          (if (>= start endincl)
                                (values start
                                        (progn (decf start) t))
                            (values nil nil))))

                       ((= step 0)
                        (lambda () (values start t)))

                       ((> step 0)
                        (setq start (* start 1.0))
                        (lambda ()
                          (if (<= start endincl)
                                (values start
                                        (progn (incf start step) t))
                            (values nil nil))))

                       (t
                        (setq start (* start 1.0))
                        (lambda ()
                          (if (>= start endincl)
                                (values start
                                        (progn (incf start step) t))
                            (values nil nil)))))

             (cond ((= step 1)
                    (lambda ()
                      (values start
                              (progn (incf start) t))))

                   ((= step -1)
                    (lambda ()
                      (values start
                              (progn (decf start) t))))
                   ((= step 0)
                    (lambda () (values start t)))

                   (t
                    (lambda ()
                      (values start
                              (progn (incf start step) t))))))))

        ((consp arg)
         (when more-args (setq arg (nthcdr (car more-args) arg)))
         (if (cadr more-args)

               (let* ((n (- (cadr more-args) (car more-args))))
                 (lambda ()
                   (if (and arg (> n 0))
                         (values (prog1 (car arg) (setq arg (cdr arg)) (decf n))
                                 t)
                     (values nil nil))))

           (lambda ()
             (values (car arg)
                     (when arg
                       (setq arg (cdr arg))
                       t)))))

       ((vectorp arg)
        (let* ((ref (cond ((simple-vector-p arg) svref)
                          ((simple-bit-vector-p arg) sbit)
                          ((stringp arg) char)))
               (len (vector-length arg))
               (idx (if more-args (car more-args) 0)))
          (when (cdr more-args)
            (if (< (cadr more-args) len) (setq len (cadr more-args)))) 
          (lambda ()
            (if (< idx len)
                  (values (ref arg idx)
                          (progn (setq idx (1+ idx)) t))
              (values nil nil)))))

       ((null arg)
        (lambda () (values nil nil)))

       (t (fatal "scan: cannot create a generator function from given arguments"))))


;;; = Function: scan-multiple
;;;     (scan-multiple generator+) -> generator
;;;
;;; Since: 1.3
;;;
;;; `scan-multiple` combines several generators into a single generator function
;;; that returns a list with subsequent values of all generators,
;;; and whose secondary value is nil if any generator returns nil as their secondary value.
;;; Once the first generator indicates "at end" for the first time no more generators will be called.
(defun scan-multiple (generator . more-generators)
  (if more-generators

        (let ((generators (cons generator more-generators)) (more-accum t))
          (lambda ()
            (if more-accum
                  (let* ((list-accum (cons nil nil)) (append-to list-accum))
                    (let loop ((x generators))
                      (if x
                        (if more-accum
                          (multiple-value-bind (result more) ((car x))
                            (if more
                                  (progn (setq append-to (cdr (rplacd append-to (cons result nil)))) (loop (cdr x)))
                              (setq more-accum nil))))))
                    (values (cdr list-accum) more-accum))
              (values nil nil))))

    (lambda ()
      (if generator
            (multiple-value-bind (result more) (generator)
              (if more
                    (values (list result) more)
                (values (setq generator nil) nil)))
        (values nil nil)))))


;;; = Function: scan-concat
;;;     (scan-concat generator+) -> generator
;;;
;;; Since: 1.3
;;;
;;; `scan-concat` combines several generators into a single generator function
;;; that acts as if the given generators were concatenated.
;;; 
;;; A single generator would be returned unchanged.
(defun scan-concat (generator . more-generators)
  (if (functionp generator) nil (fatal "not a generator"))
  (if more-generators
        (let ((more-generators more-generators))
          (lambda ()
            (if generator
                  (multiple-value-bind (value more) (generator)
                    (if more
                          (values value more)
                      (if more-generators
                            (progn
                              (setq generator (car more-generators))
                              (setq more-generators (cdr more-generators))
                              (generator))
                        (values nil nil))))
              (values nil nil))))
    generator))


;;; = Macro: dogenerator
;;;     (dogenerator (var generator-form result-form*) statement*) -> result
;;;
;;; Since: 1.3
;;;
;;; `dogenerator` creates a generator by eval'ing `generator-form`
;;; and iterates over the values yielded by subsequent generator applications.
(defmacro dogenerator (loop-def . body)
  (let ((var (car loop-def))
        (more (gensym))
        (generator (gensym))
        (loop (gensym))
        (result (cddr loop-def)))
    `(let ((,generator ,(cadr loop-def)))
       (labels ((,loop (,var ,more)
                  (when ,more
                    ,@body
                    (multiple-value-call ,loop (,generator)))))
         (multiple-value-call ,loop (,generator))
         ,(if result `(let ((,var nil)) ,@result) nil)))))


; Helper macro to generate defuns for every and some
(defmacro m%mapxx (name comb lastelem)
  `(defun ,name (pred lst . more-lists)
     (if more-lists
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t)))
             (let loop ((args (mapcar m%sequence->list (cons lst more-lists))))
               (if (none-nil args)
                     (,comb (apply pred (unzip args)) (loop (unzip-tails args)))
                 ,lastelem)))
       (let loop ((lst (m%sequence->list lst)))
         (if lst (,comb (pred (car lst)) (loop (cdr lst)))
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
(defun notevery (pred seq . more-sequences)
  (null (apply every (cons pred (cons seq more-sequences)))))


;;; = Function: notany
;;;     (notany function sequence+) -> boolean
;;;
;;; Since: 1.1
;;;
;;; `function` must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;;     (notany predicate sequence+) == (not (some predicate sequence+))
(defun notany (pred seq . more-sequences)
  (null (apply some (cons pred (cons seq more-sequences)))))


;;; = Function: remove-if
;;;     (remove-if pred sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without the elements for which `pred`
;;; evaluates to non-nil.
(defun remove-if (pred seq)
  (labels ((remove-if/list (l)
              (let* ((result (cons nil nil))
                     (append-to result))
                (let loop ((l l))
                  (when l
                    (unless (pred (car l))
                      (setq append-to (cdr (rplacd append-to (cons (car l) nil)))))
                    (loop (cdr l))))
                (cdr result))))

    (cond ((null seq)             nil)
          ((consp seq)            (remove-if/list seq))
          ((stringp seq)          (list->string (remove-if/list (string->list seq))))
          ((simple-vector-p seq)  (list->simple-vector (remove-if/list (simple-vector->list seq))))
          ((simple-bit-vector-p seq)  (list->simple-bit-vector (remove-if/list (simple-bit-vector->list seq))))
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
              (let* ((result (cons nil nil))
                     (append-to result))
                (let loop ((l l))
                  (when l
                    (unless (eql elem (car l))
                      (setq append-to (cdr (rplacd append-to (cons (car l) nil)))))
                    (loop (cdr l))))
                (cdr result))))

    (cond ((null seq)             nil)
          ((consp seq)            (remove/list seq))
          ((stringp seq)          (list->string (remove/list (string->list seq))))
          ((simple-vector-p seq)  (list->simple-vector (remove/list (simple-vector->list seq))))
          ((simple-bit-vector-p seq)  (list->simple-bit-vector (remove/list (simple-bit-vector->list seq))))
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

      (cond ((null seq)                (f))
            ((consp seq)               (reduce/list seq))
            ((stringp seq)             (reduce/list (string->list seq)))
            ((simple-vector-p seq)     (reduce/list (simple-vector->list seq)))
            ((simple-bit-vector-p seq) (reduce/list (simple-bit-vector->list seq)))
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

(defun prin1 (obj) (write obj) obj)

(defun princ (obj) (write obj nil) obj)

(defun print (obj) (lnwrite obj) obj)


;;; = Function: pprint
;;;     (pprint object) -> t
;;;
;;; Since: 1.1
;;;
;;; Simple pretty printer,
;;; based on https://picolisp.com/wiki/?prettyPrint .
(defun pprint (obj)
  (labels
    ((size (l)
       (if l
             (if (consp l)
                   (+ (size (car l)) (size (cdr l)))
               1)
         1))

     (pp (obj l)
        (dotimes (ign (* l 3)) (write-char #\ ))
        (if (< (size obj) 6)
              (write obj)
          (progn
            (write-char #\()
            (let loop ()
              (when
                (and
                   (member
                      (princ (pop obj))
                      '(lambda let let* letrec defun define defmacro setq setf if when unless dotimes dolist))
                   (< (size (car obj)) 7))
                (write-char #\ )
                (loop)))
            (let loop ()
              (when obj
                (write-char #\Newline)
                (if (atom obj)
                      (pp obj (1+ l)))
                  (progn (pp (pop obj) (1+ l)) (loop))))
            (write-char #\))
            t))))

    (writeln)
    (pp obj 0)))


;;; = Function: list-length
;;;     (list-length list-or-string) -> length
;;;
;;; Since: 1.1
;;;
;;; Returns the length of `list-or-string` if it is a string or proper list.
;;; Returns `nil` if `list-or-string` is a circular list.
;;;
;;; See http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html
(defun list-length (lst) 
  (let loop ((n 0)         ; Counter 
             (fast lst)      ; Fast pointer: leaps by 2 
             (slow lst))     ; Slow pointer: leaps by 1 
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
(defun call-with-timing (func . args)
  (let* ((tstart-real (get-internal-real-time))
         (tstart-run  (get-internal-run-time))
         (result (apply func args))
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
(defmacro time (form)
  `(call-with-timing (lambda () ,form)))


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
(defun compose (func . more-functions)
  (if more-functions
        (let ((g (apply compose more-functions)))
          (lambda args (func (apply g args))))
    func))


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
(defun multiple-value-compose (func . more-functions)
  (if more-functions
        (let ((g (apply multiple-value-compose more-functions)))
          (lambda args (multiple-value-call func (apply g args))))
    func))


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
                             (list (cons (caar tail) (append (cdar tail) (list temp)))))))))
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


(provide "mlib")
