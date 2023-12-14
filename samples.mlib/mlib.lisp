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

;;; == mlib functions and macros
;;;
;;; mlib provides the following Common Lisp-like functions and macros:
;;;
;;; - logic, program structure
;;;     - [not](#function-not), [and](#macro-and), [or](#macro-or)
;;;     - [prog1, prog2](#macro-prog1-prog2)
;;;     - [when](#macro-when), [unless](#macro-unless), [case](#macro-case), [typecase](#macro-typecase)

;;; - conses and lists
;;;     - [caar..cdddr](#function-caarcdddr), [nthcdr, dotted-nthcdr, nth](#function-nthcdr-dotted-nthcdr-nth), [copy-list](#function-copy-list)
;;;     - [list-length](#function-list-length), [last](#function-last), [butlast](#function-butlast), [nbutlast](#function-nbutlast), [ldiff](#function-ldiff), [tailp](#function-tailp)
;;;     - [nconc](#function-nconc), [revappend, nreconc](#function-revappend-nreconc), [member](#function-member), [adjoin](#function-adjoin)
;;;     - [acons](#function-acons)
;;;     - [mapcar](#function-mapcar), [maplist](#function-maplist), [mapc](#function-mapc), [mapl](#function-mapl), [mapcan](#function-mapcan), [mapcon](#function-mapcon)
;;;     - [multiple-value-list](#macro-multiple-value-list), [nth-value](#macro-nth-value)

;;; - iteration
;;;     - [do, do*](#macro-do-do), [dotimes](#macro-dotimes), [dolist](#macro-dolist)

;;; - places
;;;     - [destructuring-bind](#macro-destructuring-bind)
;;;     - [get-setf-expansion](#function-get-setf-expansion)
;;;     - [setf](#macro-setf), [incf, decf](#macro-incf-decf)
;;;     - [push](#macro-push), [pop](#macro-pop), [pushnew](#macro-pushnew)

;;; - numbers, characters
;;;     - [abs](#function-abs), [min](#function-min), [max](#function-max), [zerop](#function-zerop), [evenp](#function-evenp), [oddp](#function-oddp)
;;;     - [char=](#function-char), [char](#function-char-1), [bit](#function-bit)
;;;     - [parse](#function-parse), [parse-integer](#function-parse-integer)

;;; - sequences
;;;     - [elt](#function-elt), [copy-seq](#function-copy-seq), [length](#function-length)
;;;     - [reverse](#function-reverse), [nreverse](#function-nreverse)
;;;     - [remove-if](#function-remove-if), [remove](#function-remove)
;;;     - [map](#function-map), [map-into](#function-map-into), [reduce](#function-reduce)

;;; - hash tables
;;;     - [gethash](#function-gethash), [remhash](#function-remhash), [maphash](#function-maphash)

;;; - higher order
;;;     - [identity](#function-identity), [constantly](#function-constantly), [complement](#function-complement)
;;;     - [every](#function-every), [some](#function-some), [notevery](#function-notevery), [notany](#function-notany)

;;; - I/O
;;;     - [write-char](#function-write-char)
;;;     - [terpri, prin1, princ, print](#function-terpri-prin1-princ-print), [pprint](#function-pprint)
;;;     - [with-output-to-string](#macro-with-output-to-string)

;;; - misc
;;;     - [time](#macro-time)
;;;
;;; functions and macros inspired by [Alexandria](https://alexandria.common-lisp.dev):
;;;
;;; - conses and lists
;;;     - [circular-list](#function-circular-list)
;;; - iteration
;;;     - [doplist](#macro-doplist)
;;; - higher order
;;;     - [compose](#function-compose), [multiple-value-compose](#function-multiple-value-compose)
;;;     - [conjoin](#function-conjoin), [disjoin](#function-disjoin)
;;;     - [curry](#function-curry), [rcurry](#function-rcurry)
;;; - misc
;;;     - [with-gensyms](#macro-with-gensyms)
;;;
;;; functions inspired by [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html)
;;;
;;; - conses and lists
;;;     - [unzip](#function-unzip)
;;;
;;; functions and macros inspired by [serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md)
;;;
;;; - conses and lists
;;;     - [plist-keys](#function-plist-keys), [plist-values](#function-plist-values)
;;; - misc
;;;     - [with-accumulator](#macro-with-accumulator), [summing](#macro-summing), [collecting](#macro-collecting), [reverse-collecting](#macro-reverse-collecting)
;;;
;;; as well as the following additional functions and macros:
;;;
;;; - logic and program structure
;;;     - [->](#macro), [->>](#macro-1), [and->](#macro-and-1), [and->>](#macro-and-2)
;;; - conses and lists
;;;     - [unzip-tails](#function-unzip-tails)
;;; - iteration
;;;     - [dovector](#macro-dovector), [dogenerator](#macro-dogenerator)
;;; - places
;;;     - [*f, /f, +f, -f](#macro-f-f)
;;; - generators
;;;     - [scan](#function-scan), [scan-multiple](#function-scan-multiple), [scan-concat](#function-scan-concat)
;;; - strings
;;;     - [string-trim](#function-string-trim), [string-subseq](#function-string-subseq), [string-replace](#function-string-replace), [string-split](#function-string-split), [string-join](#function-string-join)


;;; == Description of functions and macros

; logic, program structure ********************************************

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
                     (car forms)))))

    (if forms
          (if (cdr forms)
                (let ((temp (gensym)))
                  `(let ((,temp ,(car forms)))
                      (if ,temp
                            ,temp
                        ,(m%or temp (cdr forms)))))
            (car forms)))))


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
  (labels ((do-key (tmp key)
             (if (symbolp key)
                   `(eq ,tmp ',key)
               `(eql ,tmp ',key)))

           (do-keylist (tmp keylist)
             (if (cdr keylist)
                   `(or ,@(mapcar (lambda (k) (do-key tmp k)) keylist))
               (do-key tmp (car keylist))))

           (do-clause (tmp clause)
             (let ((keydesignator (car clause))
                   (forms (cdr clause)))
               (if keydesignator
                     (if (consp keydesignator)
                           (list* (do-keylist tmp keydesignator) forms)
                       (if (eq 't keydesignator)
                             `(t ,@forms)
                         `(,(do-key tmp keydesignator) ,@forms))))))

           (do-clauses (key)
             (let* ((result (cons () ()))
                    (append-to result)
                    clause)
               (let loop ((clauses clauses))
                 (when clauses
                   (setq clause (do-clause key (car clauses)))
                   (if clause (setq append-to (cdr (rplacd append-to (cons clause ())))))
                   (loop (cdr clauses))))
             (cdr result))))

    (if (atom keyform)
          `(cond ,@(do-clauses keyform))
      (let ((tmp (gensym)))
        `(let ((,tmp ,keyform))
           (cond ,@(do-clauses tmp)))))))


;;; = Macro: typecase
;;;      (typecase keyform (type forms*)* (t forms*)?) -> result
;;;
;;; Since: 1.3
;;;
;;; `typecase` allows the conditional execution of a body of forms in a clause
;;; that is selected by matching the test-key on the basis of its type.
;;;
;;; The keyform is evaluated to produce the test-key.
;;;
;;; Each of the normal-clauses is then considered in turn.
;;; If the test-key is of the type given by the clauses's type,
;;; the forms in that clause are evaluated as an implicit progn,
;;; and the values it returns are returned as the value of the typecase form.
;;;
;;; If no normal-clause matches, and there is an otherwise-clause,
;;; then that otherwise-clause automatically matches;
;;; the forms in that clause are evaluated as an implicit progn,
;;; and the values it returns are returned as the value of the typecase.
;;;
;;; If there is no otherwise-clause, typecase returns nil.
(defmacro typecase (keyform . clauses)
  (labels ((do-clause (tmp clause)
             (let ((keydesignator (car clause)))
               (if (and keydesignator (symbolp keydesignator))
                     `((typep ,tmp ',keydesignator) ,@(cdr clause))
                 (error 'simple-error "bad clause in typecase: %s" clause))))

           (do-clauses (key)
             (let* ((result (cons () ()))
                    (append-to result))
               (let loop ((clauses clauses))
                 (when clauses
                   (setq append-to (cdr (rplacd append-to (cons (do-clause key (car clauses)) ()))))
                   (loop (cdr clauses))))
             (cdr result))))

    (if (atom keyform)
          `(cond ,@(do-clauses keyform))
      (let ((tmp (gensym)))
        `(let ((,tmp ,keyform))
           (cond ,@(do-clauses tmp)))))))


; conses and lists ****************************************************

(defmacro m%def-macro-fun (name params . body)
  `(progn
     (defmacro ,name ,params ,@body)
     (defun ,name ,params (,name ,@params))))


;;; = Function: caar..cdddr
;;;     (c..r lst) -> result
;;;
;;; Since: 1.1
;;;
;;; `c..r` repeatedly apply `car` and/ or `cdr` as the name suggests.
(m%def-macro-fun caar (lst)  `(car (car ,lst)))
(m%def-macro-fun cadr (lst)  `(car (cdr ,lst)))
(m%def-macro-fun cdar (lst)  `(cdr (car ,lst)))
(m%def-macro-fun cddr (lst)  `(cdr (cdr ,lst)))

(m%def-macro-fun caaar (lst) `(car (caar ,lst)))
(m%def-macro-fun caadr (lst) `(car (cadr ,lst)))
(m%def-macro-fun cadar (lst) `(car (cdar ,lst)))
(m%def-macro-fun caddr (lst) `(car (cddr ,lst)))

(m%def-macro-fun cdaar (lst) `(cdr (caar ,lst)))
(m%def-macro-fun cdadr (lst) `(cdr (cadr ,lst)))
(m%def-macro-fun cddar (lst) `(cdr (cdar ,lst)))
(m%def-macro-fun cdddr (lst) `(cdr (cddr ,lst)))


(defun m%nonneg-integer-number (n)
  (cond ((integerp n)
         (if (< n 0) (error 'simple-type-error "must be an integer >= 0: %s" n))
         n)

        ((numberp n)
         (if (< n 0) (error 'simple-type-error "must be an integer >= 0: %s" n))
         (if (/= n (truncate n)) (error 'simple-type-error "must be an integer >= 0: %s" n))
         (truncate n))

        (t (error 'simple-type-error "must be an integer >= 0: %s" n))))


;;; = Function: nthcdr, dotted-nthcdr, nth
;;;     (nthcdr n lst) -> nth-tail
;;;     (dotted-nthcdr n lst) -> nth-tail
;;;     (nth n lst) -> nth-element
;;;
;;; Since: 1.1
;;;
;;; `nthcdr` applies `cdr` n times and returns the result.
;;; `dotted-nthcdr` works similar to `nthcdr` except:
;;; going past the end of a dotted list returns `nil`
;;; (and not an error as `nthcdr` would).
;;; `nth` works as if `(car (nthcdr n lst))` was invoked.
(defun nthcdr (n lst)
  (let loop ((n (m%nonneg-integer-number n)) (lst lst))
    (if (<= n 0) lst
      (if lst
            (loop (1- n) (cdr lst))
        ()))))

; For [n]butlast
(defun dotted-nthcdr (n lst)
  (let loop ((n (m%nonneg-integer-number n)) (lst lst))
    (if (<= n 0) lst
      (if (consp lst)
            (loop (1- n) (cdr lst))
        ()))))


(m%def-macro-fun nth (n lst)
  `(car (nthcdr ,n ,lst)))


;;; = Function: copy-list
;;;
;;;     (copy-list lst) -> copy
;;;
;;; Since: 1.3
;;;
;;; Returns a copy of `lst`. If `lst` is a dotted list,
;;; the resulting list will also be a dotted list.
;;;
;;; Only the list structure of `lst` is copied;
;;; the elements of the resulting list are the same
;;; as the corresponding elements of the given list.
(defun copy-list (lst)
  (let* loop ((lst lst)
              (result (cons () ()))
              (append-to result))
    (if (consp lst)
          (loop (cdr lst) result (cdr (rplacd append-to (cons (car lst) ()))))
      (progn
        (if lst (rplacd append-to lst))
        (cdr result)))))


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
  (if lists (cons (caar lists) (unzip (cdr lists)))))


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
  (if lists (cons (cdar lists) (unzip-tails (cdr lists)))))


;;; = Function: list-length
;;;     (list-length list-or-string) -> length
;;;
;;; Since: 1.1
;;;
;;; Returns the length of `list-or-string` if it is a string or proper list.
;;; Returns `nil` if `list-or-string` is a circular list.
(defun list-length (lst)
  ; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html
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


;;; = Function: last
;;;     (last lst n?) -> last-cons-or-nil
;;;
;;; Since: 1.2
;;;
;;; `last` returns the last `n` conses (not the last `n` elements)
;;; of a proper or dotted list or `nil` for the empty list.
;;;
;;; If `n` is zero, the atom that terminates list is returned.
;;; If `n` is greater than or equal to the number of cons cells in list,
;;; the result is `lst`.
(defmacro last0-macro ()
  `(let loop ((rest lst))
     (if (consp rest)
       (loop (cdr rest))
       rest)))

(defun last0 (lst)
  (last0-macro))

(defmacro last1-macro ()
  `(let loop ((rest lst))
     (if (consp (cdr rest))
       (loop (cdr rest))
       rest)))

(defun last1 (lst)
  (last1-macro))

; lastn-macro won't work for n <= 0.
; This causes no ill effect because the code below avoids this,
; and then lastn-macro is undefined so that user code doesn't see it.
(defmacro lastn-macro ()
  (let ((scan (gensym "scan"))
        (pop (gensym "pop")))
    `(let ((returned-lst lst)
           (checked-lst lst)
           (n n))
       (let ,scan ()
            (setq checked-lst (cdr checked-lst))
            (if (atom checked-lst)

                returned-lst

                (if (= (setq n (1- n)) 0)
                    (let ,pop ()
                         (setq returned-lst (cdr returned-lst))
                         (setq checked-lst (cdr checked-lst))
                         (if (atom checked-lst)
                             returned-lst
                             (,pop)))
                    (,scan)))))))

(defun lastn (lst n)
  (cond
    ((= n 1) (last1-macro))
    ((> n 1) (lastn-macro))
    ((= n 0) (last0-macro))
    (t (error 'type-error "last: n must be >= 0"))))

(defun last (lst . n)
  (if n
      (lastn lst (car n))
      (last1-macro)))

(defmacro last (lst . n)
  (if n
      (if (integerp (setq n (car n)))
          (cond
            ((= n 1) `(last1 ,lst))
            ((= 0 1) `(last0 ,lst))
            (t `(lastn ,lst ,n)))
          `(lastn ,lst ,n))
      `(last1 ,lst)))

(defmacro last0-macro)
(defmacro last1-macro)
(defmacro lastn-macro)


;;; = Function: nconc
;;;     (nconc lists*) -> concatenated-list
;;;
;;; Since: 1.2
;;;
;;; `nconc` concatenates lists, each list but the last is modified.
;;; If no lists are supplied, `nconc` returns `nil`.
;;; Each argument but the last must be a proper or dotted list.
(defun nconc lists
  (let* outer ((outer-lists lists)
               (result (car outer-lists)))
    (if outer-lists
      (cond
        ((consp result)
         (let ((splice result))
           (let* inner ((inner-lists (cdr outer-lists))
                        (ele (car inner-lists)))
               (if inner-lists
                 (cond
                   ((consp ele) (rplacd (last splice) ele)  (setq splice ele)  (inner (cdr inner-lists) (cadr inner-lists)))
                   ((null ele) (rplacd (last splice) ())  (inner (cdr inner-lists) (cadr inner-lists)))
                   ((atom ele) (if (cdr inner-lists)
                                     (error "nconc - not a list: %s" ele)
                                 (rplacd (last splice) ele)))))))
           result)

        ((null result)
         (outer (cdr outer-lists) (cadr outer-lists)))

        ((atom result)
         (if (cdr outer-lists)
               (error "nconc - not a list: %s" result)
           result))))))


;;; = Function: revappend, nreconc
;;;     (revappend list tail) -> result-list
;;;     (nreconc list tail) -> result-list
;;;
;;; Since: 1.3
;;;
;;; `revappend` constructs a copy of `list`, but with the elements in reverse order.
;;; It then appends (as if by `nconc`) the `tail` to that reversed list and returns the result.
;;;
;;; `nreconc` reverses the order of elements in list (as if by `nreverse`).
;;; It then appends (as if by `nconc`) the tail to that reversed list and returns the result.
;;;
;;; The resulting list shares list structure with tail.
;;;
;;;     (revappend x y)  ::=  (append (reverse x) y)
;;;     (nreconc x y)    ::=  (nconc (nreverse x) y)
(defun revappend (x y)
  (if x (revappend (cdr x) (cons (car x) y))
    y))

(defun nreconc (x y)
  (let loop ((1st (cdr x))
             (2nd x)
             (3rd y))
    (if (atom 2nd) 3rd
      (progn
        (rplacd 2nd 3rd)
        (loop (if (null 1st) 1st (cdr 1st)) 1st 2nd)))))


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
    (let loop ((lst lst))
      (if lst
            (if (pred item (car lst))
                  lst
              (loop (cdr lst)))))))


;;; = Function: adjoin
;;;     (adjoin item list [test]) -> result-list
;;;
;;; Since: 1.4.5
;;;
;;; Tests whether `item` is the same as an existing element of `lst`.
;;; If the `item` is not an existing element, `adjoin` adds it to `lst` (as if by `cons`)
;;; and returns the resulting list; otherwise, nothing is added and the original list is returned.
(defun adjoin (item lst . test)
  (if (apply member (list* item lst test)) lst
    (cons item lst)))


;;; = Function: acons
;;;     (acons key datum alist) -> new-alist
;;;
;;; Since: 1.1
;;;
;;; Prepends `alist` with a new `(key . datum)` tuple
;;; and returns the modified list.
(defun acons (key datum alist)
  (cons (cons key datum) alist))


(defmacro m%notany-null (lst)
  (let ((loop (gensym "loop"))
        (l (gensym "lst")))
    `(let ,loop ((,l ,lst))
       (if ,l (and (car ,l) (,loop (cdr ,l)))
         t))))


; Helper macros to generate defuns for the various mapXX functions
(defmacro m%mapx (name acc accn)
  `(defun ,name (func lst . more-lists)
     (if more-lists
           (let loop ((args (cons lst more-lists)))
             (when (m%notany-null args)
               (apply func ,(if accn (list accn 'args) 'args))
               (loop (unzip-tails args))))
       (let loop ((lst lst))
         (when lst
           (func ,(if acc (list acc 'lst) 'lst))
           (loop (cdr lst)))))
    lst))

(defmacro m%mapx-cons (name acc accn)
  `(defun ,name (func lst . more-lists)
     (let* ((result (cons () ())) (append-to result))
       (if more-lists
             (let loop ((args (cons lst more-lists)))
               (when (m%notany-null args)
                 (setq append-to (cdr (rplacd append-to (cons (apply func ,(if accn (list accn 'args) 'args)) ()))))
                 (loop (unzip-tails args))))
         (let loop ((lst lst))
           (when lst
             (setq append-to (cdr (rplacd append-to (cons (func ,(if acc (list acc 'lst) 'lst)) ()))))
             (loop (cdr lst)))))

       (cdr result))))

(defmacro m%mapx-nconc (name acc accn)
  `(defun ,name (func lst . more-lists)
     (let* ((result (cons () ())) (append-to result))
       (if more-lists
               (let loop ((args (cons lst more-lists)))
                 (when (m%notany-null args)
                   (setq append-to (last append-to))
                   (rplacd append-to (apply func ,(if accn (list accn 'args) 'args)))
                   (loop (unzip-tails args))))
         (let loop ((lst lst))
           (when lst
             (setq append-to (last append-to))
             (rplacd append-to (func ,(if acc (list acc 'lst) 'lst)))
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
(defmacro m%mapx-nconc)


;;; = Macro: multiple-value-list
;;;
;;; Since: 1.4
(defmacro multiple-value-list (value-form)
  `(multiple-value-call list ,value-form))


;;; = Macro: nth-value
;;;
;;; Since: 1.4
(defmacro nth-value (n value-form)
  `(nth ,n (multiple-value-list ,value-form)))


; iteration ***********************************************************

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
;;; Similar to CL `dotimes`.
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
        (count (gensym "count"))
        (loop (gensym "loop"))
        (resultform (cddr loop-def)))
    (if (integerp countform)

          (if (<= countform 0) `(progn ,@resultform)
            `(let ((,var 0))
               (let ,loop ()
                 ,@body
                 (if (>= (incf ,var) ,countform) (progn ,@resultform)
                   (,loop)))))

      `(let ((,var 0)
             (,count ,countform))
         (if (<= ,count 0) (progn ,@resultform)
           (let ,loop ()
             ,@body
             (if (>= (incf ,var) ,count) (progn ,@resultform)
               (,loop))))))))


;;; = Macro: dolist
;;;     (dolist (var list-form result-form*) statement*) -> result
;;;
;;; Since: 1.1
;;;
;;; Similar to CL `dolist`.
;;; Murmel however supports multiple result-forms which will be eval'd in an
;;; implicit `progn`, similar to `do` and `do*`;
(defmacro dolist (loop-def . body)
  (let ((var (car loop-def))
        (listform (cadr loop-def))
        (lst (gensym))
        (loop (gensym))
        (result (cddr loop-def)))
    `(let* ,loop ((,lst ,listform)
                  (,var (car ,lst)))
       (if ,lst
             (progn
               ,@body
               (,loop (cdr ,lst) (cadr ,lst)))
         (progn ,@result)))))


;;; = Macro: dovector
;;;     (dovector (var vector-form result-form*) statement*) -> result
;;;
;;; Since: 1.3
;;;
;;; Just like `dolist`, but with vectors.
(defmacro dovector (loop-def . body)
  (let ((var (car loop-def))
        (vectorform (cadr loop-def))
        (result (cddr loop-def))
        (vec (gensym))
        (acc (gensym))
        (idx (gensym))
        (limit (gensym))
        (loop (gensym)))
    `(let* ((,vec ,vectorform)
            (,acc (cond
                    ((simple-vector-p ,vec) svref)
                    ((stringp ,vec) sref)
                    ((bit-vector-p ,vec) bvref)
                    ((vectorp ,vec) seqref)
                    (t (error "dovector - not a vector: %s" ,vec))))
            (,limit (vector-length ,vec)))
       (let ,loop ((,idx 0))
         (if (< ,idx ,limit)
               (let ((,var (,acc ,vec ,idx)))
                 ,@body
                 (,loop (1+ ,idx)))
           ,(if result `(let ((,var nil)) ,@result)))))))


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
    `(let* ,loop ((,lst ,listform)
                  (,key-var (car ,lst))
                  (,value-var (cadr ,lst)))
       (if ,lst
             (if (cdr ,lst)
                   (progn
                     ,@body
                     (,loop (cddr ,lst) (caddr ,lst) (car (cdddr ,lst))))
               (error "doplist - odd number of elements in plist"))
         (progn ,@result)))))


;(defmacro while (expr . body)
;  `(let loop ()
;     (when ,expr
;        ,@body
;        (loop))))


; more lists **********************************************************

;;; = Function: butlast
;;;     (butlast lst n?) -> result-list
;;;
;;; Since: 1.4.5
;;;
;;; `butlast` returns a copy of `lst` from which the last `n` conses have been omitted.
;;; If `n` is not supplied, its value is 1. If there are fewer than `n` conses in `lst`, `nil` is returned.
(defun butlast (lst . n)
  (setq n (if n (m%nonneg-integer-number (car n))
            1))

  (if (= 0 n) (copy-list lst)

    (let ((head (dotted-nthcdr (1- n) lst))
          result
          splice)
      (if (consp head)            ; there are at least n
        (if (consp (cdr head))    ; conses
          (progn
            (setq result (cons () ()))
            (setq splice result)
            (do ((trail lst (cdr trail))
                 (head head (cdr head)))
                ;; HEAD is n-1 conses ahead of TRAIL;
                ;; when HEAD is at the last cons, return
                ;; the data copied so far.
                ((atom (cdr head))
                 (cdr result))
              (setq splice (cdr (rplacd splice (list (car trail))))))))))))


;;; = Function: nbutlast
;;;     (nbutlast lst n?) -> result-list
;;;
;;; Since: 1.4.5
;;;
;;; `nbutlast` is like `butlast`, but `nbutlast` may modify `lst`.
;;; It changes the cdr of the cons n+1 from the end of `lst` to `nil` except
;;; if there are fewer than `n` conses in `lst`, `nil` is returned and `lst` is not modified.
(defun nbutlast (lst . n)
  (setq n (if n (m%nonneg-integer-number (car n))
            1))

  (if (= 0 n) lst

    (let ((head (dotted-nthcdr (1- n) lst)))
      (if (consp head)            ; there are more than n
        (if (consp (cdr head))    ; conses.
          ;; TRAIL trails by n cons to be able to
          ;; cut the list at the cons just before.
          (do ((trail lst (cdr trail))
               (head (cdr head) (cdr head)))
              ((atom (cdr head))
               (rplacd trail nil)
               lst)))))))


;;; = Function: ldiff
;;;     (ldiff lst obj) -> result-list
;;;
;;; Since: 1.4.5
;;;
;;; Return a new list, whose elements are those of `lst` that appear before
;;; `obj`. If `obj` is not a tail of `lst`, a copy of `lst` is returned.
;;; `lst` must be a proper list or a dotted list.
(defun ldiff (lst obj)
  (let* ((result (list ()))
         (splice result))
    (let loop ((lst lst))
      (if (eq lst obj) nil
        (if (atom lst) (rplacd splice lst)
          (progn
            (setq splice (cdr (rplacd splice (list (car lst)))))
            (loop (cdr lst))))))
    (cdr result)))


;;; = Function: tailp
;;;     (tailp obj lst) -> boolean
;;;
;;; Since: 1.4.5
;;;
;;;  Return `true` if `obj` is the same as some tail of `lst`, otherwise
;;;  returns `false`. `lst` must be a proper list or a dotted list.
(defun tailp (object lst)
  (let loop ((lst lst))
    (if (eq object lst) t
      (if (atom lst) nil
        (loop (cdr lst))))))


; places **************************************************************

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
  (let ((read-var (gensym "read-var"))
        (tmp1 (gensym "tmp1"))
        (tmp2 (gensym "tmp2"))
        (store-var (gensym "store-var")))
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
                 (,(car args) ,(cadr args))
                 (,store-var)
                 (svset ,tmp1 ,tmp2 ,store-var)
                 (svref ,tmp1 ,tmp2)))

              ((or (eq 'bvref op) (eq 'bit op))
               `((,tmp1 ,tmp2)
                 (,(car args) ,(cadr args))
                 (,store-var)
                 (bvset ,tmp1 ,tmp2 ,store-var)
                 (bvref ,tmp1 ,tmp2)))

              ((or (eq 'sref op) (eq 'char op))
               `((,tmp1 ,tmp2)
                 (,(car args) ,(cadr args))
                 (,store-var)
                 (sset ,tmp1 ,tmp2 ,store-var)
                 (sref ,tmp1 ,tmp2)))

              ((or (eq 'seqref op) (eq 'elt op))
               `((,tmp1 ,tmp2)
                 (,(car args) ,(cadr args))
                 (,store-var)
                 (seqset ,tmp1 ,tmp2 ,store-var)
                 (seqref ,tmp1 ,tmp2)))

              ;; hashref with default value: setf (hashref h k def) - eval and ignore default value form
              ((and (eq 'hashref op) (cddr args))
               `((,tmp1 ,tmp2 ,read-var)
                 (,(car args) ,(cadr args) ,(caddr args))
                 (,store-var)
                 (hashset ,tmp1 ,tmp2 ,store-var)
                 (hashref ,tmp1 ,tmp2)))

              ;; hashref w/o default value
              ((eq 'hashref op)
               `((,tmp1 ,tmp2)
                 (,(car args) ,(cadr args))
                 (,store-var)
                 (hashset ,tmp1 ,tmp2 ,store-var)
                 (hashref ,tmp1 ,tmp2)))

              ;; gethash with default value: setf (gethash k hash def) - eval and ignore default value form
              ((and (eq 'gethash op) (cddr args))
               `((,tmp1 ,tmp2 ,read-var)
                 (,(cadr args) ,(car args) ,(caddr args))
                 (,store-var)
                 (hashset ,tmp1 ,tmp2 ,store-var)
                 (hashref ,tmp1 ,tmp2)))

              ((eq 'gethash op)
               `((,tmp1 ,tmp2)
                 (,(cadr args) ,(car args))
                 (,store-var)
                 (hashset ,tmp1 ,tmp2 ,store-var)
                 (hashref ,tmp1 ,tmp2)))

              (t (error "get-setf-expansion - only symbols, car..cdddr, nth, elt, seqref, hashref, gethash, svref, bvref, bit, sref and char are supported for 'place', got %s" place)))))))


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
;;; - elt seqref
;;; - svref, bvref, bit, sref, char
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
                          (error "odd number of arguments to setf"))))

                (cond ((symbolp (car args))
                       `(setq   ,(car args)  ,@(cdr args)))

                      ((eq 'svref (caar args))
                       `(svset  ,@(cdar args) ,(cadr args)))

                      ((or (eq 'bvref (caar args)) (eq 'bit (caar args)))
                       `(bvset ,@(cdar args) ,(cadr args)))

                      ((or (eq 'sref (caar args)) (eq 'char (caar args)))
                       `(sset ,@(cdar args) ,(cadr args)))

                      ((or (eq 'seqref (caar args)) (eq 'elt (caar args)))
                       `(seqset ,@(cdar args) ,(cadr args)))

                      ;; hashref with default value: setf (hashref h k def) - eval and ignore default value form
                      ((and (eq 'hashref (caar args)) (cdr (cddar args)))
                       `(prog1 (hashset ,(cadar args) ,(car (cddar args)) ,(cadr args)) ,(cadr (cddar args))))

                      ;; hashref w/o default value: setf (hashref h k)
                      ((eq 'hashref (caar args))
                       `(hashset ,@(cdar args) ,(cadr args)))

                      ;; gethash with default value: setf (gethash key hash def) - eval and ignore default value form
                      ((and (eq 'gethash (caar args)) (cdr (cddar args)))
                       `(prog1 (hashset ,(car (cddar args)) ,(cadar args) ,(cadr args)) ,(cadr (cddar args))))

                      ;; gethash w/o default value: setf (gethash key hash)
                      ((eq 'gethash (caar args))
                       `(hashset ,(car (cddar args)) ,(cadar args) ,(cadr args)))

                      (t (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion (car args))
                           `(let* (,@(mapcar list vars vals)
                                   (,(car store-vars) ,@(cdr args)))
                              ,writer-form)))))

          (error "odd number of arguments to setf"))))


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


;;; = Macro: pushnew
;;;     (pushnew item place [test]) -> new-place-value
;;;
;;; Since: 1.4.5
;;;
;;; `pushnew` tests whether `item` is the same as any existing element of the list stored in `place`.
;;; If `item` is not, it is prepended to the list, and the new list is stored in `place`.
;;;
;;; `pushnew` returns the new list that is stored in `place`.
(defmacro pushnew (item place . test)
  (if (symbolp place)
        `(setq ,place (adjoin ,item ,place ,@test))
    (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion place)
      `(let* (,@(mapcar list vars vals)
              (,(car store-vars) (adjoin ,item ,reader-form ,@test)))
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


; numbers, characters *************************************************

;;; = Function: abs
;;;     (abs number) -> result
;;;
;;; Since: 1.1
;;;
;;; Return the absoute value of a number.
(defun abs (n)
  (if (< n 0) (- n) (+ n)))


;;; = Function: min
;;;     (min number+) -> result
;;;
;;; Since: 1.4
;;;
;;; Return the smallest number of the given arguments.
(defun min (num . more-numbers)
  (dolist (n more-numbers)
    (if (< n num) (setq num n)))
  num)


;;; = Function: max
;;;     (max number+) -> result
;;;
;;; Since: 1.4
;;;
;;; Return the largest number of the given arguments.
(defun max (num . more-numbers)
  (dolist (n more-numbers)
    (if (> n num) (setq num n)))
  num)


;;; = Function: zerop
;;;     (zerop number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number zero?
(m%def-macro-fun zerop (n) `(= ,n 0))


;;; = Function: evenp
;;;     (evenp number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number even?
(defun evenp (n)
  (if (integerp n) (= 0.0 (mod n 2))
    (error "not an integer: %s" n)))


;;; = Function: oddp
;;;     (oddp number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number odd?
(defun oddp (n)
  (if (integerp n) (= 1.0 (mod n 2))
    (error "not an integer: %s" n)))


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
                  t)))
    t))


;;; = Function: char
;;;     (char str n) -> nth-character
;;;
;;; Since: 1.1
;;;
;;; Return the n-th character of the string `str`, `n` is 0-based.
(define char sref)
(defmacro char (str n)
  `(sref ,str ,n))


;;; = Function: bit
;;;     (bit bv n) -> nth bit
;;;
;;; Since: 1.3
;;;
;;; Return the n-th bit of the bitvector `bv`, `n` is 0-based.
(define bit bvref)
(defmacro bit (bv n)
  `(bvref ,bv ,n))


; ;;; = Function: equal
; ;;;     (equal a b) -> boolean
; ;;;
; ;;; Since: 1.1
; ;;;
; ;;; Return `t` if any of the following is true:
; ;;;
; ;;; - `a` and `b` are `eql`
; ;;; - `a` and `b` are strings that have the same text value
; ;;; - `a` and `b` are bitvectors whose elements are eql
; ;;; - `a` and `b` are conses whose car and cdr are `equal` respectively
; (defun equal (a b)
;   (or (eql a b)
;       (and (stringp a) (stringp b) (string= a b))
;       (and (bit-vector-p a) (bit-vector-p b) (bv= a b))
;       (and (consp a)   (consp b)   (equal (car a) (car b)) (equal (cdr a) (cdr b)))))


;;; = Function: parse
;;;     (parse result-type str [eof-obj [start [end]]]) -> result
;;;
;;; Since: 1.4
;;;
;;; Reads the token in `str` starting at `start` (which defaults to `0`),
;;; `parse-error` if the token is not of type `result-type`.
(defun parse args
  (multiple-value-bind (obj pos) (apply read-from-string (cdr args))
    (if (typep obj (car args)) ()
      (error 'parse-error "expected an object of type %s, got %s" (car args) obj))
    (values obj pos)))


;;; = Function: parse-integer
;;;     (parse-integer str [start [end]]) -> result
;;;
;;; Since: 1.4
;;;
;;; Reads the token in `str` starting at `start` (which defaults to `0`),
;;; `parse-error` if the token is not of type `integer`.
(defun parse-integer args
  (apply parse (list* 'integer (car args) nil (cdr args))))


; generators **********************************************************

;;; = Function: scan
;;;     (scan start [step [endincl]])                 -> generator-function that returns subsequent numbers starting from `start` incrementing by `step` (default: 1)
;;;     (scan seq-or-gen [start-idx [stop-idx-excl]]) -> generator-function that returns subsequent elements of the given sequence (list or vector) or generator
;;;     (scan hash-table)                             -> generator-function that returns subsequent (key . value) pairs of the given hash-table
;;;
;;; Since: 1.3
;;;
;;; `scan` creates a generator function that on subsequent calls produces subsequent values.
;;;
;;; `start-idx` and `stop-idx-excl` if given must be integer numbers >= 0, both are 0-based.
;;;
;;; A generator function takes no arguments and on subsequent applications returns `(values <next-value> t)`
;;; or `(values <undefined-value> nil)` to indicate "all values are exhausted".
(defun m%scan (arg . more-args)
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
         (when more-args
           (setq arg (nthcdr (car more-args) arg)))

         (if (cdr more-args)
                 (let* ((n (floor (- (m%nonneg-integer-number (cadr more-args)) (car more-args)))))
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
                          ((bit-vector-p arg) bvref)
                          ((stringp arg) sref)
                          ((vectorp arg) seqref)))
               (len (vector-length arg))
               (idx (if more-args (m%nonneg-integer-number (car more-args)) 0)))
          (when (cdr more-args)
            (if (< (cadr more-args) len) (setq len (m%nonneg-integer-number (cadr more-args)))))
          (lambda ()
            (if (< idx len)
                  (values (ref arg idx)
                          (progn (incf idx) t))
              (values nil nil)))))

       ((null arg)
        (lambda () (values nil nil)))

       (t (error "scan: cannot create a generator function from given arguments"))))


(defun scan (arg . more-args)
  (cond ((functionp arg) ; assume it's a generator
         (if more-args
               (if (cdr more-args)
 
                     (let* ((skip (m%nonneg-integer-number (car more-args)))
                            (count (floor (- (m%nonneg-integer-number (cadr more-args)) skip))))
                       (when (<= count 0) (setq count nil))
                       (lambda ()
                         (when skip
                           (dotimes (ignore skip) (arg))
                           (setq skip nil))
                         (if count
                               (progn
                                 (setq count (if (> count 1) (1- count) nil))
                                 (arg))
                           (values nil nil))))
 
                 (let ((skip (m%nonneg-integer-number (car more-args))))
                   (lambda ()
                     (when skip
                       (dotimes (ignore skip) (arg))
                       (setq skip nil))
                     (arg))))

           arg))

        ((hash-table-p arg)
         (apply scan-hash-table (cons arg more-args)))

        (t (apply m%scan (cons arg more-args)))))


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
  (if (functionp generator) nil (error "not a generator"))
  (if more-generators

        (let ((generators (cons generator more-generators)) (more-accum t))
          (lambda ()
            (if more-accum
                  (let* ((list-accum (cons () ())) (append-to list-accum))
                    (let loop ((x generators))
                      (if x
                        (if more-accum
                          (multiple-value-bind (result more) ((car x))
                            (if more
                                  (progn (setq append-to (cdr (rplacd append-to (cons result ())))) (loop (cdr x)))
                              (setq more-accum ()))))))
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
  (if (functionp generator) nil (error "not a generator"))
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
         ,(if result `(let ((,var nil)) ,@result))))))


; sequences ***********************************************************

;;; = Function: elt
;;;     (elt sequence n) -> nth-element
;;;
;;; Since: 1.3
;;;
;;; Similar to CL `elt`, Murmel's `elt` handles dotted lists, though.
(define elt seqref)
(defmacro elt (seq idx)
  `(seqref ,seq ,idx))


;;; = Function: copy-seq
;;;     (copy-seq sequence) -> copied-sequence
;;;
;;; Since: 1.3
;;;
;;; Creates a copy of `sequence`.
;;; The elements of the new sequence are the same as the corresponding elements of the given sequence.
;;;
;;; If `sequence` is a vector, the result is a fresh simple vector
;;; that has the same actual array element type as `sequence`.
;;; If `sequence` is a list, the result is a fresh list.
(defun copy-seq (seq)
  (cond
    ((null seq))
    ((consp seq)   (copy-list seq))
    ((vectorp seq) (vector-copy seq))
    (t      (error "copy-seq - %s is not a sequence" seq))))


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
        (t (error "length - %s is not a sequence" seq))))


;;; = Function: reverse
;;;     (reverse sequence) -> reversed-sequence
;;;
;;; Since: 1.1
;;;
;;; If `sequence` is a list then return a fresh list
;;; with elements in reversed order, if `sequence`
;;; is a vector then return a fresh reversed vector.
(defun reverse (seq)
  (labels ((reverse/list (l lp)
             (if l (reverse/list (cdr l) (cons (car l) lp))
               lp))

           (reverse/vector (from to get set)
             (let loop ((from-idx 0) (to-idx (1- (vector-length to))))
               (when (>= to-idx 0)
                 (set to to-idx (get from from-idx))
                 (loop (1+ from-idx) (1- to-idx))))
             to))

    (cond
      ((null seq))
      ((consp seq)              (reverse/list seq ()))
      ((stringp seq)            (reverse/vector seq (make-array (vector-length seq) 'character) sref sset))
      ((simple-vector-p seq)    (reverse/vector seq (make-array (vector-length seq)) svref svset))
      ((bit-vector-p seq)       (reverse/vector seq (make-array (vector-length seq) 'bit) bvref bvset))
      ((vectorp seq)            (reverse/vector seq (make-array (vector-length seq)) seqref seqset))
      (t                        (error "reverse - %s is not a sequence" seq)))))


;;; = Function: nreverse
;;;     (nreverse sequence) -> reversed-sequence
;;;
;;; Since: 1.3
;;;
;;; Similar to `reverse` `nreverse` returns a sequence with elements in reversed order.
;;; `nreverse` however may or may not reuse/ destroy the input sequence.
(defun nreverse (seq)
  (labels ((nreverse/list (list)
             (let loop ((1st (cdr list))
                        (2nd list)
                        (3rd ()))
                 (if (atom 2nd) 3rd
                   (progn
                     (rplacd 2nd 3rd)
                     (loop (cdr 1st) 1st 2nd)))))

           (nreverse/vector (vector getter setter)
             (let loop ((left-index 0)
                        (right-index (1- (vector-length vector))))
               (if (<= right-index left-index) vector
                 (let ((left (getter vector left-index))
                       (right (getter vector right-index)))
                   (setter vector left-index right)
                   (setter vector right-index left)
                   (loop (1+ left-index) (1- right-index)))))))

    (cond
      ((null seq))
      ((consp seq)              (nreverse/list seq))
      ((stringp seq)            (nreverse/vector seq sref sset))
      ((simple-vector-p seq)    (nreverse/vector seq svref svset))
      ((bit-vector-p seq)       (nreverse/vector seq bvref bvset))
      ((vectorp seq)            (nreverse/vector seq seqref seqset))
      (t (error "nreverse - %s is not a sequence" seq)))))


;;; = Function: remove-if
;;;     (remove-if pred sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without the elements for which `pred`
;;; evaluates to non-nil.
(defun remove-if (pred seq)
  (labels ((remove-if/list (l)
             (let* ((result (cons () ()))
                    (append-to result))
               (let loop ((l l))
                 (when l
                   (unless (pred (car l))
                     (setq append-to (cdr (rplacd append-to (cons (car l) ())))))
                   (loop (cdr l))))
               (cdr result)))

           (remove-if/vector (vec)
             (let* ((len (vector-length vec))
                    (result (cons () ()))
                    (append-to result)
                    tmp)
               (dotimes (i len (cdr result))
                 (unless (pred (setq tmp (seqref vec i)))
                   (setq append-to (cdr (rplacd append-to (cons tmp ())))))))))

    (cond
          ((null seq))
          ((consp seq)               (remove-if/list seq))
          ((stringp seq)             (list->string            (remove-if/vector seq)))
          ((simple-vector-p seq)     (list->simple-vector     (remove-if/vector seq)))
          ((simple-bit-vector-p seq) (list->bit-vector        (remove-if/vector seq)))
          ((vectorp seq)             (list->simple-vector     (remove-if/vector seq)))
          (t (error "remove-if - %s is not a sequence" seq)))))


;;; = Function: remove
;;;     (remove elem sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without occurrences of `elem`.
;;; An occurrence is determined by `eql`.
(defun remove (elem seq)
  (remove-if (lambda (x) (eql x elem)) seq))


(defun m%list->sequence (lst result-type)
  (cond ((null result-type)                  )
        ((eq result-type 'list)              lst)
        ((eq result-type 'cons)              (or lst (error "nil is not a sequence of type cons")))
        ((eq result-type 'vector)            (list->simple-vector lst))
        ((eq result-type 'simple-vector)     (list->simple-vector lst))
        ((eq result-type 'simple-bit-vector) (list->bit-vector lst))
        ((eq result-type 'bit-vector)        (list->bit-vector lst))
        ((eq result-type 'string)            (list->string lst))
        ((eq result-type 'simple-string)     (list->string lst))
        (t (error "type %s is not implemented" result-type))))


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
;;; Similar to CL `map`.
(defun map (result-type func seq . more-sequences)
  (if more-sequences
        (setq seq (apply scan-multiple (mapcar m%scan (cons seq more-sequences)))
              func (let ((original func))
                     (lambda (val) (apply original val))))
    (setq seq (m%scan seq)))

  (if result-type
        (let* ((result (cons () ()))
               (append-to result))
          (labels ((collect (val more)
                     (when more
                       (setq append-to (cdr (rplacd append-to (cons (func val) ()))))
                       (multiple-value-call collect (seq)))))
            (multiple-value-call collect (seq))
            (m%list->sequence (cdr result) result-type)))
    (labels ((collect (val more)
               (when more
                 (func val)
                 (multiple-value-call collect (seq)))))
      (multiple-value-call collect (seq)))))


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
;;; Similar to CL `map-into`.
(defun map-into (result func . sequences)
  (when result
    (let (result-cursor set-result has-next-result result-length seq len)
      (cond
        ((consp result) (setq result-cursor result)
                        (setq set-result (lambda (elem) (rplaca result-cursor elem) (setq result-cursor (cdr result-cursor))))
                        (setq has-next-result (lambda () result-cursor)))
        ((vectorp result) (setq result-cursor 0)
                          (setq set-result (lambda (elem) (seqset result result-cursor elem) (setq result-cursor (1+ result-cursor))))
                          (setq has-next-result (lambda () (< result-cursor result-length)))
                          (setq result-length (vector-length result)))
        (t (error "map-into: not a sequence: %s" result)))

      (if (cdr sequences)
            ; 2 or more sequences given
            (labels ((into (next-values more)
                       (when (and (has-next-result) more)
                         (set-result (apply func next-values))
                         (multiple-value-call into (seq)))))
              (setq seq (apply scan-multiple (mapcar m%scan sequences)))
              (multiple-value-call into (seq)))

        (if sequences
              ; 1 sequence given
              (cond
                ((null (setq seq (car sequences))))
                ((consp seq) (let loop ((l seq))
                               (when (and (has-next-result) l)
                               (set-result (func (car l)))
                               (loop (cdr l)))))
                ((vectorp seq) (setq len (vector-length seq))
                               (let loop ((i 0))
                                 (when (and (has-next-result) (< i len))
                                   (set-result (func (seqref seq i)))
                                   (loop (1+ i)))))
                (t (error "map-into: not a sequence: %s" seq)))

          ; 0 sequences given
          (let loop ()
            (when (has-next-result)
              (set-result (func))
              (loop))))))

    result))


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
                     (if (cddr lst)
                           (if from-end-p
                                 (let loop ((elem (f (cadr (setq lst (reverse lst))) (car lst)))
                                            (tail (cddr lst)))
                                   (if (cdr tail)
                                           (loop (f (car tail) elem) (cdr tail))
                                     (f (car tail) elem)))
                             (let loop ((elem (f (car lst) (cadr lst)))
                                        (tail (cddr lst)))
                               (if (cdr tail)
                                     (loop (f elem (car tail)) (cdr tail))
                                 (f elem (car tail)))))
                       (f (car lst) (cadr lst)))
                 (car lst)))

             (reduce/vector (vec acc)
               (if (= 0 (vector-length vec))
                     (f)
                 (if from-end-p
                       (do* ((i (1- (vector-length vec)) (1- i))
                             (accum (acc vec i) (f (acc vec i) accum)))
                            ((<= i 0) accum))

                   (do* ((limit (1- (vector-length vec)))
                         (i 0 (1+ i))
                         (accum (acc vec 0) (f accum (acc vec i))))
                        ((>= i limit) accum))))))

      (cond ((null seq)                (f))
            ((consp seq)               (reduce/list seq))
            ((stringp seq)             (reduce/vector seq sref))
            ((simple-vector-p seq)     (reduce/vector seq svref))
            ((bit-vector-p seq)        (reduce/vector seq bvref))
            ((vectorp seq)             (reduce/vector seq seqref))
            (t (error "reduce - %s is not a sequence" seq))))))


; hash tables *********************************************************

;;; = Function: gethash
;;;     (gethash key hash [default]) -> object, was-present-p
;;;
;;; Since: 1.4
(defmacro gethash (key hash . default)
  (if default
        `(hashref ,hash ,key ,(car default))
    `(hashref ,hash ,key)))

(defun gethash (key hash . default)
  (if default
        (hashref hash key (car default))
    (hashref hash key)))


;;; = Function: remhash
;;;     (remhash key hash) -> was-present-p
;;;
;;; Since: 1.4
(defmacro remhash (key hash)
  `(hash-table-remove ,hash ,key))

(defun remhash (key hash)
  (remhash key hash))

;;; = Function: maphash
;;;     (maphash function hash) -> nil
;;;
;;; Since: 1.4
;;;
;;; Similar to CL's `maphash` but modifying the hash-table
;;; from within `function` is not supported.
(defun maphash (function hash)
  (dogenerator (pair (scan-hash-table hash))
    (function (car pair) (cdr pair))))


; higher order ********************************************************

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


; Helper macro to generate defuns for every and some
(defmacro m%mapxx (name comb lastelem)
  `(defun ,name (pred seq . more-sequences)
     (if more-sequences
           (setq seq (apply scan-multiple (mapcar m%scan (cons seq more-sequences)))
                 pred (let ((original pred))
                        (lambda (val) (apply original val))))
       (setq seq (m%scan seq)))

     (labels ((do-step (val more)
                (if more
                      (,comb (pred val) (multiple-value-call do-step (seq)))
                 ,lastelem)))
       (multiple-value-call do-step (seq)))))


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
(defmacro m%notany-null)


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


; I/O *****************************************************************
;;; = Function: write-char
;;;     (write-char c) -> c
;;;
;;; Since: 1.1
;;;
;;; `write-char` outputs `c` to stdout.
(defun write-char (c)
  (if (characterp c)
        (write c nil)
    (error "write-char - %s is not a character" c)))


;;; = Function: terpri, prin1, princ, print
;;;
;;; Since: 1.1
(m%def-macro-fun terpri () `(writeln))

(m%def-macro-fun prin1 (obj) `(write ,obj))

(m%def-macro-fun princ (obj) `(write ,obj nil))

(m%def-macro-fun print (obj) `(lnwrite ,obj))


;;; = Macro: with-output-to-string
;;;     (with-output-to-string (var) forms*)
;;;
;;; Since: 1.4.2
(defmacro with-output-to-string (s . body)
  `(let ((,@s (make-array 0 'character t)))
     ,@body
     ,@s))


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
        (dotimes (ign l) (write "   " nil))
        (if (< (size obj) 6)
              (write obj)
          (progn
            (write-char #\()
            (let loop ()
              (when
                (and
                   (member
                      (write (pop obj))
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


; misc ****************************************************************
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


; Alexandria: conses and lists ****************************************
;;; = Function: circular-list
;;;     (circular-list elems*) -> circular-list
;;;
;;; Since: 1.2
;;;
;;; Creates a circular list of elements.
(defun circular-list elems
  (if elems
    (let* loop ((result (cons (car elems) ()))
                (elems (cdr elems))
                (append-to result))
      (if elems
            (loop result (cdr elems) (setq append-to (cdr (rplacd append-to (cons (car elems) ())))))
        (cdr (rplacd append-to result))))))


; Alexandria: higher order ********************************************

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
                             (loop (cdr tail) (car tail)))
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

; can't use a macro as that would have different semantics: args would be "late bound"
;(defmacro curry (func . args)
;  `(lambda callargs (apply ,func (list* ,@args callargs))))


;;; = Function: rcurry
;;;     (rcurry func args*) -> function
;;;
;;; Since: 1.2
;;;
;;; Returns a function that applies the arguments it is called with and `args` to `func`.
(defun rcurry (func . args)
  (lambda callargs (apply func (append callargs args))))


; Alexandria: misc ****************************************************

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


; logic, program structure ********************************************

(defun m%symbol-or-lambda (x)
  (or (symbolp x)
      (and (consp x) (eq 'lambda (car x)))))

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
                   (if (m%symbol-or-lambda (car partials))
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
                   (if (m%symbol-or-lambda (car partials))
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
                         (if (m%symbol-or-lambda (car tail))
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
                         (if (m%symbol-or-lambda (car tail))
                               (if (cdr tail)
                                     (cons (list 'setq temp (list (car tail) temp)) (loop (cdr tail)))
                                 (list (list (car tail) temp)))
                           (if (cdr tail)
                                 (cons (list 'setq temp (cons (caar tail) (append (cdar tail) (list temp)))) (loop (cdr tail)))
                             (list (cons (caar tail) (append (cdar tail) (list temp)))))))))
          `(let ((,temp ,init))
             (and ,@forms)))
    (car terms)))


; strings *************************************************************

;;; = Function: string-trim
;;;     (string-trim str) -> result
;;;
;;; Since: 1.4
;;;
;;; Return a fresh immutable `simple-string`
;;; with leading and/ or trailing whitespace removed.
;;;
;;; Example usage:
;;;
;;;     (string-trim "  asdf   ") ; ==> "asdf"
(define string-trim
  (jmethod "String" "trim"))


;;; = Function: string-subseq
;;;     (string-subseq str start [end-excl]) -> result
;;;
;;; Since: 1.4.2
;;;
;;; Return a fresh immutable `simple-string`
;;; whose value is the substring [start end-excl[.
(defun string-subseq (str start . end-excl)
  (if end-excl
        ((jmethod "String" "substring" "int" "int") str start (car end-excl))
    ((jmethod "String" "substring" "int") str start)))


;;; = Function: string-replace
;;;     (string-replace str srch replacement) -> result
;;;
;;; Since: 1.4
;;;
;;; Within `str` replace each occurrence of `srch` by `replacement`.
;;; Special character sequences such as `\t` and `\n` are NOT recognized
;;; and don't get special treatment.
;;;
;;; Example usage:
;;;
;;;     (string-replace "aaa aaa aaa" "aa" "b") ; ==> "ba ba ba"
(define string-replace
  (jmethod "String" "replace" "CharSequence" "CharSequence"))


;;; = Function: string-split
;;;     (string-split str regex) -> vector
;;;
;;; Since: 1.4
;;;
;;; Split `str` into a vector of simple strings.
;;; Within `regex` special character sequences such as `\t` and `\n` are recognized.
;;;
;;; Example usage:
;;;
;;;     (string-split "a b     c
;;;     d" "[ \\t\\n]")
;;;     ; ==> #("a" "b" "c" "d") 
(defmacro string-split (str regex)
  `((jmethod "String" "split" "String") ,str ,regex))
(defun string-split (str regex)
  (string-split str regex))


;;; = Function: string-join
;;;     (string-join delim first-str . more-strings) -> string
;;;
;;; Since: 1.4
(defun string-join (delim first-str . more-strings)
  (apply (jmethod "String" "join" "CharSequence" "CharSequence...") (list* delim first-str more-strings)))


; Serapeum: ***********************************************************

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
  `(with-accumulator collect (lambda (l r) (cons r l)) () ,@body))


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
    `(let* ((,result (cons () ()))
            (,append-to ,result)
            (collect (lambda (,delta)
                       (setq ,append-to (cdr (rplacd ,append-to (cons ,delta ())))))))
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


(defmacro m%def-macro-fun)
(provide "mlib")
