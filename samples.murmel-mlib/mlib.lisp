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
;;;     - [when](#macro-when), [unless](#macro-unless)
;;;     - [not](#function-not), [and](#macro-and), [or](#macro-or)
;;;     - [prog1, prog2](#macro-prog1-prog2)
;;;     - [case](#macro-case), [typecase](#macro-typecase)

;;; - conses and lists
;;;     - [caar..cdddr](#function-caarcdddr), [nthcdr, dotted-nthcdr, nth](#function-nthcdr-dotted-nthcdr-nth), [endp](#function-endp)
;;;     - [copy-list](#function-copy-list), [copy-alist](#function-copy-alist), [copy-tree](#function-copy-tree)
;;;     - [list-length](#function-list-length), [last](#function-last), [butlast](#function-butlast), [nbutlast](#function-nbutlast), [ldiff](#function-ldiff), [tailp](#function-tailp)
;;;     - [subst](#function-subst), [subst-if](#function-subst-if), [nsubst](#function-nsubst), [nsubst-if](#function-nsubst-if)
;;;     - [nconc](#function-nconc), [revappend, nreconc](#function-revappend-nreconc), [member](#function-member), [adjoin](#function-adjoin)
;;;     - [acons](#function-acons)
;;;     - [mapcar](#function-mapcar), [maplist](#function-maplist), [mapc](#function-mapc), [mapl](#function-mapl), [mapcan](#function-mapcan), [mapcon](#function-mapcon)
;;;     - [multiple-value-list](#macro-multiple-value-list), [nth-value](#macro-nth-value)

;;; - iteration
;;;     - [do, do*](#macro-do-do), [dotimes](#macro-dotimes), [dolist](#macro-dolist)

;;; - places
;;;     - [destructuring-bind](#macro-destructuring-bind)
;;;     - [get-setf-expansion](#function-get-setf-expansion)
;;;     - [setf](#macro-setf), [psetf](#macro-psetf), [shiftf](#macro-shiftf), [rotatef](#macro-rotatef)
;;;     - [incf, decf](#macro-incf-decf)
;;;     - [push](#macro-push), [pop](#macro-pop), [pushnew](#macro-pushnew)

;;; - numbers, characters
;;;     - [abs](#function-abs), [min](#function-min), [max](#function-max), [zerop](#function-zerop), [evenp](#function-evenp), [oddp](#function-oddp)
;;;     - [char=](#function-char), [char](#function-char-1), [bit](#function-bit)
;;;     - [parse](#function-parse), [parse-integer](#function-parse-integer)

;;; - sequences
;;;     - [elt](#function-elt), [copy-seq](#function-copy-seq), [length](#function-length)
;;;     - [reverse](#function-reverse), [nreverse](#function-nreverse)
;;;     - [remove-if](#function-remove-if), [remove](#function-remove)
;;;     - [concatenate](#function-concatenate)
;;;     - [map](#function-map), [map-into](#function-map-into), [reduce](#function-reduce)

;;; - hash tables
;;;     - [gethash](#function-gethash), [remhash](#function-remhash), [maphash](#function-maphash)

;;; - higher order
;;;     - [identity](#function-identity), [constantly](#function-constantly), [complement](#function-complement)
;;;     - [every](#function-every), [some](#function-some), [notevery](#function-notevery), [notany](#function-notany)

;;; - I/O
;;;     - [write-char](#function-write-char)
;;;     - [terpri, prin1, princ, print](#function-terpri-prin1-princ-print), [pprint](#function-pprint)
;;;     - [format](#function-format), [formatter](#macro-formatter)
;;;     - [error](#function-error)
;;;     - [with-output-to-string](#macro-with-output-to-string)

;;; - misc
;;;     - [time](#macro-time)
;;;
;;; functions and macros inspired by [Alexandria](https://alexandria.common-lisp.dev):
;;;
;;; - conses and lists
;;;     - [circular-list](#function-circular-list)
;;;     - [mappend](#function-mappend), [mappend-tails](#function-mappend-tails)
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
        ()
        (if (cdr body)
            (cons 'progn body)
            (car body))))


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
;;;     (and forms*) -> result
;;;
;;; Since: 1.1
;;;
;;; Short-circuiting logical and.
;;; Return `t` if no forms were given,
;;; otherwise return the values resulting from the evaluation of the last form unless any of the `forms` evaluate to `nil`,
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
             (when forms
               (if (cdr forms)
                   `(if (setq ,tmp ,(car forms))
                        ,tmp
                        ,(m%or tmp (cdr forms)))
                   (car forms)))))

    ;; strip off any leading nil
    (let loop ()
      (when forms
        (when (null (car forms))
          (setq forms (cdr forms))
          (loop))))

    (when forms
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
      (let ((result (gensym)))
        `(progn
           ,first-form
           (let ((,result ,second-form))
             ,@more-forms
             ,result)))
      `(progn ,first-form (values ,second-form))))


;;; = Macro: case
;;;      (case keyform (keys forms*)* [(t forms*)]) -> result
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
             (let* ((result (list ()))
                    (append-to result)
                    clause)
               (let loop ((clauses clauses))
                 (when clauses
                   (setq clause (do-clause key (car clauses)))
                   (if clause (setq append-to (cdr (rplacd append-to (list clause)))))
                   (loop (cdr clauses))))
               (cdr result))))

    (if (atom keyform)
        `(cond ,@(do-clauses keyform))
        (let ((tmp (gensym)))
          `(let ((,tmp ,keyform))
             (cond ,@(do-clauses tmp)))))))


;;; = Macro: typecase
;;;      (typecase keyform (type forms*)* [(t forms*)]) -> result
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
                   (jerror 'simple-error "typecase - bad clause '%s'" clause))))

           (do-clauses (key)
             (let* ((result (list ()))
                    (append-to result))
               (let loop ((clauses clauses))
                 (when clauses
                   (setq append-to (cdr (rplacd append-to (list (do-clause key (car clauses))))))
                   (loop (cdr clauses))))
             (cdr result))))

    (if (atom keyform)
        `(cond ,@(do-clauses keyform))
        (let ((tmp (gensym)))
          `(let ((,tmp ,keyform))
             (cond ,@(do-clauses tmp)))))))


; conses and lists ****************************************************

(defmacro m%def-macro-fun (name params . body)
  "Expand into a defmacro as well as a defun.
   Must only be used when body uses each parameter exactly once in the given order."

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


;;; = Function: endp
;;;     (endp list) -> boolean
;;;
;;; Since: 1.4.6
;;;
;;; This is the recommended way to test for the end of a proper list. It
;;; returns true if `obj` is `nil`, false if `obj` is a `cons`,
;;; and a `type-error` for any other type of `object`.
(defun endp (obj)
  (cond ((consp obj) nil)
        ((null  obj) t)
        (t (jerror 'simple-type-error "endp - not a list: '%s'" obj))))


(defun m%nonneg-integer-number (n)
  (cond ((integerp n)
         (if (< n 0)
             #1=(jerror 'simple-type-error "must be an integer >= 0: '%s'" n)
             n))

        ((numberp n)
         (if (< n 0) #1#
             (let ((ntrunc (truncate n)))
               (if (/= n ntrunc) #1#
                   ntrunc))))

        (t #1#)))


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
    (if (<= n 0)
        lst
        (when lst (loop (1- n) (cdr lst))))))

; For [n]butlast
(defun dotted-nthcdr (n lst)
  (let loop ((n (m%nonneg-integer-number n)) (lst lst))
    (if (<= n 0)
        lst
        (when (consp lst) (loop (1- n) (cdr lst))))))


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
              (result (list ()))
              (append-to result))
    (if (consp lst)
        (loop (cdr lst) result (cdr (rplacd append-to (list (car lst)))))
        (progn
          (when lst (rplacd append-to lst))
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
  (when lists
    (cons (caar lists) (unzip (cdr lists)))))


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
  (when lists
    (cons (cdar lists) (unzip-tails (cdr lists)))))


;;; = Function: list-length
;;;     (list-length list) -> length
;;;
;;; Since: 1.1
;;;
;;; Returns the length of `list` if it is a string or proper list.
;;; Returns `nil` if `list-or-string` is a circular list.
(defun list-length (lst)
  ;; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html
  (let loop ((n 0)         ; Counter
             (fast lst)      ; Fast pointer: leaps by 2
             (slow lst))     ; Slow pointer: leaps by 1
    ;; If fast pointer hits the end, return the count.
    (cond
      ((null fast) n)

      ((null (cdr fast)) (1+ n))

      ;; If fast pointer eventually equals slow pointer,
      ;;  then we must be stuck in a circular list.
      ;; (A deeper property is the converse: if we are
      ;;  stuck in a circular list, then eventually the
      ;;  fast pointer will equal the slow pointer.
      ;;  That fact justifies this implementation.)
      ((and (eq fast slow) (> n 0)) nil)

      (t (loop (1+ (1+ n)) (cddr fast) (cdr slow))))))


;;; = Function: last
;;;     (last lst [n]) -> tail
;;;
;;; Since: 1.2
;;;
;;; `last` returns the last `n` conses (not the last `n` elements)
;;; of a proper or dotted list or `nil` for the empty list.
;;;
;;; If `n` is zero, the atom that terminates list is returned.
;;; If `n` is greater than or equal to the number of cons cells in list,
;;; the result is `lst`.
(macrolet ((m%last0-macro ()
             `(let loop ((rest lst))
                (if (consp rest)
                    (loop (cdr rest))
                    rest)))

           (m%last1-macro ()
             `(let loop ((rest lst))
                (if (consp (cdr rest))
                    (loop (cdr rest))
                    rest)))

           ;; m%lastn-macro won't work for n <= 0.
           ;; This causes no ill effect because the code below avoids this,
           ;; and then m%lastn-macro is undefined so that user code doesn't see it.
           (m%lastn-macro ()
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
                              (setq returned-lst (cdr returned-lst)
                                    checked-lst (cdr checked-lst))
                              (if (atom checked-lst)
                                  returned-lst
                                  (,pop)))
                            (,scan))))))))

  (defun m%last0 (lst)
    (m%last0-macro))

  (defun m%last1 (lst)
    (m%last1-macro))

  (defun m%lastn (lst n)
    (setq n (m%nonneg-integer-number n))
    (cond
      ((= n 1) (m%last1-macro))
      ((> n 1) (m%lastn-macro))
      (t       (m%last0-macro))))

  (defun last (lst . n)
    (if n
        (m%lastn lst (car n))
        (m%last1-macro)))

  (defmacro last (lst . n)
    (if n
        (if (integerp (setq n (car n)))
            (cond
              ((= n 1) `(m%last1 ,lst))
              ((= 0 1) `(m%last0 ,lst))
              (t `(m%lastn ,lst ,n)))
            `(m%lastn ,lst ,n))
        `(m%last1 ,lst)))

) ; macrolet


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
                                     (jerror 'simple-type-error "nconc - not a list: '%s'" ele)
                                     (rplacd (last splice) ele)))))))
           result)

          ((null result)
           (outer (cdr outer-lists) (cadr outer-lists)))

          ((atom result)
           (if (cdr outer-lists)
               (jerror 'simple-type-error "nconc - not a list: '%s'" result)
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
  (if x
      (revappend (cdr x) (cons (car x) y))
      y))

(defun nreconc (x y)
  (let loop ((1st (cdr x))
             (2nd x)
             (3rd y))
    (if (atom 2nd)
        3rd
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
;;;     (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3))
;;;             equal)
;;;         ; => ((1 . 1) (2 . 2) (3 . 3))
;;;     (member 'c '(a b c 1 2 3) eq)
;;;         ; => (c 1 2 3)
;;;     (member 'b '(a b c 1 2 3) (lambda (a b) (eq a b)))
;;;         ; => (b c 1 2 3)
(defun member (item lst . test)
  (let* ((pred (if test (car test) eql)))
    (let loop ((lst lst))
      (when lst
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
  (if (apply member (list* item lst test))
      lst
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


(macrolet ((m%notany-null (lst)
             (let ((loop (gensym "loop"))
                   (l (gensym "lst")))
               `(let ,loop ((,l ,lst))
                  (if ,l
                      (when (car ,l) (,loop (cdr ,l)))
                      t))))

           ;; Helper macros to generate defuns for the various mapXX functions
           (m%mapx (name acc accn)
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

           (m%mapx-cons (name acc accn)
             `(defun ,name (func lst . more-lists)
                (let* ((result (list ())) (append-to result))
                  (if more-lists
                      (let loop ((args (cons lst more-lists)))
                        (when (m%notany-null args)
                          (setq append-to (cdr (rplacd append-to (list (apply func ,(if accn (list accn 'args) 'args))))))
                          (loop (unzip-tails args))))
                      (let loop ((lst lst))
                        (when lst
                          (setq append-to (cdr (rplacd append-to (list (func ,(if acc (list acc 'lst) 'lst))))))
                          (loop (cdr lst)))))

                  (cdr result))))

           (m%mapx-nconc (name acc accn)
             `(defun ,name (func lst . more-lists)
                (labels ((m%last (lst)
                           ;; returns the last cdr, similar to `last` but errors if the last cdr is a dotted pair
                           ;;(if (consp (cdr lst))  ;; with this instead of the next line Murmel is somewhat sloppy re: mapcon/ mapcon, see https://gitlab.common-lisp.net/cmucl/cmucl/-/issues/196
                           (if (cdr lst)
                               (m%last (cdr lst))
                               lst)))

                  (let* ((result (list ())) (append-to result))
                    (if more-lists
                        (let loop ((args (cons lst more-lists)))
                          (when (m%notany-null args)
                            (setq append-to (m%last append-to))
                            (rplacd append-to (apply func ,(if accn (list accn 'args) 'args)))
                            (loop (unzip-tails args))))
                        (let loop ((lst lst))
                          (when lst
                            (setq append-to (m%last append-to))
                            (rplacd append-to (func ,(if acc (list acc 'lst) 'lst)))
                            (loop (cdr lst)))))

                    (cdr result)))))

           (m%mapx-append (name acc accn)
             `(defun ,name (func lst . more-lists)
                (let* ((result (list ())) (append-to result))
                  (if more-lists
                      (let loop ((args (cons lst more-lists)))
                        (when (m%notany-null args)
                          (let loop ((r (apply func ,(if accn (list accn 'args) 'args))))
                            #1=(if (consp r)
                                   (progn
                                     (setq append-to (cdr (rplacd append-to (list (car r)))))
                                     (loop (cdr r)))
                                   (if r (jerror 'simple-type-error "%s - not a list: '%s'" ',name r))))
                          (loop (unzip-tails args))))
                      (let loop ((lst lst))
                        (when lst
                          (let loop ((r (func ,(if acc (list acc 'lst) 'lst))))
                               #1#)
                          (loop (cdr lst)))))

                  (cdr result)))))


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
;;; All function application results will be concatenated (as if by nconc) to a list
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
;;; All function application results will be concatenated (as if by nconc) to a list
;;; which is the return value of `mapcon`.
(m%mapx-nconc mapcon nil nil)


; Alexandria: conses and lists ****************************************
;;; = Function: mappend
;;;     (mappend function list+) -> appended-results
;;;
;;; Since: 1.4.7
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent items of the given lists.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of `mappend`.
;;; `function` must return a list which will not be mutated by `mappend`.
;;;
;;; `mappend` works similar to Alexandria's `mappend` and
;;; can be thought of as a non-destructive version of `mapcan`,
;;; i.e. `mappend` combines the results of applying `function`
;;; by the use of `append` rather than `nconc`.
(m%mapx-append mappend car unzip)


;;; = Function: mappend-tails
;;;     (mappend-tails function list+) -> appended-results
;;;
;;; Since: 1.4.7
;;;
;;; `function` must accept as many arguments as lists are given,
;;; and will applied to subsequent tails of the given lists.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of `mappend-tails`.
;;; `function` must return a list which will not be mutated by `mappend-tails`.
;;;
;;; `mappend-tails` can be thought of as a non-destructive version of `mapcon`,
;;; i.e. `mappend-tails` combines the results of applying `function`
;;; by the use of `append` rather than `nconc`.
(m%mapx-append mappend-tails nil nil)


) ; (macrolet...


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
;;;     (do ({var | (var [init-form [step-form]])}*)
;;;         (end-test-form result-form*)
;;;         statement*) -> result
;;;
;;;     (do* ({var | (var [init-form [step-form]])}*)
;;;          (end-test-form result-form*)
;;;          statement*) -> result
;;;
;;; Since: 1.1
;;;
;;; `do` and `do*` iterate over a group of statements while `end-test-form` returns `nil`.
(defmacro do (var-defs test-and-result . forms)
  (labels ((init-form (l)
             (if (symbolp l)
                 (list l nil)
                 (list (car l) (cadr l))))

           (step-form (l)
             (if (symbolp l)
                 l
                 (if (caddr l)
                     (caddr l)
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
             (if (symbolp l)
                 (list l nil)
                 (list (car l) (cadr l))))

           (step-form (l)
             (if (symbolp l)
                 nil
                 (when (caddr l)
                   `((setq ,(car l) ,(caddr l)))))))

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

        `(let ((,var 0))
           ,(if (<= countform 0)
                `(progn ,@resultform)
                `(let ,loop ()
                   ,@body
                   (if (>= (incf ,var) ,countform)
                       (progn ,@resultform)
                       (,loop)))))

      `(let ((,var 0)
             (,count ,countform))
         (if (<= ,count 0)
             (progn ,@resultform)
             (let ,loop ()
               ,@body
               (if (>= (incf ,var) ,count)
                   (progn ,@resultform)
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
        (resultforms (cddr loop-def))
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
                    (t (jerror 'simple-type-error "dovector - not a vector: '%s'" ,vec))))
            (,limit (vector-length ,vec))
            (,idx 0)
            ,var)
       (let ,loop ()
         (if (< ,idx ,limit)
             (progn
               (setq ,var (,acc ,vec ,idx))
               ,@body
               (incf ,idx)
               (,loop))

             ,(when resultforms
                `(progn (setq ,var nil) ,@resultforms)))))))


;;; = Macro: doplist
;;;     (doplist (key-var value-var plist-form result-form*)
;;;       statement*) -> result
;;;
;;; Since: 1.2
;;;
;;; Iterates over key-value pairs of `plist-form`.
;;; Similar to Alexandria `doplist`,
;;; see https://alexandria.common-lisp.dev/draft/alexandria.html.
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
               (jerror "doplist - odd number of elements in plist"))
           (progn ,@result)))))


;(defmacro while (expr . body)
;  `(let loop ()
;     (when ,expr
;        ,@body
;        (loop))))


; more lists **********************************************************

;;; = Function: copy-alist
;;;     (copy-alist alist) -> new-alist
;;;
;;; Since: 1.4.6
;;;
;;; `copy-alist` returns a copy of `alist`.
;;;
;;; The list structure of `alist` is copied, and the elements of `alist` which are conses are also copied (as conses only).
;;; Any other objects which are referred to, whether directly or indirectly, by the `alist` continue to be shared.
(defun copy-alist (alist)
  "Return a new association list which is EQUAL to ALIST."
  (if (endp alist)
      alist
      (let ((result
             (cons (if (atom (car alist))
                       (car alist)
                       (cons (caar alist) (cdar alist)))
                   nil)))
        (do ((x (cdr alist) (cdr x))
             (splice result
                     (cdr (rplacd splice
                                  (cons
                                   (if (atom (car x))
                                       (car x)
                                       (cons (caar x) (cdar x)))
                                   nil)))))
            ((endp x)))
        result)))


;;; = Function: copy-tree
;;;     (copy-tree tree) -> new-tree
;;;
;;; Since: 1.4.6
;;;
;;; Creates a copy of a tree of conses.
;;;
;;; If `tree` is not a `cons`, it is returned;
;;; otherwise, the result is a new cons of the results of calling `copy-tree` on the car and cdr of `tree`.
;;; In other words, all conses in the tree represented by `tree` are copied recursively,
;;; stopping only when non-conses are encountered.
;;; copy-tree does not preserve circularities and the sharing of substructure.
(defun copy-tree (object)
  "Recursively copy trees of conses."
  (if (consp object)
      (let* ((%car (car object))
             (result #1=(list (if (consp %car)
                                  (copy-tree %car)
                                  %car))))

        (let loop ((last-cons result)
                   (%cdr (cdr object)))
          (if (atom %cdr)
              (rplacd last-cons %cdr)
              (let* ((%car (car %cdr))
                     (new-cons #1#))
                (rplacd last-cons new-cons)
                (loop new-cons (cdr %cdr)))))

        result)

      object))


;;; = Function: butlast
;;;     (butlast lst [n]) -> result-list
;;;
;;; Since: 1.4.5
;;;
;;; `butlast` returns a copy of `lst` from which the last `n` conses have been omitted.
;;; If `n` is not supplied, its value is 1. If there are fewer than `n` conses in `lst`,
;;; `nil` is returned.
(defun butlast (lst . n)
  (setq n (if n
              (m%nonneg-integer-number (car n))
              1))

  (if (= 0 n)
      (copy-list lst)

      (let ((head (dotted-nthcdr (1- n) lst))
            result
            splice)
        (when (consp head)              ; there are at least n
          (when (consp (cdr head))    ; conses
            (setq result (list ())
                  splice result)
            (do ((trail lst (cdr trail))
                 (head head (cdr head)))
                ;; HEAD is n-1 conses ahead of TRAIL;
                ;; when HEAD is at the last cons, return
                ;; the data copied so far.
                ((atom (cdr head))
                 (cdr result))
              (setq splice (cdr (rplacd splice (list (car trail)))))))))))


;;; = Function: nbutlast
;;;     (nbutlast lst [n]) -> result-list
;;;
;;; Since: 1.4.5
;;;
;;; `nbutlast` is like `butlast`, but `nbutlast` may modify `lst`.
;;; It changes the cdr of the cons n+1 from the end of `lst` to `nil` except
;;; if there are fewer than `n` conses in `lst`, `nil` is returned
;;; and `lst` is not modified.
(defun nbutlast (lst . n)
  (setq n (if n
              (m%nonneg-integer-number (car n))
              1))

  (if (= 0 n)
      lst

      (let ((head (dotted-nthcdr (1- n) lst)))
        (when (consp head)            ; there are more than n
          (when (consp (cdr head))    ; conses.
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
      (unless (eq lst obj)
        (if (atom lst)
            (rplacd splice lst)
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
    (if (eq object lst)
        t
        (unless (atom lst)
          (loop (cdr lst))))))


;;; = Function: subst
;;;     (subst new old tree [test-fn [key-fn]]) -> new-tree
;;;
;;; Since: 1.4.6
;;;
;;; Substitutes `new` for subtrees of `tree` matching `old`.
(progn
(defun subst (new old tree . args)
  (let* #1=((test (if args
                      (car args)
                      eql))
            (key (cadr args))
            (satisfies-the-test (if (cdr args)
                                    (lambda (old new) (test old (key new)))
                                    (lambda (old new) (test old new)))))

    (labels ((s (subtree)
               (cond #2=((satisfies-the-test old subtree) new)
                     #3=((atom subtree) subtree)
                     #4=(t (let ((%car (s (car subtree)))
                                 (%cdr (s (cdr subtree))))
                             (if (and (eq %car (car subtree))
                                      (eq %cdr (cdr subtree)))
                                 subtree
                                 (cons %car %cdr)))))))
      (s tree))))


;;; = Function: subst-if
;;;     (subst-if new test-pred tree [key-fn]) -> new-tree
;;;
;;; Since: 1.4.6
;;;
;;; Substitutes `new` for subtrees of `tree` for which `test-pred` is true.
(defun subst-if (new test tree . args)
  (let* #5=((key (car args))
            (satisfies-the-test (if args
                                    (lambda (x) (test (key x)))
                                    (lambda (x) (test x)))))

    (labels ((s (subtree)
               (cond #6=((satisfies-the-test subtree) new)
                     #3#
                     #4#)))
      (s tree))))


;;; = Function: nsubst
;;;     (nsubst new old tree [test-fn [key-fn]]) -> new-tree
;;;
;;; Since: 1.4.6
;;;
;;; Substitutes `new` for subtrees of `tree` matching `old`.
(defun nsubst (new old tree . args)
  (let* #1#

    (labels ((do-subtree (last subtree)
               (when (satisfies-the-test old subtree)
                 (rplacd last subtree))
               #7=(when (consp subtree)
                    (rplaca subtree (s (car subtree)))
                    (do-subtree subtree (cdr subtree))))

             (s (subtree)
               (cond #2#
                     #3#
                     #8=(t (do-subtree nil subtree)
                           subtree))))

      (s tree))))


;;; = Function: nsubst-if
;;;     (nsubst-if new test-pred tree [key-fn]) -> new-tree
;;;
;;; Since: 1.4.6
;;;
;;; Substitutes `new` for subtrees of `tree` for which `test-pred` is true.
(defun nsubst-if (new test tree . args)
  (let* #5#

    (labels ((do-subtree (last subtree)
               (when (satisfies-the-test subtree)
                 (rplacd last subtree))
               #7#)

             (s (subtree)
               (cond #6#
                     #3#
                     #8#)))

      (s tree))))
)


; places **************************************************************

; m%rplaca
;     (m%rplaca lst value) -> value
;
; Replace the car of `lst` by `value` and return `value`
; (as opposed to `rplaca` which returns `lst`).
; Used in setf-expansions.
(defun m%rplaca (lst v)
  (rplaca lst v) v)


; m%rplacd
;     (m%rplacd lst value) -> value
;
; Replace the cdr of `lst` by `value` and return `value`
; (as opposed to `rplacd` which returns `lst`).
; Used in setf-expansions.
(defun m%rplacd (lst value)
  (rplacd lst value) value)


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
  (labels ((setf-helper (args tmp1 tmp2 store-var reffer setter)
             `((,tmp1 ,tmp2)
               (,(car args) ,(cadr args))
               (,store-var)
               (,setter ,tmp1 ,tmp2 ,store-var)
               (,reffer ,tmp1 ,tmp2))))

  (let ((read-var (gensym "read-var"))
        (tmp1 (gensym "tmp1"))
        (tmp2 (gensym "tmp2"))
        (store-var (gensym "store-var")))
    (if (symbolp place) `(nil nil (,read-var) (setq ,place ,read-var) ,place)
      (let ((op (car place))
            (args (cdr place)))
        (cond ((eq   'car op)  `((,read-var) (        ,@args ) #0=(,store-var) #1=(m%rplaca ,read-var ,store-var) #11=(car ,read-var)))
              ((eq  'caar op)  `((,read-var) ((   car ,@args)) #0#             #1#                                #11#               ))
              ((eq  'cadr op)  `((,read-var) ((   cdr ,@args)) #0#             #1#                                #11#               ))
              ((eq 'caaar op)  `((,read-var) ((  caar ,@args)) #0#             #1#                                #11#               ))
              ((eq 'caadr op)  `((,read-var) ((  cadr ,@args)) #0#             #1#                                #11#               ))
              ((eq 'cadar op)  `((,read-var) ((  cdar ,@args)) #0#             #1#                                #11#               ))
              ((eq 'caddr op)  `((,read-var) ((  cddr ,@args)) #0#             #1#                                #11#               ))

              ((eq   'cdr op)  `((,read-var) (        ,@args ) #0#             #2=(m%rplacd ,read-var ,store-var) #22=(cdr ,read-var)))
              ((eq  'cdar op)  `((,read-var) ((   car ,@args)) #0#             #2#                                #22#               ))
              ((eq  'cddr op)  `((,read-var) ((   cdr ,@args)) #0#             #2#                                #22#               ))
              ((eq 'cdaar op)  `((,read-var) ((  caar ,@args)) #0#             #2#                                #22#               ))
              ((eq 'cdadr op)  `((,read-var) ((  cadr ,@args)) #0#             #2#                                #22#               ))
              ((eq 'cddar op)  `((,read-var) ((  cdar ,@args)) #0#             #2#                                #22#               ))
              ((eq 'cdddr op)  `((,read-var) ((  cddr ,@args)) #0#             #2#                                #22#               ))

              ((eq 'nth op)    `((,read-var) ((nthcdr ,@args)) #0#             #1#                                #11#               ))

              ((eq 'svref op)                        (setf-helper args tmp1 tmp2 store-var 'svref 'svset))
              ((or (eq 'bvref op) (eq 'bit op))      (setf-helper args tmp1 tmp2 store-var 'bvref 'bvset))
              ((or (eq 'sref op) (eq 'char op))      (setf-helper args tmp1 tmp2 store-var 'sref 'sset))

              ((or (eq 'seqref op) (eq 'elt op))     (setf-helper args tmp1 tmp2 store-var 'seqref 'seqset))

              ;; hashref with default value: setf (hashref h k def) - eval and ignore default value form
              ((and (eq 'hashref op) (cddr args))
               `((,tmp1 ,tmp2)
                 (,(car args) ,(cadr args) ,(caddr args))
                 (,store-var)
                 (hashset ,tmp1 ,tmp2 ,store-var)
                 (hashref ,tmp1 ,tmp2)))

              ;; hashref w/o default value
              ((eq 'hashref op)                      (setf-helper args tmp1 tmp2 store-var 'hashref 'hashset))

              ;; gethash with default value: setf (gethash k hash def) - eval and ignore default value form
              ((and (eq 'gethash op) (cddr args))
               `((,tmp1 ,tmp2)
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

              ((eq 'values op)
               (let* ((vars (list ()))    (append-vars vars)
                      (vals (list ()))    (append-vals vals)
                      (stores (list ()))  (append-stores stores)
                      (setter (list ()))  (append-setter setter)
                      (reader (list ()))  (append-reader reader))
                 (dolist (arg args)
                   (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion arg)
                     (when vars
                       (rplacd append-vars vars)
                       (setq append-vars (cdr append-vars))

                       (rplacd append-vals vals)
                       (setq append-vals (cdr append-vals)))

                     (rplacd append-stores store-vars)
                     (setq append-stores (cdr append-stores))

                     (rplacd append-setter (list writer-form))
                     (setq append-setter (cdr append-setter))

                     (rplacd append-reader (list reader-form))
                     (setq append-reader (cdr append-reader))))

                 `(,(cdr vars)
                   ,(cdr vals)
                   ,(cdr stores)
                   (values ,@(cdr setter))
                   (values ,@(cdr reader)))))

              (t (jerror "get-setf-expansion - only symbols, car..cdddr, nth, elt, seqref, hashref, gethash, svref, bvref, bit, sref char and values are supported for 'place', got %s" place))))))))


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
;;; - elt, seqref
;;; - hashref, gethash
;;; - svref, bvref, bit, sref, char
;;; - values
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
                          #1=(jerror "setf - odd number of arguments"))))

              (if (symbolp (car args))
                  `(setq   ,(car args)  ,@(cdr args))

                  (let ((op (caar args)))
                    (cond
                      ((eq   'car op) `(m%rplaca         ,@#2=(cdar args)  ,#3=(cadr args)))
                      ((eq  'caar op) `(m%rplaca ( car   ,@#2#)  ,#3#))
                      ((eq  'cadr op) `(m%rplaca ( cdr   ,@#2#)  ,#3#))
                      ((eq 'caaar op) `(m%rplaca (caar   ,@#2#)  ,#3#))
                      ((eq 'caadr op) `(m%rplaca (cadr   ,@#2#)  ,#3#))
                      ((eq 'cadar op) `(m%rplaca (cdar   ,@#2#)  ,#3#))
                      ((eq 'caddr op) `(m%rplaca (cddr   ,@#2#)  ,#3#))
                      ((eq   'nth op) `(m%rplaca (nthcdr ,@#2#)  ,#3#))

                      ((eq   'cdr op) `(m%rplacd         ,@#2#   ,#3#))
                      ((eq  'cdar op) `(m%rplacd ( car   ,@#2#)  ,#3#))
                      ((eq  'cddr op) `(m%rplacd ( cdr   ,@#2#)  ,#3#))
                      ((eq 'cdaar op) `(m%rplacd (caar   ,@#2#)  ,#3#))
                      ((eq 'cdadr op) `(m%rplacd (cadr   ,@#2#)  ,#3#))
                      ((eq 'cddar op) `(m%rplacd (cdar   ,@#2#)  ,#3#))
                      ((eq 'cdddr op) `(m%rplacd (cddr   ,@#2#)  ,#3#))

                      ((eq 'svref op)                    `(svset  ,@#2# ,#3#))
                      ((or (eq 'bvref op) (eq 'bit op))  `(bvset  ,@#2# ,#3#))
                      ((or (eq 'sref op) (eq 'char op))  `(sset   ,@#2# ,#3#))
                      ((or (eq 'seqref op) (eq 'elt op)) `(seqset ,@#2# ,#3#))

                      ;; hashref with default value: setf (hashref h k def) - eval and ignore default value form
                      ((and (eq 'hashref op) (cdr (cddar args)))
                       `(prog1 (hashset ,(cadar args) ,(car (cddar args)) ,#3#) ,(cadr (cddar args))))

                      ;; hashref w/o default value: setf (hashref h k)
                      ((eq 'hashref op)
                       `(hashset ,@#2# ,#3#))

                      ;; gethash with default value: setf (gethash key hash def) - eval and ignore default value form
                      ((and (eq 'gethash op) (cdr (cddar args)))
                       `(prog1 (hashset ,(car (cddar args)) ,(cadar args) ,#3#) ,(cadr (cddar args))))

                      ;; gethash w/o default value: setf (gethash key hash)
                      ((eq 'gethash op)
                       `(hashset ,(car (cddar args)) ,(cadar args) ,#3#))

                      ;; if the assignment target is a values form with only symbols
                      ((and (eq 'values op)
                            (let loop ((places #2#))
                              (if places
                                (when (symbolp (car places))
                                  (loop (cdr places)))
                                t)))
                       (let* ((vars (mapcar (lambda (x) (gensym)) #2#)))
                         `(multiple-value-bind ,vars ,#3#
                            (values ,@(let* loop ((ret (list ()))
                                                  (append-to ret)
                                                  (places #2#)
                                                  (vars vars))
                                        (if places
                                            (progn
                                              (rplacd append-to (list (list 'setq (car places) (car vars))))
                                              (loop ret (cdr append-to) (cdr places) (cdr vars)))
                                            (cdr ret)))))))

                      ;; see https://stackoverflow.com/questions/44698426/how-do-setf-works-under-the-hood
                      (t (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion (car args))
                           `(let* ,(mapcar list vars vals)
                              (multiple-value-bind ,store-vars ,@(cdr args)
                                ,writer-form))))))))

          #1#)))


;;; = Macro: psetf
;;;     (psetf pair*) -> nil
;;;
;;; Since: 1.4.6
;;;
;;; Takes pairs of arguments like `setf`. The first is a place and the second
;;; is the value that is supposed to go into that place.
;;;
;;; If more than one pair is supplied then the assignments of new values to places are done in parallel.
;;;
;;; Similar to CL's `psetf`.
(defmacro psetf pairs
  (if (cddr pairs)
      (let ((body (list ())))
        (let loop ((pairs pairs) (append-to body))
          (if pairs
              (progn
                (unless (cdr pairs) (jerror 'program-error "psetf - odd number of arguments"))
                (let ((place (car pairs))
                      (values-form (cadr pairs)))
                  (destructuring-bind (vars vals stores setter reader) (get-setf-expansion place)
                    (cond ((and vars (cdr stores))
                           `(let ,(mapcar list vars vals)
                              #1=(multiple-value-bind ,stores ,values-form
                                   #2=,(loop (cddr pairs) (cdr (rplacd append-to (list setter)))))))

                          (vars
                           `(let (,@(mapcar list vars vals)
                                  (,(car stores) ,values-form))
                              #2#))

                          ((cdr stores)
                           `#1#)

                          (t
                           `(let ((,(car stores) ,values-form))
                              #2#))))))

              `(progn ,@(cdr body) nil))))

      `(progn (setf ,@pairs) nil)))


;;; = Macro: shiftf
;;;     (shiftf place+ newvalues) -> old-values-1
;;;
;;; Since: 1.4.8
;;;
;;; `shiftf` modifies the values of each place by storing newvalue into the last place,
;;; and shifting the values of the second through the last place into the remaining places.
;;;
;;; Similar to CL's `shiftf`.
(defmacro shiftf places-and-value
  (unless (cdr places-and-value)
    (jerror 'program-error "shiftf - not enough arguments"))

  (let* ((body (list ()))
         (append-to body))

    (destructuring-bind (vars vals stores setter reader) (get-setf-expansion (car places-and-value))
      (setq append-to (cdr (rplacd append-to (list setter))))
      (let ((out (mapcar (lambda (x) (gensym "out")) stores)))
        `(let* ,(mapcar list vars vals)
           (multiple-value-bind ,out ,reader
             ,(let loop ((prev-stores stores) (prev-setter setter) (l (cdr places-and-value)) (append-to append-to))

                (if (cdr l)

                    (destructuring-bind (vars vals stores setter reader) (get-setf-expansion (car l))
                      `(let* ,(mapcar list vars vals)
                         (multiple-value-bind ,prev-stores ,reader
                           ,(loop stores setter (cdr l) (cdr (rplacd append-to (list prev-setter)))))))

                    `(multiple-value-bind ,prev-stores ,(car l)
                       ,@(cdr body)
                       ,prev-setter
                       (values ,@out))))))))))


;;; = Macro: rotatef
;;;     (rotatef place*) -> nil
;;;
;;; Since: 1.4.8
;;;
;;; `rotatef` modifies the values of each place by rotating values from one place into another.
;;;
;;; If a place produces more values than there are store variables, the extra values are ignored.
;;; If a place produces fewer values than there are store variables, the missing values are set to `nil`.
;;;
;;; Similar to CL's `rotatef`.
(defmacro rotatef places
  (if (cdr places)
      (let* ((body (list ()))
             (append-to body))

        (destructuring-bind (vars vals stores setter first-reader) (get-setf-expansion (car places))
          (let ((out (mapcar (lambda (x) (gensym "out")) stores)))
            `(let* ,(mapcar list vars vals)
               ,(let loop ((prev-stores stores) (prev-setter setter) (l (cdr places)) (append-to append-to))

                  (destructuring-bind (vars vals stores setter reader) (get-setf-expansion (car l))
                    `(let* ,(mapcar list vars vals)
                       (multiple-value-bind ,prev-stores ,reader
                         ,(if (cdr l)

                              (loop stores setter (cdr l) (cdr (rplacd append-to (list prev-setter))))

                              `(multiple-value-bind ,stores ,first-reader
                                 ,@(cdr body)
                                 ,prev-setter
                                 ,setter
                                 nil))))))))))
      nil))


(macrolet (;; Helper macro to generate defmacro's for inplace modification macros.
           (m%inplace (name noarg arg)
             `(defmacro ,name (place . delta-form)
               (let ((tmp (gensym)))
                  (if (symbolp place)
                      `(setq ,place ,(cond ((null delta-form)
                                            `(,,@noarg ,place))
                                           ((atom (car delta-form))
                                            `(,,@arg ,place ,@delta-form))
                                           (t
                                            `(let ((,tmp ,@delta-form))
                                               (,,@arg ,place ,tmp)))))
                      (destructuring-bind (vars vals store-vars writer-form reader-form) (get-setf-expansion place)
                        `(let* (,@(mapcar list vars vals)
                                ,@(when (consp delta-form) `((,tmp ,@delta-form)))
                                (,(car store-vars) ,(cond ((null delta-form)
                                                           `(,,@noarg ,reader-form))
                                                          ((atom (car delta-form))
                                                           `(,,@arg ,reader-form ,@delta-form))
                                                          (t
                                                           `(,,@arg ,reader-form ,tmp))))
                                ,@(cdr store-vars)) ; bind remaining store-vars to nil as per "CLHS 5.1.2.3 VALUES Forms as Places"
                           ,writer-form)))))))


;;; = Macro: incf, decf
;;;     (incf place [delta-form]) -> new-value
;;;     (decf place [delta-form]) -> new-value
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
;;;     (*f place [delta-form]) -> new-value
;;;     (/f place [delta-form]) -> new-value
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
(m%inplace *f ('identity) ('*))
(m%inplace /f ('/)        ('/))


;;; = Macro: +f, -f
;;;     (+f place [delta-form]) -> new-value
;;;     (-f place [delta-form]) -> new-value
;;;
;;; Since: 1.1
;;;
;;; `+f` and `+f` are used for adding and subtracting
;;; to/ from the value of `place`, respectively.
;;;
;;; The delta is added (in the case of `+f`) to
;;; or subtracted (in the case of `-f`) from the number in `place`
;;; and the result is stored in `place`.
;;;
;;; Without `delta-form` `-f` will return the negation of the number in `place`,
;;; `+f` will return the number in `place`.
;;;
;;; Without `delta-form` the return type of `+f` will be
;;; the type of the number in `place`, otherwise the return type will be float.
(m%inplace +f ('identity) ('+))
(m%inplace -f ('-)        ('-))

) ; (macrolet...


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
        (let ((tmpitem (gensym "item")))
          `(let* ((,tmpitem ,item) ; eval item before place, see http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_5-1-1-1-1.html
                  ,@(mapcar list vars vals)
                  (,(car store-vars) (cons ,tmpitem ,reader-form))
                  ,@(cdr store-vars)) ; bind remaining store-vars to nil as per "CLHS 5.1.2.3 VALUES Forms as Places"
             ,writer-form)))))


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
        (let ((tmpitem (gensym "item")))
          `(let* ((,tmpitem ,item) ; eval item before place, see http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_5-1-1-1-1.html
                  ,@(mapcar list vars vals)
                  (,(car store-vars) (adjoin ,tmpitem ,reader-form ,@test))
                  ,@(cdr store-vars)) ; bind remaining store-vars to nil as per "CLHS 5.1.2.3 VALUES Forms as Places"
             ,writer-form)))))


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
                  (,(car new) (cdr ,reader-form))
                  ,@(cdr new) ; bind remaining store-vars to nil as per "CLHS 5.1.2.3 VALUES Forms as Places"
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
    (when (< n num)
      (setq num n)))
  num)


;;; = Function: max
;;;     (max number+) -> result
;;;
;;; Since: 1.4
;;;
;;; Return the largest number of the given arguments.
(defun max (num . more-numbers)
  (dolist (n more-numbers)
    (when (> n num)
      (setq num n)))
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
  (if (integerp n)
      (= 0.0 (mod n 2))
      (jerror 'simple-type-error "evenp - not an integer: '%s'" n)))


;;; = Function: oddp
;;;     (oddp number) -> boolean
;;;
;;; Since: 1.1
;;;
;;; Is this number odd?
(defun oddp (n)
  (if (integerp n)
      (= 1.0 (mod n 2))
      (jerror 'simple-type-error "oddp - not an integer: '%s'" n)))


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
    (if (typep obj (car args))
        (values obj pos)
        (jerror 'parse-error "parse - expected an object of type '%s', got '%s'" (car args) obj))))


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
             (when (< (cadr more-args) len)
               (setq len (m%nonneg-integer-number (cadr more-args)))))
           (lambda ()
             (if (< idx len)
                 (values (ref arg idx)
                         (progn (incf idx) t))
                 (values nil nil)))))

        ((null arg)
         (lambda () (values nil nil)))

        (t (jerror "scan - cannot create a generator function from given arguments"))))


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
  (unless (functionp generator)
    (jerror 'simple-type-error "scan-multiple - not a generator"))

  (if more-generators

      (let ((generators (cons generator more-generators))
            (more-accum t))
        (lambda ()
          (if more-accum
              (let* ((list-accum (list ())) (append-to list-accum))
                (let loop ((x generators))
                  (when x
                    (when more-accum
                      (multiple-value-bind (result more) ((car x))
                        (if more
                            (progn (setq append-to (cdr (rplacd append-to (list result)))) (loop (cdr x)))
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
  (unless (functionp generator)
    (jerror 'simple-type-error "scan-concat - not a generator"))

  (if more-generators
      (let ((more-generators more-generators))
        (lambda ()
          (if generator
              (multiple-value-bind (value more) (generator)
                (if more
                    (values value more)
                    (if more-generators
                        (progn
                          (setq generator (car more-generators)
                                more-generators (cdr more-generators))
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
         ,(if result `(let ((,var ())) ,@result))))))


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
    ((null seq)    nil)
    ((consp seq)   (copy-list seq))
    ((vectorp seq) (vector-copy seq))
    (t             (jerror 'simple-type-error "copy-seq - not a sequence: '%s'" seq))))


;;; = Function: length
;;;     (length sequence) -> length
;;;
;;; Since: 1.1
;;;
;;; Returns the length of `sequence`.
(defun length (seq)
  (cond
    ((null seq) 0)
    ((listp seq) (list-length seq))
    ((vectorp seq) (vector-length seq))
    (t (jerror 'simple-type-error "length - not a sequence: '%s'" seq))))


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
             (if l
                 (reverse/list (cdr l) (cons (car l) lp))
                 lp))

           (reverse/vector (from to get set)
             (let loop ((from-idx 0) (to-idx (1- (vector-length to))))
               (when (>= to-idx 0)
                 (set to to-idx (get from from-idx))
                 (loop (1+ from-idx) (1- to-idx))))
             to))

    (cond
      ((null seq)               nil)
      ((consp seq)              (reverse/list seq ()))
      ((stringp seq)            (reverse/vector seq (make-array (vector-length seq) 'character) sref sset))
      ((simple-vector-p seq)    (reverse/vector seq (make-array (vector-length seq)) svref svset))
      ((bit-vector-p seq)       (reverse/vector seq (make-array (vector-length seq) 'bit) bvref bvset))
      ((vectorp seq)            (reverse/vector seq (make-array (vector-length seq)) seqref seqset))
      (t                        (jerror 'simple-type-error "reverse - not a sequence: '%s'" seq)))))


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
               (if (atom 2nd)
                   3rd
                   (progn
                     (rplacd 2nd 3rd)
                     (loop (cdr 1st) 1st 2nd)))))

           (nreverse/vector (vector getter setter)
             (let loop ((left-index 0)
                        (right-index (1- (vector-length vector))))
               (if (<= right-index left-index)
                   vector
                   (let ((left (getter vector left-index))
                         (right (getter vector right-index)))
                     (setter vector left-index right)
                     (setter vector right-index left)
                     (loop (1+ left-index) (1- right-index)))))))

    (cond
      ((null seq)               nil)
      ((consp seq)              (nreverse/list seq))
      ((stringp seq)            (nreverse/vector seq sref sset))
      ((simple-vector-p seq)    (nreverse/vector seq svref svset))
      ((bit-vector-p seq)       (nreverse/vector seq bvref bvset))
      ((vectorp seq)            (nreverse/vector seq seqref seqset))
      (t                        (jerror 'simple-type-error "nreverse - not a sequence: '%s'" seq)))))


;;; = Function: remove-if
;;;     (remove-if pred sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without the elements for which `pred`
;;; evaluates to non-nil.
(defun remove-if (pred seq)
  (labels ((remove-if/list (l)
             (let* ((result (list ()))
                    (append-to result))
               (let loop ((l l))
                 (when l
                   (unless (pred (car l))
                     (setq append-to (cdr (rplacd append-to (list (car l))))))
                   (loop (cdr l))))
               (cdr result)))

           (remove-if/vector (vec)
             (let* ((len (vector-length vec))
                    (result (list ()))
                    (append-to result)
                    tmp)
               (dotimes (i len (cdr result))
                 (unless (pred (setq tmp (seqref vec i)))
                   (setq append-to (cdr (rplacd append-to (list tmp)))))))))

    (cond
      ((null seq)                nil)
      ((consp seq)               (remove-if/list seq))
      ((stringp seq)             (list->string            (remove-if/vector seq)))
      ((simple-vector-p seq)     (list->simple-vector     (remove-if/vector seq)))
      ((simple-bit-vector-p seq) (list->bit-vector        (remove-if/vector seq)))
      ((vectorp seq)             (list->simple-vector     (remove-if/vector seq)))
      (t                         (jerror 'simple-type-error "remove-if - not a sequence: '%s'" seq)))))


;;; = Function: remove
;;;     (remove elem sequence) -> sequence
;;;
;;; Since: 1.1
;;;
;;; Return a fresh sequence without occurrences of `elem`.
;;; An occurrence is determined by `eql`.
(defun remove (elem seq)
  (remove-if (lambda (x) (eql x elem)) seq))


(labels ((m%list->sequence (lst result-type)
           (cond ((eq result-type 'null)              (when lst
                                                        (jerror 'simple-type-error "cannot create a sequence of type null w/ length > 0"))
                                                      nil)
                 ((eq result-type 'list)              lst)
                 ((eq result-type 'cons)              (unless lst
                                                        (jerror 'simple-type-error "cannot create a sequence of type cons w/ length 0"))
                                                      lst)
                 ((eq result-type 'vector)            (list->simple-vector lst))
                 ((eq result-type 'simple-vector)     (list->simple-vector lst))
                 ((eq result-type 'simple-bit-vector) (list->bit-vector lst))
                 ((eq result-type 'bit-vector)        (list->bit-vector lst))
                 ((eq result-type 'string)            (list->string lst))
                 ((eq result-type 'simple-string)     (list->string lst))
                 (t                                   (jerror 'simple-type-error "%s is a bad type specifier for sequences" result-type)))))


;;; = Function: concatenate
;;;     (concatenate result-type sequences*) -> result-sequence
;;;
;;; Since 1.4.7
;;;
;;; `concatenate` returns a sequence that contains all the individual elements
;;; of all the sequences in the order that they are supplied.
;;; The sequence is of type result-type, which must be a subtype of type sequence.
;;;
;;; All of the sequences are copied from; the result does not share any structure
;;; with any of the sequences.
(defun concatenate (result-type . sequences)
  (let ((result (list nil)))
    (let* loop ((sequences sequences)
                (append-to result))
      (when sequences
        (dogenerator (x (scan (car sequences)))
          (setq append-to (m%rplacd append-to (list x))))
        (loop (cdr sequences) append-to)))
    (m%list->sequence (cdr result) result-type)))


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
      (let* ((result (list ()))
             (append-to result))
        (labels ((collect (val more)
                   (when more
                     (setq append-to (cdr (rplacd append-to (list (func val)))))
                     (multiple-value-call collect (seq)))))
          (multiple-value-call collect (seq))
          (m%list->sequence (cdr result) result-type)))
      (labels ((collect (val more)
                 (when more
                   (func val)
                   (multiple-value-call collect (seq)))))
        (multiple-value-call collect (seq)))))

) ; labels


;;; = Function: map-into
;;;     (map-into result-sequence function sequence*) -> result-sequence
;;;
;;; Since: 1.2
;;;
;;; Destructively modifies `result-sequence` to contain the results
;;; of applying `function` to each element in the argument sequences in turn.
;;; The iteration terminates when the shortest sequence (of any of
;;; the sequences or the result-sequence) is exhausted.
;;;
;;; If `result-sequence` is `nil`, `map-into` returns `nil`.
;;;
;;; Similar to CL `map-into`.
(defun map-into (result func . sequences)
  (when result
    (let (result-cursor set-result has-next-result result-length seq len)
      (cond
        ((consp result)
         (setq result-cursor result
               set-result (lambda (elem)
                            (rplaca result-cursor elem) (setq result-cursor (cdr result-cursor)))
               has-next-result (lambda () result-cursor)))

        ((vectorp result)
         (setq result-cursor 0
               set-result (lambda (elem)
                            (seqset result result-cursor elem) (setq result-cursor (1+ result-cursor)))
               has-next-result (lambda () (< result-cursor result-length))
               result-length (vector-length result)))

        (t (jerror 'simple-type-error "map-into - not a sequence: '%s'" result)))

      (if (cdr sequences)
          ;; 2 or more sequences given
          (labels ((into (next-values more)
                     (when (and (has-next-result) more)
                       (set-result (apply func next-values))
                       (multiple-value-call into (seq)))))
            (setq seq (apply scan-multiple (mapcar m%scan sequences)))
            (multiple-value-call into (seq)))

          (if sequences
              ;; 1 sequence given
              (cond
                ((null (setq seq (car sequences))) nil)

                ((consp seq)
                 (let loop ((l seq))
                      (when (and (has-next-result) l)
                        (set-result (func (car l)))
                        (loop (cdr l)))))

                ((vectorp seq)
                 (setq len (vector-length seq))
                 (let loop ((i 0))
                      (when (and (has-next-result) (< i len))
                        (set-result (func (seqref seq i)))
                        (loop (1+ i)))))
                (t (jerror 'simple-type-error "map-into: not a sequence: '%s'" seq)))

              ;; 0 sequences given
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
            (t (jerror 'simple-type-error "reduce - not a sequence: '%s'" seq))))))


; hash tables *********************************************************

;;; = Function: gethash
;;;     (gethash key hash [default]) -> object, was-present-p
;;;
;;; Since: 1.4
(defmacro gethash (key hash . default)
  (if default
      `(hashref ,hash ,key ,@default)
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
(macrolet ((m%mapxx (name comb lastelem)
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
                  (multiple-value-call do-step (seq))))))


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

) ; (macrolet...


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
;;;     (write-char c [dest]) -> c
;;;
;;; Since: 1.1
;;;
;;; `write-char` outputs `c` to stdout.
(defun write-char (c . dest)
  (if (characterp c)
      (write c nil (car dest))
      (jerror 'simple-type-error "write-char - not a character: '%s'" c)))


;;; = Function: terpri, prin1, princ, print
;;;     (terpri [dest]) -> nil
;;;     (prin1 obj [dest]) -> obj
;;;     (princ obj [dest]) -> obj
;;;     (print obj [dest]) -> obj
;;;
;;; Since: 1.1
(defmacro terpri dest
  `(progn
     (writeln "" nil ,@dest)
     nil))
(defun terpri dest
  (writeln "" nil (car dest))
  nil)

(defmacro prin1 (obj . dest)
  `(write ,obj t ,@dest))
(defun prin1 (obj . dest)
  (write obj t (car dest)))

(defmacro princ (obj . dest)
  `(write ,obj nil ,@dest))
(defun princ (obj . dest)
  (write obj nil (car dest)))

(defmacro print (obj . dest)
  `(lnwrite ,obj t ,@dest))
(defun print (obj . dest)
  (lnwrite obj t (car dest)))


;;; = Macro: with-output-to-string
;;;     (with-output-to-string (var) forms*) -> string
;;;
;;; Since: 1.4.2
;;;
;;; Similar to CL's `with-output-to-string` except:
;;; CL's optional `string-form` and `element-type` are not supported,
;;; therefore the return value of `with-output-to-string` always is the string.
(defmacro with-output-to-string (s . body)
  `(let ((,@s (make-array 0 'character t)))
     ,@body
     ,@s))


;;; = Function: pprint
;;;     (pprint object [dest]) -> t
;;;
;;; Since: 1.1
;;;
;;; Simple pretty printer,
;;; based on https://picolisp.com/wiki/?prettyPrint .
(defun pprint (obj . dest)
  (setq dest (car dest))
  (labels
      ((size (l)
         (if l
             (if (consp l)
                 (+ (size (car l)) (size (cdr l)))
                 1)
             1))

       (pp (obj l)
         (dotimes (ign l)
           (write " " nil dest))

         (if (< (size obj) 6)
             (write obj t dest)

             (progn
               (write-char #\( dest)

               (let loop ()
                 ;;(progn (write "*** size=" nil) (write (size obj)) (write " " nil) (write obj) (writeln "***" nil) t)
                 (cond ((member (car obj) '(defun define defmacro))

                        (incf l)
                        (write (pop obj) t dest)                  ; defun/define/defmacro
                        (write-char #\  dest)

                        (when (symbolp (car obj))
                          (write (pop obj) t dest)                ; name/ looplabel
                          (write-char #\  dest)

                          (when obj (write (pop obj) t dest))))   ; lambdalist

                       ((member (car obj) '(lambda let let* letrec dotimes dolist dovector))

                        (incf l)
                        (write (pop obj) t dest)
                        (write-char #\  dest)

                        (when (symbolp (car obj))
                          (write (pop obj) t dest)
                          (write-char #\  dest))

                        (when (and obj (< (size (car obj)) 7))
                          (pp (pop obj) 0)))

                       ((and (eq (car obj) 'if)
                             (incf l 3)
                             (write (pop obj) t dest)
                             (write-char #\  dest)
                             (< (size obj) 6))

                        (loop))

                       ((and (member (write (pop obj) t dest) '(cond labels setq setf psetf when unless))
                             (incf l)
                             (< (size obj) 6))

                        (write-char #\  dest)
                        (loop))))

               (let loop ()
                 (when obj
                   (writeln "" nil dest)
                   (if (atom obj)
                       (pp obj (1+ l))
                       (progn
                         (pp (pop obj) (1+ l))
                         (loop)))))

               (write-char #\) dest)))

         t))

    (writeln "" nil dest)
    (pp obj 0)))


; misc ****************************************************************
; helper function for time
(defun call-with-timing (func . args)
  (let* ((tstart-real (get-internal-real-time))
         (tstart-run  (get-internal-run-time))
         (result (apply func args))
         (tend-real (get-internal-real-time))
         (tend-run (get-internal-run-time))
         (secs-real (/ (- tend-real tstart-real) internal-time-units-per-second))
         (secs-run  (/ (- tend-run  tstart-run) internal-time-units-per-second)))
    (jformat t "Evaluation took:%n  %g seconds of real time%n  %g seconds of total run time%n" secs-real secs-run)
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
  (when elems
    (let* loop ((result (list (car elems)))
                (elems (cdr elems))
                (append-to result))
      (if elems
          (loop result (cdr elems) (setq append-to (cdr (rplacd append-to (list (car elems))))))
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
  (lambda callargs
    (apply func (append args callargs))))

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
  (lambda callargs
    (apply func (append callargs args))))


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
           (when names
             (cons (list (car names) '(gensym)) (loop (cdr names)))))
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
                     ;; if it's a list with other parameters, insert expr (recursive call)
                     ;; as second parameter into partial (note need to use cons to ensure same list for func args)
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
                     ;; if it's a list with other parameters, insert expr (recursive call)
                     ;; as last form
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
;;;     (with-accumulator accumulator-name accumulator start-value-form
;;;       forms*) -> result
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
    `(let* ((,result (list ()))
            (,append-to ,result)
            (collect (lambda (,delta)
                       (setq ,append-to (cdr (rplacd ,append-to (list ,delta)))))))
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


; format **************************************************************

;;; = Function: format
;;;     (format destination control-string args*) -> result
;;;
;;; Since: 1.5
;;;
;;; A simplified subset of Common Lisp's function `format`.
;;;
;;; Note that this simplified `format` does not use or set CL's printer variables
;;; such as `*print-base*`, `*print-circle*`, ... .
;;;
;;; Only the format characters `C, %, &, |, ~, *, B ,D, O, R, X, E, F, G, A, S, W` and Tilde-Newline are supported.
;;;
;;; `C` supports the modifier `@` for printing #\-style escaping.
;;;
;;; `B, D, O, R, X` support `mincol, padchar, commachar` and `comma-interval`,
;;; the modifier `@` for always printing the sign and the modifier `:` for grouping digits.
;;;
;;; `R` does not support printing english numbers (giving the base, `@` or `:@` is required).
;;;
;;; `E, F, G`: CL's full `format` is `~w,d,k,overflowchar,padcharF`, this subset only supports `~w,dF`
;;; and the modifier `@` for always printing the sign.
;;;
;;; `A` and `S` support `~mincol,colinc,minpad,padcharA` for padding, `:`, and the modifier `@` for left-padding.

(macrolet ((require-argument ()
             `(if arguments
                  (car arguments)
                  (jerror 'simple-error "format - not enough arguments")))

           (prefix-param (value-form)
             `(if (eql #\v (car params))
                  (prog1
                      (require-argument)
                    (setq arguments (cdr arguments)))
                  ,value-form)))

(defun m%nonneg-integer-for-format (arg)
  "check that arg is a non-negative integer number and return it truncated to an integer"
  (if (and (numberp arg)
           (= arg (truncate arg))
           (>= arg 0))
      (truncate arg)
      (jerror "format - not an integer >= 0: '%s'" arg)))

(labels ((char-with-default (obj default)
           (if obj
               (if (characterp obj)
                   obj
                   (jerror "format - not a character: '%s'" obj))
               default))

         (int-with-default (obj default)
           (if obj
               (if (and (numberp obj)
                        (= obj (setq obj (truncate obj)))
                        (>= obj 0))
                   obj
                   (jerror "format - not an integer >= 0: '%s'" obj))
               default))

         (append-reversed-num (rev arg base)
           (let loop ((n (if (> arg 0) (- arg) arg))) ; normalize integers to negative numbers because e.g. (abs most-negative-fixnum) would not fit in a fixnum
             (when (< n 0)
               (vector-add rev (sref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- (rem n base))))
               (loop (truncate n base))))))



;; semi-private: used by the function generated by 'format-function' and the expansion of 'formatter'
(defun m%print-integer (arguments output-stream base colonp atp params)
  (let* ((mincol (prefix-param (car params)))
         (params (cdr params))
         (padchar (prefix-param (char-with-default (car params) #\ )))
         (params (cdr params))
         (commachar (prefix-param (char-with-default (car params) #\,)))
         (params (cdr params))
         (comma-interval (prefix-param (int-with-default (car params) 3)))
         (arg (car arguments))
         (rev (make-array 0 'character t)))

    (if (integerp arg)
        (progn
          (if colonp
              ;; grouping: separate 'comma-interval' digits with 'commachar'
              (labels ((loop (n pos)
                        (when (< n 0)
                          (when (= pos comma-interval)
                            (vector-add rev commachar)
                            (setq pos 0))
                          (vector-add rev (sref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- (rem n base))))
                          (loop (truncate n base) (1+ pos)))))
                (loop (if (> arg 0) (- arg) arg) 0))

              ;; no grouping
              (append-reversed-num rev arg base))

          ;; print sign and number
          (if (< arg 0)
              (vector-add rev #\-)
              (when atp
                (vector-add rev #\+)))

          ;; padding
          (if mincol
              (dotimes (i (- mincol (slength rev)))
                (vector-add rev padchar)))

          (write (nreverse rev) nil output-stream))

        (write (car arguments) nil output-stream)))

  (cdr arguments))


;; semi-private: used by the expansion of 'formatter'
(defun m%print-simple-integer (arguments output-stream base)
  (if (and (/= base 10) (integerp (car arguments)))
      (let ((arg (car arguments))
            (rev (make-array 0 'character t)))

        (append-reversed-num rev arg base)

        ;; print sign and number
        (when (< arg 0)
          (vector-add rev #\-))
        (write (nreverse rev) nil output-stream))

      (write (car arguments) nil output-stream))
  (cdr arguments))


;; semi-private: used by the function generated by 'format-function' and the expansion of 'formatter'
(defun m%print-roman (arguments output-stream colonp)
  (let ((n (car arguments)))
    (unless (and (numberp n) (= n (truncate n)))
      (jerror "format - not an integer: '%s'" n))
    (unless (<= 1 n (if colonp 4999 3999))
      (jerror "format - number too large to print in Roman numerals: '%s'" n))

    (write (if colonp
               (string-join
                 ""
                 (svref #("" "M" "MM" "MMM" "MMMM")                               (floor n            1000))
                 (svref #("" "C" "CC" "CCC" "CCCC" "D" "DC" "DCC" "DCCC" "DCCCC") (floor (rem n 1000) 100))
                 (svref #("" "X" "XX" "XXX" "XXXX" "L" "LX" "LXX" "LXXX" "LXXXX") (floor (rem n 100)  10))
                 (svref #("" "I" "II" "III" "IIII" "V" "VI" "VII" "VIII" "VIIII")        (rem n 10)))
               (string-join
                 ""
                 (svref #("" "M" "MM" "MMM")                                 (floor n            1000))
                 (svref #("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM") (floor (rem n 1000) 100))
                 (svref #("" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC") (floor (rem n 100)  10))
                 (svref #("" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")        (rem n 10))))
           nil
           output-stream))

  (cdr arguments))


;; semi-private: used by the expansion of 'formatter'
(defun m%print-float (arguments output-stream jformat-string)
  (if (numberp (car arguments))
      (jformat-locale output-stream "en-US" jformat-string (* 1.0 (car arguments)))
      (write (car arguments) nil output-stream))
  (cdr arguments))


;; semi-private: used by the function generated by 'format-function' and the expansion of 'formatter'
(defun m%print-obj (arguments output-stream escapep colonp atp params)
  (let* ((mincol (prefix-param (int-with-default (car params) 0)))
         (params (cdr params))
         (colinc (prefix-param (int-with-default (car params) 1)))
         (params (cdr params))
         (minpad (prefix-param (int-with-default (car params) 0)))
         (params (cdr params))
         (padchar (prefix-param (char-with-default (car params) #\ )))
         (arg (car arguments))
         (str (if arg
                  (write-to-string arg escapep)
                  (if colonp
                      "()"
                      "nil"))))

    (unless atp
      (write str nil output-stream))

    (dotimes (i (+ minpad (* (ceiling (- mincol minpad (slength str)) colinc) colinc)))
      (write padchar nil output-stream))

    (when atp
      (write str nil output-stream)))
  (cdr arguments))

) ; labels


(labels ((parse-control-string (control-string)
           ;; used by the macro 'formatter' and by the function 'm%format-function'
           (let* ((result (list ()))
                  (append-to result)
                  (i 0)
                  (j nil)
                  (control-string-length (slength control-string)))
             (when (> control-string-length 0)
               (labels ((collect (obj)
                          (setq append-to (cdr (rplacd append-to (list obj)))))

                        (start ()
                          (when (< i control-string-length)
                            (when (eql (sref control-string i) #\~)
                              (and j (< j i) (collect (string-subseq control-string j i)))
                              (incf i)
                              (let* (code colonp atp arg
                                     (args (list ()))
                                     (append-to-args args))
                                (labels ((collect-arg (arg)
                                           (setq append-to-args (cdr (rplacd append-to-args (list arg)))))

                                         (next ()
                                           (setq code (sref control-string i))
                                           (case code
                                             (#\'
                                              (setq arg (sref control-string (incf i)))
                                              (incf i)
                                              (next))

                                             (#\,
                                              (collect-arg arg)
                                              (setq arg nil)
                                              (incf i)
                                              (next))

                                             (#\:
                                              (setq colonp t)
                                              (incf i)
                                              (next))

                                             (#\@
                                              (setq atp t)
                                              (incf i)
                                              (next))

                                             ((#\+ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                              (setq arg 0)
                                              (let ((sign 1))
                                                (if (eql code #\+)
                                                    (setq i (1+ i) code (sref control-string i))
                                                    (when (eql code #\-)
                                                      (setq i (1+ i) code (sref control-string i) sign -1)))
                                                (let loop ((code (char-code code)))
                                                  (when (and (>= code 48) (<= code 57))
                                                    (setq arg (+ (* arg 10) (- code 48))
                                                          i (1+ i))
                                                    (when (< i control-string-length)
                                                      (loop (char-code (sref control-string i))))))
                                                (setq arg (truncate arg sign)))
                                              (next))

                                             ((#\v #\V)
                                              (setq arg #\v)
                                              (incf i)
                                              (next))

                                             (#\Newline
                                              (let loop ()
                                                (when (and (< (1+ i) control-string-length)
                                                           (member (sref control-string (1+ i)) '(#\  #\Tab #\Vt #\Page #\Return)))
                                                  (incf i)
                                                  (loop))))

                                             (t
                                              (when arg
                                                (collect-arg arg)
                                                (setq arg nil))
                                              (collect (list* code colonp atp (cdr args)))))))
                                  (next)))

                              (setf j (1+ i)))
                            (unless j (setf j i))
                            (incf i)
                            (start))
                          (when (< j i)
                            (collect (string-subseq control-string j i))
                            (setq j i))))
                 (start)))

             (cdr result)))

         (float-fmtstring (c atp w d)
           (let ((jformat-string (make-array 0 'character t)))
             (vector-add jformat-string #\%)
             (when atp (vector-add jformat-string #\+))
             (when w
               (dovector (w (write-to-string (m%nonneg-integer-for-format w)))
                         (vector-add jformat-string w)))
             (when d
               (vector-add jformat-string #\.)
               (dovector (d (write-to-string (m%nonneg-integer-for-format d)))
                         (vector-add jformat-string d)))
             (vector-add jformat-string c)
             jformat-string)))


;; semi-private: used by the expansion of 'formatter'
(defun m%print-float-fmt (arguments output-stream c atp w d)
  (when (eql #\v w)
    (setq w (require-argument))
    (setq arguments (cdr arguments)))
  (when (eql #\v d)
    (setq d (require-argument))
    (setq arguments (cdr arguments)))

  (let ((arg (car arguments)))
    (if (numberp arg)
        (jformat-locale output-stream "en-US" (float-fmtstring c atp w d) (* 1.0 arg))
        (write arg nil output-stream)))
  (cdr arguments))


;;; = Macro: formatter
;;;     (formatter control-string) -> function
;;;
;;; Since: 1.5
;;;
;;; Returns a function with the argument list `(destination . args)`
;;; that writes to `destination` and returns unused args as a list.
;;;
;;; Sample usage:
;;;
;;;     (let ((dest (make-array 0 'character t)))
;;;       (values ((formatter "~&~A~A") dest 'a 'b 'c)
;;;               dest))
;;;
;;;     -> (c)
;;;     -> "
;;;     ab"
(defmacro formatter (control-string)
  (let* ((body (list ()))
         (append-to body))

    (labels ((require-argument-sexp ()
               `(if arguments
                    (car arguments)
                    (jerror 'simple-error "format - not enough arguments")))

             (collect (form)
               (setq append-to (cdr (rplacd append-to (list form)))))

             (collect-shift (form)
               (collect form)
               (collect `(setq arguments (cdr arguments))))

             (collect-setq (form)
               (collect `(setq arguments ,form)))

             (nchars (c params)
               (let* ((n (or (car params) 1))
                      (result (make-array n 'character nil)))
                 (dotimes (i n result)
                   (setf (sref result i) c))))

             (do-char (c params)
               (if (eql #\v (car params))
                   (collect `(dotimes (n (prog1 ,(require-argument-sexp) (setq arguments (cdr arguments)))) (write ,c nil output-stream)))
                   (collect `(write ,(nchars c params) nil output-stream))))

             (do-integer (base colonp atp params)
               (collect-setq (if (or colonp atp params)
                                 `(m%print-integer        arguments output-stream ,base ,colonp ,atp ',params)
                                 `(m%print-simple-integer arguments output-stream ,base))))

             (do-float (c atp params)
               (let ((w (car params))
                     (d (cadr params)))
                 (if (or (eql #\v w) (eql #\v d))
                     (collect-setq `(m%print-float-fmt arguments output-stream ,c ,atp ',w ',d))
                     (collect-setq `(m%print-float arguments output-stream ,(float-fmtstring c atp w d)))))))

      `(lambda (output-stream . orig-arguments)
         (let ((arguments orig-arguments))
           ,@(dolist (elem (parse-control-string control-string) (cdr body))
               (if (stringp elem)
                   (collect `(write ,elem nil output-stream))
                   (let ((colonp (cadr elem))
                         (atp (caddr elem))
                         (params (cdddr elem)))

                     (case (car elem)

                       ;; Basic output

                       ;; Tilde C: Character
                       ;; The next arg should be a character.
                       ;; ~c and ~:c will print the character, ~@c and ~@:c will print a #\... sequence,
                       ;; (i.e. : is ignored)
                       ((#\c #\C)
                        (collect-shift `(write (car arguments) ,atp output-stream)))

                       ;; Tilde Percent: Newline
                       ;; Tilde Ampersand: Fresh-Line
                       ;; ~n% and ~n& output n newlines. No arg is used.
                       ;; (~& should omit the first newline if the output stream
                       ;; is already at the beginning of a line, this is not implemented.)
                       ((#\% #\&)
                        (do-char #\Newline params))

                       ;; Tilde Vertical-Bar: Page
                       ;; This outputs a page separator character, if possible. ~n| does this n times.
                       (#\|
                        (do-char #\Page params))

                       ;; Tilde Tilde: Tilde
                       ;; This outputs a tilde. ~n~ outputs n tildes.
                       (#\~
                        (do-char #\~ params))


                       ;; Radix Control

                       ;; Tilde R: Radix
                       ;; ~nR prints arg in radix n. sbcl supports 2..36
                       ((#\r #\R)
                        (if (car params)
                            (do-integer (car params) colonp atp (cdr params))
                            (collect-setq
                             (if atp
                                 `(m%print-roman arguments output-stream ,colonp)
                                 (jerror "format - english numbers are not supported")))))

                       ;; Tilde D: Decimal
                       ;; ~mincolD uses a column width of mincol; spaces are inserted on the left
                       ;; if the number requires fewer than mincol columns for its digits and sign.
                       ;; ~mincol,padcharD uses padchar as the pad character instead of space.
                       ;; If arg is not an integer, it is printed in ~A format and decimal base.
                       ;; The @ modifier causes the number's sign to be printed always; the default
                       ;; is to print it only if the number is negative. The : modifier is ignored.
                       ((#\d #\D)
                        (if (or colonp params)
                            (collect-setq `(m%print-integer arguments output-stream 10 ,colonp ,atp ',params))
                            (if atp
                                (collect-setq `(let ((arg (car arguments)))
                                                 (and (integerp arg)
                                                      (>= arg 0)
                                                      (write #\+ nil output-stream))
                                                 (write arg nil output-stream)
                                                 (cdr arguments)))
                                (collect-shift `(write (car arguments) nil output-stream)))))

                       ;; Tilde B: Binary
                       ((#\b #\B)
                        (do-integer 2 colonp atp params))

                       ;; Tilde O: Octal
                       ((#\o #\O)
                        (do-integer 8 colonp atp params))

                       ;; Tilde X: Hexadecimal
                       ((#\x #\X)
                        (do-integer 16 colonp atp params))


                       ;; Floating point

                       ;; Tilde E: Exponential Floating-Point
                       ((#\e #\E)
                        (do-float #\e atp params))

                       ;; Tilde F: Fixed-Format Floating-Point
                       ((#\f #\F)
                        (do-float #\f atp params))

                       ;; Tilde G: General Floating-Point
                       ((#\g #\G)
                        (if params
                            (do-float #\g atp params)
                            (collect-shift (if atp
                                               `(let ((arg (car arguments)))
                                                  (and (floatp arg)
                                                       (>= arg 0)
                                                       (write #\+ nil output-stream))
                                                  (write arg nil output-stream))
                                               `(write (car arguments) nil output-stream)))))


                       ;; Printer Operations

                       ;; Tilde A: Aesthetic
                       ((#\a #\A)
                        (if (or colonp (car params))
                            (collect-setq `(m%print-obj arguments output-stream nil ,colonp ,atp ',params))
                            (collect-shift `(write (car arguments) nil output-stream))))

                       ;; Tilde S: Standard
                       ((#\s #\S)
                        (if (or colonp (car params))
                            (collect-setq `(m%print-obj arguments output-stream t ,colonp ,atp ',params))
                            (collect-shift `(write (car arguments) t output-stream))))

                       ;; Tilde W: Write
                       ((#\w #\W)
                        ;;(when params (jerror "format - too many arguments, format character W accepts 0"))
                        (collect-shift `(write (car arguments) t output-stream)))


                       ;; Layout Control

                       ;; Tilde T: Tabulate
                       ((#\t #\T)
                        (do-char #\Tab params))


                       ;; Control flow

                       ;; Tilde *: Goto
                       ;; @... absolute, :... backwards
                       (#\*
                        (cond (atp
                               (when colonp (jerror 'simple-error "can't use both : and @ modifiers with ~*"))
                               (case (car params)
                                 ((nil) (collect `(setq arguments orig-arguments)))
                                 (#\v   (collect `(setq arguments (nthcdr ,(require-argument-sexp) orig-arguments))))
                                 (t     (collect `(setq arguments (nthcdr ,(car params) orig-arguments))))))

                              (colonp
                               (case (car params)
                                 ((nil) (collect `(setq arguments (last orig-arguments (+ (list-length arguments) 1)))))
                                 (#\v   (collect `(setq arguments (last orig-arguments (+ (list-length arguments) ,(require-argument-sexp))))))
                                 (t     (collect `(setq arguments (last orig-arguments (+ (list-length arguments) ,(car params))))))))

                              (t
                               (case (car params)
                                 ((nil) (collect `(setq arguments (cdr arguments))))
                                 (#\v   (collect `(setq arguments (nthcdr ,(require-argument-sexp) (cdr arguments)))))
                                 (t     (collect `(setq arguments (nthcdr ,(car params) arguments))))))))


                       (t (jerror "format - unimplemented format character '%s'" (car elem)))))))

           ,'arguments)))))


;; private: used by the function 'format'
(defun m%format-function (control-string)
  (lambda (output-stream . orig-arguments)
    (let ((arguments orig-arguments))
      (labels ((do-char (c params)
                 (dotimes (n (prefix-param (or (car params) 1)))
                   (write c nil output-stream)))

               (do-integer (base colonp atp params)
                 (setq arguments (m%print-integer arguments output-stream base colonp atp params)))

               (do-float (c atp params)
                 (let ((w (prefix-param (car params)))
                       (d (prefix-param (cadr params)))
                       (arg (car arguments)))
                   (if (floatp arg)
                       (jformat-locale output-stream "en-US" (float-fmtstring c atp w d) arg)
                       (write arg nil output-stream)))
                 (setq arguments (cdr arguments)))

               (do-general-float (atp params)
                 (let ((w (prefix-param (car params)))
                       (d (prefix-param (cadr params)))
                       (arg (car arguments)))
                   (if (and (floatp arg) params)
                       (jformat-locale output-stream "en-US" (float-fmtstring atp w d) arg)
                       (progn
                         (and atp (floatp arg) (>= arg 0) (write #\+ nil output-stream))
                         (write arg nil output-stream))))
                 (setq arguments (cdr arguments))))

        (dolist (elem (parse-control-string control-string))
          (if (stringp elem)
              (write elem nil output-stream)
              (let ((colonp (cadr elem))
                    (atp (caddr elem))
                    (params (cdddr elem)))

                (case (car elem)

                  ;; Basic output

                  ;; Tilde C: Character
                  ;; The next arg should be a character.
                  ;; ~c and ~:c will print the character, ~@c and ~@:c will print a #\... sequence,
                  ;; (i.e. : is ignored)
                  ((#\c #\C)
                   (write (car arguments) atp output-stream)
                   (setq arguments (cdr arguments)))

                  ;; Tilde Percent: Newline
                  ;; Tilde Ampersand: Fresh-Line
                  ;; ~n% and ~n& output n newlines. No arg is used.
                  ;; (~& should omit the first newline if the output stream
                  ;; is already at the beginning of a line, this is not implemented.)
                  ((#\% #\&)
                   (do-char #\Newline params))

                  ;; Tilde Vertical-Bar: Page
                  ;; This outputs a page separator character, if possible. ~n| does this n times.
                  (#\|
                   (do-char #\Page params))

                  ;; Tilde Tilde: Tilde
                  ;; This outputs a tilde. ~n~ outputs n tildes.
                  (#\~
                   (do-char #\~ params))


                  ;; Radix Control

                  ;; Tilde R: Radix
                  ;; ~nR prints arg in radix n. sbcl supports 2..36
                  ((#\r #\R)
                   (if (car params)
                       (setq arguments (m%print-integer arguments output-stream (car params) colonp atp (cdr params)))
                       (if atp
                           (setq arguments (m%print-roman arguments output-stream colonp))
                           (jerror "format - english numbers are not supported"))))

                  ;; Tilde D: Decimal
                  ;; ~mincolD uses a column width of mincol; spaces are inserted on the left
                  ;; if the number requires fewer than mincol columns for its digits and sign.
                  ;; ~mincol,padcharD uses padchar as the pad character instead of space.
                  ;; If arg is not an integer, it is printed in ~A format and decimal base.
                  ;; The @ modifier causes the number's sign to be printed always; the default
                  ;; is to print it only if the number is negative. The : modifier is ignored.
                  ((#\d #\D)
                   (do-integer 10 colonp atp params))

                  ;; Tilde B: Binary
                  ((#\b #\B)
                   (do-integer 2 colonp atp params))

                  ;; Tilde O: Octal
                  ((#\o #\O)
                   (do-integer 8 colonp atp params))

                  ;; Tilde X: Hexadecimal
                  ((#\x #\X)
                   (do-integer 16 colonp atp params))


                  ;; Floating point

                  ;; Tilde E: Exponential Floating-Point
                  ((#\e #\E)
                   (do-float #\e atp params))

                  ;; Tilde F: Fixed-Format Floating-Point
                  ((#\f #\F)
                   (do-float #\f atp params))

                  ;; Tilde G: General Floating-Point
                  ((#\g #\G)
                   (do-general-float atp params))


                  ;; Printer Operations

                  ;; Tilde A: Aesthetic
                  ((#\a #\A)
                   (if (or colonp (car params))
                       (setq arguments (m%print-obj arguments output-stream nil colonp atp params))
                       (progn (write (car arguments) nil output-stream)
                              (setq arguments (cdr arguments)))))

                  ;; Tilde S: Standard
                  ((#\s #\S)
                   (if (or colonp (car params))
                       (setq arguments (m%print-obj arguments output-stream t colonp atp params))
                       (progn (write (car arguments) t output-stream)
                              (setq arguments (cdr arguments)))))

                  ;; Tilde W: Write
                  ((#\w #\W)
                   ;;(when params (jerror "format - too many arguments, format character C accepts 0"))
                   (write (car arguments) t output-stream)
                   (setq arguments (cdr arguments)))


                  ;; Layout Control

                  ;; Tilde T: Tabulate
                  ((#\t #\T)
                   (do-char #\Tab params))


                  ;; Control flow

                  ;; Tilde *: Goto
                  ;; @... absolute, :... backwards
                  (#\*
                   (cond (atp
                          (when colonp (jerror 'simple-error "can't use both : and @ modifiers with ~*"))
                          (case (car params)
                            ((nil) (setq arguments orig-arguments))
                            (#\v   (setq arguments (nthcdr (m%nonneg-integer-for-format (require-argument)) orig-arguments)))
                            (t     (setq arguments (nthcdr (car params) orig-arguments)))))

                         (colonp
                          (case (car params)
                            ((nil) (setq arguments (last orig-arguments (+ (list-length arguments) 1))))
                            (#\v   (setq arguments (last orig-arguments (+ (list-length arguments) (m%nonneg-integer-for-format (require-argument))))))
                            (t     (setq arguments (last orig-arguments (+ (list-length arguments) (car params)))))))

                         (t
                          (case (car params)
                            ((nil) (setq arguments (cdr arguments)))
                            (#\v   (setq arguments (nthcdr (m%nonneg-integer-for-format (require-argument)) (cdr arguments))))
                            (t     (setq arguments (nthcdr (car params) arguments)))))))


                  (t (jerror "format - unimplemented format character '%s'" (car elem)))))))))))

) ; labels
) ; macrolet


;; semi-private: used by the function 'm%format' and the expansion of the macro 'format'
(defun m%do-format (destination f . args)
  (if destination
      (progn (apply f (cons destination args))
             nil)
      (with-output-to-string (destination)
        (apply f (cons destination args)))))


(defun m%format (destination control-string . args)
  (apply #'m%do-format (list* destination
                              (if (functionp control-string)
                                  control-string
                                  (m%format-function control-string))
                              args)))


(defmacro format (destination control-string . args)
  (if (stringp control-string)
      `(m%do-format ,destination (formatter ,control-string) ,@args)
      `(m%format ,destination ,control-string ,@args)))


(define format m%format)


;;; = Function: error
;;;     (error [condition-type] control-string args*) -> |
;;;
;;; Since: 1.5
;;;
;;; A simplified subset of Common Lisp's function `error`.
(defun error (datum . args)
  (if (typep datum 'symbol)
      (jerror datum (apply format (cons nil args)))
      (jerror 'simple-error (apply format (list* nil datum args)))))


(defmacro m%def-macro-fun)

(provide "mlib")
