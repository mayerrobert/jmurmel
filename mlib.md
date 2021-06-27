# Mlib - Default library for Murmel

mlib adds commonly used Lisp functions and macros to the
[core Murmel language](murmel-langref.md).

Most of mlib's functions and macros are modeled after Common Lisp,
(often with reduced functionality) plus some additional macros and functions.

### Usage:

Copy mlib.lisp into the directory containing jmurmel.jar
or into the directory specified with --libdir
and begin your source file with

    (require "mlib")

### Provides:
mlib provides the following Common Lisp-like functions and macros:

- [caar..cdddr](#caarcdddr), [nthcdr, nth](#nthcdr-nth)
- [destructuring-bind](#destructuring-bind)
- [get-setf-expansion](#get-setf-expansion)
- [setf](#setf), [incf, decf](#incf-decf)
- [push](#push), [pop](#pop)
- [acons](#acons)
- [not](#not), [and](#and), [or](#or)
- [abs](#abs), [zerop](#zerop), [evenp](#evenp), [oddp](#oddp)
- [char=](#char), [char](#char-1)
- [equal](#equal)
- [prog1, prog2](#prog1-prog2)
- [when](#when), [unless](#unless), [do, do*](#do-do), [dotimes](#dotimes), [dolist](#dolist)
- [identity](#identity), [constantly](#constantly), [complement](#complement)
- [member](#member)
- [mapcar](#mapcar), [maplist](#maplist), [mapc](#mapc), [mapl](#mapl), [mapcan](#mapcan), [mapcon](#mapcon)
- [every](#every), [some](#some), [notevery](#notevery), [notany](#notany)
- [remove-if](#remove-if), [remove](#remove)
- [reduce](#reduce)
- [write-char](#write-char)
- [terpri, prin1, princ, print](#terpri-prin1-princ-print), [pprint](#pprint)
- [list-length](#list-length), [length](#length)

as well as the following additional functions and macros:

- [rplaca*](#rplaca), [rplacd*](#rplacd)
- [*f, /f, +f, -f](#f-f)
- [with-gensyms](#with-gensyms)
- [->](#-), [->>](#--1), [and->](#and-), [and->>](#and--1)
### caar..cdddr
    (c..r lst) -> result

c..r repeatedly apply car and/ or cdr as the name suggests.
### nthcdr, nth
    (nthcdr n lst) -> nth-tail
    (nth n lst) -> nth-element

nthcdr applies cdr n times and returns the result.
nth works as if `(car (nth n lst))` was invoked.
### rplaca*
    (rplaca* lst value) -> value

Replace the car of lst by value and return value (as opposed to rplaca which returns lst).
### rplacd*
    (rplacd* lst value) -> value

Replace the cdr of lst by value and return value (as opposed to rplacd which returns lst).
### destructuring-bind
    (destructuring-bind (vars*) (expressions*) forms*)

Murmel's destructuring-bind is a subset of CL's destructuring-bind,
trees are not supported, only lists are.

destructuring-bind binds the variables specified in vars
to the corresponding values in the list resulting from the evaluation
of expression; then destructuring-bind evaluates forms. 
### get-setf-expansion
    (get-setf-expansion place) -> vars, vals, store-vars, writer-form, reader-form
### setf
    (setf pair*) -> result

Takes pairs of arguments like SETQ. The first is a place and the second
is the value that is supposed to go into that place. Returns the last
value. The place argument may be any of the access forms for which SETF
knows a corresponding setting form, which currently are:

- symbols
- car..cdddr
### incf, decf
    (incf place [delta-form]) -> new-value
    (decf place [delta-form]) -> new-value

incf and decf are used for incrementing and decrementing
the value of place, respectively.

The delta is added to (in the case of incf) or subtracted
from (in the case of decf) the number in place and the result
is stored in place.

Without delta-form the return type of incf and decf will be
the type of the number in place, otherwise the return type will be float.
### *f, /f
    (*f place [delta-form]) -> new-value
    (/f place [delta-form]) -> new-value

*f and /f are used for multiplying and dividing
the value of place, respectively.

The number in place is multiplied (in the case of *f) by delta
or divided (in the case of /f) by delta and the result
is stored in place.

Without delta /f will return the reciprocal of the number in place,
*f will return the number in place.

Without delta-form the return type of *f will be
the type of the number in place, otherwise the return type will be float.
### +f, -f
    (+f place [delta-form]) -> new-value
    (-f place [delta-form]) -> new-value

+f and +f are used for adding and subtracting
to/ from the value of place, respectively.

The delta is added (in the case of *f) to
or subtracted (in the case of /f) from the number in place
and the result is stored in place.

Without delta -f will return the negation of the number in place,
+f will return the number in place.

Without delta-form the return type of +f will be
the type of the number in place, otherwise the return type will be float.
### push
    (push item place) -> new-place-value

push prepends item to the list that is stored in place,
stores the resulting list in place, and returns the list.
### pop
    (pop place) -> element

pop reads the value of place, remembers the car of the list which
was retrieved, writes the cdr of the list back into the place,
and finally yields the car of the originally retrieved list.
### acons
    (acons key datum alist) -> new-alist

Prepends alist with a new (key . datum) tuple
and returns the modified list.
### not
    (not form) -> boolean

Logical not.
### and
    (and forms*) -> boolean

Short-circuiting logical and.
Return T unless any of the forms evaluate to NIL,
NIL otherwise.
### or
    (or forms*) -> result

Short-circuiting logical or.
Return NIL unless any of the forms evaluate to non-NIL,
the result of the first form returning non-NIL otherwise.
### abs
    (abs n) -> result

Return the absoute value of a number.
### zerop
    (zerop number) -> boolean

Is this number zero?
### evenp
    (evenp number) -> boolean

Is this number even?
### oddp
    (oddp number) -> boolean

Is this number odd?
### char=
    (char= characters+) -> boolean

Return t if all of the arguments are the same character
### char
    (char string n) -> nth-character
### equal
    (equal x y) -> boolean

Return t if any of the following is true
a and b are eql
a and b are strings, characters or symbols and have the same text value
a and b are conses whose car and cdr are equal respectively
### prog1, prog2
    (prog1 first-form forms*) -> result-1
    (prog2 first-form second-form forms*) -> result-2
### when
    (when condition forms*) -> result

Execute forms if condition evaluates to true
and return the result of the last form if any
Orherwise f condition evaluates to false,
the forms are not evaluated and the return value
of the when-form is nil.
### unless
    (unless condition forms*) -> result

Execute forms if condition evaluates to false
and return the result of the last form if any
Orherwise f condition evaluates to true,
the forms are not evaluated and the return value
of the unless-form is nil.
### do, do*
    (do ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result
    (do* ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result

do and do* iterate over a group of statements while "end-test-form" returns nil.
### dotimes
    (dotimes (var count-form [result-form]) statement*) -> result

Similar to CL dotimes http://clhs.lisp.se/Body/m_dotime.htm
### dolist
    (dolist (var list-form [result-form]) statement*) -> result

Similar to CL dolist http://clhs.lisp.se/Body/m_dolist.htm
### identity
    (identity object) -> object

Returns its argument object.
### constantly
    (constantly value) -> function

constantly returns a function that accepts any number of arguments,
that has no side-effects, and that always returns value. 
### complement
    (complement function) -> complement-function

complement returns a function that takes the same arguments as function,
and has the same side-effect behavior as function, but returns only
a single value: a boolean with the opposite truth value of that
which would be returned as the value of function.
### member
    (member item list [test]) -> tail

member searches list for item or for a top-level element that
satisfies the test.

"test" if given must be a function that takes to arguments.
If "test" was omitted or nil then "eql" will be used.

Example usage:

    (member 2 '(1 2 3))
        ; => (2 3)
    (member 'e '(a b c d))
        ; => NIL
    (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3)) equal)
        ; => ((1 . 1) (2 . 2) (3 . 3))
    (member 'c '(a b c 1 2 3) eq)
        ; => (c 1 2 3)
    (member 'b '(a b c 1 2 3) (lambda (a b) (eq a b)))
        ; => (b c 1 2 3)
### reverse
    (reverse sequence) -> reversed-sequence

If sequence is a list then return a fresh list
with elements in reversed order, if sequence
is a string then return a fresh reversed string.
### mapcar
    (mapcar function sequence+) -> list

"Function" must accept as many arguments as sequences are given,
and will applied to subsequent items of the given sequences.
All function application results will be combined into a list
which is the return value of mapcar.
### maplist
    (maplist function sequence+) -> list

"Function" must accept as many arguments as sequences are given,
and will applied to subsequent tails of the given sequences.

All function application results will be combined into a list
which is the return value of maplist.
### mapc
    (mapc function sequence+) -> first-arg

"Function" must accept as many arguments as sequences are given,
and will applied to subsequent cars items of the given sequences.
### mapl
    (mapl function sequence+) -> first-arg

"Function" must accept as many arguments as sequences are given,
and will applied to subsequent tails of the given sequences.
### mapcan
    (mapcan function sequence+) -> concatenated-results

"Function" must accept as many arguments as sequences are given,
and will applied to subsequent items of the given sequences.

All function application results will be concatenated to a list
which is the return value of mapcan.
### mapcon
    (mapcon function sequence+) -> concatenated-results

"Function" must accept as many arguments as sequences are given,
and will applied to subsequent tails of the given sequences.

All function application results will be concatenated to a list
which is the return value of mapcon.
### every
    (every function sequence+) -> boolean

"Function" must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

Immediately return nil if an application of function returns nil,
t otherwise.
### some
    (some function sequence+) -> result

"Function" must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

Immediately return the first non-nil-value of an application of function,
or nil if no applications yield non-nil.
### notevery
    (notevery function sequence+) -> boolean

"Function" must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

(notevery predicate sequence+) == (not (every predicate sequence+))
### notany
    (notany function sequence+) -> boolean

"Function" must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

(notany predicate sequence+) == (not (some predicate sequence+))
### remove-if
    (remove-if pred list) -> list

Return a fresh list without the elements for which pred
evaluates to non-nil.
### remove
    (remove elem list) -> list

Return a fresh list without occurrences of elem.
An occurrence is determined by eql.
### reduce
    (reduce func sequence [from-end-p]) -> result

If sequence is empty then "reduce" will return (f).

Otherwise if sequence contains one element then "reduce will
return this element.

Otherwise if from-end is omitted or nil then
f will be called with the first two elements
of the sequence and subsequently with the previous result
and the next element, and "reduce" will return the last
result from f.

Otherwise if from-end is given and non-nil then
f will be called with the last two elements
of the sequence and subsequently with the previous result
and the previous element, and "reduce" will return the last
result from f.
### write-char
### terpri, prin1, princ, print
### pprint
    (pprint object) -> t

Simple pretty printer,
based on https://picolisp.com/wiki/?prettyPrint .
### list-length
    (list-length list-or-string) -> length

Returns the length of list-or-string if it is a string or proper list.
Returns nil if list is a circular list.

See http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html
### length
    (length sequence) -> length

Same as list-length.
### with-gensyms
    (with-gensyms (names*) forms*) -> result

"with-gensyms" is a macro commonly used by Common Lispers
to help with avoiding name capture when writing macros.
See "Practical Common Lisp, Peter Seibel"
(http://www.gigamonkeys.com/book/macros-defining-your-own.html)
### ->
    (-> forms*) -> result

thread-first, inspired by https://github.com/amirgamil/lispy/blob/master/lib/library.lpy

Inserts first form as the first argument of the second form, and so forth.

Usage is illustrated by:

    (macroexpand-1 '(-> 1 f g h))
      ; ==> (h (g (f 1)))
    (macroexpand-1 '(-> 1 (f farg) (g garg) (h harg)))
      ; ==> (h (g (f 1 farg) garg) harg)

### ->>
    (->> forms*) -> result

thread-last

Same as -> but inserts first form as last argument of the second form, and so forth.

Usage is illustrated by:

    (macroexpand-1 '(->> 1 f g h))
      ; ==> (h (g (f 1)))
    (macroexpand-1 '(->> 1 (f farg) (g garg) (h harg)))
      ; ==> (h harg (g garg (f farg 1)))

### and->
    (and-> forms*) -> result

Short-circuiting thread-first

Same as -> but if one function returns nil then the remaining
functions are not called and the overall result is nil.

### and->>
    (and->> forms*) -> result

Short circuiting thread-last

Same as ->> but if one function returns nil then the remaining
functions are not called and the overall result is nil.

