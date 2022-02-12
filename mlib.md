# Mlib - Default library for Murmel

mlib adds commonly used Lisp functions and macros to the
[core Murmel language (see murmel-langref.md)](murmel-langref.md).

Most of mlib's functions and macros are modeled after Common Lisp,
(often with reduced functionality) plus some additional macros and functions.

### Usage:

Copy `mlib.lisp` into the directory containing `jmurmel.jar`
or into the directory specified with `--libdir`
and begin your source file with

    (require "mlib")

### Provides:

mlib provides the following Common Lisp-like functions and macros:

- [caar..cdddr](#function-caarcdddr), [nthcdr, nth](#function-nthcdr-nth)
- [destructuring-bind](#macro-destructuring-bind)
- [get-setf-expansion](#function-get-setf-expansion)
- [setf](#macro-setf), [incf, decf](#macro-incf-decf)
- [push](#macro-push), [pop](#macro-pop)
- [acons](#function-acons)
- [not](#function-not), [and](#macro-and), [or](#macro-or)
- [abs](#function-abs), [zerop](#function-zerop), [evenp](#function-evenp), [oddp](#function-oddp)
- [char=](#function-char), [char](#function-char-1)
- [equal](#function-equal)
- [prog1, prog2](#macro-prog1-prog2)
- [when](#macro-when), [unless](#macro-unless), [case](#macro-case), [do, do*](#macro-do-do), [dotimes](#macro-dotimes), [dolist](#macro-dolist)
- [identity](#function-identity), [constantly](#function-constantly), [complement](#function-complement)
- [member](#function-member), [reverse](#function-reverse)
- [map-into](#function-map-into), [mapcar](#function-mapcar), [maplist](#function-maplist), [mapc](#function-mapc), [mapl](#function-mapl), [mapcan](#function-mapcan), [mapcon](#function-mapcon)
- [every](#function-every), [some](#function-some), [notevery](#function-notevery), [notany](#function-notany)
- [remove-if](#function-remove-if), [remove](#function-remove)
- [reduce](#function-reduce)
- [write-char](#function-write-char)
- [terpri, prin1, princ, print](#function-terpri-prin1-princ-print), [pprint](#function-pprint)
- [list-length](#function-list-length), [length](#function-length)
- [time](#macro-time)

functions and macros inspired by [Alexandria](https://alexandria.common-lisp.dev):

- [compose](#function-compose)
- [curry](#function-curry), [rcurry](#function-rcurry)
- [with-gensyms](#macro-with-gensyms)

functions inspired by [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html)

- [unzip](#function-unzip)

as well as the following additional functions and macros:

- [*f, /f, +f, -f](#macro-f-f)
- [->](#macro), [->>](#macro-1), [and->](#macro-and-1), [and->>](#macro-and-2)
### Function: caar..cdddr
    (c..r lst) -> result

`c..r` repeatedly apply `car` and/ or `cdr` as the name suggests.
### Function: nthcdr, nth
    (nthcdr n lst) -> nth-tail
    (nth n lst) -> nth-element

`nthcdr` applies `cdr` n times and returns the result.
`nth` works as if `(car (nthcdr n lst))` was invoked.


### Macro: destructuring-bind
    (destructuring-bind (vars*) (expressions*) forms*)

Murmel's `destructuring-bind` is a subset of CL's `destructuring-bind`,
trees are not supported, only lists are.

`destructuring-bind` binds the variables specified in `vars`
to the corresponding values in the list resulting from the evaluation
of `expressions`; then `destructuring-bind` evaluates `forms`. 
### Function: get-setf-expansion
    (get-setf-expansion place) -> vars, vals, store-vars, writer-form, reader-form
### Macro: setf
    (setf pair*) -> result

Takes pairs of arguments like `setq`. The first is a place and the second
is the value that is supposed to go into that place. Returns the last
value. The place argument may be any of the access forms for which `setf`
knows a corresponding setting form, which currently are:

- symbols
- car..cdddr
- nth
### Macro: incf, decf
    (incf place delta-form*) -> new-value
    (decf place delta-form*) -> new-value

`incf` and `decf` are used for incrementing and decrementing
the value of `place`, respectively.

The delta is added to (in the case of `incf`) or subtracted
from (in the case of `decf`) the number in `place` and the result
is stored in `place`.

Without `delta-form` the return type of `incf` and `decf` will be
the type of the number in `place`, otherwise the return type will be float.
### Macro: *f, /f
    (*f place delta-form*) -> new-value
    (/f place delta-form*) -> new-value

`*f` and `/f` are used for multiplying and dividing
the value of `place`, respectively.

The number in `place` is multiplied (in the case of `*f`) by delta
or divided (in the case of `/f`) by delta and the result
is stored in `place`.

Without `delta-form` `/f` will return the reciprocal of the number in `place`,
`*f` will return the number in `place`.

Without `delta-form` the return type of `*f` will be
the type of the number in `place`, otherwise the return type will be float.
### Macro: +f, -f
    (+f place delta-form*) -> new-value
    (-f place delta-form*) -> new-value

`+f` and `+f` are used for adding and subtracting
to/ from the value of `place`, respectively.

The delta is added (in the case of `*f`) to
or subtracted (in the case of `/f`) from the number in `place`
and the result is stored in `place`.

Without `delta-form` `-f` will return the negation of the number in `place`,
`+f` will return the number in `place`.

Without `delta-form` the return type of `+f` will be
the type of the number in `place`, otherwise the return type will be float.
### Macro: push
    (push item place) -> new-place-value

`push` prepends `item` to the list that is stored in `place`,
stores the resulting list in `place`, and returns the list.
### Macro: pop
    (pop place) -> element

`pop` reads the value of `place`, remembers the car of the list which
was retrieved, writes the cdr of the list back into the `place`,
and finally yields the car of the originally retrieved list.
### Function: acons
    (acons key datum alist) -> new-alist

Prepends `alist` with a new `(key . datum)` tuple
and returns the modified list.
### Function: not
    (not form) -> boolean

Logical not.
### Macro: and
    (and forms*) -> boolean

Short-circuiting logical and.
Return `t` unless any of the `forms` evaluate to `nil`,
`nil` otherwise.
### Macro: or
    (or forms*) -> result

Short-circuiting logical or.
Return `nil` unless any of the `forms` evaluate to non-nil,
the result of the first form returning non-nil otherwise.
### Function: abs
    (abs n) -> result

Return the absoute value of a number.
### Function: zerop
    (zerop number) -> boolean

Is this number zero?
### Function: evenp
    (evenp number) -> boolean

Is this number even?
### Function: oddp
    (oddp number) -> boolean

Is this number odd?
### Function: char=
    (char= characters+) -> boolean

Return `t` if all of the arguments are the same character.
### Function: char
    (char string n) -> nth-character

Return the n-th character of the string `string`, `n` is 0-based.
### Function: equal
    (equal a b) -> boolean

Return `t` if any of the following is true:

- `a` and `b` are `eql`
- `a` and `b` are strings, characters or symbols and have the same text value
- `a` and `b` are conses whose car and cdr are `equal` respectively
### Macro: prog1, prog2
    (prog1 first-form forms*) -> result-1
    (prog2 first-form second-form forms*) -> result-2
### Macro: when
    (when condition forms*) -> result

Execute `forms` if `condition` evaluates to true
and return the result of the last form if any.
Otherwise if `condition` evaluates to false,
the forms are not evaluated and the return value
of the `when`-form is `nil`.
### Macro: unless
    (unless condition forms*) -> result

Execute `forms` if `condition` evaluates to false
and return the result of the last form if any.
Otherwise if `condition` evaluates to true,
the forms are not evaluated and the return value
of the `unless`-form is `nil`.
### Macro: case
     (case keyform (keys forms*)* (t forms*)?) -> result

`keys` can be a single key or a list of keys, keys will not be evaluated.
`keyform` will be matched against `keys` using `eql`, the `forms` of the
matching clause will be eval'd and the last form determines the result.
Subsequent clauses will be ignored.

A clause with a key that is a single `t` is used as the default clause
if no key matches.
### Macro: do, do*
    (do ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result
    (do* ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) statement*) -> result

`do` and `do*` iterate over a group of statements while `end-test-form` returns `nil`.
### Macro: dotimes
    (dotimes (var count-form [result-form]) statement*) -> result

Similar to CL `dotimes`, see http://clhs.lisp.se/Body/m_dotime.htm
### Macro: dolist
    (dolist (var list-form [result-form]) statement*) -> result

Similar to CL `dolist`, see http://clhs.lisp.se/Body/m_dolist.htm
### Function: identity
    (identity object) -> object

`identity` returns its argument `object`.
### Function: constantly
    (constantly value) -> function

`constantly` returns a function that accepts any number of arguments,
that has no side-effects, and that always returns `value`.
### Function: complement
    (complement function) -> complement-function

`complement` returns a function that takes the same arguments as `function`,
and has the same side-effect behavior as `function`, but returns only
a single value: a boolean with the opposite truth value of that
which would be returned as the value of `function`.
### Function: member
    (member item list [test]) -> tail

`member` searches list for `item` or for a top-level element that
satisfies the `test`.

`test` if given must be a function that takes two arguments.
If `test` was omitted or `nil` then `eql` will be used.

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
### Function: reverse
    (reverse sequence) -> reversed-sequence

If `sequence` is a list then return a fresh list
with elements in reversed order, if `sequence`
is a string then return a fresh reversed string.
### Function: unzip
    (unzip lst) -> result-list

`unzip` takes a list of lists, and returns a list
containing the initial element of each such list,
e.g.:

    (unzip '((1 2) (11 22) (111 222))) ; ==> (1 11 111)
    (unzip '(nil nil nil)) ; ==> (nil nil nil)
    (unzip nil) ; ==> nil

Similar to SRFI-1 `unzip1`, see https://srfi.schemers.org/srfi-1/srfi-1.html#unzip1.
### Function: map-into
    (map-into result-list function list*) -> result-list

Destructively modifies `result-list` to contain the results
of applying `function` to each element in the argument lists in turn.
The iteration terminates when the shortest list (of any of
the lists or the result-list) is exhausted.

If `result-list` is `nil`, `map-into` returns `nil`.

Similar to CL `map-into`, see http://clhs.lisp.se/Body/f_map_in.htm.
### Function: mapcar
    (mapcar function sequence+) -> list

`function` must accept as many arguments as sequences are given,
and will applied to subsequent items of the given sequences.
All `function` application results will be combined into a list
which is the return value of `mapcar`.
### Function: maplist
    (maplist function sequence+) -> list

`function` must accept as many arguments as sequences are given,
and will applied to subsequent tails of the given sequences.

All `function` application results will be combined into a list
which is the return value of `maplist`.
### Function: mapc
    (mapc function sequence+) -> first-arg

`function` must accept as many arguments as sequences are given,
and will applied to subsequent cars items of the given sequences.
### Function: mapl
    (mapl function sequence+) -> first-arg

`function` must accept as many arguments as sequences are given,
and will applied to subsequent tails of the given sequences.
### Function: mapcan
    (mapcan function sequence+) -> concatenated-results

`function` must accept as many arguments as sequences are given,
and will applied to subsequent items of the given sequences.

All function application results will be concatenated to a list
which is the return value of `mapcan`.
### Function: mapcon
    (mapcon function sequence+) -> concatenated-results

`function` must accept as many arguments as sequences are given,
and will applied to subsequent tails of the given sequences.

All function application results will be concatenated to a list
which is the return value of `mapcon`.
### Function: every
    (every function sequence+) -> boolean

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

Immediately return `nil` if an application of function returns `nil`,
`t` otherwise.
### Function: some
    (some function sequence+) -> result

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

Immediately return the first non-nil-value of an application of `function`,
or `nil` if no applications yield non-nil.
### Function: notevery
    (notevery function sequence+) -> boolean

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

    (notevery predicate sequence+) == (not (every predicate sequence+))
### Function: notany
    (notany function sequence+) -> boolean

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

    (notany predicate sequence+) == (not (some predicate sequence+))
### Function: remove-if
    (remove-if pred list) -> list

Return a fresh list without the elements for which `pred`
evaluates to non-nil.
### Function: remove
    (remove elem list) -> list

Return a fresh list without occurrences of `elem`.
An occurrence is determined by `eql`.
### Function: reduce
    (reduce func sequence [from-end-p]) -> result

If `sequence` is empty then `reduce` will return `(func)`.

Otherwise if `sequence` contains one element then `reduce` will
return this element.

Otherwise if `from-end-p` is omitted or `nil` then
`func` will be called with the first two elements
of the `sequence` and subsequently with the previous result
and the next element, and `reduce` will return the last
result from `func`.

Otherwise if `from-end-p` is given and non-nil then
`func` will be called with the last two elements
of the `sequence` and subsequently with the previous result
and the previous element, and `reduce` will return the last
result from `func`.
### Function: write-char
    (write-char c) -> c

`write-char` outputs `c` to stdout.
### Function: terpri, prin1, princ, print
### Function: pprint
    (pprint object) -> t

Simple pretty printer,
based on https://picolisp.com/wiki/?prettyPrint .
### Function: list-length
    (list-length list-or-string) -> length

Returns the length of `list-or-string` if it is a string or proper list.
Returns `nil` if `list.or-string` is a circular list.

See http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html
### Function: length
    (length sequence) -> length

Same as `list-length`.
### Macro: time
    (time form) -> result

`time` evaluates `form` and prints various timing data.
### Function: compose
    (compose func1 funcs*) -> function

Returns a function that composes the given functions, applying the last function first
and the first function last. The compose function allows the last function to consume
any number of values, internal value passing is a single value.

The input arity of the last function is unrestricted, and it becomes the corresponding arity
of the resulting composition.

When exactly one function is given, it is returned.
### Function: curry
    (curry func args*) -> function

Returns a function that applies `args` and the arguments it is called with to `func`.
### Function: rcurry
    (rcurry func args*) -> function

Returns a function that applies the arguments it is called with and `args` to `func`.
### Macro: with-gensyms
    (with-gensyms (names*) forms*) -> result

`with-gensyms` is a macro commonly used by Common Lispers
to help with avoiding name capture when writing macros.
See "Practical Common Lisp, Peter Seibel"
(http://www.gigamonkeys.com/book/macros-defining-your-own.html)
### Macro: ->
    (-> forms*) -> result

thread-first, inspired by https://github.com/amirgamil/lispy/blob/master/lib/library.lpy

Inserts first form as the first argument of the second form, and so forth.

Usage is illustrated by:

    (macroexpand-1 '(-> 1 f g h))
      ; ==> (h (g (f 1)))
    (macroexpand-1 '(-> 1 (f farg) (g garg) (h harg)))
      ; ==> (h (g (f 1 farg) garg) harg)

### Macro: ->>
    (->> forms*) -> result

thread-last

Same as `->` but inserts first form as last argument of the second form, and so forth.

Usage is illustrated by:

    (macroexpand-1 '(->> 1 f g h))
      ; ==> (h (g (f 1)))
    (macroexpand-1 '(->> 1 (f farg) (g garg) (h harg)))
      ; ==> (h harg (g garg (f farg 1)))

### Macro: and->
    (and-> forms*) -> result

Short-circuiting thread-first

Same as `->` but if one function returns `nil` then the remaining
functions are not called and the overall result is `nil`.

### Macro: and->>
    (and->> forms*) -> result

Short circuiting thread-last

Same as `->>` but if one function returns nil then the remaining
functions are not called and the overall result is `nil`.

