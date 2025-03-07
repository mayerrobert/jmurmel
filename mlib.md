
# Mlib - Default library for Murmel

mlib adds commonly used Lisp functions and macros to the
[core Murmel language (see murmel-langref.md)](murmel-langref.md).

Most of mlib's functions and macros are modeled after Common Lisp,
(often with reduced functionality) plus some additional macros and functions.

## Usage

Copy `mlib.lisp` into the directory containing `jmurmel.jar`
or into the directory specified with `--libdir`
and begin your source file with

    (require "mlib")

## mlib functions and macros

mlib provides the following Common Lisp-like functions and macros:

- logic, program structure
    - [when](#macro-when), [unless](#macro-unless)
    - [not](#function-not), [and](#macro-and), [or](#macro-or)
    - [prog1, prog2](#macro-prog1-prog2)
    - [case](#macro-case), [typecase](#macro-typecase)
- conses and lists
    - [caar..cdddr](#function-caarcdddr), [nthcdr, dotted-nthcdr, nth](#function-nthcdr-dotted-nthcdr-nth), [endp](#function-endp)
    - [copy-list](#function-copy-list), [copy-alist](#function-copy-alist), [copy-tree](#function-copy-tree)
    - [list-length](#function-list-length), [last](#function-last), [butlast](#function-butlast), [nbutlast](#function-nbutlast), [ldiff](#function-ldiff), [tailp](#function-tailp)
    - [subst](#function-subst), [subst-if](#function-subst-if), [nsubst](#function-nsubst), [nsubst-if](#function-nsubst-if)
    - [nconc](#function-nconc), [revappend, nreconc](#function-revappend-nreconc), [member](#function-member), [adjoin](#function-adjoin)
    - [acons](#function-acons)
    - [mapcar](#function-mapcar), [maplist](#function-maplist), [mapc](#function-mapc), [mapl](#function-mapl), [mapcan](#function-mapcan), [mapcon](#function-mapcon)
    - [multiple-value-list](#macro-multiple-value-list), [nth-value](#macro-nth-value)
- iteration
    - [do, do*](#macro-do-do), [dotimes](#macro-dotimes), [dolist](#macro-dolist)
- places
    - [destructuring-bind](#macro-destructuring-bind)
    - [get-setf-expansion](#function-get-setf-expansion)
    - [setf](#macro-setf), [psetf](#macro-psetf), [shiftf](#macro-shiftf), [rotatef](#macro-rotatef)
    - [incf, decf](#macro-incf-decf)
    - [push](#macro-push), [pop](#macro-pop), [pushnew](#macro-pushnew)
- numbers, characters
    - [abs](#function-abs), [min](#function-min), [max](#function-max), [zerop](#function-zerop), [evenp](#function-evenp), [oddp](#function-oddp)
    - [char=](#function-char), [char](#function-char-1), [bit](#function-bit)
    - [parse](#function-parse), [parse-integer](#function-parse-integer)
- sequences
    - [elt](#function-elt), [copy-seq](#function-copy-seq), [length](#function-length)
    - [reverse](#function-reverse), [nreverse](#function-nreverse)
    - [remove-if](#function-remove-if), [remove](#function-remove)
    - [concatenate](#function-concatenate)
    - [map](#function-map), [map-into](#function-map-into), [reduce](#function-reduce)
- hash tables
    - [gethash](#function-gethash), [remhash](#function-remhash), [maphash](#function-maphash)
- higher order
    - [identity](#function-identity), [constantly](#function-constantly), [complement](#function-complement)
    - [every](#function-every), [some](#function-some), [notevery](#function-notevery), [notany](#function-notany)
- I/O
    - [write-char](#function-write-char)
    - [terpri, prin1, princ, print](#function-terpri-prin1-princ-print), [pprint](#function-pprint)
    - [format](#function-format), [formatter](#macro-formatter)
    - [error](#function-error)
    - [with-output-to-string](#macro-with-output-to-string)
- misc
    - [time](#macro-time)

functions and macros inspired by [Alexandria](https://alexandria.common-lisp.dev):

- conses and lists
    - [circular-list](#function-circular-list)
    - [mappend](#function-mappend), [mappend-tails](#function-mappend-tails)
- iteration
    - [doplist](#macro-doplist)
- higher order
    - [compose](#function-compose), [multiple-value-compose](#function-multiple-value-compose)
    - [conjoin](#function-conjoin), [disjoin](#function-disjoin)
    - [curry](#function-curry), [rcurry](#function-rcurry)
- misc
    - [with-gensyms](#macro-with-gensyms)

functions inspired by [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html)

- conses and lists
    - [unzip](#function-unzip)

functions and macros inspired by [serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md)

- conses and lists
    - [plist-keys](#function-plist-keys), [plist-values](#function-plist-values)
- misc
    - [with-accumulator](#macro-with-accumulator), [summing](#macro-summing), [collecting](#macro-collecting), [reverse-collecting](#macro-reverse-collecting)

as well as the following additional functions and macros:

- logic and program structure
    - [->](#macro), [->>](#macro-1), [and->](#macro-and-1), [and->>](#macro-and-2)
- conses and lists
    - [unzip-tails](#function-unzip-tails)
- iteration
    - [dovector](#macro-dovector), [dogenerator](#macro-dogenerator)
- places
    - [*f, /f, +f, -f](#macro-f-f)
- generators
    - [scan](#function-scan), [scan-multiple](#function-scan-multiple), [scan-concat](#function-scan-concat)
- strings
    - [string-trim](#function-string-trim), [string-subseq](#function-string-subseq), [string-replace](#function-string-replace), [string-split](#function-string-split), [string-join](#function-string-join)
- hash tables
    - [frequencies](#function-frequencies)

## Description of functions and macros

### Macro: when
    (when condition forms*) -> result

Since: 1.1

Execute `forms` if `condition` evaluates to true
and return the result of the last form if any.
Otherwise if `condition` evaluates to false,
the forms are not evaluated and the return value
of the `when`-form is `nil`.

### Macro: unless
    (unless condition forms*) -> result

Since: 1.1

Execute `forms` if `condition` evaluates to false
and return the result of the last form if any.
Otherwise if `condition` evaluates to true,
the forms are not evaluated and the return value
of the `unless`-form is `nil`.

### Function: not
    (not form) -> boolean

Since: 1.1

Logical not.

### Macro: and
    (and forms*) -> result
    (and* forms*) -> boolean

Since: 1.1

Short-circuiting logical and.
Return `t` if no forms were given,
otherwise return the values resulting from the evaluation of the last form unless any of the `forms` evaluate to `nil`,
`nil` otherwise,
or in case of `and*`: `t` or `nil` as appropriate.

### Macro: or
    (or forms*) -> result
    (or* forms*) -> boolean

Since: 1.1

Short-circuiting logical or.
Return `nil` unless any of the `forms` evaluate to non-nil,
the result of the first form returning non-nil otherwise,
or in case of `or*`: `t` or `nil` as appropriate.

### Macro: prog1, prog2
    (prog1 first-form more-forms*) -> result-1
    (prog2 first-form second-form more-forms*) -> result-2

Since: 1.1

### Macro: case
     (case keyform (keys forms*)* [(t forms*)]) -> result

Since: 1.1

`keys` can be a single key or a list of keys, keys will not be evaluated.
`keyform` will be matched against `keys` using `eql`, the `forms` of the
matching clause will be eval'd and the last form determines the result.
Subsequent clauses will be ignored.

A clause with a key that is a single `t` is used as the default clause
if no key matches.

### Macro: typecase
     (typecase keyform (type forms*)* [(t forms*)]) -> result

Since: 1.3

`typecase` allows the conditional execution of a body of forms in a clause
that is selected by matching the test-key on the basis of its type.

The keyform is evaluated to produce the test-key.

Each of the normal-clauses is then considered in turn.
If the test-key is of the type given by the clauses's type,
the forms in that clause are evaluated as an implicit progn,
and the values it returns are returned as the value of the typecase form.

If no normal-clause matches, and there is an otherwise-clause,
then that otherwise-clause automatically matches;
the forms in that clause are evaluated as an implicit progn,
and the values it returns are returned as the value of the typecase.

If there is no otherwise-clause, typecase returns nil.

### Function: caar..cdddr
    (c..r lst) -> result

Since: 1.1

`c..r` repeatedly apply `car` and/ or `cdr` as the name suggests.

### Function: endp
    (endp list) -> boolean

Since: 1.4.6

This is the recommended way to test for the end of a proper list. It
returns true if `obj` is `nil`, false if `obj` is a `cons`,
and a `type-error` for any other type of `object`.

### Function: nthcdr, dotted-nthcdr, nth
    (nthcdr n lst) -> nth-tail
    (dotted-nthcdr n lst) -> nth-tail
    (nth n lst) -> nth-element

Since: 1.1

`nthcdr` applies `cdr` n times and returns the result.
`dotted-nthcdr` works similar to `nthcdr` except:
going past the end of a dotted list returns `nil`
(and not an error as `nthcdr` would).
`nth` works as if `(car (nthcdr n lst))` was invoked.

### Function: copy-list

    (copy-list lst) -> copy

Since: 1.3

Returns a copy of `lst`. If `lst` is a dotted list,
the resulting list will also be a dotted list.

Only the list structure of `lst` is copied;
the elements of the resulting list are the same
as the corresponding elements of the given list.

### Function: unzip
    (unzip lists) -> result-list

Since: 1.2

`unzip` takes a list of lists, and returns a list
containing the initial element of each such list,
e.g.:

    (unzip '((1 2) (11 22) (111 222))) ; ==> (1 11 111)
    (unzip '(nil nil nil)) ; ==> (nil nil nil)
    (unzip nil) ; ==> nil

Similar to SRFI-1 `unzip1`, see https://srfi.schemers.org/srfi-1/srfi-1.html#unzip1.

See also: [unzip-tails](#function-unzip-tails).

### Function: unzip-tails
    (unzip-tails lists) -> result-list

Since: 1.2

`unzip-tails` takes a list of lists, and returns a list
containing the `cdr`s of each such list.

See also: [unzip](#function-unzip).

### Function: list-length
    (list-length list) -> length

Since: 1.1

Returns the length of `list` if it is a string or proper list.
Returns `nil` if `list-or-string` is a circular list.

### Function: last
    (last lst [n]) -> tail

Since: 1.2

`last` returns the last `n` conses (not the last `n` elements)
of a proper or dotted list or `nil` for the empty list.

If `n` is zero, the atom that terminates list is returned.
If `n` is greater than or equal to the number of cons cells in list,
the result is `lst`.

### Function: nconc
    (nconc lists*) -> concatenated-list

Since: 1.2

`nconc` concatenates lists, each list but the last is modified.
If no lists are supplied, `nconc` returns `nil`.
Each argument but the last must be a proper or dotted list.

### Function: revappend, nreconc
    (revappend list tail) -> result-list
    (nreconc list tail) -> result-list

Since: 1.3

`revappend` constructs a copy of `list`, but with the elements in reverse order.
It then appends (as if by `nconc`) the `tail` to that reversed list and returns the result.

`nreconc` reverses the order of elements in list (as if by `nreverse`).
It then appends (as if by `nconc`) the tail to that reversed list and returns the result.

The resulting list shares list structure with tail.

    (revappend x y)  ::=  (append (reverse x) y)
    (nreconc x y)    ::=  (nconc (nreverse x) y)

### Function: member
    (member item list [test]) -> tail

Since: 1.1

`member` searches list for `item` or for a top-level element that
satisfies the `test`.

`test` if given must be a function that takes two arguments.
If `test` was omitted or `nil` then `eql` will be used.

Example usage:

    (member 2 '(1 2 3))
        ; => (2 3)
    (member 'e '(a b c d))
        ; => NIL
    (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3))
            equal)
        ; => ((1 . 1) (2 . 2) (3 . 3))
    (member 'c '(a b c 1 2 3) eq)
        ; => (c 1 2 3)
    (member 'b '(a b c 1 2 3) (lambda (a b) (eq a b)))
        ; => (b c 1 2 3)

### Function: adjoin
    (adjoin item list [test]) -> result-list

Since: 1.4.5

Tests whether `item` is the same as an existing element of `lst`.
If the `item` is not an existing element, `adjoin` adds it to `lst` (as if by `cons`)
and returns the resulting list; otherwise, nothing is added and the original list is returned.

### Function: acons
    (acons key datum alist) -> new-alist

Since: 1.1

Prepends `alist` with a new `(key . datum)` tuple
and returns the modified list.

### Function: mapcar
    (mapcar function list+) -> list

Since: 1.1

`function` must accept as many arguments as lists are given,
and will applied to subsequent items of the given lists.
All `function` application results will be combined into a list
which is the return value of `mapcar`.

### Function: maplist
    (maplist function list+) -> list

Since: 1.1

`function` must accept as many arguments as lists are given,
and will applied to subsequent tails of the given lists.

All `function` application results will be combined into a list
which is the return value of `maplist`.

### Function: mapc
    (mapc function list+) -> first-arg

Since: 1.1

`function` must accept as many arguments as lists are given,
and will applied to subsequent cars items of the given lists.

### Function: mapl
    (mapl function list+) -> first-arg

Since: 1.1

`function` must accept as many arguments as lists are given,
and will applied to subsequent tails of the given lists.

### Function: mapcan
    (mapcan function list+) -> concatenated-results

Since: 1.1

`function` must accept as many arguments as lists are given,
and will applied to subsequent items of the given lists.

All function application results will be concatenated (as if by nconc) to a list
which is the return value of `mapcan`.

### Function: mapcon
    (mapcon function list+) -> concatenated-results

Since: 1.1

`function` must accept as many arguments as lists are given,
and will applied to subsequent tails of the given lists.

All function application results will be concatenated (as if by nconc) to a list
which is the return value of `mapcon`.

### Function: mappend
    (mappend function list+) -> appended-results

Since: 1.4.7

`function` must accept as many arguments as lists are given,
and will applied to subsequent items of the given lists.

All function application results will be concatenated to a list
which is the return value of `mappend`.
`function` must return a list which will not be mutated by `mappend`.

`mappend` works similar to Alexandria's `mappend` and
can be thought of as a non-destructive version of `mapcan`,
i.e. `mappend` combines the results of applying `function`
by the use of `append` rather than `nconc`.

### Function: mappend-tails
    (mappend-tails function list+) -> appended-results

Since: 1.4.7

`function` must accept as many arguments as lists are given,
and will applied to subsequent tails of the given lists.

All function application results will be concatenated to a list
which is the return value of `mappend-tails`.
`function` must return a list which will not be mutated by `mappend-tails`.

`mappend-tails` can be thought of as a non-destructive version of `mapcon`,
i.e. `mappend-tails` combines the results of applying `function`
by the use of `append` rather than `nconc`.

### Macro: multiple-value-list

Since: 1.4

### Macro: nth-value

Since: 1.4

### Macro: do, do*
    (do ({var | (var [init-form [step-form]])}*)
        (end-test-form result-form*)
        statement*) -> result

    (do* ({var | (var [init-form [step-form]])}*)
         (end-test-form result-form*)
         statement*) -> result

Since: 1.1

`do` and `do*` iterate over a group of statements while `end-test-form` returns `nil`.

### Macro: dotimes
    (dotimes (var count-form result-form*) statement*) -> result

Since: 1.1

Similar to CL `dotimes`.
Murmel however supports multiple result-forms which will be eval'd in an
implicit `progn`, similar to `do` and `do*`;

Sample usage:

    (let (l)
      (dotimes (i 10 l)
        (push i l))) ; ==> (9 8 7 6 5 4 3 2 1 0)

### Macro: dolist
    (dolist (var list-form result-form*) statement*) -> result

Since: 1.1

Similar to CL `dolist`.
Murmel however supports multiple result-forms which will be eval'd in an
implicit `progn`, similar to `do` and `do*`;

### Macro: dovector
    (dovector (var vector-form result-form*) statement*) -> result

Since: 1.3

Just like `dolist`, but with vectors.

### Macro: doplist
    (doplist (key-var value-var plist-form result-form*)
      statement*) -> result

Since: 1.2

Iterates over key-value pairs of `plist-form`.
Similar to Alexandria `doplist`,
see https://alexandria.common-lisp.dev/draft/alexandria.html.

### Function: copy-alist
    (copy-alist alist) -> new-alist

Since: 1.4.6

`copy-alist` returns a copy of `alist`.

The list structure of `alist` is copied, and the elements of `alist` which are conses are also copied (as conses only).
Any other objects which are referred to, whether directly or indirectly, by the `alist` continue to be shared.

### Function: copy-tree
    (copy-tree tree) -> new-tree

Since: 1.4.6

Creates a copy of a tree of conses.

If `tree` is not a `cons`, it is returned;
otherwise, the result is a new cons of the results of calling `copy-tree` on the car and cdr of `tree`.
In other words, all conses in the tree represented by `tree` are copied recursively,
stopping only when non-conses are encountered.
copy-tree does not preserve circularities and the sharing of substructure.

### Function: butlast
    (butlast lst [n]) -> result-list

Since: 1.4.5

`butlast` returns a copy of `lst` from which the last `n` conses have been omitted.
If `n` is not supplied, its value is 1. If there are fewer than `n` conses in `lst`,
`nil` is returned.

### Function: nbutlast
    (nbutlast lst [n]) -> result-list

Since: 1.4.5

`nbutlast` is like `butlast`, but `nbutlast` may modify `lst`.
It changes the cdr of the cons n+1 from the end of `lst` to `nil` except
if there are fewer than `n` conses in `lst`, `nil` is returned
and `lst` is not modified.

### Function: ldiff
    (ldiff lst obj) -> result-list

Since: 1.4.5

Return a new list, whose elements are those of `lst` that appear before
`obj`. If `obj` is not a tail of `lst`, a copy of `lst` is returned.
`lst` must be a proper list or a dotted list.

### Function: tailp
    (tailp obj lst) -> boolean

Since: 1.4.5

 Return `true` if `obj` is the same as some tail of `lst`, otherwise
 returns `false`. `lst` must be a proper list or a dotted list.

### Function: subst
    (subst new old tree [test-fn [key-fn]]) -> new-tree

Since: 1.4.6

Substitutes `new` for subtrees of `tree` matching `old`.

### Function: subst-if
    (subst-if new test-pred tree [key-fn]) -> new-tree

Since: 1.4.6

Substitutes `new` for subtrees of `tree` for which `test-pred` is true.

### Function: nsubst
    (nsubst new old tree [test-fn [key-fn]]) -> new-tree

Since: 1.4.6

Substitutes `new` for subtrees of `tree` matching `old`.

### Function: nsubst-if
    (nsubst-if new test-pred tree [key-fn]) -> new-tree

Since: 1.4.6

Substitutes `new` for subtrees of `tree` for which `test-pred` is true.

### Macro: destructuring-bind
    (destructuring-bind (vars*) expression forms*)

Since: 1.1

Murmel's `destructuring-bind` is a subset of CL's `destructuring-bind`,
trees are not supported, only lists are.

`destructuring-bind` binds the variables specified in `vars`
to the corresponding values in the list resulting from the evaluation
of `expression`; then `destructuring-bind` evaluates `forms`.

### Function: get-setf-expansion
    (get-setf-expansion place) -> vars, vals, store-vars, writer-form, reader-form

Since: 1.1

### Macro: setf
    (setf pair*) -> result

Since: 1.1

Takes pairs of arguments like `setq`. The first is a place and the second
is the value that is supposed to go into that place. Returns the last
value. The place argument may be any of the access forms for which `setf`
knows a corresponding setting form, which currently are:

- symbols
- car..cdddr
- nth
- elt, seqref
- hashref, gethash
- svref, bvref, bit, sref, char
- values

### Macro: psetf
    (psetf pair*) -> nil

Since: 1.4.6

Takes pairs of arguments like `setf`. The first is a place and the second
is the value that is supposed to go into that place.

If more than one pair is supplied then the assignments of new values to places are done in parallel.

Similar to CL's `psetf`.

### Macro: shiftf
    (shiftf place+ newvalues) -> old-values-1

Since: 1.4.8

`shiftf` modifies the values of each place by storing newvalue into the last place,
and shifting the values of the second through the last place into the remaining places.

Similar to CL's `shiftf`.

### Macro: rotatef
    (rotatef place*) -> nil

Since: 1.4.8

`rotatef` modifies the values of each place by rotating values from one place into another.

If a place produces more values than there are store variables, the extra values are ignored.
If a place produces fewer values than there are store variables, the missing values are set to `nil`.

Similar to CL's `rotatef`.

### Macro: incf, decf
    (incf place [delta-form]) -> new-value
    (decf place [delta-form]) -> new-value

Since: 1.1

`incf` and `decf` are used for incrementing and decrementing
the value of `place`, respectively.

The delta is added to (in the case of `incf`) or subtracted
from (in the case of `decf`) the number in `place` and the result
is stored in `place`.

Without `delta-form` the return type of `incf` and `decf` will be
the type of the number in `place`, otherwise the return type will be float.

### Macro: *f, /f
    (*f place [delta-form]) -> new-value
    (/f place [delta-form]) -> new-value

Since: 1.1

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
    (+f place [delta-form]) -> new-value
    (-f place [delta-form]) -> new-value

Since: 1.1

`+f` and `+f` are used for adding and subtracting
to/ from the value of `place`, respectively.

The delta is added (in the case of `+f`) to
or subtracted (in the case of `-f`) from the number in `place`
and the result is stored in `place`.

Without `delta-form` `-f` will return the negation of the number in `place`,
`+f` will return the number in `place`.

Without `delta-form` the return type of `+f` will be
the type of the number in `place`, otherwise the return type will be float.

### Macro: push
    (push item place) -> new-place-value

Since: 1.1

`push` prepends `item` to the list that is stored in `place`,
stores the resulting list in `place`, and returns the list.

### Macro: pushnew
    (pushnew item place [test]) -> new-place-value

Since: 1.4.5

`pushnew` tests whether `item` is the same as any existing element of the list stored in `place`.
If `item` is not, it is prepended to the list, and the new list is stored in `place`.

`pushnew` returns the new list that is stored in `place`.

### Macro: pop
    (pop place) -> element

Since: 1.1

`pop` reads the value of `place`, remembers the car of the list which
was retrieved, writes the cdr of the list back into the `place`,
and finally yields the car of the originally retrieved list.

### Function: abs
    (abs number) -> result

Since: 1.1

Return the absoute value of a number.

### Function: min
    (min number+) -> result

Since: 1.4

Return the smallest number of the given arguments.

### Function: max
    (max number+) -> result

Since: 1.4

Return the largest number of the given arguments.

### Function: zerop
    (zerop number) -> boolean

Since: 1.1

Is this number zero?

### Function: evenp
    (evenp number) -> boolean

Since: 1.1

Is this number even?

### Function: oddp
    (oddp number) -> boolean

Since: 1.1

Is this number odd?

### Function: char=
    (char= characters+) -> boolean

Since: 1.1

Return `t` if all of the arguments are the same character.

### Function: char
    (char str n) -> nth-character

Since: 1.1

Return the n-th character of the string `str`, `n` is 0-based.

### Function: bit
    (bit bv n) -> nth bit

Since: 1.3

Return the n-th bit of the bitvector `bv`, `n` is 0-based.

### Function: parse
    (parse result-type str [eof-obj [start [end]]]) -> result

Since: 1.4

Reads the token in `str` starting at `start` (which defaults to `0`),
`parse-error` if the token is not of type `result-type`.

### Function: parse-integer
    (parse-integer str [start [end]]) -> result

Since: 1.4

Reads the token in `str` starting at `start` (which defaults to `0`),
`parse-error` if the token is not of type `integer`.

### Function: scan
    (scan start [step [endincl]])                 -> generator-function that returns subsequent numbers starting from `start` incrementing by `step` (default: 1)
    (scan seq-or-gen [start-idx [stop-idx-excl]]) -> generator-function that returns subsequent elements of the given sequence (list or vector) or generator
    (scan hash-table)                             -> generator-function that returns subsequent (key . value) pairs of the given hash-table

Since: 1.3

`scan` creates a generator function that on subsequent calls produces subsequent values.

`start-idx` and `stop-idx-excl` if given must be integer numbers >= 0, both are 0-based.

A generator function takes no arguments and on subsequent applications returns `(values <next-value> t)`
or `(values <undefined-value> nil)` to indicate "all values are exhausted".

### Function: scan-multiple
    (scan-multiple generator+) -> generator

Since: 1.3

`scan-multiple` combines several generators into a single generator function
that returns a list with subsequent values of all generators,
and whose secondary value is nil if any generator returns nil as their secondary value.
Once the first generator indicates "at end" for the first time no more generators will be called.

### Function: scan-concat
    (scan-concat generator+) -> generator

Since: 1.3

`scan-concat` combines several generators into a single generator function
that acts as if the given generators were concatenated.

A single generator would be returned unchanged.

### Macro: dogenerator
    (dogenerator (var generator-form result-form*) statement*) -> result

Since: 1.3

`dogenerator` creates a generator by eval'ing `generator-form`
and iterates over the values yielded by subsequent generator applications.

### Function: elt
    (elt sequence n) -> nth-element

Since: 1.3

Similar to CL `elt`, Murmel's `elt` handles dotted lists, though.

### Function: copy-seq
    (copy-seq sequence) -> copied-sequence

Since: 1.3

Creates a copy of `sequence`.
The elements of the new sequence are the same as the corresponding elements of the given sequence.

If `sequence` is a vector, the result is a fresh simple vector
that has the same actual array element type as `sequence`.
If `sequence` is a list, the result is a fresh list.

### Function: length
    (length sequence) -> length

Since: 1.1

Returns the length of `sequence`.

### Function: reverse
    (reverse sequence) -> reversed-sequence

Since: 1.1

If `sequence` is a list then return a fresh list
with elements in reversed order, if `sequence`
is a vector then return a fresh reversed vector.

### Function: nreverse
    (nreverse sequence) -> reversed-sequence

Since: 1.3

Similar to `reverse` `nreverse` returns a sequence with elements in reversed order.
`nreverse` however may or may not reuse/ destroy the input sequence.

### Function: remove-if
    (remove-if pred sequence) -> sequence

Since: 1.1

Return a fresh sequence without the elements for which `pred`
evaluates to non-nil.

### Function: remove
    (remove elem sequence) -> sequence

Since: 1.1

Return a fresh sequence without occurrences of `elem`.
An occurrence is determined by `eql`.

### Function: concatenate
    (concatenate result-type sequences*) -> result-sequence

Since 1.4.7

`concatenate` returns a sequence that contains all the individual elements
of all the sequences in the order that they are supplied.
The sequence is of type result-type, which must be a subtype of type sequence.

All of the sequences are copied from; the result does not share any structure
with any of the sequences.

### Function: map
    (map result-type function sequences+) -> result

Since 1.3

Applies `function` to successive sets of arguments in which one argument
is obtained from each sequence. The function is called first on all the elements
with index 0, then on all those with index 1, and so on.
The result-type specifies the type of the resulting sequence.

`map` returns `nil` if `result-type` is `nil`. Otherwise, `map` returns a sequence
such that element j is the result of applying `function` to element j of each
of the sequences. The result sequence is as long as the shortest of the sequences.

Similar to CL `map`.

### Function: map-into
    (map-into result-sequence function sequence*) -> result-sequence

Since: 1.2

Destructively modifies `result-sequence` to contain the results
of applying `function` to each element in the argument sequences in turn.
The iteration terminates when the shortest sequence (of any of
the sequences or the result-sequence) is exhausted.

If `result-sequence` is `nil`, `map-into` returns `nil`.

Similar to CL `map-into`.

### Function: reduce
    (reduce func sequence [from-end-p]) -> result

Since: 1.1

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

### Function: gethash
    (gethash key hash [default]) -> object, was-present-p

Since: 1.4

### Function: remhash
    (remhash key hash) -> was-present-p

Since: 1.4

### Function: maphash
    (maphash function hash) -> nil

Since: 1.4

Similar to CL's `maphash` but modifying the hash-table
from within `function` is not supported.

### Function: frequencies
    (frequencies sequence-or-generator [test]) -> hash-table

Since: 1.5

Count the number of times each value occurs in `sequence-or-generator`
according to the test function `test` which defaults to `eql`.

Sample usage:

    (frequencies ()) ; ==> nil
    (frequencies #(1 2 3 1 2 1)) ; ==> #H(eql 1 3 2 2 3 1)

### Function: identity
    (identity object) -> object

Since: 1.1

`identity` returns its argument `object`.

### Function: constantly
    (constantly value) -> function

Since: 1.1

`constantly` returns a function that accepts any number of arguments,
that has no side-effects, and that always returns `value`.

### Function: complement
    (complement function) -> complement-function

Since: 1.1

`complement` returns a function that takes the same arguments as `function`,
and has the same side-effect behavior as `function`, but returns only
a single value: a boolean with the opposite truth value of that
which would be returned as the value of `function`.

### Function: every
    (every function sequence+) -> boolean

Since: 1.1

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

Immediately return `nil` if an application of function returns `nil`,
`t` otherwise.

### Function: some
    (some function sequence+) -> result

Since: 1.1

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

Immediately return the first non-nil-value of an application of `function`,
or `nil` if no applications yield non-nil.

### Function: notevery
    (notevery function sequence+) -> boolean

Since: 1.1

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

    (notevery predicate sequence+) == (not (every predicate sequence+))

### Function: notany
    (notany function sequence+) -> boolean

Since: 1.1

`function` must accept as many arguments as sequences are given,
and will be applied to subsequent items of the given sequences.

    (notany predicate sequence+) == (not (some predicate sequence+))

### Function: write-char
    (write-char c [dest]) -> c

Since: 1.1

`write-char` outputs `c` to stdout.

### Function: terpri, prin1, princ, print
    (terpri [dest]) -> nil
    (prin1 obj [dest]) -> obj
    (princ obj [dest]) -> obj
    (print obj [dest]) -> obj

Since: 1.1

### Macro: with-output-to-string
    (with-output-to-string (var) forms*) -> string

Since: 1.4.2

Similar to CL's `with-output-to-string` except:
CL's optional `string-form` and `element-type` are not supported,
therefore the return value of `with-output-to-string` always is the string.

### Function: pprint
    (pprint object [dest]) -> t

Since: 1.1

Simple pretty printer,
based on https://picolisp.com/wiki/?prettyPrint .

### Macro: time
    (time form) -> result

Since: 1.1

`time` evaluates `form` and prints various timing data.

### Function: circular-list
    (circular-list elems*) -> circular-list

Since: 1.2

Creates a circular list of elements.

### Function: compose
    (compose func1 funcs*) -> function

Since: 1.2

Returns a function that composes the given functions, applying the last function first
and the first function last. The compose function allows the last function to consume
any number of values, internal value passing is a single value.

The input arity of the last function is unrestricted, and it becomes the corresponding arity
of the resulting composition.

When exactly one function is given, it is returned.

### Function: multiple-value-compose
    (multiple-value-compose func1 funcs*) -> function

Since: 1.2

Returns a function that composes the given functions, applying the last function first
and the first function last. The compose function allows the last function to consume
any number of values, internal value passing is all return values of the previous function.

The input arity of the last function is unrestricted, and it becomes the corresponding arity
of the resulting composition.

When exactly one function is given, it is returned.

### Function: conjoin
    (conjoin predicate more-predicates*) -> function

Since: 1.2

Returns a function that applies each of `predicate` and `more-predicates`
functions in turn to its arguments, returning `nil` if any of the predicates
returns false, without calling the remaining predicates. If none of the
predicates returns false, returns the value of the last predicate.

### Function: disjoin
    (disjoin predicate more-predicates*) -> function

Since: 1.2

Returns a function that applies each of `predicate` and `more-predicates`
functions in turn to its arguments, returning the value of the first
predicate that returns true, without calling the remaining predicates.
If none of the predicates returns true, `nil` is returned.

### Function: curry
    (curry func args*) -> function

Since: 1.2

Returns a function that applies `args` and the arguments it is called with to `func`.

### Function: rcurry
    (rcurry func args*) -> function

Since: 1.2

Returns a function that applies the arguments it is called with and `args` to `func`.

### Macro: with-gensyms
    (with-gensyms (names*) forms*) -> result

Since: 1.1

`with-gensyms` is a macro commonly used by Common Lispers
to help with avoiding name capture when writing macros.
See "Practical Common Lisp, Peter Seibel"
(http://www.gigamonkeys.com/book/macros-defining-your-own.html)

### Macro: ->
    (-> forms*) -> result

Since: 1.1

thread-first, inspired by https://github.com/amirgamil/lispy/blob/master/lib/library.lpy

Inserts first form as the first argument of the second form, and so forth.

Usage is illustrated by:

    (macroexpand-1 '(-> 1 f g h))
      ; ==> (h (g (f 1)))
    (macroexpand-1 '(-> 1 (f farg) (g garg) (h harg)))
      ; ==> (h (g (f 1 farg) garg) harg)

### Macro: ->>
    (->> forms*) -> result

Since: 1.1

thread-last

Same as `->` but inserts first form as last argument of the second form, and so forth.

Usage is illustrated by:

    (macroexpand-1 '(->> 1 f g h))
      ; ==> (h (g (f 1)))
    (macroexpand-1 '(->> 1 (f farg) (g garg) (h harg)))
      ; ==> (h harg (g garg (f farg 1)))

### Macro: and->
    (and-> forms*) -> result

Since: 1.1

Short-circuiting thread-first

Same as `->` but if one function returns `nil` then the remaining
functions are not called and the overall result is `nil`.

### Macro: and->>
    (and->> forms*) -> result

Since: 1.1

Short circuiting thread-last

Same as `->>` but if one function returns nil then the remaining
functions are not called and the overall result is `nil`.

### Function: string-trim
    (string-trim str) -> result

Since: 1.4

Return a fresh immutable `simple-string`
with leading and/ or trailing whitespace removed.

Example usage:

    (string-trim "  asdf   ") ; ==> "asdf"

### Function: string-subseq
    (string-subseq str start [end-excl]) -> result

Since: 1.4.2

Return a fresh mutable `simple-string`
whose value is the substring `[start end-excl[` of `str`.

### Function: string-replace
    (string-replace str srch replacement) -> result

Since: 1.4

Within `str` replace each occurrence of `srch` by `replacement`.
Special character sequences such as `\t` and `\n` are NOT recognized
and don't get special treatment.

Example usage:

    (string-replace "aaa aaa aaa" "aa" "b") ; ==> "ba ba ba"

### Function: string-split
    (string-split str regex) -> vector

Since: 1.4

Split `str` into a vector of simple strings.
Within `regex` special character sequences such as `\t` and `\n` are recognized.

Example usage:

    (string-split "a b     c
    d" "[ \\t\\n]")
    ; ==> #("a" "b" "c" "d")

### Function: string-join
    (string-join delim first-str . more-strings) -> string

Since: 1.4

### Macro: with-accumulator
    (with-accumulator accumulator-name accumulator start-value-form
      forms*) -> result

Since: 1.2

Within `forms`, bind the symbol given by `accumulator-name` to an accumulator-function of one argument
that "accumulates" the arguments of all invocations.
This accumulator-function will be constructed from the two-argument-function `accumulator`
which will be invoked with two arguments: "accumulated value so far" and "argument to `accumulator-name`".
"accumulated-value so far" will be initialized from `start-value-form`.

Sample usage:

    (defun factorial (n)
      (with-accumulator mult * 1
        (dotimes (i n)
          (mult (1+ i)))))

    (factorial 50) ; ==> 3.0414093201713376E64

### Macro: summing
    (summing forms*) -> result-sum

Since: 1.2

Within `forms`, bind `sum` to a function of one argument that sums the arguments
of all invocations.

Sample usage:

    (summing (dotimes (i 10) (sum i))) ; ==> 45.0

### Macro: reverse-collecting
    (reverse-collecting forms*) -> result-list

Since: 1.2

Within `forms`, bind `collect` to a function of one argument that accumulates
all the arguments it has been called with in reverse order.

Sample usage:

    (reverse-collecting (dotimes (i 10) (collect i)))
    ; ==> (9 8 7 6 5 4 3 2 1 0)

### Macro: collecting
    (collecting forms*) -> result-list

Since: 1.2

Within `forms`, bind `collect` to a function of one argument that accumulates
all the arguments it has been called with in order.

Sample usage:

    (collecting (dotimes (i 10) (collect i)))
    ; ==> (0 1 2 3 4 5 6 7 8 9)

### Function: plist-keys
    (plist-keys plist) -> result-list

Since: 1.2

Return the keys of a plist.

Sample usage:

    (plist-keys '(a 1 b 2 c 3)) ; ==> (a b c)

### Function: plist-values
    (plist-values plist) -> result-list

Since: 1.2

Return the values of a plist.

Sample usage:

    (plist-values '(a 1 b 2 c 3)) ; ==> (1 2 3)

### Function: format
    (format destination control-string args*) -> result

Since: 1.5

A simplified subset of Common Lisp's function `format`.

Note that this simplified `format` does not use or set CL's printer variables
such as `*print-base*`, `*print-circle*`, ... .

Only the format characters `C, %, &, |, ~, *, B ,D, O, R, X, E, F, G, A, S, W, T` and Tilde-Newline are supported.

`C` supports the modifier `@` for printing `#\`-style escaping.

`B, D, O, R, X` support `mincol, padchar, commachar` and `comma-interval`,
the modifier `@` for always printing the sign and the modifier `:` for grouping digits.

`R` does not support printing english numbers (giving the base, `@` or `:@` is required).

`E, F, G`: CL's full `format` is `~w,d,k,overflowchar,padcharF`, this subset only supports `~w,dF`
and the modifier `@` for always printing the sign.

`A` and `S` support `~mincol,colinc,minpad,padcharA` for padding, `:`, and the modifier `@` for left-padding.

`T` supports `@` for relative tabbing but ignores the modifier colon (`:`).

### Macro: formatter
    (formatter control-string) -> function

Since: 1.5

Returns a function with the argument list `(destination . args)`
that writes to `destination` and returns unused arguments as a list.

Sample usage:

    (let ((dest (make-array 0 'character t)))
      (values ((formatter "~&~A~A") dest 'a 'b 'c)
              dest))

    -> (c)
    -> "
    ab"

### Function: error
    (error [condition-type] controlstring-or-formatfunction args*) -> |

Since: 1.5

A simplified subset of Common Lisp's function `error`.
