
# Murmel Language Reference 

The file `murmel-langref.lisp`
is an executable language reference manual
for Murmel, a single-namespace Lisp dialect
insipred by Common Lisp.

See also Murmel's default library [Mlib](mlib.md) 
which contains additional functions and macros.

The file `murmel-langref.lisp` can be read as-is or run with:

    $ java -jar jmurmel.jar --repl --echo < murmel-langref.lisp

or transformed to Markdown:

    $ sed -nf doc/langref-to-md.sed murmel-langref.lisp > murmel-langref.md

Murmel is WIP, please note the section
[Known issues](#known-issues) at the end of this file.

## Quick links 

- [Predefined Symbols](#predefined-symbols)
- [Basic Special Forms](#basic-special-forms)
- [Additional Special Forms](#additional-special-forms)
- [Backquote - fill-in templates](#backquote)
- [Predefined Primitives](#predefined-primitives)
- [Predefined Numeric Primitives](#predefined-numeric-primitives)
- [Predefined Graphics Primitives](#predefined-graphics-primitives)

Additional functions that can be loaded with `(require "mlib")`
- [Mlib - Default library for Murmel](mlib.md)


## Introduction 

"Hello, World!" program written in Murmel:

    (format t "Hello, World!")

The program text above when run in the REPL should print the famous

    Hello, World!

followed by the result of `format`

    ==> nil

and the prompt `JMurmel>`.


## S-expressions 

Murmel is a Lisp dialect. As such the language isn't
really defined in terms of program text but in terms
of in-memory objects (lists, symbols and other atoms)
that are acceptable to `eval`.

That said, JMurmel's default reader turns S-expressions
from the input stream into equivalent in-memory objects. So
for this reference we'll use S-expressions to describe
programs that are valid Murmel, i.e. are acceptable to `eval`.

In this reference in-memory objects that are valid Murmel
(as well as their textual representation as S-expressions)
are referred to as "forms".

The following are S-expressions that JMurmel's reader
will accept and transform into in-memory objects.
Valid S-expressions may or may not be "forms"
(i.e. may or may not eval w/o error).

### Atoms that are not symbols

    1
    1.0
    "a string"
    #\a   ; the character 'a'
    #xff  ; the integer number 255 in hex


### Single quote

A single quote `'` is a shorthand for `(quote form)`,
see [Basic special forms](#basic-special-forms).

    'form


### Atoms that are symbols

Symbols usually start with a letter and are composed of
letters, digits and dashes ('-'),
but pretty much any token that is not another atom will be
parsed as a symbol.
Murmel handles symbols similar to Common Lisp,
see "CLHS 2.3.4 Symbols as Tokens" http://clhs.lisp.se/Body/02_cd.htm
for details.

Examples:

    'a-symbol
    '|a symbol|
    'a\ symbol
    '123"345
    '123.456.789


### Conses (pairs) and lists

A dotted pair

    '(a . b)              ; ==> (a . b)

A dotted list

    '(a . (b . (c . d)))

Shorthand for dotted list

    '(a b c . d)

A proper list

    '(a . (b . (c . ()))) ; ==> (a b c)

Shorthand for a proper list

    '(a b c)              ; ==> (a b c)


## Comments 

One line comments are started with `;`, i.e. everything between a semicolon
and the end of the line is ignored:

    ; this is a comment

Multiline comments are started with `#|` and ended with `|#`:

    #|
    This is a
    multiline comment.
    |#


## Predefined Symbols 

### `nil` and `t`
`nil` and `t` are pre-defined self-evaluating symbols.

    nil ; ==> nil
    t   ; ==> t

Murmel treats the symbols `nil` and `t` the same way
as Mr. Moon specified them in a Memo (see "The Evolution
of Lisp pp 62"):

> NIL is a symbol, the empty list, and the distinguished
> "false" value. SYMBOLP, ATOM, and LISTP are true of it;
> CONSP is not. CAR, CDR, and EVAL of NIL are NIL.
> NIL may not be used as a function, nor as a variable.
>
> T is a symbol and the default "true" value used by predicates that
> are not semi-predicates (i.e., that don???t return "meaningful" values
> when they are true.) EVAL of T is T. T may not be used as a variable.
> T is a keyword recognized by certain functions, such as FORMAT.


### dynamic
`dynamic` is a pre-defined self-evaluating symbol that may be used
in the `(lambda dynamic...` (interpreter only) and `(let* dynamic...` forms, see below.

    dynamic ; ==> dynamic


### internal-time-units-per-second
`internal-time-units-per-second` contains the resolution
of the time related functions, see below.

    internal-time-units-per-second ; ==> 1.0E9


### pi
`pi` contains the value of the mathematical constant pi
in double precision.

    pi ; ==> 3.141592653589793


### *command-line-argument-list*
`*command-line-argument-list*` contains all command line arguments
to the Murmel program. Below example illustrates this:

    C:\> java -jar jmurmel.jar -- a b c
    ...
    JMurmel> *command-line-argument-list*
    
    ==> ("a" "b" "c")
    JMurmel>


## Basic Special Forms 

### quote
    (quote form) -> form

`quote` returns a form without evaluating it.

    (quote a-symbol) ; ==> a-symbol

### lambda
    (lambda (params...) forms...) -> closure

When a lambda is created by the special form `lambda`
then the lexical environment is captured at the time of lambda creation.
Arguments to the special form `lambda` are not evaluated.

    (lambda (p1 p2) p1)

`lambda` with varargs:
If paramlist is a symbol then all arguments will be
packed into a list and bound to the symbol.
If paramlist is a dotted list then remaining arguments
will be bound to the last parameter.

    (lambda popt (write popt)) ; no mandatory arguments
    (lambda (p1 p2 . prest) (write prest)) ; two mandatory arguments


## Data types 

Murmel supports symbols and lists as well as other atoms
that are not symbols.
These other atoms are double precision floating point numbers,
64bit integer numbers, vectors, strings and characters. Custom primitives
may support additional atoms.

Murmel treats symbols case-insensitive.
Symbol names are of arbitrary length, however only the
first 30 chars are significant.

Implementation note: JMurmel preserves the
capitalization of the first encounter of a symbol
for printing, e.g.:

    '(AbC . dEf) -> (AbC . dEf)

but

    '(AbC . abc) -> (AbC . AbC)

    '*a-sample-symbol*
    'a\ symbol\ with\ spaces!

Empty list, printed as `nil`

    ()

Shorthand for empty list

    nil

Tokens that consist of a sign or digit followed by digits
are interpreted as 64bit integer numbers (java.lang.Long)

    1

Datatype float: a number in double precision

numbers that contain the characters '.eE' are interpreted
as floating point numbers (java.lang.Double)

    1.0

a float in scientific notation

    1e3

Stringliterals may have a maximum length of 2000 chars.
Stringliterals of the same value are coalesced (interned).

    "a string literal"
    "another 'literal' \"string literal\""


## Reserved words 

In addition to `nil` and `t` the symbols of the special forms are reserved, i.e.
may not be used as a function nor as a variable:

    nil, t,
    lambda, quote, cond, labels, if, define, defun, let, let*, letrec,
    setq, progn, defmacro, declaim


## Variables and Scope 

Symbols are bound to global variables using `define` or `defun`
(see below), and to local variables using `let, let*, letrec` or
lambda parameter lists.

Murmel's global bindings are lexical and must be defined before use,
i.e. a symbol is bound to
a newly created variable when define or defun are actually executed.
The symbol's binding as well as the variable's extent (lifetime)
last to the end of the program (the symbol's binding may be
temporarily replaced by a local or dynamic binding, though).

Murmel's local bindings are lexical, i.e. a symbol is bound to
a newly created variable when a let/let*/letrec/lambda form
is evaluated. The symbol's binding as well as the associated variable
are removed when leaving the lexical scope of the `let/let*/letrec/lambda`
form, restoring any previously existing binding (which may have
been local or global).
Except: `let* dynamic` will treat global symbols as "special", see below.


## Additional Special Forms 

### (define symbol optional-object?) -> symbol

`define` binds symbols in the global environment with
memory locations that hold values.
Murmel's `define` is somewhat similar to Common Lisp's `defparameter`
except:

CL's `defparameter` creates special global variables while
Murmel's `define` creates global variables that can be
lexically hidden by e.g. a let-binding.

The first argument is not evaluated, the second one - if given - is.
The second argument to `define` is optional and defaults to `nil`.

    (define *global-var* 42)               ; ==> *gloval-var*
    (define f1 (lambda (p1 p2) (+ p1 p2))) ; ==> f1

### (defun symbol (params...) forms...) -> symbol

`defun` is a shorthand for defining functions:

    (defun symbol (params...) forms...)
       <=>
    (define symbol (lambda (params...) forms...))

Arguments to `defun` are not evaluated.

    (defun f2 (p1 p2) (+ p1 p2)) ; ==> f2

### (defmacro name (params...) forms...) -> symbol<br/>(defmacro name) -> prev-name

`defmacro` defines a macro, similar to CL's `defmacro`.
Macros are somewhat similar to functions:
On a function application the functions's arguments
are eval'd and the result of the function will be used as is.
On a macro application the macro's arguments are
NOT eval'd, but the result of the macro is.

IOW a function produces a value, a macro application
produces code that will be eval'd.
Macros are defined in a macro namespace.
All macros are in the global scope, i.e. macros
are not scoped but once defined they are visible everywhere.

`(defmacro name)` can be used to unbind previously
defined macros.

    (defmacro twice (arg) (list '* arg 2))  ; ==> twice
    (twice 3) ; ==> 6.0
    (defmacro twice) ; ==> twice; macro is unbound
    (defmacro twice) ; ==> twice; no-op

### (setq symbol value...) -> last-value

`setq` updates the value(s) of the given global or local symbol(s).
In interpreted Murmel undefined variables will be created on the fly,
in compiled mode variables must have been defined previously.

    (define a nil)
    (let ((b nil) (c nil))
      (setq a 1 b 2 c (+ a b))) ; ==> 3.0

### (if condform form optionalform) -> result

    (if nil 'YASSS! 'OHNOOO!!!) ; ==> OHNOOO!!!

### (progn expr...) -> result

    (if t (progn (write 'abc) (write 'def)))

### (cond (condform forms...)... ) -> result

### (labels ((symbol (params...) forms...)...) forms...) -> result


### (let optsymbol? ((symbol bindingform)...) bodyforms...) -> result

Works similar to CL's `let` with the addition
of Scheme's "named let".
The let-bound variables `symbol` as well as `optsymbol` - if given -
are bound inside bodyforms.
`optsymbol` will be bound inside `bodyforms` to a lambda
whose parameters are the let-variables and whose code is
`bodyforms`. Therefore `optsymbol` can be used for
recursive calls within `bodyforms`.

    (let loop ((x 3)
               (msg "hi"))
      (if (= x 0)
          (write msg)
        (progn (write (floor x)) (loop (- x 1) msg))))

### (let dynamic ((symbol bindingform)...) bodyforms...) -> result

Similar to `let` except: globals are not shadowed but temporarily
bound to the given value, and the previous value is restored when
leaving the scope of the `let` form.
I.e. `let dynamic` treats globals as "special".

    (setq a 1)
    (define b 2)
    (defun f () (write (cons a b)))

    (f)
    (let dynamic ((a 11) (b a))
      (f))
    (f)
will print (1 . 2)(11 . 1)(1 . 2)

### (let* optsymbol? ((symbol bindingform)...) bodyforms...) -> result

Works like `let` (see above) with the addition:
each `bindingform` "sees" the previous symbols. If multiple
let-bindings use the same symbol the last one hides
preceeding ones.

    (let* loop ((y 10)
                (x (+ y 20))
                (x (floor (/ x 10)))
                (msg 'hi))
      (if (= x 0)
          (write msg)
        (progn (write (floor x)) (loop 0 0 (- x 1) msg))))

### (let* dynamic ((symbol bindingform)...) bodyforms...) -> result

Similar to `let*` except: globals are not shadowed but temporarily
bound to the given value, and the previous value is restored when
leaving the scope of the `let*` form.
I.e. `let* dynamic` treats globals as "special".

    (define *g* 'global)
    (defun fun () (write *g*))
    (let* dynamic ((*g* 'temp)) (fun)) ; fun will write temp
    *g* ; ==> 'global

### (letrec optsybol? ((symbol bindingform)...) bodyforms...) -> result

`letrec` works like `let` and `let*` except each bindingform "sees"
all other let symbols as well as it's own symbol.
All symbols are bound, but only the preceeding bindings
are defined (have a value) while eval'ing the `bindingform`.
That way a let-bound variable could be a recursive lambda.

    (letrec ((x 1) (y (+ x 1))) (write y))


### (catch tagform forms...) -> result

`catch` is used as the destination of a non-local control transfer by `throw`.

### (throw tagform resultform) -> |

`throw` causes a non-local control transfer to a `catch` whose tag is `eq` to tag.
TODO: If there is no outstanding catch tag that matches the throw tag,
no unwinding of the stack is performed, and an error of type control-error is signaled.

### (unwind-protect protected-form cleanupforms...) -> result

`unwind-protect` evaluates `protected-form` and guarantees that `cleanup-forms`
are executed before unwind-protect exits, whether it terminates normally or is aborted
by a control transfer of some kind.


### (multiple-value-call function-form values-form*) -> result

`multiple-value-call` first evaluates the `function-form` to obtain function,
and then evaluates each `values-form`. All the values of each form
are gathered together (not just one value from each) and given as arguments
to the function.

    (multiple-value-call + 0.0 (values 1 2) 3) ; ==> 6.0

### (multiple-value-bind (symbols...) values-form bodyforms...) -> result

`values-form` is evaluated, and each of the `symbols` is bound
to the respective value returned by that form.
If there are more `symbols` than values returned,
`nil` is assigned to the remaining vars.
If there are more values than symbols, the excess values are discarded.
The symbols are bound to the values over the execution of the `bodyforms`,
which make up an implicit progn.

    (multiple-value-bind (a b) (values 'Hello\, '\ World!) (format nil "%s%s" a b))
     ; ==> "Hello, World!"


### (load filespec) -> result

Eval the contents of the given file, return value
is the value returned by the last form or nil
in case of an empty file.

When compiling Murmel `load` is performed at
compile time.

`filespec` is not eval'd and must be a string.
Unless filespec ends with ".lisp" the file extension
".lisp" will be appended.
If filespec is an absolute path then it will be used as is.
Otherwise the file will be searched in the same directory
as the file that contains the `load` and after that
in "libdir" (set with `--libdir`, libdir defaults to the
directory containing jmurmel.jar).
If `load` is entered into the REPL then the file
will be searched in the current directory and then
in the directory that contains jmurmel.jar.

    (load "nul") ; ==> nil, NUL is Windows specific
    (load "lib") ; will search for lib.lisp

### (require module-name optional-file-path)

Load the given file once. Murmel maintains an internal
set of loaded modules, and `require` will ignore
loading files that were already loaded by comparing
`module-name` to the set of already loaded modules.

When compiling Murmel `require` is performed at
compile time.

If `optional-file-path` is omitted or nil then
`module-name` will be used as the file path.

`module-name` and `optional-file-path` are not eval'd
and must be strings.

    (require "mlib") ; will search for the file mlib.lisp
                     ; unless the module "mlib" was already loaded

### (provide module-name)

Set a file's modulename so that `require` won't
load it twice.


### (declaim (optimize ...

`declaim` currently only supports `optimize`, others will be ignored.
`optimize` only supports speed, others will be ignored.


## Function application 

### function call

Applies the operator returned by operatorform to
the eval'd operands

    (operatorform operands...)


## Backquote 

Backquote "\`" starts "fill-in templates":
backquote, comma and comma-at work similar to CL,
except: comma-dot is not supported.

    (setq a 'a-val) (setq b 'b-val) (define c 'c-val)
    (define d '(d-val1 d-val2))
    `((,a b) ,c ,@d)      ; ==> ((a-val b) c-val d-val1 d-val2)

    (define y 'b) (define l '(a b))
    (eval ``(,a ,,@l ,,y) '((a . a) (b . b) (y . y))) ; ==> (a-val a-val b-val b-val)

    (define x '(1 2 3))
    `(normal= ,x splicing= ,@x see?) ; ==> (normal= (1 2 3) splicing= 1 2 3 see?)

    `(normal= ,x fakesplicing= . ,x) ; ==> (normal= (1 2 3) fakesplicing= 1 2 3)


## Predefined Primitives 

### (apply form argform) -> result

`form` must eval to a symbol, primitive or lambda.
`argform` must eval to a proper list. 

    (apply + '(1 2)) ; ==> 3.0

### (eval form) -> result<br/> (eval form env) -> result

`form` will be eval'd, it must return a form.
The optional argument `env` will be eval'd, it must return a list of `(symbol . value)`.
If the optional argument `env` is omitted or `nil`
then the environment for the recursive eval is "all predefined global symbols"
else it is the concatenation of `env` and all predefined globals

    (eval '(+ 1 2)) ; ==> 3.0
    (eval '(+ x y) (list '(x . 2) '(y . 3))) ; ==> 5.0
    (eval '(+ x y) (list (cons 'x 2) (cons 'y 3))) ; ==> 5.0

### (cons e1 e2) -> conscell

    (cons 'a 'b) ; ==> (a . b)

### (car list) -> 1st element of list

    (car '(a b c)) ; ==> a
    (car "abc") ; ==> #\a

### (cdr list) -> rest of list

    (cdr '(a b c)) ; ==> (b c)
    (cdr "abc") ; ==> "bc"

### rplaca, rplacd

Replace the value of the CAR or CDR slot of a cons cell.

    (setq l '(1 2))
    (rplaca l 11) ; ==> (11 2)
    (rplacd l 22) ; ==> (11 . 22)

### (values object*) -> mutliple-values

`values` returns the objects as multiple values.


### (list elems*) -> list<br/>(list* elems+) -> atom-or-dotted-list

`list` will create a list consisting of it's arguments,
`list*` will create a dotted list.

    (list) ; ==> nil
    (list 1) ; ==> (1)
    (list 1 2 3) ; ==> (1 2 3)

    (list* 1) ; ==> 1
    (list* 1 2 3) ; ==> (1 2 . 3)

### (eq x y) -> boolean

Returns `t` if `x` and `y` are the same object, `nil` otherwise.

### (eql x y) -> boolean

Return `t` if any of the following is true

- `a` and `b` are `eq`
- `a` and `b` are numbers of the same type and have the same value
- `a` and `b` are the same characters

Examples:

    (eql 2 2) ; ==> t
    (eql #\a (car "aaa")) ; ==> t
    (eql -0.0 0.0) ; ==> nil

### null, atom, consp, listp, symbolp, stringp, simple-string-p, characterp, functionp

### numberp, integerp, floatp

- `numberp` returns `t` for all Java Number types.
- `integerp` returns `t` for Murmel's integral number type (which internally is a `Long`).
- `floatp` returns `t` for Murmel's decimal number type (which internally is a `Double`).

### (make-array length) -> simple-vector

Only one-dimensional simple arrays of element-type T are supported

### vector, vector-length, vectorp, simple-vector-p, svref, svset, svlength

Vectors are on-dimensional arrays (which internally are Object[]).

Example usage:

    (define *v* (vector 1 2 3))
    (vectorp *v*) ; ==> t
    (svlength *v*) ; ==> 3
    (svref *v* 1) ; ==> 2

### list->simple-vector and simple-vector->list

### (assoc key alist) -> cons or nil

`assoc` takes a key and a list of key/value tupels (lists or conses).
The return value is the first cons whose car is equal(*) to `key`
or `nil` if no such cons was found. `nil`-elements in `alist` are ignored.

(*) `assoc` compares two items as if eql was used.
    `assoc` considers two items as "equal" if

- Both are `eq` (are the same object)
- Both are integers or floats or characters respectively and have the same value

Examples:

    (assoc 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4))) ; ==> (a-key 3)
    (cdr (assoc 'a-key '((key-1 . 1) (key-2 . 2) (a-key . 3) (key-4 . 4)))) ; ==> 3
    (assoc nil '((key-1 1) nil (nil 2) (a-key 3) (key-4 4))) ; ==> (nil 2)

### (assq key alist) -> cons or nil

`assq` is similar to `assoc` except `assq` compares keys using `eq`.

### (append lists...) -> list

`append` nondestructively append it's arguments. All arguments except the last
are shallow copied, all arguments except the last must be lists.

    (append)                    ; ==> nil
    (append 'a)                 ; ==> a
    (append '(a b) 'c)          ; ==> (a b . c)
    (append '(a b) '(c d))      ; ==> (a b c d)
    (append '(a . b) '(c d))    ; ==> (a b c d)
    (append '(a . b) '(c . d))  ; ==> (a b c . d)

### (macroexpand-1 quoted-form) -> expanded-form, boolean

`macroexpand-1` is a simplified version of CL's `macroexpand-1`.
It is only fully supported in interpreted code,
in compiled code `macroexpand-1` does quoted forms only.
If the operator of the list `quoted-form` is a macroname then
the macrocall will be expanded, and the secondary return value
will be `t`, e.g.:

    (defmacro add2 (a) `(+ ,a 2))  ; ==> add2
    (macroexpand-1 '(add2 3))      ; --> (+ 3 2)
                                   ; --> t

### (gensym) -> uninterned symbol

Return a new unique symbol.
    (gensym)

### read, write, writeln, lnwrite

    (read) -> obj
    (write   obj  print-escape-p?) -> t
    (writeln obj? print-escape-p?) -> t
    (lnwrite obj? print-escape-p?) -> t

`write` accepts an optional boolean argument `print-escape-p`.
`writeln` and `lnwrite` accept an optional argument `obj`
and an optional boolean argument `print-escape-p`.

`writeln` will write the argument if given, followed by a newline.  
`lnwrite` will write a newline followed by the argument if given,
followed by a ' ', i.e. writeln is C-style, lnwrite is Lisp-style.

    (writeln "Hello, ")
    (writeln)
    (writeln "World!")

`write`, `writeln` and `lnwrite` will escape atoms by default,
the optioal parameter `print-escape-p` can be used to turn off escaping.
    (writeln "Hello, World!" nil)

### (get-internal-real-time) -> number

Walltime in internal time units, relative to an arbitrary time base

    (get-internal-real-time)

### (get-internal-run-time) -> number

User cpu time in internal time units

    (get-internal-run-time)

### (get-internal-cpu-time) -> number

User + system cpu time in internal time units

    (get-internal-cpu-time)

### (sleep desired-duration) -> nil

Pause execution for approx. x seconds.
This example code will pause execution for approx. 1 second.

    (sleep 1)

### (get-universal-time) -> seconds since 1.1.1900 00:00:00 UTC

This example code will return approx. years since 1.1.1900

    (/ (get-universal-time) 60 60 24 365)

### (get-decoded-time) -> second, minute, hour, date, month, year, day, daylight-p, zone

NOTE: In Common Lisp zone is given as a a rational multiple of 1/3600 of hours
      offset from Greenwich Mean Time.
      In Murmel zone is given as a double, e.g. Vienna in winter is -1.0.
      The result type of Murmel's (get-decoded-time) is a list
      while CL's returns an ordered sequence.

    (get-decoded-time)

### format, format-locale

`format t` writes a formatted string to stdout and returns `nil`.
format's parameters work as java.lang.String.format().

    (format t "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

`format-locale` works similar to format except it has an additional
first string parameter that should be a locale, `nil` means use Java's
default locale.

    (format-locale t
       "de-DE" "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

`format nil` and `format-locale nil` work similar
to `format` and `format-locale` except they don't write to stdout
but return the string

    (format-locale nil
       "de-DE" "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

### (code-char integer) -> character

### (char-code character) -> integer

### (string= s1 s2) -> boolean

### (string->list str) -> list-of-characters

### (list->string list-of-characters) -> string


## Predefined Numeric Primitives 

Murmel's two numeric datatypes are implemented as `long` (64 bit signed integer)
and `double` (64 bit floating point).
Other Java datatypes (which may occur when using Java FFI) will be automatically converted
(with null-, lost-precision- and over/underflow checks) as appropriate.

Murmel does most maths in double precision:

- Except with `1+, 1-, ceiling, floor, round, truncate and signum`,
  arguments to numeric functions will be converted to `double`,
  and the result will be `double`.  
  Over/underflow during argument conversion will be signalled as an error.
  Results for large numbers (i.e. over/ underflow) will go towards +/- Infinity.
- `1+, 1-`: Byte, Short, Integer and Long arguments will be converted to long,
  in that case the result type is long. Over/ underflow of the result will be signalled as an error.
  Other argument types will be handled as above.
- `ceiling, floor, round, truncate`: result type is long, over/ underflow will be signalled as an error.
- `signum`: result is a "signed prototype" - either `long -1/0/1` or `double -1.0/-0.0/0.0/1.0`.

### +, -, *, /, mod, rem, sqrt, log, log10, exp, expt

The math operators accept numbers only.
`log` only takes 1 argument,
but otherwise should work similar to CL.
All numeric operators return a double.
eg. `(+ number number) -> double`.

    (+ 1 1) ; ==> 2.0

### 1+, 1-

Increment and decrement return the same type as the argument.

    (1+ 1)   ; ==> 2
    (1+ 1.0) ; ==> 2.0

### round, truncate, floor, ceiling

These functions take one or two arguments and return an integer value or an exception
if the value cannot be represented by a long
(NaN, Infinite, integer overflow or underflow),
eg. `(floor number) -> long`

    (floor 1.1) ; ==> 1

### fround, ftruncate, ffloor, fceiling

These functions take one or two arguments and return an integer value as a double,
eg. `(ffloor number) -> double`

    (ffloor 1.1) ; ==> 1.0

### (signum number) -> signed-prototype

`signum` determines a numerical value that indicates whether
number is negative, zero, or positive. 

    (signum 0)    ; => 0
    (signum -0)   ; => 0
    (signum 3)    ; => 1
    (signum -3)   ; => -1

    (signum 0.0)  ; => 0.0
    (signum -0.0) ; => -0.0
    (signum 3.0)  ; => 1.0
    (signum -3.0) ; => -1.0

### = < > <= >= /=

The numeric comparison operators take one or more number arguments.

    (= 1 1.0)         ; ==> t
    (< 1 2 3.0 4 5.0) ; ==> t
    (< 1 2 3 3 4 5)   ; ==> nil


## Predefined Graphics Primitives 

Murmel features primitives for graphics output to a frame.

A frame is a toplevel GUI window with a title. One can write text on a frame,
and/ or draw lines with Turtle graphics or moveto/lineto functions.
Also one can attach a bitmap to a frame and use pixel-graphics primitives.

0/0 is towards left bottom, any drawing will be resized/ shifted so that
it fills the frame.

A frame has state: open/closed, current x/y position, current linecolor,
current turtle angle and background color.

One can use several frames but only one is the "current-frame". All functions
(except `make-frame`) have an optional last parameter `frame` that can be used
to select which frame to operate on (if omitted or `nil` then the current frame is used).

Hint: if graphics primitives are slow then maybe switching to OpenGL
will speed things up:

    C:\> java -Dsun.java2d.opengl=true -jar jmurmel.jar

Colors are 0..15 corresponding to

      0 ; white
      1 ; black
      2 ; red
      3 ; green
      4 ; blue
      5 ; pink
      6 ; orange
      7 ; yellow
      8 ; magenta
      9 ; cyan
     10 ; darkGray
     11 ; gray
     12 ; lightGray
     13 ; darkred
     14 ; darkgreen
     15 ; darkblue

### (make-frame window-title optwidthpixels optheightpixels optpaddingpixels) -> frame

Creates a new frame, sets current frame.
If width and height are omitted or nil then half of the physical screen
width/ height will be used.
If padding is omitted or nil then 40 will be used.
The newly created frame will not yet be visible (see open-frame below).

    (let ((window-title "test") (optwidthpixels 100) (optheightpixels 100) (optpaddingpixels 10))
      (make-frame window-title optwidthpixels optheightpixels optpaddingpixels)) ; ==> frame

### (current-frame) -> frame

Returns current frame.

### (current-frame optional-frame) -> frame

Set new current frame, returns previous current frame.

### More frame-functions

open-frame ... make frame visible  
close-frame ... hide frame  
reset-frame ... reset pen to "down", turtle position and angle, color and bgcolor    
clear-frame ... reset frame and discard frame contents  
repaint-frame ... force full repaint  
flush-frame ... paint operations won't take immediate effect, flush-frame makes them visible  
pen-up ... subsequent line operations will only move the position  
pen-down ... subsequent line operations will have visible effect  
push-pos ... save current position and angle  
pop-pos ... restore previous position and angle

The above functions all take one optional frame parameter. If omitted or nil
then the current frame will be used.

### (color color optional-frame) -> frame

Set color for following lines, color must be >= 0 and <= 12

    (color 1) ; ==> frame

### (bgcolor color optional-frame) -> frame

Set background color for frame, color must be >= 0 and <= 12

    (bgcolor 0) ; ==> frame

### (text str optional-frame) -> frame

Writes str at current position, does not change position.

### (left  deg optional-frame) -> frame</br>(right deg optional-frame) -> frame

Increase/ decrease current angle by `deg` degrees, does not change position.

### (forward len optional-frame) -> frame

If pen is down then this function paints a line of length `len` from current
position in current direction, changes position.
If pen is up then only the position is changed.

### move-to, line-to, move-rel, line-rel

These functions take two arguments denoting the new position
and an optional frame argument.

    (define new-x 1)
    (define new-y 2)
    (define optional-frame nil)
    (line-rel new-x new-y optional-frame) ; ==> frame

### (make-bitmap w h optional-frame) -> frame

Associates a bitmap of the given width and height
with the current or given frame. The bitmap will be scaled
to fill the frame.

### (discard-bitmap optional-frame)

Remove the bitmap of the given or current frame,
and release the resources associated with the bitmap.

### (rgb-to-pixel r g b) -> 24bit color value acceptable to set-pixel

`r`, `g` and `b` are 0..255

### (hsb-to-pixel h s b) ; -> 24bit color value acceptable to set-pixel

`h`, `s` and `b` are 0..1.0

### (set-pixel x y color-as-24-bit optional-frame) -> frame

Set pixel at the given xy-position to the given color.

--- End of Murmel reference ---


## Extensions 

JMurmel adds some extra features to Murmel which are listed below.

### lambda dynamic

For experimentation purposes the interpreter also supports
lambdas that are not lexical closures.

When the optional keyword `dynamic` is given then
no environment is captured, and lambdas - when applied -
get the dynamic environment.

    (lambda dynamic (params...) forms...) -> anonymous function with
                                             dynamic environment

### Custom primitives

In embedded use primitives may be added, also Java methods
created by the primitive `jmethod` (see below) act as builtins.
As far as the language is concerned, all primitives are variadic functions.
The primitives themselves may have parameter checks, tough.
In case they are called with the wrong type(s) or number of parameters
the primitives may throw an error.

### Language subsets

JMurmel supports commandline parameters that can be used to
disable many of Murmel's forms, predefined variables, ... .
These commandline parameters can be used to experiment
with an even more reduced Lisp.

See `jmurmel --help-features`.


## Additional JMurmel special forms and primitives 

### (trace function-name*) -> trace-result<br/> (untrace function-name*) -> untrace-result

Arguments to trace/ untrace are eval'd.
In interpreted code these work similar to CL's trace/ untrace macros,
see http://clhs.lisp.se/Body/m_tracec.htm.
Only interpreted code will be traced (user code as well as
interpreter primities).

    (trace 'write '+)  
    (write (+ 1 2))  
    (untrace)


### (jmethod classname methodname paramclass...) -> primitive

The primitive `jmethod` will return a newly created primitive
that is implemented by the method `methodname` of the Java class
`classname` that has the formal parameters `paramclass...`.
Parameters to `jmethod` must be strings.

    (jmethod "java.lang.System" "currentTimeMillis")

When invoking primitives created by `jmethod` the first argument must be
a Java object of the primitive's method's class. This is not the case
for static methods.
Invoke static method:

    (define ctm (jmethod "java.lang.System" "currentTimeMillis"))  
    (ctm)

invoke method on an object

    (define make-hash (jmethod "java.util.HashMap" "new"))
    (define put-hash (jmethod "java.util.Map" "put" "java.lang.Object" "java.lang.Object"))
    (define my-hash (make-hash))
    (put-hash my-hash "key-1" "value-1")
    (write my-hash)


### (jproxy interfacename javamethodname symbol-or-lambda...) -> Java-object

The primitive `jproxy` takes a Java interface name and zero or more
methodname/ murmelcode tupels, and returns a Java object that implements
`interfacename`.


## Known issues 

Murmel language:

- needs some means of catching and processing runtime errors,
  e.g. unwind-potect and catch/ throw
- file i/o

Compiler issues:

- define/ defun/ defmacro only work as top level forms or within a `progn` form,
  other use as non-toplevel form will throw a "not-yet-implemented" compiler error.
- define-ing an already define-d symbol is not supported
- macroexpand-1 is limited to quoted forms
- macro expansion is only done at compiletime, e.g. (defmacro m() 1) (eval '(m))
  won't work
- reassigning predefined primitives with setq is not supported, e.g.
  `(setq trunc (lambda (p) (1+ p)))` is an error (and wouldn't make much sense)


## Copyright 

Murmel and JMurmel are Copyright (C) 2020-2022 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT.

At the end of the input file JMurmel will print "bye." and exit.
