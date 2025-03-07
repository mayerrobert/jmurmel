
# Murmel Language Reference 

The file `murmel-langref.lisp`
is an executable language reference manual
for Murmel, a single-namespace Lisp dialect
inspired by Common Lisp.

See also Murmel's default library [Mlib](mlib.md) 
which contains additional functions and macros.

The file `murmel-langref.lisp` can be read as-is or run with:

    $ java -jar jmurmel.jar --repl --echo < murmel-langref.lisp

or transformed to Markdown:

    $ sed -nf scripts/langref-to-md.sed murmel-langref.lisp \
    > murmel-langref.md

Murmel is WIP, please note the section
[Known issues](#known-issues) at the end of this file.

## Murmel Reference 

- [S-expressions](#s-expressions)
- [Comments](#comments)
- [Predefined Symbols](#predefined-symbols)
- [Basic Special Forms](#basic-special-forms)
- [Function application](#function-application)
- [Data types](#data-types)
- [Reserved words](#reserved-words)
- [Variables and scope](#variables-and-scope)
- [Additional Special Forms](#additional-special-forms)
- [Backquote - fill-in templates](#backquote)
- [Basic Primitives](#basic-primitives)
- [Logic, Predicates](#logic-predicates)
- [Conses and lists](#conses-and-lists)
- [Vectors, Sequences](#vectors-sequences)
- [Hashtables](#hash-tables)
- [I/O](#io)
- [Misc](#misc)
- [Time](#time)
- [Predefined Numeric Primitives](#predefined-numeric-primitives)
- [Predefined Graphics Primitives](#predefined-graphics-primitives)
- [Java FFI](#java-ffi)

Additional functions that can be loaded with `(require "mlib")`
- [Mlib - Default library for Murmel](mlib.md)


## Introduction 

"Hello, World!" program written in Murmel:

    (jformat t "Hello, World!")

The program text above when run in the REPL should print the famous

    Hello, World!

followed by the result of `jformat`

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


### Labeled subobjects

Since: 1.4.6

    '(a #1=b #1# c)       ; ==> (a b b c)

Note that a label can only be used after it's value was completely read,
i.e. `'#1=(a b . #1#) is an error in Murmel (while valid in CL).


### Hashtables

Since: 1.3.1

    #H(eql k1 v1 k2 v2 k3 v3)


## Comments 

One line comments are started with `;`, i.e. everything between a semicolon
and the end of the line is ignored:

    ; this is a comment

Multiline comments are started with `#|` and ended with `|#`:

    #|
    This is a
    multiline comment.
    |#

The pair `#!` and `!#` can also be used for multiline comments.

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
> are not semi-predicates (i.e., that don’t return "meaningful" values
> when they are true.) EVAL of T is T. T may not be used as a variable.
> T is a keyword recognized by certain functions, such as JFORMAT.


### internal-time-units-per-second
`internal-time-units-per-second` contains the resolution
of the time related functions, see below.

    internal-time-units-per-second ; ==> 1.0E9


### pi
`pi` contains the value of the mathematical constant pi
in double precision.

    pi ; ==> 3.141592653589793


### \*command-line-argument-list\*
`*command-line-argument-list*` contains all command line arguments
to the Murmel program. Below example illustrates this:

    C:\> java -jar jmurmel.jar -- a b c
    ...
    JMurmel> *command-line-argument-list*
    
    ==> ("a" "b" "c")
    JMurmel>


### array-dimension-limit
Largest acceptable vector index.


### most-positive-fixnum, most-negative-fixnum
These global variables contain the smallest and largest fixnum value.


### \*condition-handler\*

Since: 1.4

This variable can be set to a function of one parameter.
If `*condition-handler*` is non-nil then it will be invoked in case of an error,
the argument will be the condition describing the error.
During invocation of the condition handler it will be disabled:
if the current handler dynamically replaced a previous handler
then the previous handler will be temporarily restored.


### \*random-state\*

Since: 1.4.4

Will initially be `nil`. Will be lazily created by one-arg `random`
or zero-arg `make-random-state`.
     

### REPL variables
    @-, @+, @++, @+++, @\*, @\*\*, @\*\*\*, @/, @//, @///

These variables are only defined when using the REPL.
They work similar to CL's REPL variables without the leading `@`.

The global variables @\*, @\*\*, @\*\*\* are maintained by the Lisp read-eval-print loop
to save the values of results that are printed each time through the loop.

The value of @\* is the most recent primary value that was printed,
the value of @\*\* is the previous value of @\*,
and the value of @\*\*\* is the previous value of @\*\*.

If several values are produced, @\* contains the first value only;
@\* contains nil if zero values are produced.

The values of @\*, @\*\*, and @\*\*\* are updated immediately prior to printing the return value
of a top-level form by the Lisp read-eval-print loop.
If the evaluation of such a form is aborted prior to its normal return,
the values of @\*, @\*\*, and @\*\*\* are not updated.


## Basic Special Forms 

### quote
    (quote form) -> form

`quote` returns a form without evaluating it.

    (quote a-symbol) ; ==> a-symbol

### lambda
    (lambda (params*) forms*) -> closure

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

Murmel supports symbols and cons cells (and lists built from cons cells)
as well as other atoms that are not symbols.
These other atoms are double precision floating point numbers,
integer numbers, vectors, strings, characters, bits and more.
Custom primitives may support additional atoms.

Murmel's type system (and JMurmel's corresponding host types) look like so:

    ;; Murmel type                ; description or "Murmel form -> Java class used in JMurmel"

    t

       cons                       ; (cons 1 2)                     -> ConsCell

       atom                       ; all Murmel objects except cons cells
                                  ; and all Java Objects are atoms

          symbol                  ; 'sym                           -> LambdaJSymbol
             null                 ; nil                            -> null
                                  ; nil is the only object of type null

          number                  ; java.lang.Number is accepted for reading
             float                ; 2.3                            -> java.lang.Double
             integer              ; 42                             -> java.lang.Long
                                  ; only 54 bits are used

          character               ; #\A                            -> java.lang.Character

          random-state            ; (make-random-state)            -> java.util.Random

          vector                  ; (make-array NN t t)            -> java.util.ArrayList
                                  ; (make-array NN t CC)           -> java.util.ArrayList
                                  ; makes an ArrayList w/ size NN and initial capacity CC
                                  ; java.util.List is acceptable for seqref and seqset
             simple-vector        ; (make-array NN t nil)          -> Object[]
                                  ; #(1 2 3)                       -> Object[]
             string               ; (make-array NN 'character t)   -> java.lang.StringBuilder
                                  ; java.lang.StringBuffer is acceptable for sref and sset
                                  ; java.lang.CharSequence is accepted for sref
                simple-string     ; (make-array NN 'character nil) -> char[]
                                  ; "abc"                          -> java.lang.String
             bit-vector           ; (make-array NN 'bit t)         -> BitVector
                simple-bit-vector ; (make-array NN 'bit nil)       -> boolean[]
                                  : #*0101                         -> boolean[]

          hash-table              ; (make-hash-table [test [size]])
                                  ; #H(eql one 1 two 2 three 3)     -> EqlMap
                                  ; (make-hash-table ['eql [size]]) -> EqlMap
                                  ; (make-hash-table ['equal [size]]) -> EqualMap
                                  ; (make-hash-table 'eq [size])    -> java.util.IdentityHashMap
                                  ; (make-hash-table 't [size])     -> java.util.HashMap
                                  ; java.util.Map is acceptable for hashref, hashset, clrhash...

          function                ; (lambda (param) param)         -> Closure or MurmelFunction

          (list ::= cons | null)
          (sequence ::= list | vector)


    ;; Murmel's condition type hierarchy is a subset of CL's condition type hierarchy,
    ;; `error` and subtypes are pretty much the same. Murmel doesn't have multiple
    ;; inheritance, though, and `reader-error` only extends `stream-error`.
    ;; JMurmel maps conditions to Java exceptions as best as possible.

          condition                           java.lang.Throwable
              error                           java.lang.Exception

                (murmel-error                 LambdaJError extends RuntimeException extends Exception)

                                              ;;; extends LambdaJError
                  simple-error                SimpleError extends LambdaJError

                  cell-error                  CellError extends LambdaJError
                      unbound-variable        UnboundVariable extends CellError extends LambdaJError
                      undefined-function      UndefinedFunction extends CellError extends LambdaJError

                  control-error               ControlError extends LambdaJError

                  program-error               ProgramError extends LambdaJError

                  parse-error                 SExpressionReader.ParseError extends LambdaJError


                  arithmetic-error            java.lang.ArithmeticException extends RuntimeException
                      (overflow, underflow)
      
                  type-error                  java.lang.ClassCastException extends RuntimeException
                      simple-type-error       SimpleTypeError extends ClassCastException
                      invalid-index-error     InvalidIndexError extends IndexOutOfBoundsException
      
                  file-error                  java.nio.file.InvalidPathException
                                              extends IllegalArgumentException extends RuntimeException


                  stream-error                java.io.IOException
                      end-of-file             java.io.EOFException extends IOException
                      reader-error            ReaderError extends IOException
                                              NOTE:
                                              CL's reader-error has two superclasses: stream-error and parse-error
                                              if c was a reader-error then both
                                              (typep c 'stream-error) and (typep c 'parse-error) are true

The above is a subset of CLtL2's predefined data types,
see "CLtL2 2. Data Types" https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node15.html
Murmel's condition hierarchy is a subset of CLtL2 predefined condition types,
see "CLtL2 29.5. Predefined Condition Types" https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node346.html
and "CLHS 9.1.1 Condition Types" http://clhs.lisp.se/Body/09_aa.htm

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
are interpreted as integer numbers (java.lang.Long)

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
    setq, progn, catch, thwrow, unwind-protect, try,
    multiple-value-bind, multiple-value-call,
    macrolet, defmacro, declaim, load, require, provide


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
a newly created variable when a `let/let*/letrec/lambda` form
is evaluated. The symbol's binding as well as the associated variable
are removed when leaving the lexical scope of the `let/let*/letrec/lambda`
form, restoring any previously existing binding (which may have
been local or global).
Except: `let* dynamic` will treat global symbols as "special", see below.


## Additional Special Forms 

### (define symbol [optional-object]) -> symbol

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

`define` forms can appear inside toplevel `let, let*, letrec, multiple-value-bind, labels` forms.

### (defun symbol (params\*) [docstring] forms\*) -> symbol

`defun` is a shorthand for defining functions:

    (defun symbol (params*) forms*)
       <=>
    (define symbol (lambda (params*) forms*))

An optional docstring is ignored.
Arguments to `defun` are not evaluated.

    (defun f2 (p1 p2) (+ p1 p2)) ; ==> f2

`defun` forms can appear inside toplevel `let, let*, letrec, multiple-value-bind, labels` forms
and will bind a globally visible symbol to a closure (aka let-over-lambda):

    (let ((counter 0))
      (defun pre-increment-counter ()
        (setq counter (1+ counter)))

      (defun pre-decrement-counter ()
        (setq counter (1- counter)))

      (defun peek-counter ()
        counter))

    (pre-increment-counter)  ; ==> 1
    (pre-increment-counter)  ; ==> 2
    (pre-decrement-counter)  ; ==> 1
    (peek-counter)  ; ==> 1

### (defmacro name (params\*) [docstring] forms\*) -> symbol<br/>(defmacro name) -> prev-name

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

An optional docstring is ignored.

    (defmacro twice (arg) (list '* arg 2))  ; ==> twice
    (twice 3) ; ==> 6.0
    (defmacro twice) ; ==> twice; macro is unbound
    (defmacro twice) ; ==> twice; no-op

### (setq symbol value [more-symbols more-values]\*) -> last-value

`setq` updates the value(s) of the given global or local symbol(s).
In interpreted Murmel undefined variables will be created on the fly,
in compiled mode variables must have been defined previously.

    (define a nil)
    (let ((b nil) (c nil))
      (setq a 1 b 2 c (+ a b))) ; ==> 3.0

### (if condform form [optionalform]) -> result

    (if nil 'YASSS! 'OHNOOO!!!) ; ==> OHNOOO!!!

### (progn forms\*) -> result

    (if t (progn (write 'abc) (write 'def)))

### (cond (condform forms\*)\* [(t forms\*)]) -> result

### (labels ((symbol (params\*) forms\*)\*) forms\*) -> result


### (let [optsymbol] ((symbol bindingform)\*) bodyforms\*) -> result

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

### (let dynamic ((symbol bindingform)\*) bodyforms\*) -> result

Similar to `let` except: globals are not shadowed but temporarily
bound to the given value, and the previous value is restored when
leaving the scope of the `let` form.
I.e. `let dynamic` treats globals as "special".

Example:

    (setq a 1)
    (define b 2)
    (defun f () (write (cons a b)))

    (f)
    (let dynamic ((a 11) (b a))
      (f))
    (f)
will print `(1 . 2)(11 . 1)(1 . 2)`.

### (let* [optsymbol] ((symbol bindingform)\*) bodyforms\*) -> result

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

### (let* dynamic ((symbol bindingform)\*) bodyforms\*) -> result

Similar to `let*` except: globals are not shadowed but temporarily
bound to the given value, and the previous value is restored when
leaving the scope of the `let*` form.
I.e. `let* dynamic` treats globals as "special".

    (define *g* 'global)
    (defun fun () (write *g*))
    (let* dynamic ((*g* 'temp)) (fun)) ; fun will write temp
    *g* ; ==> 'global

### (letrec [optsybol] ((symbol bindingform)\*) bodyforms\*) -> result

`letrec` works like `let` and `let*` except each bindingform "sees"
all other let symbols as well as it's own symbol.
All symbols are bound, but only the preceeding bindings
are defined (have a value) while eval'ing the `bindingform`.
That way a let-bound variable could be a recursive lambda.

    (letrec ((x 1) (y (+ x 1))) (write y))


### (macrolet ((symbol params [docstring] forms)\*) forms\*) -> result

Since: 1.4.7

`macrolet` defines local macros and executes `forms` using the local definitions.
It is an error to shadow local macros with a local function (see `labels`)
or with a named-let loop label.
A docstring if given will be ignored.


### (catch tagform forms\*) -> result

Since: 1.3

`catch` is used as the destination of a non-local control transfer by `throw`.


### (throw tagform resultform) -> |

Since: 1.3

`throw` causes a non-local control transfer to a `catch` whose tag is `eq` to tag.
TODO: If there is no outstanding catch tag that matches the throw tag,
no unwinding of the stack is performed, and an error of type control-error is signaled.


### (unwind-protect protected-form cleanupforms\*) -> result

Since: 1.3

`unwind-protect` evaluates `protected-form` and guarantees that `cleanup-forms`
are executed before unwind-protect exits, whether it terminates normally or is aborted
by a control transfer of some kind.


### (try protected-form . error-obj) -> result

Since 1.4

`try` evaluates `protected-form`. If no error occurred during evaluation then
the values from `protected-form` are the final result.
If an error occurs then `try` returns the `error-obj` if given or null as the primary result,
the secondary result is the condition.

Example:

    (multiple-value-bind (result condition)
      (try (1+ most-positive-fixnum) 'error)
      (if (eq result 'error)
            (progn (write "an error occurred: " nil)
                   (write condition)
                   'bummer)
        result))
    ; ==> bummer


### (multiple-value-call function-form values-forms\*) -> result

Since: 1.2

`multiple-value-call` first evaluates the `function-form` to obtain function,
and then evaluates each `values-form`. All the values of each form
are gathered together (not just one value from each) and given as arguments
to the function.

    (multiple-value-call + 0.0 (values 1 2) 3) ; ==> 6.0

### (multiple-value-bind (symbols\*) values-form bodyforms\*) -> result

Since: 1.2

`values-form` is evaluated, and each of the `symbols` is bound
to the respective value returned by that form.
If there are more `symbols` than values returned,
`nil` is assigned to the remaining vars.
If there are more values than symbols, the excess values are discarded.
The symbols are bound to the values over the execution of the `bodyforms`,
which make up an implicit progn.

    (multiple-value-bind (a b) (values 'Hello\, '\ World!)
      (jformat nil "%s%s" a b))
    ==> "Hello, World!"

    (multiple-value-bind (a b . c) (values 1 2 3 4 5)
      (writeln a) (writeln b) (writeln c))
    ; 1
    ; 2
    ; (3 4 5)
    ; ==> (3 4 5)

    (multiple-value-bind a (values 1 2 3 4 5)
      (write a))
    ; (1 2 3 4 5)
    ; ==> (1 2 3 4 5)


### (load filespec) -> result

Since: 1.1

Eval the contents of the given file, return value
is the value returned by the last form or nil
in case of an empty file.

When compiling Murmel `load` is performed at
compile time and must appear as a toplevel form.

`filespec` is not eval'd and must be a string.
Unless filespec ends with ".lisp" the file extension
".lisp" will be appended.
If filespec is an absolute path then it will be used as is.
Otherwise the file will be searched in the same directory
as the file that contains the `load` and after that
in "libdir" (set with `--libdir`, "libdir" defaults to the
directory containing jmurmel.jar).
If `load` is entered into the REPL then the file
will be searched in the current directory and then
in the "libdir".

    (load "nul") ; ==> nil, NUL is Windows specific
    (load "lib") ; will search for lib.lisp and eval it's contents

### (require module-name optional-file-path)

Since: 1.1

Load the given file once. Murmel maintains an internal
set of loaded modules, and `require` will ignore
loading files that were already loaded by comparing
`module-name` to the set of already loaded modules.

When compiling Murmel `require` is performed at
compile time and must appear as a toplevel form.

If `optional-file-path` is omitted or nil then
`module-name` will be used as the file path.

`module-name` and `optional-file-path` are not eval'd
and must be strings.

    (require "mlib") ; will search for the file mlib.lisp
                     ; unless the module "mlib" was already loaded

### (provide module-name)

Since: 1.1

Set a file's modulename so that `require` won't
load it twice.


### (declaim (optimize ...

`declaim` currently only supports `optimize`, others will be ignored.
`optimize` only supports speed, others will be ignored.


## Function application 

### function call

Applies the operator returned by operatorform to
the eval'd operands

    (operatorform operands*)


## Backquote 

Backquote "\`" starts "fill-in templates":
backquote, comma and comma-at work similar to CL,
except: comma-dot is not supported.

    (setq a 'a-val) (setq b 'b-val) (define c 'c-val)
    (define d '(d-val1 d-val2))
    `((,a b) ,c ,@d)      ; ==> ((a-val b) c-val d-val1 d-val2)

    (define y 'b) (define l '(a b))
    (eval ``(,a ,,@l ,,y) '((a . a) (b . b) (y . y)))
    ; ==> (a-val a-val b-val b-val)

    (define x '(1 2 3))
    `(normal= ,x splicing= ,@x see?)
    ; ==> (normal= (1 2 3) splicing= 1 2 3 see?)

    `(normal= ,x fakesplicing= . ,x)
    ; ==> (normal= (1 2 3) fakesplicing= 1 2 3)


## Basic Primitives 

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


## Logic, predicates

### (eq x y) -> boolean

Returns `t` if `x` and `y` are the same object, `nil` otherwise.

### (eql x y) -> boolean

Return `t` if any of the following is true

- `a` and `b` are `eq`
- `a` and `b` are numbers of the same type and have the same value
- `a` and `b` are the same characters

Examples:

    (eql 2 2) ; ==> t
    (eql #\a (sref "aaa" 0)) ; ==> t
    (eql -0.0 0.0) ; ==> nil

### (equal a b) -> boolean

Since: 1.4

Return `t` if any of the following is true:

- `a` and `b` are `eql`
- `a` and `b` are strings that have the same text value
- `a` and `b` are bitvectors whose elements are eql
- `a` and `b` are conses whose car and cdr are `equal` respectively


### consp, atom, symbolp, null, listp

### numberp, integerp, floatp

- `numberp` returns `t` for all Murmel and Java number types.
- `integerp` returns `t` for all Murmel and Java integral number types.
- `floatp` returns `t` for Murmel and Java decimal number types.

### characterp

### random-state-p

Since: 1.4.3

### hash-table-p

Since: 1.4

### functionp

Since: 1.3

### vectorp, simple-vector-p
Since: 1.3

### stringp

Return `t` for Murmel strings and all Java Objects implementing java.lang.CharSequence

### simple-string-p, bit-vector-p, simple-bit-vector-p,
Since: 1.3

### (typep obj typespec) -> boolean

Since: 1.4

`typep` returns `t` if `obj` is of type `typespec` or of a subtype.

### (adjustable-array-p obj) -> boolean

Since: 1.3

`adjustable-array-p` returns `t` if `obj` is an adjustable vector.

Note that in Murmel `(adjustable-array-p 1)` returns nil
while in Common Lisp that would signal a `type-error`. 


## Conses and lists

### (car list) -> 1st element of list

    (car '(a b c)) ; ==> a

### (cdr list) -> rest of list

    (cdr '(a b c)) ; ==> (b c)

### (cons e1 e2) -> conscell

    (cons 'a 'b) ; ==> (a . b)

### rplaca, rplacd

Replace the value of the CAR or CDR slot of a cons cell.

    (setq l '(1 2))
    (rplaca l 11) ; ==> (11 2)
    (rplacd l 22) ; ==> (11 . 22)

### (list elems\*) -> list<br/>(list\* elems+) -> atom-or-dotted-list

`list` will create a list consisting of it's arguments,
`list*` will create a dotted list.

    (list) ; ==> nil
    (list 1) ; ==> (1)
    (list 1 2 3) ; ==> (1 2 3)

    (list* 1) ; ==> 1
    (list* 1 2 3) ; ==> (1 2 . 3)

### (append lists\*) -> list

`append` nondestructively append it's arguments. All arguments except the last
are shallow copied, all arguments except the last must be proper lists.

    (append)                    ; ==> nil
    (append 'a)                 ; ==> a
    (append '(a b) 'c)          ; ==> (a b . c)
    (append '(a b) '(c d))      ; ==> (a b c d)
    (append '(a b) '(c . d))    ; ==> (a b c . d)

### (assq key alist) -> cons or nil

Since: 1.2

`assq` is similar to `assoc` except `assq` compares keys using `eq`.

### (assoc key alist) -> cons or nil

`assoc` takes a key and a list of key/value tupels (lists or conses).
The return value is the first cons whose car is equal(\*) to `key`
or `nil` if no such cons was found. `nil`-elements in `alist` are ignored.

(\*) `assoc` compares two items as if `eql` was used.
    `assoc` considers two items as "equal" if

- Both are `eq` (are the same object)
- Both are integers or floats or characters respectively and have the same value

Examples:

    (assoc 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4)))
      ; ==> (a-key 3)
    (cdr
      (assoc 'a-key '((key-1 . 1) (key-2 . 2) (a-key . 3) (key-4 . 4))))
      ; ==> 3
    (assoc nil '((key-1 1) nil (nil 2) (a-key 3) (key-4 4)))
      ; ==> (nil 2)


## Vectors, Sequences


### (make-array length [element-type [adjustablep-or-capacity]]) -> vector

Since: 1.3

Only one-dimensional arrays of element-type `t`, `'bit` or `'character` are supported.

### vector-length, vector-copy, vector-fill, vector-add, vector->list, list->vector
    (vector-length v) -> length
    (vector-copy v [adjustablep]) -> fresh-copy
    (vector-fill v new-elem) -> vector
    (vector-add v elem [pos]) -> position-of-added-element
    (vector->list v) -> list
    (list->vector lst [adjustablep]) -> vector

Since: 1.3

`vector-add` is similar to CL's `vector-push-extend` but with swapped parameters,
and Murmel's `vector-add` accepts an optional parameter `pos`.

### vector-remove
    (vector-remove adjustable-vector pos) -> removed-element

Since: 1.4.7

`vector-remove` removes the element at index `pos`, moves the following elements
and decreases the vector's size.

### svlength, svref, svset, vector

Since: 1.3

Simple vectors are one-dimensional arrays
(implementation note: or objects of class java.lang.String in case of string literals).

Example usage:

    (define *v* (vector 1 2 3))
    (vectorp *v*) ; ==> t
    (svlength *v*) ; ==> 3
    (svref *v* 1) ; ==> 2

### list->simple-vector and simple-vector->list

Since: 1.3


### string
    (string X) -> string

Since: 1.4.1

`string` coerces `X` into a mutable string.
If `X` is a mutable string, `X` is returned.
If `X` is an immutable string, a fresh mutable simple string is returned.
If `X` is a symbol, its name is returned.
If `X` is a character then a one element string containing that character is returned.
If `X` cannot be coerced into a string, an error occurs.

### slength, sref, sset
    (slength str) -> length
    (sref    str n) -> nth-character
    (sset    str n new-char) -> new-char

Since: 1.3

`sref` returns the n-th character of the string `str`, `n` is 0-based.
Similar to CL `char`.

`sset` sets the n-th character of `str` to `new-char`.
Using `sset` with a string literal is an error.

### (string= s1 s2) -> boolean

### (string->list str) -> list-of-characters

### (list->string list-of-characters [adjustablep]) -> string

### (code-char integer) -> character

### (char-code character) -> integer


### bvlength, bvref, bvset, bv=
    (bvref bitvector n) -> nth-bit
    (bvset bitvector n new-bit) -> new-bit

Since: 1.3

`bvref` returns the n-th bit of the bitvector `bitvector`, `n` is 0-based.
Similar to CL `bit`.

`bvset` sets the n-th bit of `bitvector` to `new-bit`.

### bit-vector->list, list->bit-vector
    (bit-vector->list bv) -> list-of-bits
    (list->bit-vector lst [adjustablep]) -> bitvector

Since: 1.3

### seqref, seqset
    (seqref seq n) -> nth-element
    (seqset seq n new-element) -> new-element

Since: 1.3

`seqref` is similar to CL `elt` function.
Murmel's `seqref` and `seqset` will handle dotted lists, though.

    (seqref "abc" 2) ; ==> #\c
    (seqref #(0 1 2 3) 3)  ; ==> 3
    (seqref '(0 1 2 . 3) 3) ; ==> 3

    (let ((l (list* 0 1 2)))
      (seqset l 2 22) l)
      ; ==> (0 1 . 22)


## Hash-tables

### hash-table-p, hash, make-hash-table, hashref, hashset,<br/>hash-table-count, clrhash, hash-table-remove

    (hash-table-p hash-table) -> boolean
    (hash [test [key1 value1 key2 value2...]]) -> hash-table
    (make-hash-table [test [size]]) -> hash-table
    (hashref hash-table key [default]) -> value, was-present-p
    (hashset hash-table key value) -> value
    (hashset generator value) -> value
    (hash-table-count hash-table) -> number-of-entries
    (clrhash hash-table) -> hash-table
    (hash-table-remove hash-table key) -> was-present-p 
    (hash-table-remove generator) -> was-present-p 

Since: 1.4

`hash-table-p, make-hash-table, hash-table-count, clrhash`
are similar to CL's functions with the same name.

`test` defaults to `eql` and is currently limited to
`nil, t, eq, eql, compare-eql, equal, compare-equal`.
When `t` is specified as `key` then object comparisons are done using
Java's `Object.equals()` method.

Implementation note: hashtables with `compare-eql` or `compare-equal`
are actually implemented as trees, so operations will be O(log n).
hashtables with `eq, eql, equal` or `t` are real hashtables, so operations will be O(n).

### sxhash

    (sxhash obj) -> hash-code

Since: 1.4

For two objects that are `equal` `sxhash` will return the same hash-code.

### scan-hash-table

    (scan-hash-table hash) -> key-value-generator

Since: 1.4

`scan-hash-table` returns a function of no arguments that will
on subsequent invocations return `(key . value), valid?`.

Murmel's (and [mlib's](mlib.md)) generators are somewhat similar to what
SRFI 158 https://srfi.schemers.org/srfi-158/srfi-158.html
provides.


## I/O

### read

    (read [eof-obj]) -> obj

Read an S-expression from stdin.

`read` w/o an argument will throw an error when encountering EOF.
If an optional argument was given then EOF does not throw an error
but the given argument is returned, e.g.:

    C:\> echo (read)| java -jar lambda\target\jmurmel.jar
    Error: read: EOF
    error occurred in line 1:1..1:5: (read)

but

    C:\> echo (read 'xyxxy)| java -jar lambda\target\jmurmel.jar
    ==> xyxxy

Note that if an EOF occurs while reading an S-expression an error
will be thrown even when `eof-obj` was used. 


### write, writeln, lnwrite

    (write   obj  [print-escape-p [dest]]) -> obj
    (writeln [obj [print-escape-p [dest]]]) -> obj
    (lnwrite [obj [print-escape-p [dest]]]) -> obj

Write a Lisp object as an S-expression to `dest`.

`write` accepts an optional boolean argument `print-escape-p`.
`writeln` and `lnwrite` accept an optional argument `obj`
and an optional boolean argument `print-escape-p`.

`write`, `writeln` and `lnwrite` accept an optional argument `dest`
which may be `t` or an adjustable string.
`t` means: write to stdout, else write to `dest`.
(Implementation note: any Java object implementing `java.lang.Appendable`
is acceptable.)

`writeln` will write the argument if given, followed by a newline.  
`lnwrite` will write a newline followed by the argument if given,
followed by a ' ', i.e. writeln is C-style, lnwrite is Lisp-style.

    (writeln "Hello, ")
    (writeln)
    (writeln "World!")

`write`, `writeln` and `lnwrite` will escape atoms by default,
the optioal parameter `print-escape-p` can be used to turn off escaping:

    (writeln "Hello, World!" nil)


### (fresh-line [dest]) -> boolean

Since: 1.5

Print an EOL character (-sequence) unless already at the beginning of line.


### (tabulate relativep colnum colinc) -> nil

Since: 1.5

If `relativep` is nil then output sufficient spaces to move the cursor to column `colnum`.
If the cursor was already at or beyond column `colnum` and `colinc` is non-zero,
then output spaces to move the cursor to column `colnum+k*colinc` for the smallest positive integer `k` possible.

If `relativep` is non-nil then output `colnum` spaces and then output the smallest non-negative number of additional spaces
necessary to move the cursor to a column that is a multiple of `colinc`.


### (make-string-writer) -> writer

Since: 1.5

Create a Lisp writer that writes to an internal buffer.
The writer object can also be used as a string.

    (define s (make-string-writer)) ; ==> s
    (typep s 'string)               ; ==> t
    (write 'abc nil s)              ; ==> abc
    (slength s)                     ; ==> 3
    (string->list s)                ; ==> (#\a #\b #\c)


### (write-to-string obj [print-escape-p]) -> result-string

Since: 1.4

Convert `obj` to it's string representation.


### (read-from-string str [eof-obj [start [end]]]) -> object, position

Since: 1.4

Similar to CL's `read-from-string` which has more parameters.
If `eof-obj` is not given or `nil` then end-of-file during reading
will be signalled as an `end-of-file` condition.

    (read-from-string "123 456" nil 4)     ; ==> 456, 7
    (read-from-string "123 456" 'eof 7)    ; ==> eof, 7
    (read-from-string "123 456   " 'eof 7) ; ==> eof, 10
    (read-from-string "123 456" nil 1 4)   ; ==> 23, 4
    (read-from-string "          " 'eof 5) ; ==> eof, 10


### (read-textfile-lines filename [charset]) -> result-string-vector

Since: 1.4

Read the file with the given filename (or stdin if `filename` is `t`) and return it's contents
as a `simple-vector` containing one `simple-string` for each line.

If `charset` is `nil` then `UTF-8` will be used.


### (read-textfile filename [charset [translate-lineend-p]]) -> result-string

Since: 1.4

Read the file with the given filename (or stdin if `filename` is `t`)
and return it's contents as a mutable `string`.

If `charset` is `nil` then `UTF-8` will be used.

If `translate-lineend-p` is omitted or non-nil then (possibly OS-dependent)
lineend character sequences will be normalized to `#\Newline`.

`charset` defaults to UTF-8.


### (write-textfile-lines filename string-sequence  [appendp [charset [translate-lineend-p]]]) -> nil

Since: 1.4

Write all elements (which must be of type `string`)
of `string-sequence` to the file `filename` (or stdout if `filename` is `t`).
Each element (line) will be terminated with the OS-default line-end character(-sequence).
(Note: with the JVM this can be set using -Dline.separator.)

If `charset` is `nil` then `UTF-8` will be used.

If `translate-lineend-p` is omitted or non-nil then each #\Newline character
will be written as the OS-dependent line-end character(-sequence).


### (write-textfile filename string  [appendp [charset [translate-lineend-p]]]) -> nil

Since: 1.4

Write `string` to the file `filename` (or stdout if `filename` is `t`).

If `charset` is `nil` then `UTF-8` will be used.

If `translate-lineend-p` is omitted or non-nil then each #\Newline character
will be written as the OS-dependent line-end character(-sequence).


### (jformat dest formatstr args*), (jformat-locale dest locale formatstr args*)

The first argument `dest` can be `t`, `nil` or an adjustable string.

`jformat t` writes a formatted string to stdout and returns `nil`.
`jformat`'s parameters work as with `java.lang.String.jformat()`
which is similar to C's `printf()`.

    (jformat t
      "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

`jformat-locale` works similar to jformat except it has an additional
second string parameter that should be a locale, `nil` means use Java's
default locale.

    (jformat-locale t "de-DE"
      "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

`jformat nil` and `jformat-locale nil` work similar
to `jformat t` and `jformat-locale t` except they don't write to stdout
but return the string.

    (jformat-locale nil "de-DE"
      "a string: %s, a number: %g, a newline:%n" "The String" 3.14)


### Misc

### (values object\*) -> multiple-values

Since: 1.2

`values` returns the objects as multiple values.

### (gensym [optional-name]) -> uninterned symbol

Return a new uninterned symbol.
    (gensym)

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

### (jerror datum . arguments) -> |

Since 1.5

Similar to CL's `error`.
`datum` can also be a condition,
e.g. to re-raise a condition that is not handled.

    (try (jerror 'simple-error "an error occurred") 'err)
      ; -> err
      ; -> simple-error - an error occurred

    (writeln (try (multiple-value-bind (result condition)
                                       (try (jerror 'simple-error "an error occurred") 'err)
                    (if (eq result 'err)
                        (if (typep condition 'arithmetic-error)
                            (writeln "an arithmetic error occurred")
                            (progn (writeln "another error occurred, rethrowing")
                                   (jerror condition)))
                        (writeln "no error")))

                  'outer-err))  ; ==> outer-err


### lisp-implementation-type, lisp-implementation-version

Since: 1.4.5

Similar to CL's functions of the same name.


## Time

### (get-internal-real-time) -> number

Walltime in internal time units, relative to an arbitrary time base

    (get-internal-real-time)

### (get-internal-run-time) -> number

User cpu time in internal time units

    (get-internal-run-time)

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


## Predefined Numeric Primitives 

Murmel's two numeric datatypes are implemented as `long` (54 bit signed integer)
and `double` (64 bit floating point).
Other Java datatypes (which may occur when using Java FFI) will be automatically converted
(with null-, lost-precision- and over/underflow checks) as appropriate.

Murmel does most maths in double precision:

- Except with `1+, 1-, ceiling, floor, round, truncate and signum`,
  arguments to numeric functions will be converted to `double`,
  and the result will be `double`.  
  Integer over/underflow during argument conversion will be signalled as an error.
  Results for large numbers (i.e. over/ underflow) will go towards +/- Infinity.
- `1+, 1-`: Byte, Short, Integer and Long arguments will be converted to long,
  in that case the result type is long. Over/ underflow of the result will be signalled as an error.
  Other argument types will be handled as above.
- `ceiling, floor, round, truncate`: result type is long, over/ underflow will be signalled as an error.
- `signum`: result is a "signed prototype" - either `long -1/0/1` or `double -1.0/-0.0/0.0/1.0`.

### +, -, \*, /, mod, rem, sqrt, log, log10, exp, expt

The math operators accept numbers only.
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

### (random limit [random-state]) -> random-number

Since: 1.4.3

Similar to CL's `random`.

### (make-random-state [state]) -> new-state

Since: 1.4.3

Similar to CL's `make-random-state` with the following differences:
- If `state` is a number then it will be truncated to an integer
  and used as a seed for the newly created random state.
- Murmel's `random-state` objects will print unreadably.


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

    (let ((window-title "test")
          (optwidthpixels 100)
          (optheightpixels 100)
          (optpaddingpixels 10))
      (make-frame window-title
        optwidthpixels optheightpixels optpaddingpixels))
      ; ==> frame

### (current-frame) -> frame

Returns current frame.

### (current-frame optional-frame) -> frame

Set new current frame, returns previous current frame.

### More frame-functions

`open-frame` ... make frame visible  
`close-frame` ... hide frame  
`reset-frame` ... reset pen to "down", turtle position and angle, color and bgcolor    
`clear-frame` ... reset frame and discard frame contents  
`repaint-frame` ... force full repaint  
`flush-frame` ... paint operations won't take immediate effect, flush-frame makes them visible  
`pen-up` ... subsequent line operations will only move the position  
`pen-down` ... subsequent line operations will have visible effect  
`push-pos` ... save current position and angle  
`pop-pos` ... restore previous position and angle

The above functions all take one optional frame parameter. If omitted or `nil`
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

    (lambda dynamic (params*) forms*) -> anonymous function with
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

### (trace function-name\*) -> trace-result<br/> (untrace function-name\*) -> untrace-result

Arguments to trace/ untrace are eval'd.
In interpreted code these work similar to CL's trace/ untrace macros.
Only interpreted code will be traced (user defined functions as well as
interpreter primities, primitives are only traced if speed=0).

    (trace 'write '+)  
    (write (+ 1 2))  
    (untrace)


## Java FFI

### (jmethod classname methodname paramclasses\*) -> primitive

The primitive `jmethod` will return a newly created primitive
that is implemented by the method `methodname` of the Java class
`classname` that has the formal parameters `paramclasses*`.
Parameters to `jmethod` must be strings.

    (jmethod "java.lang.System" "currentTimeMillis")

When invoking primitives created by `jmethod` the first argument must be
a Java object of the primitive's method's class. This is not the case
for static methods.

Invoke a static method:

    (define ctm (jmethod "java.lang.System" "currentTimeMillis"))  
    (ctm) ; ==> 1704017140837 (actual result will vary)

Invoke an instance method on an object:

    (define make-hash (jmethod "java.util.HashMap" "new"))
    (define put-hash (jmethod "java.util.Map" "put"
                              "java.lang.Object" "java.lang.Object"))
    (define my-hash (make-hash))
    (put-hash my-hash "key-1" "value-1")
    (write my-hash)  ; ==> #H(t "key-1" "value-1")


### (jproxy interfacename [javamethodname symbol-or-lambda]\*) -> Java-object

Since: 1.2

The primitive `jproxy` takes a Java interface name and zero or more
methodname/ murmelcode tupels, and returns a Java object that implements
`interfacename`.


## Known issues 

Murmel language:

- eval-when
- multithreading

Compiler issues:

- `define` and `defun` only work as top level forms
  or within `progn, let, let*, letrec, labels, macrolet` or `multiple-value-bind` forms,
  other use as non-toplevel form will throw a "not-yet-implemented" compiler error.
- `defmacro` only works as a top level form
  or within `progn, macrolet` or `labels` forms,
- `define`-ing an already `define`-d symbol is not supported
- `macroexpand-1` is limited to quoted forms
- macro expansion is only done at compiletime, e.g. `(defmacro m() 1) (eval '(m))`
  won't work
- reassigning predefined primitives with `setq` is not supported, e.g.
  `(setq truncate (lambda (p) (1+ p)))` is an error (and wouldn't make much sense)


## Copyright 

Murmel and JMurmel are Copyright (C) 2020-2024 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT.
