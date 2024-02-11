# Murmel vs Common Lisp

Disclaimer: this document is WIP and probably incomplete, i.e. doesn't fully reflect the current status.
Some changes/ omissions may go away in future versions of Murmel.

Murmel is inspired by Common Lisp but is not quite a subset in the sense of the ANSI spec https://www.lispworks.com/documentation/lw51/CLHS/Body/01_g.htm.
Murmel is somewhat close to but not really a "subset of Common Lisp as specified by ANSI _standard number_."

Changes, extensions and omissions are listed below.
The list looks long, but a surprising amount of CL code (at least simple CL code snippets) just works,
or works after some massaging which may or may not introduce some `#+murmel`.


## Changes:

* Murmel is a Lisp-1
* special variables work differently: Murmel doesn't have special variables but dynamic bindings via `(let dynamic (...`.  
  CL's `defparameter` or `defvar` create special variables, re-binding them will be dynamic;  
  Murmel's `define` creates global variables that by default will be shadowed by local variables or parameters.
* vararg lambda lists are specified as a as a dotted list (CL has `&rest` and `&body`)
* the reader macro `#!` is used for multi-line comments
* the default library is not automatically available, i.e. programs need to `(require "mlib")` to use library functions
* floating point by default is `double-float`
* math functions such as `+` return a `double-float` regardless of their arguments
* the numeric tower is tiny: no `bignum`, `ratio`, `complex`, Murmel only has `fixnum` and `double-float`
* REPL variables are prefixed by @, e.g. `@*`
* `format`'s syntax currently is different, e.g. Murmel: `%s` vs. CL: `~A`
* `load` and `require` are performed at compile time
* some functions have different parameters because Murmel doesn't have keyword arguments, see "Omissions" below


## Extensions:

* `letrec`
* named `let, let*, letrec`
* hash literals
* the default library "Mlib" provides various functions and macros not available in Common Lisp
  (most of Mlib's extensions are available through commonly used CL-libraries such as alexandria or serapeum, though)


## Omissions:

### Surface syntax

* no special handling for function names that are lists like `(setf ...)`
* no keyword arguments, e.g.

    - `(write "bla" :escape nil :stream t)  ; CL`
    - `(write "bla" nil t)  ; Murmel`

* no special handling for `:keywords`
* no optional or default parameters: CL has `&optional` and `&default`,
  in Murmel the last parameter in a dotted lambda list has to be processed manually
* no symbol macros
* no userdefined reader macros
* no `#.` reader macro
* the reader rejects numbers with a trailing dot, i.e. `1.` is the integer 1 in Common Lisp but a `reader-error` in Murmel
* the reader doesn't eval anything

### Special forms

* no `eval-when`, no `load-time-value`
* no `function`, no `tagbody/go`, no `block/return-from`, no `flet`, no `the`, no `locally`
* no local macros via `macrolet` or `symbol-macrolet`

### Library

* no multi-dimension arrays, specialized arrays only accept `bit` and `character`
* no `loop` macro
* no `defstruct`
* no CLOS (Murmel has some conditions related stuff, though)
* various other special forms, global variables and functions from the ANSI spec are missing

### System

* no package system, i.e. no `defpackage`, no `in-package`
