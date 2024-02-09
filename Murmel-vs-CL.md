# Murmel vs Common Lisp

Disclaimer: this document is WIP and incomplete, i.e. doesn't fully reflect the current status.
Some changes/ omissions may go away in future versions of Murmel.


Murmel is inspired by Common Lisp but is not quite a subset in the sense of the ANSI spec https://www.lispworks.com/documentation/lw51/CLHS/Body/01_g.htm.
Murmel is somewhat close to but not really a "subset of Common Lisp as specified by ANSI __standard number__."


## Changes:

- Murmel is a Lisp-1
- special variables work differently: Murmel doesn't have special variables but dynamic bindings via `(let dynamic (...`
- vararg lambda lists are specified as a as a dotted list (CL has &rest)
- the reader macro `#!` is used for multi-line comments
- the default library is not automatically available, i.e. programs need to `(require "mlib")` to use library functions
- floating point by default is `double-float`
- math functions such as `+` return a `double-float`
- REPL variables are prefixed by @, e.g. `@*`


## Extensions:

- `letrec`
- named `let, let*, letrec`
- the default library "Mlib" provides various functions and macros not available in Common Lisp


## Omissions:

- no keyword arguments, e.g.

    (write "bla" :escape nil :stream t)  ; CL
    (write "bla" nil t)  ; Murmel

- no `tagbody/go`, no `loop` macro
- the numeric tower is tiny: no bigints, ratios, complex
- no multi-dimension arrays
- the reader rejects numbers with a trailing dot, i.e. `1.` is the integer 1 in Common Lisp but an `reader-error` in Murmel
- no symbol macros
- no userdefined reader macros
- no `#.` reader macro
- the reader doesn't eval anything
- various other special forms and functions from the ANSI spec are missing
