
;;;
;;; === Murmel Language Reference ====
;;;
;;; This file is an executable language reference manual
;;; for Murmel, a single-namespace Lisp dialect.
;;;
;;; You can read this file or run it with:
;;; java -jar jmurmel.jar --tty --echo < jmurmel-langref.lisp
;;; or:
;;; java -jar jmurmel.jar --tty --echo < murmel-langref.lisp > murmel.txt
;;;
;;; Note that many of these forms, predefined variables, ... can be
;;; disabled using commandline arguments if you want to play
;;; with an even more reduced Lisp.
;;;
;;; See also:
;;; java -jar jmurmel.jar --help ... will print usage information
;;; JMurmel> :h                  ... will print REPL command help


;;; == Introduction ===================
;;;
;;; "Hello, World!" program:

(write "Hello, World!")

;;; the expression above should print the famous "Hello, World!"
;;; followed by the result
;;; ==> t
;;; and the prompt "JMurmel>"


;;; == Terminology ====================
;;;
;;; Murmel is a Lisp dialect. As such the language isn't
;;; really defined in terms of program text but in terms
;;; of in-memory objects that will be passed to eval.
;;;
;;; That said, Murmel's default parser turns S-expressions
;;; in the input file into equivalent in-memory objects. So
;;; for this reference we'll just pretend Murmel was defined
;;; in terms of S-expressions, and use S-expressions to describe
;;; expressions that are valid Murmel, i.e. are acceptable to eval.
;;; In this reference in-memory objects (or sloppily: S-expressions)
;;; that are valid Murmel are referred to as "forms". 
;;;


;;; == S-expressions ==================

; a single quote is a shorthand for (quote an-expression)
'an-expression

; returns a dotted pair       ==> (a . b)
'(a . b)

; returns a dotted list       ==> (a b c . d)
'(a . (b . (c . d)))

; shorthand for dotted list   ==> (a b c . d)
'(a b c . d)

; returns a proper list       ==> (a b c)
'(a . (b . (c . ())))

; shorthand for a proper list ==> (a b c)
'(a b c)


;;; == Basic Special Forms ============

;;; (quote symbol) -> symbol
; quote returns an expression w/o evaluating it
(quote a-symbol)

;;; (lambda (params...) forms...) -> lambda or closure
; In --dyn mode no environment is captured,
; lambdas - when applied - get the dynamic environment.
; In --lex mode the dynamic global environment plus
; the lexical environment are captured at the time of
; lambda creation.
; arguments to lambda are not evaluated
(lambda (p1 p2) p1)


;;; == Data types =====================
;;;
;;; Murmel supports symbols, lists, double precision numbers and strings

; a symbol
'*a-sample-symbol*

; Guess. Max symbol length is 2000 chars, anything longer
; will be silently truncated
'a\ symbol\ with\ spaces!

; empty list, printed as "nil"
'()

; shorthand for empty list
nil

; an integer number
; tokens that consist of a sign or digit followed by digits
; are interpreted as integer numbers (java.lang.Long)
1

; a number in double precision
; numbers that contain the characters '.eE' are interpreted
; as double precision (java.lang.Double)
1.

; a number in scientific notation
1e3

; strings literals are max length 2000 chars, too
"a string literal"
"another 'literal' \"string literal\""


;;; == Additional Special Forms =======

;;; (define symbol object) -> symbol
; define associates symbols in the global environment
; with expressions. Redefining already defined symbols
; is an error.
; The first argument is not evaluated, the second is
(define *global-var* 42)
(define f1 (lambda (p1 p2) (+ p1 p2)))

;;; (defun symbol (params...) forms...) -> symbol
; defun is a shorthand for defining functions
; arguments to defun are not evaluated
(defun f2 (p1 p2) (+ p1 p2))

;;; (eval form) -> object
; form will be eval'd, it must return a form
(eval '(+ 1 2))

;;; (if condform form optionalform) -> object

;;; (progn expr...) -> object
(if t (progn (write 'abc) (write 'def)))

;;; (cond (condform forms...)... ) -> object

;;; (labels ((symbol (params...) forms...)...) forms...) -> object

;;; (let* optsymbol? (bindings...) forms...) -> object
; works like let* of others Lisps
; each binding "sees" the previous ones, optsymbol if given will be bound
; to "forms..." inside forms... for recursive calls
(let* loop ((x 3) (msg 'hi))
           (if (= x 0) msg (progn (write (floor x)) (loop (- x 1) msg))))

;;; (letrec (bindings...) expr...) -> object
; works like let and let* of others Lisps
; except each binding "sees" the previous as well as itself.
; that way a let-bound variable could be a recursive lambda
(letrec ((x 1) (y (+ x 1))) (write y))

;;; (apply form argform) -> object
; form must return a primitive or lambda
; argform must return a proper list
; e.g. apply the function + to the arguments 1 and 2
(apply + '(1 2))

;;; function call
; applies the operator returned by operatorform to
; the eval'd operands
; (operatorform operands...)

;;; == Predefined Symbols =============

; nil and t are pre-defined self-evaluating symbols
nil
t

; resolution of the time related functions
internal-time-units-per-second


;;; == Predefined Primitives ==========

; (cons 'a 'b) ==> (a . b)
(cons 'a 'b)

; (car '(a b c)) ==> a
(car '(a b c))

; (cdr '(a b c)) ==> (b c)
(cdr '(a b c))

; eq, atom, consp, listp, symbolp, numberp, stringp, null?

; assoc takes a key and a list of key/value tupels (lists or conses)
(assoc 'a-key '((key-1 1) (key-2 2) (a-key 3) (key-4 4)))
(cdr (assoc 'a-key '((key-1 . 1) (key-2 . 2) (a-key . 3) (key-4 . 4))))

; +, -, *, /, mod
; The math operators accept numbers only, but otherwise should
; work as expected, all numeric operators return a double
; eg. (+ number number) -> double
(+ 1 1)

; round, floor, ceiling
; These operators return an integer (java.lang.Long)
; eg. (floor number) -> integer
(floor 1.)

; = < > <= >=
(= 1 1.0)

; Both expressions as well as data are read from stdin.
; The following expression reads the expression immediately following it
; (in this case the expression to be read is the string "Hello!").
(write (read)) "Hello!"

; writeln accepts one optional argument
(writeln "Hello, ")
(writeln)
(writeln "World!")

; format writes a formatted string to stdout and returns t.
; format's parameters work as java.lang.String.format().
(format "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

; format-locale works similar to format except it has an additional
; first string parameter that should be a locale, nil means use Java's
; default locale.
(format-locale
   "de-DE" "a string: %s, a number: %g, a newline:%n" "The String" 3.14)

; string-format string-format-locale work similar
; to format and format-locale except they don't write to stdout
; but return the string
(string-format-locale
   "de-DE" "a string: %s, a number: %g, a newline:%n" "The String" 3.14)
               
; walltime in internal time units, relative to an arbitrary time base
(get-internal-real-time)

; user cpu time in internal time units
(get-internal-run-time)

; user + system cpu time in internal time units
(get-internal-cpu-time)

; pause execution for x internal time units, returns actually slept
; wall time in internal time units
(sleep (* 1 internal-time-units-per-second))

; get-universal-time <no arguments> => seconds since 1.1.1900 00:00:00 UTC
; this will return approx. years since 1.1.1900
(/ (get-universal-time) 60 60 24 365)

; get-decoded-time <no arguments> => second, minute, hour, date, month, year, day, daylight-p, zone
(get-decoded-time)


;;; == Copyright ======================
;;;
;;; Murmel and JMurmel are Copyright (C) 2020 Robert Mayer. All rights reserved.
;;;
;;; This work is licensed under the terms of the MIT license.
;;; For a copy, see https://opensource.org/licenses/MIT.

;;; At the end of the input file JMurmel will print "bye." and exit.

;;; $Id: lambdaj-langref.lisp,v 1.12 2020/10/26 07:42:58 Robert Exp $