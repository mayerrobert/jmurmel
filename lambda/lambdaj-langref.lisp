
;;;
;;; === LambdaJ Language Reference ====
;;;
;;; This file is an executable language reference manual
;;; for LambdaJ.
;;;
;;; You can read this file or run it with:
;;; java -jar lambdaj.jar --tty --echo < lambdaj-langref.lisp
;;; or:
;;; java -jar lambdaj.jar --tty --echo <lambdaj-langref.lisp >session.txt
;;;
;;; Note that many of these forms, predefined variables can be
;;; disabled using commandline arguments if you want to play
;;; with an even more reduced Lisp.
;;;
;;; See also:
;;; java -jar lambdaj.jar --help ... will print usage information
;;; LambdaJ> :h                  ... will print REPL command help


;;; == Introduction ===================
;;;
;;; "Hello, World!" program:

(write "Hello, World!")

;;; the expression above should print the famous "Hello, World!"
;;; followed by the result
;;; ==> t
;;; and the prompt "LambdaJ>"


;;; == Basic Special Forms ============

;;; (lambda (params...) forms...) -> lambda or closure
; lambda returns a lambda expression
; arguments are not evaluated
(lambda (p1 p2) p1)

;;; (quote symbol) -> symbol
; quote returns an expression w/o evaluating it
(quote a-symbol)

; a single quote is a shorthand for (quote a-symbol)
'a-symbol


;;; == S-expressions ==================

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


;;; == Data types =====================
;;;
;;; LambdaJ supports symbols, lists, double precision numbers and strings

; a symbol
'*a-sample-symbol*

; Guess. Max symbol length is 2000 chars, anything longer
; will be silently truncated
'a\ symbol\ with\ spaces!

; empty list, printed as "nil"
'()

; shorthand for empty list
nil

; a number, currently double precision only
1

; a number in scientific notation
1e3

; strings literals are max length 2000 chars, too
"a string literal"
"another 'literal' \"string literal\""


;;; == Additional Special Forms =======

; the following special forms should work as expected
; cond
; labels
; if

;;; (define symbol object) -> object
; define associates symbols in the global environment
; with expressions. Redefining already defined symbols
; is an error.
; The first argument is not evaluated, the second is
(define *global-var* 42)
(define f1 (lambda (p1 p2) (+ p1 p2)))

;;; (defun symbol (params...) forms...) -> lambda or closure
; defun is a shorthand for defining functions
; arguments to defun are not evaluated
(defun f2 (p1 p2) (+ p1 p2))

;;; (letrec (bindings...) forms...) -> object
; works like let and let* of others Lisps
; except each binding "sees" the previous as well as itself
(letrec ((x 1) (y (+ x 1))) (write y))

;;; (apply expr arglist) -> object
; apply the function + to the arguments 1 and 2
; expects 2 arguments which both will be evaluated
(apply + '(1 2))

;;; (progn forms...) -> object
(if t (progn (write 'abc) (write 'def)))


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

; The following operators accept numbers only, but otherwise should
; work as expected. 
; + - * /
; mod
; = < > <= >=

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
; first string parameter that should be a locale, nil means Java's
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
;;; LambdaJ is Copyright (C) 2020 Robert Mayer. All rights reserved.
;;;
;;; This work is licensed under the terms of the MIT license.
;;; For a copy, see https://opensource.org/licenses/MIT.

;;; At the end of the input file LambdaJ will print "bye." and exit.

;;; $Id: lambdaj-langref.lisp,v 1.8 2020/10/24 16:56:29 Robert Exp $