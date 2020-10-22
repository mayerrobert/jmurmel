
;;;
;;; === LambdaJ Reference ===
;;;
;;; This file is an executable language reference manual
;;; for LambdaJ.
;;;
;;; You can read this file or run it with:
;;; java -jar lambdaj.jar --tty --echo < lambdaj-langref.lisp
;;;
;;; See also:
;;; java -jar lambdaj.jar --help ... will print usage information
;;; LambdaJ> :h                  ... will print REPL command help


;;; == Introduction ==
;;;
;;; "Hello, World!" program:

(write "Hello, World!")

;;; the expression above should print the famous "Hello, World!"
;;; followed by the result
;;; ==> t
;;; and the prompt "LambdaJ>"


;;; == Basic Special Forms ===

; returns a lambda expression
(lambda (p1 p2) p1)

; returns an expression w/o evaluating it
(quote a-symbol)

; shorthand for (quote a-symbol)
'a-symbol


;;; == S-expressions ==

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


;;; == Data types ==
;;;
;;; LambdaJ supports symbols, lists, double precision numbers and strings

; a symbol
'*a-sample-symbol*

; guess. Max length is 2000 chars
'a\ symbol\ with\ spaces

; empty list, printed as "nil"
'()

; shorthand for empty list
nil

; 1.0
1

; scientific notation
1e3

; strings literals are max length 2000 chars, too
"a string literal"


;;; == Additional Special Forms ==

labels
cond
if
(apply + '(1 2))
(define f1 (lambda (p1 p2) (+ p1 p2)))
(defun f2 (p1 p2) (+ p1 p2))


;;; == Predefined Symbols ==

nil
t
internal-time-units-per-second


;;; == Predefined Primitives ==
(cons 'a 'b)   ; ==> (a . b)
(car '(a b c))  ; a
(cdr '(a b c))  ; (b c)
eq, atom, consp, listp, symbolp, numberp, stringp, null?
assoc
+-+/
mod
= < > <= >=
read
write
writeln
format
format-locale
string-format
string-format-locale
get-internal-real-time
get-internal-run-time ; user
get-internal-cpu-time ; user + system
sleep


;;; == Copyright ==
;;;
;;; LambdaJ is Copyright (C) 2020 Robert Mayer. All rights reserved.
;;;
;;; This work is licensed under the terms of the MIT license.
;;; For a copy, see https://opensource.org/licenses/MIT.

;;; At the end of the input file LambdaJ will print "bye." and exit.
