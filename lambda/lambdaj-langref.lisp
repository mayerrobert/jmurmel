
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


;;; == Basis Special Forms ===

(lambda (p1 p2) p1)             ; returns a lambda expression
(quote a-symbol)                ; returns an expression w/o evaluating it
'a-symbol                       ; shorthand for (quote a-symbol


;;; == S-expressions ==

'(a . b)                        ; returns a dotted pair       ==> (a . b)
'(a . (b . (c . d)))            ; returns a dotted list       ==> (a b c . d)
'(a b c . d)                    ; shorthand for dotted list   ==> (a b c . d)
'(a . (b . (c . ())))           ; returns a proper list       ==> (a b c)
'(a b c)                        ; shorthand for a proper list ==> (a b c)


;;; == Data types ==
;;;
;;; LambdaJ supports symbols, lists, double precision numbers and strings

'*a-sample-symbol*              ; a symbol
'a\ symbol\ with\ spaces        ; guess. Max length is 2000 chars
'()                             ; empty list, printed as "nil"
nil                             ; shorthand for empty list
1                               ; 1.0
1e3                             ; scientific notation
"a string literal"              ; strings literals are max length 2000 chars, too


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
