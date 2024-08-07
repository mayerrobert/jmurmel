;;; from https://graham.main.nc.us/~bhammel/graham/lisp.html
;;;
;;; use like this:
;;;   jm --repl hanoi.lisp
;;;   JMurmel> (hanoi 5)
;;; or:
;;;   jm --repl hanoi.lisp
;;;   JMurmel> (pr-hanoi (hanoi 5))

; some LISP functions that are missing in Murmel:

(defun sub1 (i)
  (1- i))

; Helper to print the result of hanoi:
; print a list of lists, each on it's line
(defun pr-hanoi (l)
  (if l 
        (progn (writeln (car l))
               (pr-hanoi (cdr l)))
    (jformat t "Done.")))

;;; end Murmel additions



; File: /usr/lib/ulisp/hanoi.L
;
; Tower of Hanoi Problem - 
;
; E.g. (hanoi 5) returns a list of the sequence of moves for 5 disks
;
; Sources: Lisp (second edition), P. H. Winston & B. K. P. Horn, pp.112-114
;	   Addison-Wesley (1984);
;	   Metamagical Themas, Douglas Hofstadter, pp. 425-430
;	   Basic Books (1985).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	N disks on spindle A to start
(defun hanoi (N)
	(transfer 'A 'B 'C N)
)
;
(defun move-disk (from to)
	(list (list 'move 'disk 'from from 'to to))
)
;
(defun transfer (from to spare N)
	(cond ((eql N 1) (move-disk from to) )
	      (t         (append (transfer from spare to (sub1 N))
				             (move-disk from to)
				             (transfer spare to from (sub1 N))))
	)
)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;