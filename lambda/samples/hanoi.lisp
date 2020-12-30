;;; from https://graham.main.nc.us/~bhammel/graham/lisp.html

;;; some LISP functions that are missing in Murmel:

(defun eql (x y)
  (= x y))

(defun append2 (x y)
  (if (null x)
        (if (consp y) y (list y))
    (if (atom x)
          (if (consp y)
                (cons x y)
            (if (null y)
                  (list x)
              (list x y)))
      (cons (car x) (append2 (cdr x) y)))))

(defun append (x y . rest)
  (if (null rest)
        (append2 x y)
    (if (null (car rest))
          (append2 x y)
      (append (append2 x y) (car rest) (cdr rest)))))

(defun sub1 (i)
  (- i 1))

; Helper to print the result of hanoi:
; print a list of lists, each on it's line
(defun pr-hanoi (l)
  (if l 
        (progn (writeln (car l))
               (pr-hanoi (cdr l)))
    (format t "Done.")))

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