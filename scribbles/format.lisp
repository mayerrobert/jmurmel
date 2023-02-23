;;;; A simplified subset of Common Lisp's function 'format'
;;;
;;; Note that this simplified 'format' does not use or set CL' printer variables
;;; such as *print-base*, *print-circle*, ... .
;;;
;;; Only the format characters C, %, &, |, ~, B ,D, O, R, X, A, S, W are supported.
;;;
;;; C supports the modifier @ for printing #\-style escaping.
;;;
;;; B, D, O, R, X support mincol, padchar, commachar and comma-interval,
;;; the modifier @ for always printing the sign and the modifier : for grouping digits.
;;;
;;; R does not support printing english or roman numbers (giving the base is required).
;;;
;;; A and S support ~mincol,colinc,minpad,padcharA for padding, and the modifier @ for left-padding.

#-murmel
(defpackage my-format
  (:import-from common-lisp
                nil t character

                lambda defun defmacro progn setq labels

                let let*
                incf setf and or dotimes dolist if case when unless push pop

                car cadr caddr cdr cdddr cons rplacd list*

                error apply values eql equal integerp stringp
                < <= = >= > 1+ + - * abs ceiling truncate mod
                char-code length nreverse

                with-output-to-string
                ))

#-murmel
(in-package my-format)

#-murmel (progn

(defun write (obj cl:&rest args)
  (cl:write obj
            :escape (if args (car args) t)
            :stream (cadr args)))

(defun write-to-string (obj cl:&rest args)
  (cl:write-to-string obj
                      :escape (if args (car args) t)))

(defun make-array (size type adjustablep)
  (cl:make-array size :element-type type :adjustable adjustablep :fill-pointer adjustablep))

(defun vector-add (vec val)
  (cl:vector-push-extend val vec))

(cl:declaim (cl:ftype (cl:function (cl:string (cl:integer 0 *)) character) sref))
(defun sref (str idx)
  (cl:aref str idx))

(defun string-subseq (str start end)
  (cl:subseq str start end))

)


#+murmel
(progn

(declaim (optimize (speed 0)))

(require "mlib")

)


;; private
(defun print-integer (arg output-stream base colonp atp params)
  (if (integerp arg)
      (let ((mincol (if params (pop params)))
            (padchar (if params (pop params) #\ ))
            (commachar #\,)
            (comma-interval 3)
            (digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            (rev (make-array 0 'character t))
            (len 0))

        (if colonp
            ;; grouping: separate 'comma-interval' digits with 'commachar'
            (progn
              (when params
                (when (car params)
                  (setq commachar (car params)))
                (when (cadr params)
                  (setq comma-interval (cadr params))))
              (labels ((loop (n pos)
                        (when (> n 0)
                          (when (= pos comma-interval)
                            (vector-add rev commachar)
                            (incf len)
                            (setq pos 0))
                          (vector-add rev (sref digits (mod n base)))
                          (incf len)
                          (loop (truncate n base) (1+ pos)))))
                (loop (abs arg) 0)))

            ;; no grouping
            (labels ((loop (n)
                      (when (> n 0)
                        (vector-add rev (sref digits (mod n base)))
                        (incf len)
                        (loop (truncate n base)))))
              (loop (abs arg))))

        ;; padding
        (if mincol
            (dotimes (i (- mincol
                           len
                           (if atp 1
                               (if (< arg 0) 1
                                   0))))
              (write padchar nil output-stream)))

        ;; print sign and number
        (if (< arg 0)
            (write #\- nil output-stream)
            (when atp
              (write #\+ nil output-stream)))
        (write (nreverse rev) nil output-stream))

      (write arg nil output-stream)))


;; private
(defun print-obj (arg output-stream escapep atp params)
  (let* (mincol colinc minpad padchar
         (str (write-to-string arg escapep)))

    (unless atp
      (write str nil output-stream))

    (when params
      (setq mincol  (pop params))
      (setq colinc  (if (car params) (car params) 1)) (pop params)
      (setq minpad  (if (car params) (car params) 0)) (pop params)
      (setq padchar (if (car params) (pop params) #\ ))

      (dotimes (i (+ minpad (* (ceiling (- mincol minpad (length str)) colinc) colinc)))
        (write padchar nil output-stream)))

    (when atp
      (write str nil output-stream))))


;; private
(defun parse-control-string (control-string)
  (let* ((result (cons () ()))
         (append-to result)
         (i 0)
         (j nil)
         (control-string-length (length control-string)))
    (labels ((collect (obj)
               (setq append-to (cdr (rplacd append-to (cons obj ())))))

             (start ()
               (when (< i control-string-length)
                 (when (eql (sref control-string i) #\~)
                   (when j
                     (collect (string-subseq control-string j i)))
                   (incf i)
                   (let* (code colonp atp arg
                          (args (cons () ()))
                          (append-to-args args))
                     (labels ((collect-arg (arg)
                                (setq append-to-args (cdr (rplacd append-to-args (cons arg ())))))

                              (next ()
                                (setq code (sref control-string i))
                                (case code
                                  (#\'
                                   (setq arg (sref control-string (incf i)))
                                   (incf i)
                                   (next))

                                  (#\,
                                   (collect-arg arg)
                                   (setq arg nil)
                                   (incf i)
                                   (next))

                                  (#\:
                                   (setq colonp t)
                                   (incf i)
                                   (next))

                                  (#\@
                                   (setq atp t)
                                   (incf i)
                                   (next))

                                  ((#\+ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (setq arg 0)
                                   (labels ((loop (code)
                                             (when (and (>= code 48) (<= code 57))
                                               (setq arg (+ (* arg 10) (- code 48))
                                                     i (1+ i))
                                               (when (< i control-string-length)
                                                 (loop (char-code (sref control-string i)))))))
                                     (loop (char-code (sref control-string i))))
                                   (next))

                                  (t
                                   (when arg
                                     (collect-arg arg)
                                     (setq arg nil))
                                   (collect (list* code colonp atp (cdr args)))))))
                       (next)))

                   (setf j (1+ i)))
                 (unless j (setf j i))
                 (incf i)
                 (start))
               (when (< j i)
                 (collect (string-subseq control-string j i))
                 (setq j i))))
      (start))

    (cdr result)))


;; private
(defun format-function (program)
  (lambda (output-stream #-murmel cl:&rest #+murmel . arguments)
    (dolist (elem program arguments)
      (if (stringp elem)
          (write elem nil output-stream)
          (let ((colonp (cadr elem))
                (atp (caddr elem))
                (params (cdddr elem)))
            (case (car elem)

              ;; Basic output

              ;; Tilde C: Character
              ;; The next arg should be a character.
              ;; ~c and ~:c will print the character, ~@c and ~@:c will print a #\... sequence,
              ;; (i.e. : is ignored)
              ((#\c #\C)
               (write (pop arguments) atp output-stream))

              ;; Tilde Percent: Newline
              ;; Tilde Ampersand: Fresh-Line
              ;; ~n% and ~n& output n newlines. No arg is used.
              ;; (~& should omit the first newline if the output stream
              ;; is already at the beginning of a line, this is not implemented.)
              ((#\% #\&)
               (dotimes (n (or (car params) 1))
                 (write #\Newline nil output-stream)))

              ;; Tilde Vertical-Bar: Page
              ;; This outputs a page separator character, if possible. ~n| does this n times.
              (#\|
               (dotimes (n (or (car params) 1))
                 (write #\Page nil output-stream)))

              ;; Tilde Tilde: Tilde
              ;; This outputs a tilde. ~n~ outputs n tildes.
              (#\~
               (dotimes (n (or (car params) 1))
                 (write #\~ nil output-stream)))


              ;; Radix Control

              ;; Tilde R: Radix
              ;; ~nR prints arg in radix n. sbcl supports 2..36
              ((#\r #\R)
               (print-integer (pop arguments) output-stream (car params) colonp atp (cdr params)))

              ;; Tilde D: Decimal
              ;; ~mincolD uses a column width of mincol; spaces are inserted on the left
              ;; if the number requires fewer than mincol columns for its digits and sign.
              ;; ~mincol,padcharD uses padchar as the pad character instead of space.
              ;; If arg is not an integer, it is printed in ~A format and decimal base.
              ;; The @ modifier causes the number's sign to be printed always; the default
              ;; is to print it only if the number is negative. The : modifier is ignored.
              ((#\d #\D)
               (print-integer (pop arguments) output-stream 10 colonp atp params))

              ;; Tilde B: Binary
              ((#\b #\B)
               (print-integer (pop arguments) output-stream 2 colonp atp params))

              ;; Tilde O: Octal
              ((#\o #\O)
               (print-integer (pop arguments) output-stream 8 colonp atp params))

              ;; Tilde X: Hexadecimal
              ((#\x #\X)
               (print-integer (pop arguments) output-stream 16 colonp atp params))


              ;; Printer Operations

              ;; Tilde A: Aesthetic
              ((#\a #\A)
               (print-obj (pop arguments) output-stream nil atp params))

              ;; Tilde S: Standard
              ((#\s #\S)
               (print-obj (pop arguments) output-stream t atp params))

              ;; Tilde W: Write
              ((#\w #\W)
               ;(when params (error "too many arguments, format character C accepts 0"))
               (write (pop arguments) t output-stream))


              ;; Layout Control

              ;; Tilde T: Tabulate
              ((#\t #\T)
               (dotimes (n (or (car params) 1))
                 (write #\Tab nil output-stream)))

              (t (error "unimplemented format character"))))))))


(defun format (destination control-string #-murmel cl:&rest #+murmel . args)
  (if destination
      (progn (apply (format-function (parse-control-string control-string)) (cons destination args)) nil)
      (with-output-to-string (destination)
        (apply (format-function (parse-control-string control-string)) (cons destination args)))))



;; Some tests:

(defun assert-equal (actual expected)
  (unless (equal actual expected)
    (write "Fail: expected: " nil) (write expected nil) (write #\Newline nil)
    (write "      actual:   " nil) (write actual nil) (write #\Newline nil)
    nil))


#+sbcl
(assert-equal (cl:format nil "hello")
              "hello")
(assert-equal (format nil "hello")
              "hello")

; C
#+sbcl
(assert-equal (cl:format nil "~@c" #\c)
              "#\\c")
(assert-equal (format nil "~@c" #\c)
              "#\\c")

; B
#+sbcl
(assert-equal (cl:format nil "~a x~20,'0bx" "asdf" 15)
              "asdf x00000000000000001111x")
(assert-equal (format nil "~a x~20,'0bx" "asdf" 15)
              "asdf x00000000000000001111x")

#+sbcl
(assert-equal (cl:format nil "x~20,'0,'_,2:bx" 255)
              "x00000000011_11_11_11x")
(assert-equal (format nil "x~20,'0,'_,2:bx" 255)
              "x00000000011_11_11_11x")

; O
#+sbcl
(assert-equal (cl:format nil "~a x~20,'0ox" "asdf" 15)
              "asdf x00000000000000000017x")
(assert-equal (format nil "~a x~20,'0ox" "asdf" 15)
              "asdf x00000000000000000017x")

; D
#+sbcl
(assert-equal (cl:format nil "~a x~5,'0@dx" "asdf" 15)
              "asdf x00+15x")
(assert-equal (format nil "~a x~5,'0@dx" "asdf" 15)
              "asdf x00+15x")

#+sbcl
(assert-equal (cl:format nil "~a x~5,'0dx" "asdf" -15)
              "asdf x00-15x")
(assert-equal (format nil "~a x~5,'0dx" "asdf" -15)
              "asdf x00-15x")

; R
#+sbcl
(assert-equal (cl:format nil "x~12,5,'0rx" -20)
              "x00-18x")
(assert-equal (format nil "x~12,5,'0rx" -20)
              "x00-18x")

#+sbcl
(assert-equal (cl:format nil "x~4,20,'0,,2:rx" 255)
              "x00000000000000033,33x")
(assert-equal (format nil "x~4,20,'0,,2:rx" 255)
              "x00000000000000033,33x")

#+sbcl
(assert-equal (cl:format nil "x~4,20,'0,'_:rx" 255)
              "x0000000000000003_333x")
(assert-equal (format nil    "x~4,20,'0,'_:rx" 255)
              "x0000000000000003_333x")

#+sbcl
(assert-equal (cl:format nil "x~12,5,'0rx" 'HELLO)
              "xHELLOx")
(assert-equal (format nil "x~12,5,'0rx" 'HELLO)
              "xHELLOx")

; X
#+sbcl
(assert-equal (cl:format nil "~a x~20,'*Xx" "asdf" 15)
              "asdf x*******************Fx")
(assert-equal (format nil "~a x~20,'*Xx" "asdf" 15)
              "asdf x*******************Fx")

; A
#+sbcl
(assert-equal (cl:format nil "x~20,1,0,'_@ax" "123")
              "x_________________123x")
(assert-equal (format nil "x~20,1,0,'_@ax" "123")
              "x_________________123x")

#+sbcl
(assert-equal (cl:format nil "x~20,,,'*ax" '(1 2 3))
              "x(1 2 3)*************x")
(assert-equal (format nil "x~20,,,'*ax" '(1 2 3))
              "x(1 2 3)*************x")

#+sbcl
(assert-equal (cl:format nil "x~5,3,1,'*Ax" '(1))
              "x(1)****x")
(assert-equal (format nil "x~5,3,1,'*Ax" '(1))
              "x(1)****x")
