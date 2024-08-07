;;;; A simplified subset of Common Lisp's function 'format'
;;;
;;; Note that this simplified 'format' does not use or set CL' printer variables
;;; such as *print-base*, *print-circle*, ... .
;;;
;;; Only the format characters C, %, &, |, ~, B ,D, O, R, X, E, F, G, A, S, W are supported.
;;;
;;; C supports the modifier @ for printing #\-style escaping.
;;;
;;; B, D, O, R, X support mincol, padchar, commachar and comma-interval,
;;; the modifier @ for always printing the sign and the modifier : for grouping digits.
;;;
;;; R does not support printing english or roman numbers (giving the base is required).
;;;
;;; E, F, G: CL's full format is ~w,d,k,overflowchar,padcharF, this subset only supports ~w,dF
;;; and the modifier @ will always print the sign.
;;;
;;; A and S support ~mincol,colinc,minpad,padcharA for padding, and the modifier @ for left-padding.

#-murmel
(defpackage my-format
  (:import-from common-lisp
                nil t character

                lambda defun defmacro progn setq labels

                let let*
                incf setf and or dotimes dolist if case when unless push pop

                car cadr caddr cdr cdddr cons rplacd list list*

                error apply values eql equal functionp floatp integerp stringp
                < <= = >= > 1+ + - * ceiling truncate rem
                char-code length nreverse

                with-output-to-string
                )

  (:import-from sb-impl dovector))

#-murmel
(in-package my-format)

#-murmel
(progn

(defun write (obj cl:&rest args)
  (cl:write obj
            :escape (if args (car args) t)
            :stream (cadr args)))

(defun write-to-string (obj cl:&rest args)
  (cl:write-to-string obj
                      :escape (if args (car args) t)))

(defun make-array (size type cl:&optional (adjustablep nil))
  (cl:make-array size :element-type type :adjustable adjustablep :fill-pointer adjustablep))

(defun vector-add (vec val)
  (cl:vector-push-extend val vec))

(cl:declaim (cl:ftype (cl:function (cl:string (cl:integer 0 *)) character) sref))
(defun sref (str idx)
  (cl:aref str idx))

(defun (setf sref) (NEWVAL SIMPLE-VECTOR INDEX)
  (cl:funcall #'(setf cl:aref) NEWVAL SIMPLE-VECTOR INDEX))

(defun string-subseq (str start end)
  (cl:subseq str start end))

;; limited to: transform a Java format string for floats back to a CL format string
(defun jformat-locale (os locale str cl:&rest args)
  (cl:declare (cl:ignore locale))
  (cl:nsubstitute #\~ #\% str)
  (cl:nsubstitute #\, #\. str)
  (when (eql (cl:aref str 1) #\+)
    (if (= (length str) 3)
        (cl:nsubstitute #\@ #\+ str)
        (setq str (cl:concatenate 'cl:string
                                  "~"
                                  (cl:subseq str 2 (cl:1- (length str)))
                                  "@"
                                  (cl:subseq str (cl:1- (length str)))))))
  (apply #'cl:format os str args))

)


#+murmel
(progn

(declaim (optimize (speed 0)))

(require "mlib")

)


;; semi-private: used by the function generated by 'format-function' and the expansion of 'formatter'
(defun m%print-integer (arguments output-stream base colonp atp params)
  (if (integerp (car arguments))
      (let* ((arg (car arguments))
             (mincol (car params))
             (params (cdr params))
             (tmp (car params))  (padchar (if tmp tmp #\ ))
             (params (cdr params))
             (rev (make-array 0 'character t))
             (len 0))

        (if colonp
            ;; grouping: separate 'comma-interval' digits with 'commachar'
            (let* ((tmp (car params))   (commachar (if tmp tmp #\,))
                   (tmp (cadr params))  (comma-interval (if tmp tmp 3)))
              (labels ((loop (n pos)
                        (when (< n 0)
                          (when (= pos comma-interval)
                            (vector-add rev commachar)
                            (incf len)
                            (setq pos 0))
                          (vector-add rev (sref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- (rem n base))))
                          (incf len)
                          (loop (truncate n base) (1+ pos)))))
                (loop (if (> arg 0) (- arg) arg) 0)))

            ;; no grouping
            (labels ((loop (n)
                      (when (< n 0)
                        (vector-add rev (sref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- (rem n base))))
                        (incf len)
                        (loop (truncate n base)))))
              (loop (if (> arg 0) (- arg) arg))))

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

      (write (car arguments) nil output-stream))
  (cdr arguments))


;; semi-private: used by the expansion of 'formatter'
(defun m%print-simple-integer (arguments output-stream base)
  (if (integerp (car arguments))
      (let ((arg (car arguments))
            (rev (make-array 0 'character t)))
        (when (< arg 0)
          (vector-add rev #\-))

        (labels ((loop (n)
                  (when (< n 0)
                    (vector-add rev (sref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- (rem n base))))
                    (loop (truncate n base)))))
          (loop (if (> arg 0) (- arg) arg))) ; normalize integers to negative numbers because e.g. (abs most-negative-fixnum) would not fit in a fixnum

        ;; print sign and number
        (write (nreverse rev) nil output-stream))

      (write (car arguments) nil output-stream))
  (cdr arguments))


;; semi-private: used by the expansion of 'formatter'
(defun m%print-float (arguments output-stream jformat-string)
  (if (floatp (car arguments))
      (jformat-locale output-stream "en-US" jformat-string (car arguments))
      (write (car arguments) nil output-stream))
  (cdr arguments))


;; semi-private: used by the function generated by 'format-function' and the expansion of 'formatter'
(defun m%print-obj (arguments output-stream escapep atp params)
  (let* ((mincol  (car params))
         (params (cdr params))
         (tmp (car params))  (colinc  (if tmp tmp 1))
         (params (cdr params))
         (tmp (car params))  (minpad  (if tmp tmp 0))
         (params (cdr params))
         (tmp (car params))  (padchar (if tmp tmp #\ ))
         (arg (car arguments))
         (str (write-to-string arg escapep)))

    (unless atp
      (write str nil output-stream))

    (dotimes (i (+ minpad (* (ceiling (- mincol minpad (length str)) colinc) colinc)))
      (write padchar nil output-stream))

    (when atp
      (write str nil output-stream)))
  (cdr arguments))


;; private: used by the macro 'formatter' and by the function 'format-function'
(defun m%parse-control-string (control-string)
  (let* ((result (cons () ()))
         (append-to result)
         (i 0)
         (j nil)
         (control-string-length (length control-string)))
    (when (> control-string-length 0)
      (labels ((collect (obj)
                 (setq append-to (cdr (rplacd append-to (cons obj ())))))

               (start ()
                 (when (< i control-string-length)
                   (when (eql (sref control-string i) #\~)
                     (and j (< j i) (collect (string-subseq control-string j i)))
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
                                     (let ((sign 1))
                                       (if (eql code #\+)
                                           (setq i (1+ i) code (sref control-string i))
                                           (when (eql code #\-)
                                             (setq i (1+ i) code (sref control-string i) sign -1)))
                                       (labels ((loop (code)
                                                 (when (and (>= code 48) (<= code 57))
                                                   (setq arg (+ (* arg 10) (- code 48))
                                                         i (1+ i))
                                                   (when (< i control-string-length)
                                                     (loop (char-code (sref control-string i)))))))
                                         (loop (char-code code)))
                                       (setq arg (truncate arg sign)))
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
        (start)))

    (cdr result)))


;; private: this snippet is used in formatter and format-function
(defmacro m%float-fmtstring ()
  `(let ((w (car params))
         (d (cadr params))
         (jformat-string (make-array 0 'character t)))
     (vector-add jformat-string #\%)
     (when atp (vector-add jformat-string #\+))
     (when w
       (dovector (w (write-to-string w))
         (vector-add jformat-string w)))
     (when d
       (vector-add jformat-string #\.)
       (dovector (d (write-to-string d))
         (vector-add jformat-string d)))
     (vector-add jformat-string c)
     jformat-string))


(defmacro formatter (control-string)
  (let* ((body (cons () ()))
         (append-to body))
    (labels ((collect (form)
               (setq append-to (cdr (rplacd append-to (cons form nil)))))

             (collect-shift (form)
               (collect form)
               (collect `(setq arguments (cdr arguments))))

             (collect-setq (form)
               (collect `(setq arguments ,form)))

             (nchars (params c)
               (let* ((n (or (car params) 1))
                      (result (make-array n 'character nil)))
                 (dotimes (i n result)
                   (setf (sref result i) c))))

             (do-integer (base colonp atp params)
               (collect-setq (if (or colonp atp params)
                                 `(m%print-integer        arguments output-stream ,base ,colonp ,atp ',params)
                                 `(m%print-simple-integer arguments output-stream ,base))))

             (do-float (c atp params)
               (collect-setq `(m%print-float arguments output-stream ,(m%float-fmtstring)))))

      `(lambda (output-stream #-murmel cl:&rest #+murmel . arguments)
         ,@(dolist (elem (m%parse-control-string control-string) (cdr body))
             (if (stringp elem)
                 (collect `(write ,elem nil output-stream))
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
                      (collect-shift `(write (car arguments) ,atp output-stream)))

                     ;; Tilde Percent: Newline
                     ;; Tilde Ampersand: Fresh-Line
                     ;; ~n% and ~n& output n newlines. No arg is used.
                     ;; (~& should omit the first newline if the output stream
                     ;; is already at the beginning of a line, this is not implemented.)
                     ((#\% #\&)
                      (collect `(write ,(nchars params #\Newline) nil output-stream)))

                     ;; Tilde Vertical-Bar: Page
                     ;; This outputs a page separator character, if possible. ~n| does this n times.
                     (#\|
                      (collect `(write ,(nchars params #\Page) nil output-stream)))

                     ;; Tilde Tilde: Tilde
                     ;; This outputs a tilde. ~n~ outputs n tildes.
                     (#\~
                      (collect `(write ,(nchars params #\~) nil output-stream)))


                     ;; Radix Control

                     ;; Tilde R: Radix
                     ;; ~nR prints arg in radix n. sbcl supports 2..36
                     ((#\r #\R)
                      (do-integer (car params) colonp atp (cdr params)))

                     ;; Tilde D: Decimal
                     ;; ~mincolD uses a column width of mincol; spaces are inserted on the left
                     ;; if the number requires fewer than mincol columns for its digits and sign.
                     ;; ~mincol,padcharD uses padchar as the pad character instead of space.
                     ;; If arg is not an integer, it is printed in ~A format and decimal base.
                     ;; The @ modifier causes the number's sign to be printed always; the default
                     ;; is to print it only if the number is negative. The : modifier is ignored.
                     ((#\d #\D)
                      (if (or colonp params)
                          (collect-setq  `(m%print-integer arguments output-stream 10 ,colonp ,atp ',params))
                          (if atp
                              (collect-setq `(let ((arg (car arguments)))
                                               (and (integerp arg)
                                                    (>= arg 0)
                                                    (write #\+ nil output-stream))
                                               (write arg nil output-stream)
                                               (cdr arguments)))
                              (collect-shift `(write (car arguments) nil output-stream)))))

                     ;; Tilde B: Binary
                     ((#\b #\B)
                      (do-integer 2 colonp atp params))

                     ;; Tilde O: Octal
                     ((#\o #\O)
                      (do-integer 8 colonp atp params))

                     ;; Tilde X: Hexadecimal
                     ((#\x #\X)
                      (do-integer 16 colonp atp params))


                     ;; Floating point

                     ;; Tilde E: Exponential Floating-Point
                     ((#\e #\E)
                      (do-float #\e atp params))

                     ;; Tilde F: Fixed-Format Floating-Point
                     ((#\f #\F)
                      (do-float #\f atp params))

                     ;; Tilde G: General Floating-Point
                     ((#\g #\G)
                      (if params
                          (do-float #\g atp params)
                          (collect-shift (if atp
                                             `(let ((arg (car arguments)))
                                                (and (floatp arg)
                                                     (>= arg 0)
                                                     (write #\+ nil output-stream))
                                                (write arg nil output-stream))
                                             `(write (car arguments) nil output-stream)))))


                     ;; Printer Operations

                     ;; Tilde A: Aesthetic
                     ((#\a #\A)
                      (if (car params)
                          (collect-setq `(m%print-obj arguments output-stream nil ,atp ',params))
                          (collect-shift `(write (car arguments) nil output-stream))))

                     ;; Tilde S: Standard
                     ((#\s #\S)
                      (if (car params)
                          (collect-setq `(m%print-obj arguments output-stream t ,atp ',params))
                          (collect-shift `(write (car arguments) t output-stream))))

                     ;; Tilde W: Write
                     ((#\w #\W)
                      ;;(when params (error "too many arguments, format character W accepts 0"))
                      (collect-shift `(write (car arguments) t output-stream)))


                     ;; Layout Control

                     ;; Tilde T: Tabulate
                     ((#\t #\T)
                      (collect `(write ,(nchars params #\Tab) nil output-stream)))

                     (t (error "unimplemented format character"))))))
         ,'arguments))))


;; private: used by the function 'format'
(defun m%format-function (control-string)
  (lambda (output-stream #-murmel cl:&rest #+murmel . arguments)
    (dolist (elem (m%parse-control-string control-string))
      (if (stringp elem)
          (write elem nil output-stream)
          (let ((colonp (cadr elem))
                (atp (caddr elem))
                (params (cdddr elem)))

            (labels ((do-float (c arg)
                       (if (floatp arg)
                           (jformat-locale output-stream "en-US" (m%float-fmtstring) arg)
                           (write arg nil output-stream))
                       (setq arguments (cdr arguments)))

                     (do-general-float (c arg)
                       (if (and (floatp arg) params)
                           (jformat-locale output-stream "en-US" (m%float-fmtstring) arg)
                           (progn
                             (and atp (floatp arg) (>= arg 0) (write #\+ nil output-stream))
                             (write arg nil output-stream)))
                       (setq arguments (cdr arguments))))

              (case (car elem)

                ;; Basic output

                ;; Tilde C: Character
                ;; The next arg should be a character.
                ;; ~c and ~:c will print the character, ~@c and ~@:c will print a #\... sequence,
                ;; (i.e. : is ignored)
                ((#\c #\C)
                 (write (car arguments) atp output-stream)
                 (setq arguments (cdr arguments)))

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
                 (setq arguments (m%print-integer arguments output-stream (car params) colonp atp (cdr params))))

                ;; Tilde D: Decimal
                ;; ~mincolD uses a column width of mincol; spaces are inserted on the left
                ;; if the number requires fewer than mincol columns for its digits and sign.
                ;; ~mincol,padcharD uses padchar as the pad character instead of space.
                ;; If arg is not an integer, it is printed in ~A format and decimal base.
                ;; The @ modifier causes the number's sign to be printed always; the default
                ;; is to print it only if the number is negative. The : modifier is ignored.
                ((#\d #\D)
                 (setq arguments (m%print-integer arguments output-stream 10 colonp atp params)))

                ;; Tilde B: Binary
                ((#\b #\B)
                 (setq arguments (m%print-integer arguments output-stream 2 colonp atp params)))

                ;; Tilde O: Octal
                ((#\o #\O)
                 (setq arguments (m%print-integer arguments output-stream 8 colonp atp params)))

                ;; Tilde X: Hexadecimal
                ((#\x #\X)
                 (setq arguments (m%print-integer arguments output-stream 16 colonp atp params)))


                ;; Floating point

                ;; Tilde E: Exponential Floating-Point
                ((#\e #\E)
                 (do-float #\e (car arguments)))

                ;; Tilde F: Fixed-Format Floating-Point
                ((#\f #\F)
                 (do-float #\f (car arguments)))

                ;; Tilde G: General Floating-Point
                ((#\g #\G)
                 (do-general-float #\g (car arguments)))


                ;; Printer Operations

                ;; Tilde A: Aesthetic
                ((#\a #\A)
                 (if (car params)
                     (setq arguments (m%print-obj arguments output-stream nil atp params))
                     (progn (write (car arguments) nil output-stream)
                            (setq arguments (cdr arguments)))))

                ;; Tilde S: Standard
                ((#\s #\S)
                 (if (car params)
                     (setq arguments (m%print-obj arguments output-stream t atp params))
                     (progn (write (car arguments) t output-stream)
                            (setq arguments (cdr arguments)))))

                ;; Tilde W: Write
                ((#\w #\W)
                 ;;(when params (error "too many arguments, format character C accepts 0"))
                 (write (car arguments) t output-stream)
                 (setq arguments (cdr arguments)))


                ;; Layout Control

                ;; Tilde T: Tabulate
                ((#\t #\T)
                 (dotimes (n (or (car params) 1))
                   (write #\Tab nil output-stream)))

                (t (error "unimplemented format character")))))))))

#+murmel
(defmacro m%float-fmtstring)


;; semi-private: used by the function generated by 'format-function' and the expansion of 'formatter'
(defun m%format (destination f #-murmel cl:&rest #+murmel . args)
  (if destination
      (progn (apply f (cons destination args))
             nil)
      (with-output-to-string (destination)
        (apply f (cons destination args)))))


(defun format (destination control-string #-murmel cl:&rest #+murmel . args)
  (apply #'m%format (list* destination
                           (if (functionp control-string)
                               control-string
                               (m%format-function control-string))
                           args)))


#+murmel
(defmacro format (destination control-string #-murmel cl:&rest #+murmel . args)
  (if (stringp control-string)
      `(m%format ,destination (formatter ,control-string) ,@args)
      `(format ,destination ,control-string ,@args)))



;; Some tests:

(defun assert-equal (actual expected)
  (unless (equal actual expected)
    (write "Fail: expected: " nil) (write expected nil) (write #\Newline nil)
    (write "      actual:   " nil) (write actual nil) (write #\Newline nil)
    nil))

(defmacro test (expected str #-murmel cl:&rest #+murmel . args)
  `(progn
     #+sbcl
     (assert-equal (cl:format nil ,str ,@args) ,expected)
     (assert-equal (format    nil ,str ,@args) ,expected)
     (assert-equal (apply #'format (list nil (formatter ,str) ,@args)) ,expected)
     ))


;; no format characters
(test "hello"
      "hello")


;; C
(test "#\\c"
      "~@c" #\c)


;; B
(test "asdf x00000000000000001111x"
      "~a x~20,'0bx" "asdf" 15)

(test "x00000000011_11_11_11x"
      "x~20,'0,'_,2:bx" 255)


;; O
(test "asdf x00000000000000000017x"
      "~a x~20,'0ox" "asdf" 15)


;; D
(test "asdf x00+15x"
      "~a x~5,'0@dx" "asdf" 15)


(test "asdf x00-15x"
      "~a x~5,'0dx" "asdf" -15)


;; R
(test "x00-18x"
      "x~12,5,'0rx" -20)

(test "x00000000000000033,33x"
      "x~4,20,'0,,2:rx" 255)

(test "x0000000000000003_333x"
      "x~4,20,'0,'_:rx" 255)

(test "xHELLOx"
      "x~12,5,'0rx" 'HELLO)


;; X
(test "asdf x*******************Fx"
      "~a x~20,'*Xx" "asdf" 15)


;; A
(test "x_________________123x"
      "x~20,1,0,'_@ax" "123")

(test "x(1 2 3)*************x"
      "x~20,,,'*ax" '(1 2 3))

(test "x(1)****x"
      "x~5,3,1,'*Ax" '(1))


;; print 65535 with a leading sign in radix 4, group 3 digits with '_', left pad with '0' to 20 chars
(assert-equal (with-output-to-string (s)
                (apply (formatter "x~4,20,'0,'_:@rx") (list s 65535)))
              "x000000000+33_333_333x")


;; F
(test "123.46"
      "~5,2f" 123.456)

(test "+123.46"
      "~5,2@f" 123.456)

(test #+murmel "+123.456789"
      #-murmel "+123.45679"
      "~@f" 123.456789)

;; G
;; ~g shows differences between CL and Murmel as CL's format may append spaces after ~g
#+murmel
(test "x123.456x"
      "x~gx" 123.456)

#+murmel
(test "y+123.456y"
      "y~@gy" 123.456)
