;;;; Default library for Murmel
;;;
;;; Usage:
;;; copy into the directory containing jmurmel.jar or into the directory specified with --libdir
;;; and begin your file with
;;;
;;;     (require "mlib")
;;;
;;; Provides:
;;;     caar..cdddr
;;;     acons
;;;     not, and, or
;;;     abs, zerop, evenp, oddp
;;;     char=
;;;     eql, equal
;;;     when, unless, dotimes, dolist
;;;     identity, constantly, complement
;;;     member
;;;     mapcar, maplist, mapc, mapl, mapcan, mapcon
;;;     remove-if, remove
;;;     
;;;     with-gensyms


(defun  caar (l) (car (car l)))
(defun  cadr (l) (car (cdr l)))
(defun  cdar (l) (cdr (car l)))
(defun  cddr (l) (cdr (cdr l)))

(defun caaar (l) (car (car (car l))))
(defun caadr (l) (car (car (cdr l))))
(defun cadar (l) (car (cdr (car l))))
(defun caddr (l) (car (cdr (cdr l))))

(defun cdaar (l) (cdr (car (car l))))
(defun cdadr (l) (cdr (car (cdr l))))
(defun cddar (l) (cdr (cdr (car l))))
(defun cdddr (l) (cdr (cdr (cdr l))))


;;; (acons key datum alist) -> new-alist
(defun acons (key datum alist)
  (cons (cons key datum) alist))


;;; (not form) -> boolean
;;;
;;; Logical not.
(defun not (e)
  (null e))


;;; (and forms*) -> result
;;;
;;; Short-circuiting logical and.
;;; Return T unless any of the forms evaluate to NIL,
;;; NIL otherwise.
(defmacro and args
   (if args
         (if (cdr args)
               `(if ,(car args)
                 (and ,@(cdr args)))
           (car args))
     t))


;;; (or forms*) -> result
;;;
;;; Short-circuiting logical or.
;;; Return NIL unless any of the forms evaluate to non-NIL,
;;; the result of the first form returning non-NIL otherwise.
(defmacro or args
   (if args
         (if (cdr args)
               (let ((temp (gensym)))
                 `(let ((,temp ,(car args)))
                     (if ,temp
                           ,temp
                       (or ,@(cdr args)))))
           (car args))
     nil))


;;; (abs n) -> result
;;;
;;; Return the absoute value of a number.
(defun abs (n)
  (if (< n 0) (- n) (+ n)))


;;; (zerop number) -> boolean
;;;
;;; Is this number zero?
(defun zerop (n) (= n 0))


;;; (evenp number) -> boolean
;;;
;;; Is this number even?
(defun evenp (n) (= 0.0 (mod n 2)))


;;; (oddp number) -> boolean
;;;
;;; Is this number odd?
(defun oddp  (n) (= 1.0 (mod n 2)))


;;; (char= characters+) -> boolean
;;;
;;; Return t if all of the arguments are the same character
(defun char= (c . more)
  (if more
        (let loop ((code (char-int c)) (l more))
          (if (= code (char-int (car l)))
                (if (cdr l)
                      (loop code (cdr l))
                  t)
            nil))
    t))


;;; (eql x y) -> boolean
;;;
;;; Return t if any of the following is true
;;; a and b are eq
;;; a and b are numbers of the same type and have the same value
;;; a and b are the same characters
(defun eql (a b)
  (or (eq a b)
      (and (integerp a) (integerp b)     (= a b))
      (and (floatp a)   (floatp b)       (= a b))
      (and (characterp a) (characterp b) (char= a b))))


;;; (equal x y) -> boolean
;;;
;;; Return t if any of the following is true
;;; a and b are eql
;;; a and b are strings, characters or symbols and have the same text value
;;; a and b are conses whose car and cdr are equal respectively
(defun equal (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (consp a)   (consp b)   (equal (car a) (car b)) (equal (cdr a) (cdr b)))))


;;; (when condition forms*) -> result
;;;
;;; Execute forms if condition evaluates to true
;;; and return the result of the last form if any
;;; Orherwise f condition evaluates to false,
;;; the forms are not evaluated and the return value
;;; of the when-form is nil.
(defmacro when (condition . body)
  (list 'if
        condition
        (if (cdr body)
              (cons 'progn body)
          (car body))))


;;; (unless condition forms*) -> result
;;;
;;; Execute forms if condition evaluates to false
;;; and return the result of the last form if any
;;; Orherwise f condition evaluates to true,
;;; the forms are not evaluated and the return value
;;; of the unless-form is nil.
(defmacro unless (condition . body)
  (list 'if
        condition
        nil
        (if (cdr body)
              (cons 'progn body)
          (car body))))


;;; (dotimes (var count-form [result-form]) statement*) -> result
;;;
;;; Similar to CL dotimes http://clhs.lisp.se/Body/m_dotime.htm
(defmacro dotimes (exp . body)
  (let ((var (car exp))
        (countform (car (cdr exp)))
        (count (gensym))
        (loop (gensym))
        (result (car (cdr (cdr exp)))))
    `(let ((,count ,countform))
       (if (<= ,count 0)
             (let ((,var 0)) ,result)
         (let ,loop ((,var 0))
           (if (>= ,var ,count) ,result
             (progn
               ,@body
               (,loop (1+ ,var)))))))))


;;; (dolist (var list-form [result-form]) statement*) -> result
;;;
;;; Similar to CL dolist http://clhs.lisp.se/Body/m_dolist.htm
(defmacro dolist (exp . body)
  (let ((var (car exp))
        (listform (car (cdr exp)))
        (lst (gensym))
        (loop (gensym))
        (result (car (cdr (cdr exp)))))
    `(let ,loop ((,lst ,listform))
       (let ((,var (car ,lst)))
         (if ,lst
               (progn
                 ,@body
                 (,loop (cdr ,lst)))
           ,result)))))


;;;  (identity object) -> object
;;;
;;; Returns its argument object.
(defun identity (x) x)


;;; (constantly value) -> function
;;;
;;; constantly returns a function that accepts any number of arguments,
;;; that has no side-effects, and that always returns value. 
(defun constantly (value)
  (lambda arguments value))


;;; (complement function) -> complement-function
;;;
;;; complement returns a function that takes the same arguments as function,
;;; and has the same side-effect behavior as function, but returns only
;;; a single value: a boolean with the opposite truth value of that
;;; which would be returned as the value of function.
(defun complement (f)
  (lambda arguments
    (null (apply f arguments))))


;;; (member item list [test]) -> tail
;;;
;;; member searches list for item or for a top-level element that
;;; satisfies the test.
;;;
;;; "test" if given must be a function that takes to arguments.
;;; If "test" was omitted or nil then "eql" will be used.
;;;
;;; Example usage:
;;;     (member 2 '(1 2 3))
;;;         ; => (2 3)
;;;     (member 'e '(a b c d))
;;;         ; => NIL
;;;     (member '(1 . 1) '((a . a) (b . b) (c . c) (1 . 1) (2 . 2) (3 . 3)) equal)
;;;         ; => ((1 . 1) (2 . 2) (3 . 3))
;;;     (member 'c '(a b c 1 2 3) eq)
;;;         ; => (c 1 2 3)
;;;     (member 'b '(a b c 1 2 3) (lambda (a b) (eq a b)))
;;;         ; => (b c 1 2 3)
(defun member (obj l . test)
  (let* ((tst (car test))
         (pred (if tst tst eql)))
    (if l
          (if (pred obj (car l))
                l
            (member obj (cdr l) pred))
      nil)))


;;; (reverse sequence) -> reversed-sequence
;;;
;;; If sequence is a list then return a fresh list
;;; with elements in reversed order, if sequence
;;; is a string then return a fresh reversed string.
(defun reverse (l)
  (labels ((rev (l lp)
             (if l (rev (cdr l) (cons (car l) lp))
               lp)))
    (if (stringp l) (list->string (rev l nil))
      (rev l nil))))


; Helper macro to generate defuns for the various maxXX functions
(defmacro mapx (name comb acc accn return-list)
  `(defun ,name (f l . more)
     (if more
           (labels ((none-nil (lists)
                      (if lists (and (car lists) (none-nil (cdr lists)))
                        t))
                    (cdrs (lists)
                      (if lists (cons (cdr (car lists)) (cdrs (cdr lists)))
                        nil))
                    (cars (lists)
                      (if lists (cons (car (car lists)) (cars (cdr lists)))
                        nil)))
             (let loop ((args (cons l more)))
               (if (none-nil args)
                     (,comb (apply f ,(if accn (list accn 'args) 'args)) (loop (cdrs args)))
                 nil)))
       (let loop ((f f) (l l))
         (if l (,comb (f ,(if acc (list acc 'l) 'l)) (loop f (cdr l)))
           nil)))
    ,@(when return-list '(l))))

;;; (mapcar function list [more-lists]) -> list
(mapx mapcar  cons    car cars nil)

;;; (maplist function list [more-lists]) -> list
(mapx maplist cons    nil nil nil)

;;; (mapc function list [more-lists]) -> first-arg-list
(mapx mapc    progn   car cars t)

;;; (mapl function list [more-lists]) -> first-arg-list
(mapx mapl    progn   nil nil t)

;;; (mapcan function list [more-lists]) -> concatenated-results
(mapx mapcan  append  car cars nil)

;;; (mapcon function list [more-lists]) -> concatenated-results
(mapx mapcon  append  nil nil nil)

; undef mapx
(defmacro mapx)


;;; (remove pred list) -> list
;;;
;;; Return a fresh list without the elements for which pred
;;; evaluates to non-nil.
(defun remove-if (pred l)
  (if l
        (let ((obj (car l)))
          (if (pred obj)
                (remove-if pred (cdr l))
            (cons obj (remove-if pred (cdr l)))))
    nil))


;;; (remove elem list) -> list
;;;
;;; Return a fresh list without occurrences of elem.
;;; An occurrence is determined by eql.
(defun remove (elem l)
  (if l
        (let ((obj (car l)))
          (if (eql elem obj)
                (remove elem (cdr l))
            (cons obj (remove elem (cdr l)))))
    nil))


;;; (reduce func sequence [from-end-p]) -> result
;;;
;;; If sequence is empty then "reduce" will return (f).
;;;
;;; Otherwise if sequence contains one element then "reduce will
;;; return this element.
;;;
;;; Otherwise if from-end is omitted or nil then
;;; f will be called with the first two elements
;;; of the sequence and subsequently with the previous result
;;; and the next element, and "reduce" will return the last
;;; result from f.
;;;
;;; Otherwise if from-end is given and non-nil then
;;; f will be called with the last two elements
;;; of the sequence and subsequently with the previous result
;;; and the previous element, and "reduce" will return the last
;;; result from f.
(defun reduce (f seq . from-end)
  (let ((from-end-p (car from-end)))
    (if seq
          (if (cdr seq)
                (let loop ((elem (car seq))
                           (tail (cdr seq)))
                  (if (cdr tail)
                        (if from-end-p
                              (f elem (loop (car tail) (cdr tail)))
                          (loop (f elem (car tail)) (cdr tail)))
                    (f elem (car tail))))
            (car seq))
      (f))))


;;; (with-gensyms (names*) forms*) -> result
;;;
;;; "with-gensyms" is a macro commonly used by Common Lispers
;;; to help with avoiding name capture when writing macros.
;;; See "Practical Common Lisp, Peter Seibel"
;;; (http://www.gigamonkeys.com/book/macros-defining-your-own.html)
(defmacro with-gensyms (names . body)
  `(let ,(let loop ((names names))
           (if names (cons (list (car names) '(gensym)) (loop (cdr names)))))
     ,@body))


;;; (-> forms*) -> result
;;;
;;; thread-first, inspired by https://github.com/amirgamil/lispy/blob/master/lib/library.lpy
;;;
;;; Inserts first form as the first argument (second in list) of the second form, and so forth
;;;
;;; Usage is illustrated by:
;;;
;;;     (macroexpand-1 '(-> 1 f g h))
;;;       ; ==> (h (g (f 1)))
;;;     (macroexpand-1 '(-> 1 (f farg) (g garg) (h harg)))
;;;       ; ==> (h (g (f 1 farg) garg) harg)
;;;
(defmacro -> terms
  (labels ((apply-partials (partials expr)
             (if partials
                   (if (symbolp (car partials))
                         (list (car partials) (apply-partials (cdr partials) expr))
                     ; if it's a list with other parameters, insert expr (recursive call) 
                     ; as second parameter into partial (note need to use cons to ensure same list for func args)
                     (cons (caar partials) (cons (apply-partials (cdr partials) expr) (cdar partials))))
               expr)))
  (apply-partials (reverse (cdr terms)) (car terms))))


;;; (-> forms*) -> result
;;;
;;; thread-last
;;;
;;; Same as -> but inserts first form as last argument (last in list) of second form, and so forth
;;;
;;; Usage is illustrated by:
;;;
;;;     (macroexpand-1 '(->> 1 f g h))
;;;       ; ==> (h (g (f 1)))
;;;     (macroexpand-1 '(->> 1 (f farg) (g garg) (h harg)))
;;;       ; ==> (h harg (g garg (f farg 1)))
;;;
(defmacro ->> terms
  (labels ((apply-partials (partials expr)
             (if partials
                   (if (symbolp (car partials))
                         (list (car partials) (apply-partials (cdr partials) expr))
                     ; if it's a list with other parameters, insert expr (recursive call) 
                     ; as last form 
                     (cons (caar partials) (append (cdar partials) (list (apply-partials (cdr partials) expr)))))
               expr)))
  (apply-partials (reverse (cdr terms)) (car terms))))
