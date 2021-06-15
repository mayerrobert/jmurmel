;;;; Default library for Murmel
;;;
;;; Usage:
;;; copy into the directory containing jmurmel.jar or into the directory specified with --libdir
;;; and begin your file with
;;;
;;;     (require "mlib")
;;;
;;; Provides:
;;;
;;;     caar..cdddr
;;;     setf, push, pop
;;;     acons
;;;     not, and, or
;;;     abs, zerop, evenp, oddp
;;;     char=
;;;     equal
;;;     when, unless, dotimes, dolist
;;;     identity, constantly, complement
;;;     member
;;;     mapcar, maplist, mapc, mapl, mapcan, mapcon
;;;     every, some, notevery, notany
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


(defun rplaca* (l v) (rplaca l v) v)
(defun rplacd* (l v) (rplacd l v) v)

;;; (setf pair*) -> result
;;;
;;; Takes pairs of arguments like SETQ. The first is a place and the second
;;; is the value that is supposed to go into that place. Returns the last
;;; value. The place argument may be any of the access forms for which SETF
;;; knows a corresponding setting form, which currently are:
;;;
;;; - symbols
;;; - car..cdddr
(defmacro setf args
  (if args
        (if (cdr args)
              (if (cddr args)
                    (cons 'progn
                      (let loop ((args args))
                        (if (cdr args)
                              (cons `(setf ,(car args) ,(cadr args))
                                    (if (cddr args)
                                          (loop (cddr args))))
                          (fatal "odd number of arguments to setf"))))
                (if (symbolp (car args))
                      `(setq ,(car args) ,(cadr args))
                  (let* ((place (car args))
                         (place-op (car place))
                         (place-args (cdr place))
                         (value (cadr args)))
                    (cond ((eq 'car   place-op) `(rplaca*       ,@place-args  ,value))
                          ((eq 'caar  place-op) `(rplaca* (car  ,@place-args) ,value))
                          ((eq 'cadr  place-op) `(rplaca* (cdr  ,@place-args) ,value))
                          ((eq 'caaar place-op) `(rplaca* (caar ,@place-args) ,value))
                          ((eq 'caadr place-op) `(rplaca* (cadr ,@place-args) ,value))
                          ((eq 'cadar place-op) `(rplaca* (cdar ,@place-args) ,value))
                          ((eq 'caddr place-op) `(rplaca* (cddr ,@place-args) ,value))

                          ((eq 'cdr   place-op) `(rplacd*       ,@place-args  ,value))
                          ((eq 'cdar  place-op) `(rplacd* (car  ,@place-args) ,value))
                          ((eq 'cddr  place-op) `(rplacd* (cdr  ,@place-args) ,value))
                          ((eq 'cdaar place-op) `(rplacd* (caar ,@place-args) ,value))
                          ((eq 'cdadr place-op) `(rplacd* (cadr ,@place-args) ,value))
                          ((eq 'cddar place-op) `(rplacd* (cdar ,@place-args) ,value))
                          ((eq 'cdddr place-op) `(rplacd* (cddr ,@place-args) ,value))

                          (t (fatal "only symbols and car..cdddr are supported for 'place'"))))))
          (fatal "odd number of arguments to setf"))))


;;; (push item place) -> new-place-value
;;;
;;; push prepends item to the list that is stored in place,
;;; stores the resulting list in place, and returns the list.
(defmacro push (item place)
  (if (symbolp place)
        `(setq ,place (cons ,item ,place))
    (let ((tmp (gensym))
          (place-op (car place)))
      (append
        `(let ((,tmp ,(cadr place))))
        (cond ((eq   'car place-op) (list `(rplaca*       ,tmp  (cons ,item (  car ,tmp)))))
              ((eq  'caar place-op) (list `(rplaca* ( car ,tmp) (cons ,item ( caar ,tmp)))))
              ((eq  'cadr place-op) (list `(rplaca* ( cdr ,tmp) (cons ,item ( cadr ,tmp)))))
              ((eq 'caaar place-op) (list `(rplaca* (caar ,tmp) (cons ,item (caaar ,tmp)))))
              ((eq 'caadr place-op) (list `(rplaca* (cadr ,tmp) (cons ,item (caadr ,tmp)))))
              ((eq 'cadar place-op) (list `(rplaca* (cdar ,tmp) (cons ,item (cadar ,tmp)))))
              ((eq 'caddr place-op) (list `(rplaca* (cddr ,tmp) (cons ,item (caddr ,tmp)))))

              ((eq   'cdr place-op) (list `(rplacd*       ,tmp  (cons ,item (  cdr ,tmp)))))
              ((eq  'cdar place-op) (list `(rplacd* ( car ,tmp) (cons ,item ( cdar ,tmp)))))
              ((eq  'cddr place-op) (list `(rplacd* ( cdr ,tmp) (cons ,item ( cddr ,tmp)))))
              ((eq 'cdaar place-op) (list `(rplacd* (caar ,tmp) (cons ,item (cdaar ,tmp)))))
              ((eq 'cdadr place-op) (list `(rplacd* (cadr ,tmp) (cons ,item (cdadr ,tmp)))))
              ((eq 'cddar place-op) (list `(rplacd* (cdar ,tmp) (cons ,item (cddar ,tmp)))))
              ((eq 'cdddr place-op) (list `(rplacd* (cddr ,tmp) (cons ,item (cdddr ,tmp)))))

              (t (fatal "only symbols and car..cdddr are supported for 'place'")))))))


;;; (pop place) -> element
;;;
;;; pop reads the value of place, remembers the car of the list which
;;; was retrieved, writes the cdr of the list back into the place,
;;; and finally yields the car of the originally retrieved list.
(defmacro pop (place)
  (if (symbolp place)
        (let ((result (gensym)))
          `(let ((,result (car ,place)))
             (setq ,place (cdr ,place))
             ,result))
    (let ((lst (gensym))
          (result (gensym))
          (place-op (car place))
          (place-arg (cadr place)))
      (cond ((eq   'car place-op) `(let* ((,lst       ,place-arg)   (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))
            ((eq  'caar place-op) `(let* ((,lst  (car ,place-arg))  (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))
            ((eq  'cadr place-op) `(let* ((,lst  (cdr ,place-arg))  (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))

            ((eq 'caaar place-op) `(let* ((,lst (caar ,place-arg))  (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))
            ((eq 'caadr place-op) `(let* ((,lst (cadr ,place-arg))  (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))
            ((eq 'cadar place-op) `(let* ((,lst (cdar ,place-arg))  (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))
            ((eq 'caddr place-op) `(let* ((,lst (cddr ,place-arg))  (,result (caar ,lst))) (rplaca ,lst (cdar ,lst)) ,result))

            ((eq   'cdr place-op) `(let* ((,lst       ,place-arg)   (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))
            ((eq  'cdar place-op) `(let* ((,lst  (car ,place-arg))  (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))
            ((eq  'cddr place-op) `(let* ((,lst  (cdr ,place-arg))  (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))

            ((eq 'cdaar place-op) `(let* ((,lst (caar ,place-arg))  (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))
            ((eq 'cdadr place-op) `(let* ((,lst (cadr ,place-arg))  (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))
            ((eq 'cddar place-op) `(let* ((,lst (cdar ,place-arg))  (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))
            ((eq 'cdddr place-op) `(let* ((,lst (cddr ,place-arg))  (,result (cadr ,lst))) (rplacd ,lst (cddr ,lst)) ,result))

            (t (fatal "only symbols, car..cdddr are supported for 'place'"))))))


;;; (acons key datum alist) -> new-alist
;;;
;;; Prepends alist with a new (key . datum) tuple
;;; and returns the modified list.
(defun acons (key datum alist)
  (cons (cons key datum) alist))


;;; (not form) -> boolean
;;;
;;; Logical not.
(defun not (e)
  (null e))


;;; (and forms*) -> boolean
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
(defun oddp (n) (= 1.0 (mod n 2)))


;;; (char= characters+) -> boolean
;;;
;;; Return t if all of the arguments are the same character
(defun char= (c . more)
  (if more
        (let loop ((code (char-code c)) (l more))
          (if (= code (char-code (car l)))
                (if (cdr l)
                      (loop code (cdr l))
                  t)
            nil))
    t))


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
(defmacro mapx (name comb acc accn return-list lastelem)
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
                 ,lastelem)))
       (let loop ((f f) (l l))
         (if l (,comb (f ,(if acc (list acc 'l) 'l)) (loop f (cdr l)))
           ,lastelem)))
    ,@(when return-list '(l))))


;;; (mapcar function sequence+) -> list
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent items of the given sequences.
;;; All function application results will be combined into a list
;;; which is the return value of mapcar.
(mapx mapcar  cons    car cars nil nil)


;;; (maplist function sequence+) -> list
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent tails of the given sequences.
;;;
;;; All function application results will be combined into a list
;;; which is the return value of maplist.
(mapx maplist cons    nil nil nil nil)


;;; (mapc function sequence+) -> first-arg-list
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent cars items of the given sequences.
(mapx mapc    progn   car cars t nil)


;;; (mapl function sequence+) -> first-arg-list
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent tails of the given sequences.
(mapx mapl    progn   nil nil t nil)


;;; (mapcan function sequence+) -> concatenated-results
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent items of the given sequences.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of mapcan.
(mapx mapcan  append  car cars nil nil)


;;; (mapcon function sequence+) -> concatenated-results
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will applied to subsequent tails of the given sequences.
;;;
;;; All function application results will be concatenated to a list
;;; which is the return value of mapcon.
(mapx mapcon  append  nil nil nil nil)


;;; (every function sequence+) -> boolean
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; Immediately return nil if an application of function returns nil,
;;; t otherwise.
(mapx every and car cars nil t)


;;; (some function sequence+) -> result
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; Immediately return the first non-nil-value of an application of function,
;;; or nil if no applications yield non-nil.
(mapx some or car cars nil nil)


;;; (notevery function sequence+) -> boolean
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; (notevery predicate sequence+) == (not (every predicate sequence+))
(defun notevery (f seq . more)
  (not (apply some (cons f (cons seq more)))))


;;; (notany function sequence+) -> boolean
;;;
;;; "Function" must accept as many arguments as sequences are given,
;;; and will be applied to subsequent items of the given sequences.
;;;
;;; (notany predicate sequence+) == (not (some predicate sequence+))
(defun notany (f seq . more)
  (not (apply every (cons f (cons seq more)))))

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
;;; Inserts first form as the first argument of the second form, and so forth.
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


;;; (->> forms*) -> result
;;;
;;; thread-last
;;;
;;; Same as -> but inserts first form as last argument of the second form, and so forth.
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


;;; (and-> forms*) -> result
;;;
;;; Short-circuiting thread-first
;;;
;;; Same as -> but if one function returns nil then the remaining
;;; functions are not called and the overall result is nil.
;;;
(defmacro and-> terms
  (if (cdr terms)
        (let* ((temp (gensym))
               (init (car terms))
               (forms (let loop ((tail (cdr terms)))
                         (if (symbolp (car tail))
                               (if (cdr tail)
                                     (cons (list 'setq temp (list (car tail) temp)) (loop (cdr tail)))
                                 (list (list (car tail) temp)))
                           (if (cdr tail)
                                 (cons (list 'setq temp (cons (caar tail) (cons temp (cdar tail)))) (loop (cdr tail)))
                             (list (cons (caar tail) (cons temp (cdar tail)))))))))
          `(let ((,temp ,init))
             (and ,@forms)))
    (car terms)))


;;; (and->> forms*) -> result
;;;
;;; Short circuiting thread-last
;;;
;;; Same as ->> but if one function returns nil then the remaining
;;; functions are not called and the overall result is nil.
;;;
(defmacro and->> terms
  (if (cdr terms)
        (let* ((temp (gensym))
               (init (car terms))
               (forms (let loop ((tail (cdr terms)))
                         (if (symbolp (car tail))
                               (if (cdr tail)
                                     (cons (list 'setq temp (list (car tail) temp)) (loop (cdr tail)))
                                 (list (list (car tail) temp)))
                           (if (cdr tail)
                                 (cons (list 'setq temp (cons (caar tail) (append (cdar tail) (list temp)))) (loop (cdr tail)))
                             (list (cons (caar tail) (append (cdar tail) (list temp)))))
                             ))))
          `(let ((,temp ,init))
             (and ,@forms)))
    (car terms)))
