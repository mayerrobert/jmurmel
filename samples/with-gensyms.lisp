;;; gensym must be used to avoid name capture when writing macros.
;;; "with-gensyms" is a commonly used macro to help with that.

; CL version of "with-gensyms", see "Practical Common Lisp, Peter Seibel" (http://www.gigamonkeys.com/book/macros-defining-your-own.html)
;
;(defmacro with-gensyms ((&rest names) &body body)
;  `(let ,(loop for n in names collect `(,n (gensym)))
;     ,@body))



(defmacro with-gensyms (names . body)
  `(let ,(let loop ((names names))
           (if names
                 (cons (list (car names) '(gensym)) (loop (cdr names)))
             nil))
     ,@body))

;(macroexpand-1 '(with-gensyms (a b c) `(+ ,a ,b ,c)))
; ==> (let ((a (gensym)) (b (gensym)) (c (gensym))) (append (quote (+)) (append (list a) (append (list b) (list c)))))



; define a "non-shortcircuiting logical and" as a macro
; todo doesn't work because JMurmel has a bug in backquote expansion: nil is swallowed
(defmacro logical-and-3 (a b c)
  (with-gensyms (result)
    `(let ((,result t))
       (if ,a nil (setq ,result nil))
       (if ,b nil (setq ,result nil))
       (if ,c nil (setq ,result nil))
       ,result)))

(define result 3) ; the symbol "result" is used in the macro, name-capturing must be avoided
(logical-and-3 result 2 3) ; ==> t
result ; ==> 3