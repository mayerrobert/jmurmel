;;; loop macros dotimes and dolist

; similar to CL dotimes http://clhs.lisp.se/Body/m_dotime.htm
; dotimes (var count-form [result-form]) statement* => result
(defmacro dotimes (exp . body)
  (let ((var (car exp))
        (countform (car (cdr exp)))
        (count (gensym))
        (result (car (cdr (cdr exp)))))
    `(let ((,count ,countform))
       (if (<= ,count 0)
             (let ((,var 0)) ,result)
         (let loop ((,var 0))
           (if (>= ,var ,count) ,result
             (progn
               ,@body
               (loop (+ ,var 1)))))))))

(macroexpand-1 '(dotimes (i 5 'done) (write i) (writeln)))

(dotimes (i 3 'done) (write i) (writeln))
;0
;1.0
;2.0
;==> done

(dotimes (i 3) (write i) (writeln))
;0
;1.0
;2.0
;==> nil

(dotimes (temp-one 10 temp-one)) ; ==> 10.0



; similar to CL dolist http://clhs.lisp.se/Body/m_dolist.htm
; dolist (var list-form [result-form]) statement* => result*
(defmacro dolist (exp . body)
  (let ((var (car exp))
        (listform (car (cdr exp)))
        (lst (gensym))
        (result (car (cdr (cdr exp)))))
    `(let loop ((,lst ,listform))
       (let ((,var (car ,lst)))
         (if (null ,lst) ,result
           (progn
             ,@body
             (loop (cdr ,lst))))))))

(dolist (e '(1 nil 3) 'done) (write e) (writeln))
;1
;nil
;3
;==> done

(dolist (e '() 'done) (write e) (writeln)) ; ==> done

(dolist (e nil) (write e) (writeln)) ; ==> nil