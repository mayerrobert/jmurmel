;;;; Experiments with delimited continuations implemented in Murmel using the monad pattern.
;;;;
;;;; (Quite literally) based on https://8c6794b6.github.io/posts/Delimited-continuations-with-monadic-functions-in-Common-Lisp.html
;;;; which has the following notice:
;;;;
;;;; Copyright © 2011-2017 8c6794b6; Unless otherwise noted, site contents licensed under CC-by-SA.
;;;;
;;;; (CC-by-SA is a link to http://creativecommons.org/licenses/by-sa/4.0/)
;;;;
;;;; see also https://groups.google.com/g/comp.lang.lisp/c/D72jdiAmiBE
;;;;
(require "mlib")

;(defstruct (cont (:constructor make-cont (fn)))
;  (fn #'values :type function))

(defun make-cont (fn)
  (list 'cont
    (if fn fn
      values)))

(defun cont-p (c)
  (and (consp c) (eq 'cont (car c))))

(defun cont-fn (c)
  (cadr c))



(defun run-cont (c k)
  ((cont-fn c) k))

(defun returnc (x)
  (make-cont (lambda (k)
               (k x))))

(defun bindc (c f)
  (make-cont (lambda (k)
               (run-cont c (lambda (x)
                             (run-cont (f x) k))))))


#|
(run-cont (returnc 'foo)
          #'values)
; ==> FOO
(run-cont (bindc (returnc 21) (lambda (x) (returnc (* x 2))))
          #'values)
; ==> 42
|#


(defmacro letc* (bindings . body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (name c) (car bindings)
        `(bindc ,c (lambda (,name)
                     (letc* ,(cdr bindings) ,@body))))))


#|
(run-cont (letc* ((x (returnc 21)))
            (returnc (* x 2)))
          #'values)
; ==> 42
|#


(defmacro progc body
  (if (null (cdr body))
      (car body)
      (let ((garg (gensym)))
        `(bindc ,(car body)
                (lambda (,garg)
                  (progc ,@(cdr body)))))))


(defun reset (k)
  (if (cont-p k)
      (run-cont k
                #'values)
      k))


(defmacro shift (var expr)
  (let ((gk (gensym))
        (garg (gensym)))
    `(make-cont (lambda (,gk)
                  (labels ((,var (,garg)
                             (,gk ,garg)))
                    ,expr)))))


#|
(reset (shift k (k 2)))
; ==> 2

(reset (letc* ((x (shift k (k 2))))
         (returnc (+ x 3))))
; ==> 5

(reset (letc* ((x (returnc 100))
               (y (shift k 'foo)))
         (returnc (+ x y))))
; ==> FOO
|#


(defun fail ()
  (shift k 'no))

(defun choice (n)
  (shift k (dotimes (i n 'no) (k (1+ i)))))

(defun triple (n s)
  (letc* ((i (choice n))
          (j (choice (- i 1)))
          (k (choice (- j 1))))
    (if (= s (+ i j k))
        (returnc (list i j k))
        (fail))))


#|
(reset (letc* ((ijk (triple 9 15)))
         (returnc (print ijk))))
; (6 5 4)
; (7 5 3)
; (7 6 2)
; (8 4 3)
; (8 5 2)
; (8 6 1)
; (9 4 2)
; (9 5 1)
; ==> no
|#


(defun donep (x) (eq 'done x))

(defun nextp (x) (not (donep x)))

(defun next (n k) (lambda () (values n k)))

(defun walkerc (tree)
  (cond
    ((null tree) (returnc 'done))
    ((atom tree) (shift k (next tree #'k)))
    (t (progc
         (walkerc (car tree))
         (walkerc (cdr tree))))))


(defun same-fringe (t1 t2)
  (labels ((rec (r1 r2)
             (if (nextp r1)
                 (and (nextp r2)
                      (multiple-value-bind (n1 k1) (r1)
                        (multiple-value-bind (n2 k2) (r2)
                          (and (eql n1 n2)
                               (rec (k1 nil)
                                    (k2 nil))))))
                 (donep r2))))
    (rec (reset (walkerc t1))
         (reset (walkerc t2)))))


#|
(same-fringe '((1) (2 (3 (4) 5))) '((1 (2) (3 4) 5)))
; ==> T

(same-fringe '((1) (2 (3 (4) 5))) '((1 (2) (4) 5)))
; ==> NIL
|#
