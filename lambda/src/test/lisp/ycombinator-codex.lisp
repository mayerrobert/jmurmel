;;; based on https://gitlab.com/emacsomancer/ycombinator-codex
;;; the Y-combinator below is an applicative version also known as the Z-combinator

; result: t
; output: (1.0 1.0 2.0 3.0 5.0 8.0 13.0 21.0 34.0)\n(1.0 2.0 6.0 24.0 120.0 720.0 5040.0 40320.0 362880.0)\n

; mapcar for Murmel
(defun mapcar (f l)
  (if l (cons (f (car l)) (mapcar f (cdr l)))
    nil))

; zerop for Murmel
(defun zerop (n)
  (= n 0))

; Murmel's arithmetic results are double, round to int if possible
(defun maybe-int (d)
  (if (= d (round d)) (round d)
     d))


;; Y Combinator  
(defun Y (f)
 ((lambda (g) (g g))
   (lambda (x)
     (f (lambda a
         (apply (x x) a))))))

;; fibonacci    
(defun fib (n)
  ((Y (lambda (f)
       (lambda (n a b)
        (if (< n 1)
           a
           (f (- n 1) b (+ a b))))))
   n 0 1))

;; factorial
(defun fac (n)
  ((Y (lambda (f)
       (lambda (n)
        (if (zerop n)
           1
           (* n (f (- n 1)))))))
   n))

(writeln (mapcar maybe-int (mapcar fib '(1 2 3 4 5 6 7 8 9))))  ; ==> (1.0 1.0 2.0 3.0 5.0 8.0 13.0 21.0 34.0)

(writeln (mapcar maybe-int (mapcar fac '(1 2 3 4 5 6 7 8 9))))  ; ==> (1.0 2.0 6.0 24.0 120.0 720.0 5040.0 40320.0 362880.0)