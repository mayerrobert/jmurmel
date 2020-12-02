;;; based on https://gitlab.com/emacsomancer/ycombinator-codex
;;; the Y-combinator below is an applicative version also known as the Z-combinator

; result: t
; output: (1 1 2 3 5 8 13 21 34)\n(1 2 6 24 120 720 5040 40320 362880)\n

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

(writeln (mapcar maybe-int (mapcar fib '(1 2 3 4 5 6 7 8 9))))  ; ==> (1 1 2 3 5 8 13 21 34)

(writeln (mapcar maybe-int (mapcar fac '(1 2 3 4 5 6 7 8 9))))  ; ==> (1 2 6 24 120 720 5040 40320 362880)