(define iterative-fib-tr
  (lambda (n i previous current)
    (if (>= i n)
          current
      (iterative-fib-tr n (+ i 1) current (+ previous current)))))

(define iterative-fib
  (lambda (n)
    (iterative-fib-tr n 1 1 1)))

(write "iterative-fib 35: ")
(write (iterative-fib 35))



(define recursive-fib
  (lambda (n)
    (if (<= n 1)
          1
      (+ (recursive-fib (- n 1)) (recursive-fib (- n 2))))))

(write ", recursive-fib 35: ")
(write (recursive-fib 35))