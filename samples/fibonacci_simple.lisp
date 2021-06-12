(define recursive-fibonacci
  (lambda (n)
    (if (<= n 1)
          1
      (+ (recursive-fibonacci (- n 1)) (recursive-fibonacci (- n 2))))))

(write ", recursive-fibonacci 35: ")
(write (recursive-fibonacci 35))

(define iterative-fibonacci-tr
  (lambda (n i previous current)
    (if (>= i n)
          current
      (iterative-fibonacci-tr n (+ i 1) current (+ previous current)))))

(define iterative-fibonacci
  (lambda (n)
    (iterative-fibonacci-tr n 1 1 1)))

(write "iterative-fibonacci 35: ")
(write (iterative-fibonacci 35))