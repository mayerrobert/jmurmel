(define recursive-fibonacci
  (lambda (n)
    (if (<= n 1)
          1
      (+ (recursive-fibonacci (- n 1)) (recursive-fibonacci (- n 2))))))

(format t "recursive-fibonacci 25: ")
(write (recursive-fibonacci 25))

(define iterative-fibonacci-tr
  (lambda (n i previous current)
    (if (>= i n)
          current
      (iterative-fibonacci-tr n (+ i 1) current (+ previous current)))))

(define iterative-fibonacci
  (lambda (n)
    (iterative-fibonacci-tr n 1 1 1)))

(format t ", iterative-fibonacci 25: ")
(write (iterative-fibonacci 25))