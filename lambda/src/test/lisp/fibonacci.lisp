;unsigned long long recursive_fib(int n) {
;    if (n <= 1) {
;        return 1;
;    } else {
;        return recursive_fib(n - 1) + recursive_fib(n - 2);
;    }
;}

(defun recursive-fib (n)
       (if (<= n 1)
           1
           (+ (recursive-fib (- n 1)) (recursive-fib (- n 2)))))

;unsigned long long iterative_fib(int n) {
;    int previous = 1;
;    int current = 1;
;    int next = 0;
;    for (int i = 0; i < n; i++) {
;        next = previous + current;
;        previous = current;
;        current = next;
;    }
;    return current;
;}

(defun iterative-fib-tr (n i previous current)
       (if (>= i n)
           current
           (iterative-fib-tr n (+ i 1) current (+ previous current))))

(defun iterative-fib (n) (iterative-fib-tr n 1 1 1))

(define *n* 27)

(writeln (string-format-locale "en-US" "recursive-fib %g: %g" *n* (recursive-fib *n*)))
(writeln (string-format-locale "en-US" "iterative-fib %g: %g" *n* (iterative-fib *n*)))
