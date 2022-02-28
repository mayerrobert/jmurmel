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


(defun timed (f) 
       ((lambda (start-realtime start-runtime f)
                (format t "starting... ")
                (f)
                (format-locale t *locale* "done in %g seconds realtime, %g seconds runtime%n%n"
                        (/ (- (get-internal-real-time) start-realtime) internal-time-units-per-second)
                        (/ (- (get-internal-run-time)  start-runtime)  internal-time-units-per-second)
                        ))
           (get-internal-real-time) (get-internal-run-time) f))

(define *n* 30)
(define *locale* "en-US")

(format t "%nFibonacci comparison recursive vs. pseudo iterative:%n%n")
(timed (lambda () (format-locale t *locale* "recursive-fib %d: %g%n" *n* (recursive-fib *n*))))
(timed (lambda () (format-locale t *locale* "iterative-fib %d: %g%n" *n* (iterative-fib *n*))))