(defun recursive-fibonacci (n)
  (if (< n 2)
        n
    (+ (recursive-fibonacci (1- n)) (recursive-fibonacci (- n 2)))))

(write "recursive-fibonacci 25: " nil)
(write (recursive-fibonacci 25))


;; only works for n >= 3
(defun iterative-fibonacci (n)
  (labels ((iterative-fibonacci-tr (fn-2 fn-1 fn)
             (let loop ((i 3))
               (setq fn (+ fn-1 fn-2)
                     fn-2 fn-1
                     fn-1 fn)
               (if (< i n)
                 (loop (1+ i))
                 fn))))

    (iterative-fibonacci-tr 1 1 2)))

(write ", iterative-fibonacci 25: " nil)
(write (iterative-fibonacci 25))


;; from https://www.reddit.com/r/lisp/comments/yh8b1s/is_your_nthfib_faster_than_mine/iuco6ss/
(defun matrix-fibonacci (n)
  (labels ((evenp (n) (= 0.0 (mod n 2)))
           (fib-iter (a b p q count)
             (cond ((= count 0) b)
                   ((evenp count)
                    (fib-iter a
                              b
                              (+ (* p p) (* q q))
                              (+ (* 2 p q) (* q q))
                              (/ count 2)))
                   (t (fib-iter (+ (* b q) (* a q) (* a p))
                                (+ (* b p) (* a q))
                                p
                                q
                                (- count 1))))))
    (fib-iter 1 0 0 1 n)))

(write ", matrix-fibonacci 25: " nil)
(write (matrix-fibonacci 25))


(writeln)