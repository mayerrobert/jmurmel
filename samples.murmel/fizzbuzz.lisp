;;;; Fizzbuzz using named let

(defun fizzbuzz ()
  (let loop ((x 1))
    (if (<= x 100)
      (progn
        (cond ((= (mod x 15) 0) (write "Fizz Buzz" nil))
              ((= (mod x 3) 0)  (write "Fizz" nil))
              ((= (mod x 5) 0)  (write "Buzz" nil))
              (t                (write x)))
        (if (< x 100) (write ", " nil))
        (loop (1+ x))))))

(fizzbuzz)