;;;; Compute all Armstrong number in the range of 0 and 999
;;;
;;; Problem Statement
;;; An Armstrong number of three digits is an integer such that
;;; the sum of the cubes of its digits is equal to the number itself.
;;; For example, 371 is an Armstrong number since 3**3 + 7**3 + 1**3 = 371.
;;;
;;; see https://pages.mtu.edu/%7Eshene/COURSES/cs201/NOTES/chap04/arms.html, https://en.wikipedia.org/wiki/Narcissistic_number
;;; and https://www.reddit.com/r/ProgrammingLanguages/comments/176it3o/showcase_your_lang_by_sharing_an_armstrong_number/

(require "mlib")

(defmacro pr items
  "Helper that will print each argument and then a newline."
  `(progn ,@(mapcar (lambda (it)
                      (list 'write it nil))
                    items)
          (writeln)))



(pr #\Newline "Nested loops:")

(defun arms ()
  "Print all Armstrong numbers from 0 to 999."
  (let ((count 0) (abc 0))
    (dotimes (a 10)
      (dotimes (b 10)
        (dotimes (c 10)
          (let ((a3b3c3 (+ (expt a 3) (expt b 3) (expt c 3))))
            (when (= abc a3b3c3)
              (incf count)
              (pr "Armstrong number " count ": " abc))
            (incf abc)))))))

(time (arms))



(pr #\Newline "One loop:")

(defun arms2 ()
  "Print all Armstrong numbers from 0 to 999."
  (let ((count 0))
    (dotimes (abc 1000)
      (let ((a3b3c3 (+ (expt (floor abc 100) 3)
                       (expt (mod (floor abc 10) 10) 3)
                       (expt (mod abc 10) 3))))
        (when (= abc a3b3c3)
          (incf count)
          (pr "Armstrong number " count ": " abc))))))

(time (arms2))



(pr #\Newline "Using serapeum's 'summing' macro and a generator:")

(defun digits (n)
  "Return a generator (parameterless function)
   that on subsequent invocations will return
   the base 10 digits of 'n' from right to left."
  (lambda ()
    (if n
        (values (if (< n 10)
                    (prog1 n
                           (setq n nil))
                    (prog1 (mod n 10)
                           (setq n (truncate n 10))))
                t)
        (values nil nil))))


(defun armstrongp (n)
  "Return true if 'n' is an Armstrong number."
  (= n (summing
         (dogenerator (n (digits n))
           (sum (expt n 3))))))

(time (let ((count 0))
        (dotimes (n 1000)
          (when (armstrongp n)
            (incf count)
            (pr "Armstrong number " count ": " n)))))



(pr #\Newline "Using mapcar:")

(defun armsp (n)
  "Return true if 'n' is an Armstrong number."
  (labels ((number->digit-list (n)
             (string->list (jformat nil "%d" n)))
           (char->number (c)
             (- (char-code c) (char-code #\0))))

    (= n (apply + (mapcar (lambda (c) (expt (char->number c) 3))
                          (number->digit-list n))))))

(time (let ((count 0))
        (dotimes (n 1000)
          (when (armsp n)
            (incf count)
            (pr "Armstrong number " count ": " n)))))
