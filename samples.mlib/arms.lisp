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
  `(progn ,@(mapcar (lambda (it)
                      (list 'write it nil))
                    items)
          (writeln)))

(defun arms ()
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


(defun arms2 ()
  (let ((count 0))
    (dotimes (abc 999)
      (let ((a3b3c3 (+ (expt (floor abc 100) 3)
                       (expt (mod (floor abc 10) 10) 3)
                       (expt (mod abc 10) 3))))
        (when (= abc a3b3c3)
          (incf count)
          (pr "Armstrong number " count ": " abc))))))

(time (arms2))
