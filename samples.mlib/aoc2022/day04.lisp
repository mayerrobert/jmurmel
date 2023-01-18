;;;; Advent of code day 4, see https://adventofcode.com/2022/day/4
;;;; CL/ Murmel solution based on https://github.com/ryukinix/adventofcode/blob/master/2022/day04/main.lisp

;; Run with
;;   sbcl --script day04.lisp
;; or
;;   java -jar jmurmel.jar day04.lisp

#-murmel (require 'uiop)
#-murmel (progn

(defun string-split-list (str sep)
  (uiop:split-string str :separator sep))

(defun read-lines (fname)
  (uiop:read-file-lines fname))

)



#+murmel (progn

(require "mlib")

;; (string-split-list str regex) -> list
(defun string-split-list (str sep)
  (vector->list (string-split str sep)))

(defun read-lines (fname)
  (vector->list (read-textfile-lines fname)))

)



(defun contains (r1 r2)
  (destructuring-bind (min1 max1) r1
    (destructuring-bind  (min2 max2) r2
      (and (<= min1 min2) (>= max1 max2)))))

(defun overlaps (r1 r2)
  (or (contains r1 r2)
      (contains r2 r1)))

(defun overlaps-partial (r1 r2)
  (destructuring-bind (min1 max1) r1
    (destructuring-bind  (min2 max2) r2
      (and (<= min1 max2) (<= min2 max1)))))

(defun parse-pair (pair)
  (let* ((ranges (string-split-list pair ","))
         (range1 (string-split-list (nth 0 ranges) "-"))
         (range2 (string-split-list (nth 1 ranges) "-")))
    (list (mapcar #'parse-integer range1)
          (mapcar #'parse-integer range2))))

(defun resolve (pairs overlapfn)
  (let ((sum 0))
    (dolist (range (mapcar #'parse-pair pairs))
      (when (apply overlapfn range)
        (incf sum)))
    sum))

(defun main ()
  (let ((pairs (read-lines "day04.txt")))
    (princ "AOC 2022 day4 part1: ") (princ (resolve pairs #'overlaps)) (terpri)
    (princ "AOC 2022 day4 part2: ") (princ (resolve pairs #'overlaps-partial)) (terpri)))

(main) ; ==> 2 4
