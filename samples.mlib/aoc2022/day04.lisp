;;;; Advent of code day 4
;;;; CL/ Murmel solution based on https://github.com/ryukinix/adventofcode/blob/master/2022/day04/main.lisp

;; Run with
;;   sbcl --script day04.lisp
;; or
;;   java -jar jmurmel.jar day04.lisp

#|

--- Day 4: Camp Cleanup ---
Space needs to be cleared before the last supplies can be unloaded from the ships, and so several Elves have been assigned the job of cleaning up sections of the camp. Every section has a unique ID number, and each Elf is assigned a range of section IDs.

However, as some of the Elves compare their section assignments with each other, they've noticed that many of the assignments overlap. To try to quickly find overlaps and reduce duplicated effort, the Elves pair up and make a big list of the section assignments for each pair (your puzzle input).

For example, consider the following list of section assignment pairs:

2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
For the first few pairs, this list means:

Within the first pair of Elves, the first Elf was assigned sections 2-4 (sections 2, 3, and 4), while the second Elf was assigned sections 6-8 (sections 6, 7, 8).
The Elves in the second pair were each assigned two sections.
The Elves in the third pair were each assigned three sections: one got sections 5, 6, and 7, while the other also got 7, plus 8 and 9.
This example list uses single-digit section IDs to make it easier to draw; your actual list might contain larger numbers. Visually, these pairs of section assignments look like this:

.234.....  2-4
.....678.  6-8

.23......  2-3
...45....  4-5

....567..  5-7
......789  7-9

.2345678.  2-8
..34567..  3-7

.....6...  6-6
...456...  4-6

.23456...  2-6
...45678.  4-8
Some of the pairs have noticed that one of their assignments fully contains the other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained by 4-6. In pairs where one assignment fully contains the other, one Elf in the pair would be exclusively cleaning sections their partner will already be cleaning, so these seem like the most in need of reconsideration. In this example, there are 2 such pairs.

In how many assignment pairs does one range fully contain the other?

|#

#-murmel (require 'uiop)
#-murmel (progn

(defun string-split (str sep)
  (uiop:split-string str :separator sep))

(defun read-lines (fname)
  (uiop:read-file-lines fname))

)



#+murmel (progn

(require "mlib")

;; (string-split-list str regex) -> list
(defun string-split-list (str sep)
  (vector->list ((jmethod "java.lang.String" "split" "String") str sep)))

(defun read-lines (fname)
  (vector->list (read-all-lines fname)))

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
