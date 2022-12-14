;;;; AOC 2022 day 1

(load "mlib")

(princ "AOC 2022 day1 part1: total calories of the Elf carrying the most: ")
(->> (read-string "day01.txt")
     ((rcurry string-split "\\n\\n"))
     (map 'vector (rcurry string-split "\\n"))
     (map 'vector (lambda (v) (map 'vector parse-integer v)))
     (map 'list (lambda (v) (reduce + v)))
     (apply max)
     princ)
(terpri)


;; destructive vector sort
(defun sort (v comp)
  ((jmethod "java.util.Arrays" "sort" "Object?[]" "java.util.Comparator") v (jproxy "java.util.Comparator" "compare" comp))
  v)

(defun compare-desc (l r)
  (cond ((= l r) 0)
        ((> l r) -1)
        (t 1)))


(princ "AOC 2022 day1 part2: calories of top 3 Elfs:")
(->> (read-string "day01.txt")
     ((rcurry string-split "\\n\\n"))
     (map 'vector (rcurry string-split "\\n"))
     (map 'vector (lambda (v) (map 'vector parse-integer v)))
     (map 'vector (lambda (v) (reduce + v)))
     ((rcurry sort compare-desc))
     ((lambda (v)
       (dotimes (i 3)
         (print (1+ i)) (princ ": ")
         (princ (svref v i))))))

(terpri)
