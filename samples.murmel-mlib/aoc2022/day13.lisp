;;;; AOC 2022 day 13 in Murmel, see https://adventofcode.com/2022/day/13
;;;; translated from https://www.reddit.com/r/adventofcode/comments/zkmyh4/2022_day_13_solutions/j01i624/

#|

def cmp(l, r):
    match l, r:
        case int(), int():  return l-r
        case int(), list(): return cmp([l], r)
        case list(), int(): return cmp(l, [r])
        case list(), list():
            for z in map(cmp, l, r):
                if z: return z
            return cmp(len(l), len(r))

part1, _2, _6 = 0, 1, 2
for i, (l,r) in enumerate(map(eval, x.split()) for x in open('day13.txt').read().split('\n\n')):
    if cmp(l,r) < 0: part1 += i+1
    _2 += sum(cmp(x,[[2]]) < 0 for x in (l,r))
    _6 += sum(cmp(x,[[6]]) < 0 for x in (l,r))

print(part1, _2*_6)

-> 13 140

|#

(require "mlib")

(defun cmp (l r)
  (cond ((and (numberp l) (numberp r)) (- l r))
        ((numberp l) (cmp (vector l) r))
        ((numberp r) (cmp l (vector r)))
        (t (let loop ((z (map 'list cmp l r)))
             (if z
                 (if (/= (car z) 0)
                     (car z)
                   (loop (cdr z)))
               (- (length l) (length r)))))))

(defun read-packets ()
  (-> (read-textfile "day13.txt")
      (string-replace "[" "#(")  (string-replace "]" ")")  (string-replace "," " ")
      ((rcurry string-split "\\n\\n"))
      ((lambda (vec) (map 'vector (lambda (s) (map 'vector read-from-string (string-split s "\\n"))) vec)))))

(let ((part1 0) (position-1 1) (position-2 2) (i 1))
  (dovector (p (read-packets))
    (when (< (apply cmp (vector->list p)) 0) (incf part1 i))

    (incf position-1 (summing (dovector (x p) (when (< (cmp x #(#(2))) 0) (sum 1)))))

    ;; does the same as the previous line except compare with [[6]] instead of [[2]]
    (incf position-2 (length (remove-if (lambda (x) (>= (cmp x #(#(6))) 0)) p)))

    (incf i))

  (format t "AOC 2022 day 13 part1: ~d~%AOC 2022 day 13 part2: ~d~%" part1 (* position-1 position-2)))

nil
