;;;; AOC 2022 day 10 in Murmel, based on https://www.reddit.com/r/adventofcode/comments/zhjfo4/2022_day_10_solutions/izmspl7/

#| (mostly) original Python:
from itertools import accumulate

f = lambda x: int(x) if x[-1].isdigit() else 0
xs = [*map(f, open('day10.txt').read().split())]

part1, part2 = 0, '\n'
for i, x in enumerate(accumulate([1]+xs), 1):
    part1 += i*x if i%40==20 else 0
    part2 += '#' if ((i-1)%40)-x in [-1,0,1] else ' '
    if i%40 == 0:
        part2 += '\n'

print(part1, part2)
|#


(require "mlib")

(let ((xs (->> "day10.txt"
                read-textfile
                ((rcurry string-split "[\\n ]"))
                (map 'vector read-from-string)
                (map 'vector (lambda (x) (if (numberp x) x 0)))))
      (part1 0)
      (part2 (make-array 0 'character t))
      (i 1)
      (x 0))

  (vector-add part2 #\Newline)
  (dogenerator (n (scan-concat (scan '(1)) (scan xs)))
     (incf x n)
     (when (= (rem i 40) 20) (incf part1 (* i x)))
     (vector-add part2 (case (- (rem (1- i) 40) x)
                             ((-1.0 0.0 1.0) #\#)
                             (t #\ )))
     (when (zerop (rem i 40))
       (vector-add part2 #\Newline))
     (incf i))

  (princ "AOC 2022 day10 part1: ") (princ part1) (terpri)
  (princ "AOC 2022 day10 part2: ") (princ part2))

nil
