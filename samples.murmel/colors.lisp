;;; create a window and draw a line for each turtle graphics color

(make-frame "Murmel colors")
(open-frame)

(let loop ((col 15))
  (move-to 0 col)
  (color col)
  (forward 20)
  (flush-frame)
  (if (> col 0)
    (loop (- col 1))))