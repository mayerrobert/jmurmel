;;; see https://en.wikipedia.org/wiki/Dragon_curve

;def dragon_curve(order: int, length) -> None:
;    """Draw a dragon curve."""
;    turn(order * 45)
;    dragon_curve_recursive(order, length, 1)

;def dragon_curve_recursive(order: int, length, sign) -> None:
;    if order == 0:
;        drawLine(length)
;    else:
;        rootHalf = (1 / 2) ** (1 / 2)
;        dragon_curve_recursive(order - 1, length * rootHalf, 1)
;        turn(sign * -90)
;        dragon_curve_recursive(order - 1, length * rootHalf, -1)


(define root-half (sqrt 0.5))

(defun dragon-curve (order length)
  (right (* order 45))
  (dragon-curve-recursive order length 1))

(defun dragon-curve-recursive (order length sign)
  (if (= 0 order)
        (forward length)
    (progn
      (dragon-curve-recursive (- order 1) (* length root-half) 1)
      (right (* sign -90))
      (dragon-curve-recursive (- order 1) (* length root-half) -1))))



(make-frame "Dragon curve")

(left 90)
(forward 10) (text "10")
(reset-frame)
(forward 30) (text "30")
(reset-frame)

(color 1)
(dragon-curve 8 20)

(reset-frame)
(color 2)
(dragon-curve 11 20)

(open-frame)



(make-frame "Dragon curve 2")

(color 1)
(dragon-curve 10 20)

;(reset-frame)
(color 2)
;(pen-up)
;(forward 20)
;(pen-down)
(dragon-curve 13 20)

(open-frame)
