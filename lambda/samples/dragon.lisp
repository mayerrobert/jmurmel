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

(make-frame "Dragon curve")

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



(left 90)
(color 1)
(dragon-curve 15 20)

;(reset-frame)
;(right 180)
(color 2)
(dragon-curve 15 20)

(open-frame)
