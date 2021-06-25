;;; create a window with a bitmap and fill the bitmap with all colors, brightness is fixed at 1

(define W 800)
(define H 600)

;(define wlog 720) ; account for 40px default padding
;(define hlog 520) ; account for 40px default padding
(define wlog W)
(define hlog H)

(make-frame "HSB" W H 0) ; frame with zero padding
(make-bitmap wlog hlog)  ; associate bitmap with frame so that set-pixel can be used

(let xloop ((x 0))
  (let ((hue (/ x wlog)))
    (let yloop ((y 0))
      (set-pixel x y (hsb-to-pixel hue (/ y hlog) 1))
      (if (< y (- hlog 1))
        (yloop (+ y 1)))))
  (if (< x (- wlog 1))
    (xloop (+ x 1))))

(open-frame)