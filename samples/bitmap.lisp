(define W 800)
(define H 600)

(define wlog 720)
(define hlog 520)

(make-frame "HSB" W H)
(make-bitmap wlog hlog)

(let xloop ((x 0))
  (let ((hue (/ x wlog)))
    (let yloop ((y 0))
      (set-pixel x y (hsb-to-pixel hue (/ y hlog) 1))
      (if (< y (- hlog 1)) (yloop (+ y 1)))))
  (if (< x (- wlog 1)) (xloop (+ x 1))))

(move-to 0 0)
(line-to wlog hlog)

(open-frame)