(define W 800)
(define H 600)

(define w 300)
(define h 100)

(make-frame "HSB" W H)
(make-bitmap w h)

(let xloop ((x 0))
  (let ((hue (/ x w)))
    (let yloop ((y 0))
      (set-pixel x y (hsb-to-pixel hue (/ y h) 1))
      (if (< y (- h 1)) (yloop (+ y 1)))))
  (if (< x (- w 1)) (xloop (+ x 1))))

(open-frame)