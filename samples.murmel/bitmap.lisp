;;;; Demo for using a bitmap and setting pixels to an HSB color value
;;;;
;;;; create a window with a bitmap and fill the bitmap with all colors, brightness is fixed at 1

(define *width* 800)
(define *height* 600)

(make-frame "HSB Bitmap Demo" *width* *height* 0) ; frame with zero padding
(make-bitmap *width* *height*)  ; associate bitmap with frame so that set-pixel can be used

(let xloop ((x 0))
  (let ((hue (/ x *width*)))
    (let yloop ((y 0))
      (set-pixel x y (hsb-to-pixel hue (/ y *height*) 1))
      (if (< y (- *height* 1))
        (yloop (1+ y)))))
  (if (< x (- *width* 1))
    (xloop (1+ x))))

(open-frame)