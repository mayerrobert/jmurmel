;;; Print a Julia set, based on https://rosettacode.org/wiki/Julia_set#Java
;;;
;;; Runs somewhat slow, you may want to compile/run this program:
;;;
;;;     jm --run julia.lisp

;(declaim (optimize (speed 0)))

(define W 600)
(define H 400)

(define MAX_ITERATIONS 300)
(define ZOOM 1)
(define MOVE_X 0)
(define MOVE_Y 0)

;(define CX -0.7)
;(define CY 0.27015)

;(define CX -0.156844471694257101941)
;(define CY -0.649707745759247905171)

; CX=-0.76
; CY=0.025..0.08

;(define CX -0.4)
;(define CY  0.6)

(define CX -0.758750)
(define CY  0.0696875)


; https://rosettacode.org/wiki/Julia_set#Java
;
;        for (int x = 0; x < w; x++) {
;            for (int y = 0; y < h; y++) {
;                double zx = 1.5 * (x - w / 2) / (0.5 * ZOOM * w) + MOVE_X;
;                double zy = (y - h / 2) / (0.5 * ZOOM * h) + MOVE_Y;
;                float i = MAX_ITERATIONS;
;                while (zx * zx + zy * zy < 4 && i > 0) {
;                    double tmp = zx * zx - zy * zy + CX;
;                    zy = 2.0 * zx * zy + CY;
;                    zx = tmp;
;                    i--;
;                }
;                int c = Color.HSBtoRGB((MAX_ITERATIONS / i) % 1, 1, i > 0 ? 1 : 0);
;                image.setRGB(x, y, c);
;            }
;        }

(make-frame "Julia set" W H 0)
(make-bitmap W H)

(defun calc (zx zy)
  (let loop ((i MAX_ITERATIONS)
             (zx zx)
             (zy zy))
    (if (> i 0)
          (if (< (+ (* zx zx) (* zy zy)) 4)
                (loop (1- i)
                      (+ (* zx zx)
                         (* -1 zy zy)
                         CX)
                      (+ (* 2 zx zy) CY))
            i)
      i)))

(open-frame)

(define *start* (get-internal-cpu-time))

(let xloop ((x 0))
  (let yloop ((y 0))
    (let* ((zx (+ (/ (* 1.5 (- x (/ w 2)))
                     (* 0.5 ZOOM W))
                  MOVE_X))
           (zy (+ (/ (- y (/ H 2))
                     (* 0.5 ZOOM H))
                  MOVE_Y))
           (iter (calc zx zy)))
      (set-pixel x y (hsb-to-pixel (/ MAX_ITERATIONS iter) 1 (if (> iter 0) 1 0)))
      (if (< y (1- h)) (yloop (1+ y)))))
  (flush-frame)
  (if (< x (1- w)) (xloop (1+ x))))

(format t "Used CPU: %g ms%n" (/ (- (get-internal-cpu-time) *start*)
                                 (/ internal-time-units-per-second 1000)))