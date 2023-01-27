;;; Print a Julia set, based on https://rosettacode.org/wiki/Julia_set#Java
;;;
;;; Runs somewhat slow, you may want to compile/run this program:
;;;
;;;     jm --run julia.lisp

;(declaim (optimize (speed 0)))

(define W 800)
(define H 600)

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

(defun julia ()
  (let ((*start* (get-internal-cpu-time)))

  (let xloop ((x 0))
    (let yloop ((y 0))
      (let* ((zx (+ (/ (* 1.5 (- x (/ w 2)))
                       (* 0.5 ZOOM W))
                    MOVE_X))
             (zy (+ (/ (- y (/ H 2))
                       (* 0.5 ZOOM H))
                    MOVE_Y))
             (iter (calc zx zy)))
        (set-pixel x y (if (> iter 0) 
                             (hsb-to-pixel (/ MAX_ITERATIONS iter) 1 1)
                         0))
        (if (< y (1- h)) (yloop (1+ y)))))
    (if (< x (1- w)) (xloop (1+ x))))
  
  (flush-frame)
  (format t "CX: %g%nCX: %g%n" cx cy)
  (format t "Used CPU: %g ms%n" (/ (- (get-internal-cpu-time) *start*)
                                   (/ internal-time-units-per-second 1000)))))

(define *l* '((-0.7 . 0.27015)
            (-0.156844471694257101941 . -0.649707745759247905171)
            (-0.76 . 0.025) ; ..0.08
            (-0.76 . 0.03)
            (-0.76 . 0.04)
            (-0.76 . 0.05)
            (-0.76 . 0.06)
            (-0.76 . 0.07)
            (-0.76 . 0.08)
            (-0.4 . 0.6)
            (-0.758750 . 0.0696875)))


(writeln)
(writeln "Will loop forever, press CTRL-C to end." nil)
(writeln)

(let loop ((l *l*))
  (if l (let* dynamic ((params (car l))
                       (cx (car params))
                       (cy (cdr params)))
          (julia)
          (sleep 5)
          (loop (cdr l)))
    (loop *l*)))

nil
