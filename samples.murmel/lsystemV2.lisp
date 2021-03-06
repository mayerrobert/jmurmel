;;; See https://en.wikipedia.org/wiki/L-system and https://de.wikipedia.org/wiki/Fraktal
;;; and https://jsantell.com/l-systems/

(define rules nil)
(define actions nil)

(defun l-system (order size op)
  (if (= 0 order)

        (if (eq op 'X) nil
          (if (eq op 'Y) nil
            (if (eq op 'Z) nil
              (if (eq op 'W) (progn (pen-up) (forward size) (pen-down))
                (forward size)))))

    (let loop ((word (cdr (assoc op rules))))
      (let* ((nextop (car word))
             (action (cdr (assoc nextop actions))))
        (if action
              (action)
          (l-system (1- order) size nextop)))
      (if (cdr word)
        (loop (cdr word))))))



(make-frame "Gosper Curve")

(setq rules '((A . (A - B - - B + A + + A A + B -))
              (B . (+ A - B B - - B - A + + A + B))))

(setq actions (list (cons '-  (lambda () (right 60)))
                    (cons '+  (lambda () (left 60)))))

(left 90)
(l-system 4 20 'A)
(open-frame)



;Example 4: Koch curve
;
;A variant of the Koch curve which uses only right angles.
;
;    variables : F
;    constants : + -
;    start : F
;    rules : (F -> F+F-F-F+F)
;
;Here, F means "draw forward", + means "turn left 90deg", and - means "turn right 90deg"

(make-frame "Koch Curve")

(setq rules '((F . (F + F - F - F + F))))

(setq actions (list (cons '+  (lambda () (left 90)))
                    (cons '-  (lambda () (right 90)))))

;(left 90)
(l-system 3 20 'F)
(open-frame)



; Example 5: Sierpinski triangle
;The Sierpinski triangle drawn using an L-system.
;
;    variables : F G
;    constants : + -
;    start : F-G-G
;    rules : (F -> F-G+F+G-F), (G -> GG)
;    angle : 120deg
;
;Here, F means "draw forward", G means "draw forward", + means "turn left by angle", and - means "turn right by angle". 

(make-frame "Sierpinsky triangle 4")

(setq rules '((F . (F - G + F + G - F))
              (G . (G G))))

(setq actions (list (cons '+  (lambda () (left 120)))
                    (cons '-  (lambda () (right 120)))))

(left 180)
(l-system 4 20 'F)
(right 120)
(l-system 4 20 'G)
(right 120)
(l-system 4 20 'G)
(open-frame)



;It is also possible to approximate the Sierpinski triangle using a Sierpinski arrowhead curve L-system.
;
;    variables : A B
;    constants : + -
;    start : A
;    rules : (A -> B-A-B), (B -> A+B+A)
;    angle : 60deg
;
;Here, A and B both mean "draw forward", + means "turn left by angle", and - means "turn right by angle".

(make-frame "Sierpinsky arrowhead 6")

(setq rules '((A . (B - A - B))
              (B . (A + B + A))))

(setq actions (list (cons '+  (lambda () (left 60)))
                    (cons '-  (lambda () (right 60)))))

(l-system 6 20 'A)
(open-frame)



; Example 6: Dragon curve
;The dragon curve drawn using an L-system.
;
;    variables : F G
;    constants : + -
;    start : F
;    rules : (F -> F+G), (G -> F-G)
;    angle : 90deg
;
;Here, F and G both mean "draw forward", + means "turn left by angle", and - means "turn right by angle". 

(make-frame "Dragon Curve 14")

(setq rules '((F . (F + G))
              (G . (F - G))))

(setq actions (list (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left 90)))))

(left 90)
;(l-system 10 20 'F)
(l-system 14 20 'F)
(open-frame)



;Example 7: Fractal plant
;See also: Barnsley fern
;
;    variables : X F
;    constants : + - [ ]
;    start : X
;    rules : (X -> F+[[X]-X]-F[-FX]+X), (F -> FF)
;    angle : 25deg
;
;Here, F means "draw forward", - means "turn right 25deg", and + means "turn left 25deg".
;X does not correspond to any drawing action and is used to control the evolution of the curve. 
;The square bracket "[" corresponds to saving the current values for position and angle, 
;which are restored when the corresponding "]" is executed. 

(make-frame "Fern 6")

(setq rules '((X . (F + [ [ X ] - X ] - F [ - F X ] + X))
              (F . (F F))))

(setq actions (list (cons '+  (lambda () (left 25)))
                    (cons '-  (lambda () (right 25)))
                    (cons '[  (lambda () (push-pos)))
                    (cons ']  (lambda () (pop-pos)))))

(left 65)
(l-system 6 20 'X)
(open-frame)



;https://en.wikipedia.org/wiki/Dragon_curve#Heighway_dragon
;Heighway dragon
;It can be written as a Lindenmayer system with
;
;    angle 90deg
;    initial string FX
;    string rewriting rules
;        X -> X+YF+
;        Y -> -FX-Y.

(make-frame "Heighway dragon 14")

(setq rules '((X . (X + Y F +))
              (Y . (- F X - Y))))

(setq actions (list (cons 'F  (lambda () (forward 20)))
                    (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left  90)))))

(left 90)
(forward 20)
(l-system 14 20 'X)
(open-frame)



;https://en.wikipedia.org/wiki/Dragon_curve#Twindragon
;It can be also written as a Lindenmayer system - it only needs adding another section in initial string:
;
;    angle 90deg
;    initial string FX+FX+
;    string rewriting rules
;        X -> X+YF
;        Y -> FX-Y

(make-frame "Twindragon 13")

(setq rules '((X . (X + Y F))
              (Y . (F X - Y))))

(setq actions (list (cons 'F  (lambda () (forward 20)))
                    (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left  90)))))

;(left 90)
(forward 20)
(l-system 13 20 'X)
(right 90)
(forward 20)
(color 3) (l-system 13 20 'X)
(right 90)

(open-frame)




; https://en.wikipedia.org/wiki/Dragon_curve#Terdragon
; Terdragon curve
;F -> F + F - F, angle = 120

(make-frame "Terdragon Curve 9")

(setq rules '((F . (F + F - F))))

(setq actions (list (cons '+  (lambda () (right 120)))
                    (cons '-  (lambda () (left 120)))))

;(left 90)
;(l-system 10 20 'F)
(l-system 9 20 'F)
(color 3) (left 120) (l-system 9 20 'F)
(color 4) (left 120) (l-system 9 20 'F)
(open-frame)



;https://en.wikipedia.org/wiki/L%C3%A9vy_C_curve
;Variables:     F
;Constants:     + -
;Start:         F
;Rules:         F -> +F--F+
;
;where "F" means "draw forward", "+" means "turn clockwise 45deg", and "-" means "turn anticlockwise 45deg". 

(make-frame "Levy C Curve 12")

(setq rules '((F . (+ F - - F +))))

(setq actions (list (cons '+  (lambda () (right 45)))
                    (cons '-  (lambda () (left  45)))))

(l-system 12 20 'F)
(open-frame)


;https://de.wikipedia.org/wiki/Fraktal
; Hilbert Kurve, 1 : 1/2
(make-frame "Hilbert Curve 6")

(setq rules '((X . (- Y F + X F X + F Y -))
              (Y . (+ X F - Y F Y - F X +))))

(setq actions (list (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left  90)))))

(l-system 6 20 'X)
(open-frame)


; Peano Kurve
(make-frame "Peano Curve 4")

(setq rules '((X . (X F Y F X + F + Y F X F Y - F - X F Y F X))
              (Y . (Y F X F Y - F - X F Y F X + F + Y F X F Y))))

(setq actions (list (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left  90)))))

(l-system 4 20 'X)
(open-frame)


; Peano Kurve, 1 : 1/3
(make-frame "Peano Kurve 4")

(setq rules '((F . (F - F + F + F + F - F - F - F + F))))

(setq actions (list (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left  90)))))

(l-system 4 20 'F)
(open-frame)


; Penta Plexity
(make-frame "Penta Plexity 4")

(setq rules '((F . (F + + F + + F R F - F + + F))))

(setq actions (list (cons '+  (lambda () (right 36)))
                    (cons '-  (lambda () (left  36)))
                    (cons 'R  (lambda () (left 180)))))

(l-system 4 20 'F)
(right 72)
(l-system 4 20 'F)
(right 72)
(l-system 4 20 'F)
(right 72)
(l-system 4 20 'F)
(right 72)
(l-system 4 20 'F)

(open-frame)


; Sierpinsky (Menger) Teppich
(make-frame "Sierpinsky (Menger) Teppich")

(setq rules '((F . (F + F - F - F F - F - F - w F))
              (w . (w w w))))

(setq actions (list (cons '+  (lambda () (right 90)))
                    (cons '-  (lambda () (left  90)))))

(l-system 4 20 'F)
(open-frame)
