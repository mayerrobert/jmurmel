;;; See https://en.wikipedia.org/wiki/L-system

(define rules nil)
(define actions nil)

(defun l-system (order size op)
  (if (= 0 order)
        (if (eq op 'X) nil
          (if (eq op 'Y) nil
            (forward size)))
    (let loop ((word (cdr (assoc op rules))))
      ((cdr (assoc (car word) actions)) (1- order) size)
      (if (cdr word)
        (loop (cdr word))))))



(make-frame "Gosper Curve")

(setq rules '((A . (A - B - - B + A + + A A + B -))
              (B . (+ A - B B - - B - A + + A + B))))

(setq actions '((A . (lambda (o size) (l-system o size 'A)))
                (B . (lambda (o size) (l-system o size 'B)))
                (- . (lambda (o size) (right 60)))
                (+ . (lambda (o size) (left 60)))))

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

(setq actions '((F . (lambda (o size) (l-system o size 'F)))
                (+ . (lambda (o size) (left 90)))
                (- . (lambda (o size) (right 90)))))

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

(make-frame "Sierpinsky")

(setq rules '((F . (F - G + F + G - F))
              (G . (G G))))

(setq actions '((F . (lambda (o size) (l-system o size 'F)))
                (G . (lambda (o size) (l-system o size 'G)))
                (+ . (lambda (o size) (left 120)))
                (- . (lambda (o size) (right 120)))))

(left 90)
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

(make-frame "Sierpinsky triangle")

(setq rules '((A . (B - A - B))
              (B . (A + B + A))))

(setq actions '((A . (lambda (o size) (l-system o size 'A)))
                (B . (lambda (o size) (l-system o size 'B)))
                (+ . (lambda (o size) (left 60)))
                (- . (lambda (o size) (right 60)))))

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

(make-frame "Dragon Curve")

(setq rules '((F . (F + G))
              (G . (F - G))))

(setq actions '((F . (lambda (o size) (l-system o size 'F)))
                (G . (lambda (o size) (l-system o size 'G)))
                (+ . (lambda (o size) (right 90)))
                (- . (lambda (o size) (left 90)))))

(left 90)
(l-system 10 20 'F)
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

(make-frame "Fern")

(setq rules '((X . (F + [ [ X ] - X ] - F [ - F X ] + X))
              (F . (F F))))

(setq actions '((F . (lambda (o size) (l-system o size 'F)))
                (X . (lambda (o size) (l-system o size 'X)))
                (+ . (lambda (o size) (left 25)))
                (- . (lambda (o size) (right 25)))
                ([ . (lambda (o size) (push-pos)))
                (] . (lambda (o size) (pop-pos)))))

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

(make-frame "Heighway dragon")

(setq rules '((X . (X + Y F +))
              (Y . (- F X - Y))))

(setq actions '((F . (lambda (o size) (forward size)))
                (X . (lambda (o size) (l-system o size 'X)))
                (Y . (lambda (o size) (l-system o size 'Y)))
                (+ . (lambda (o size) (right 90)))
                (- . (lambda (o size) (left  90)))))

(left 90)
(forward 20)
(l-system 14 20 'X)
(open-frame)



; https://en.wikipedia.org/wiki/Dragon_curve#Terdragon
; Terdragon curve
;F -> F + F - F, angle = 120

(make-frame "Terdragon Curve")

(setq rules '((F . (F + F - F))))

(setq actions '((F . (lambda (o size) (l-system o size 'F)))
                (+ . (lambda (o size) (right 120)))
                (- . (lambda (o size) (left 120)))))

(left 90)
(l-system 9 20 'F)
(open-frame)



;https://en.wikipedia.org/wiki/L%C3%A9vy_C_curve
;Variables:     F
;Constants:     + -
;Start:         F
;Rules:         F -> +F--F+
;
;where "F" means "draw forward", "+" means "turn clockwise 45deg", and "-" means "turn anticlockwise 45deg". 

(make-frame "Levy C Curve")

(setq rules '((F . (+ F - - F +))))

(setq actions '((F . (lambda (o size) (l-system o size 'F)))
                (+ . (lambda (o size) (right 45)))
                (- . (lambda (o size) (left 45)))))

(l-system 12 20 'F)
(open-frame)
