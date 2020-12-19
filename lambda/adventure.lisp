;;; from http://www.lisperati.com/casting.html

(defun cadr (l) (car (cdr l)))
(defun caddr (l) (car (cddr l)))
(defun cddr (l) (cdr (cdr l)))

(defun first (l) (car l))
(defun second (l) (cadr l))
(defun third (l) (caddr l))

(defun mapcar (f l)
  (if l (cons (f (car l)) (mapcar f (cdr l)))
    nil))

(defun append2 (x y)
  (if (null x)
        (if (consp y) y (list y))
    (if (atom x)
          (if (consp y)
                (cons x y)
            (if (null y)
                  (list x)
              (list x y)))
      (cons (car x) (append2 (cdr x) y)))))

(defun append (x y . rest)
  (if (null rest)
        (append2 x y)
    (if (null (car rest))
          (append2 x y)
      (append (append2 x y) (car rest) (cdr rest)))))

(defun remove-if-not (pred l)
  (if l
        (let ((obj (car l)))
          (if (pred obj)
                (cons obj (remove-if-not pred (cdr l)))
            (remove-if-not pred (cdr l))))
    nil))
          
          


; http://www.lisperati.com/data.html
(define *objects* '(whiskey-bottle bucket frog chain))

(define *map* '((living-room (you are in the living-room of a wizards house. there is a wizard snoring loudly on the couch.)
                           (west door garden)  
                           (upstairs stairway attic))
              (garden (you are in a beautiful garden. there is a well in front of you.)
                      (east door living-room))
              (attic (you are in the attic of the wizards house. there is a giant welding torch in the corner.)
                     (downstairs stairway living-room))))

(define *object-locations* '((whiskey-bottle living-room)
                           (bucket living-room)
                           (chain garden)
                           (frog garden)))

(define *location* 'living-room)


; http://www.lisperati.com/looking.html
(defun describe-location (location map)
  (second (assoc location map)))

(describe-location 'living-room *map*)

(defun describe-path (path)
  (list 'there 'is 'a (second path) 'going (first path) 'from 'here.))

(describe-path '(west door garden))

(defun describe-paths (location map)
  (apply append (mapcar describe-path (cddr (assoc location map)))))

(describe-paths 'living-room *map*)

(defun is-at (obj loc obj-loc)
  (eq (second (assoc obj obj-loc)) loc))

(is-at 'whiskey-bottle 'living-room *object-locations*)

(defun describe-floor (loc objs obj-loc)
  (apply append (mapcar (lambda (x)
                            (list 'you 'see 'a x 'on 'the 'floor.))
                          (remove-if-not (lambda (x)
                                           (is-at x loc obj-loc))
                                         objs))))

(describe-floor 'living-room *objects* *object-locations*)

(defun look ()
  (append (describe-location *location* *map*)
          (describe-paths *location* *map*)
          (describe-floor *location* *objects* *object-locations*)))

(look)


; http://www.lisperati.com/walking.html
(defun walk-direction (direction)
  (let ((next (assoc direction (cddr (assoc *location* *map*)))))
    (cond (next (define *location* (third next)) (look))
	      (t    '(you cant go that way.)))))

(walk-direction 'west)