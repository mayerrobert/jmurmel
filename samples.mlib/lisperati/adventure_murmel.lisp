;;; http://www.lisperati.com/casting.html translated to murmel w/ mlib
;;; see adventure_cl.lisp for the original Common Lisp
;;; see ../../samples.murmel/lisperati/adventure_murmel.lisp for a translation to murmel w/o mlib 

(require "mlib")

(defmacro defparameter (sym val) `(define ,sym ,val))

(defun first (l) (car l))
(defun second (l) (cadr l))
(defun third (l) (caddr l))

(defun remove-if-not (pred l)
  (remove-if (complement pred) l))



; http://www.lisperati.com/data.html
(defparameter *objects* '(whiskey-bottle bucket frog chain))

(defparameter *map* '((living-room (you are in the living-room of a wizard's house. there is a wizard snoring loudly on the couch.)
                                   (west door garden)  
                                   (upstairs stairway attic))
                      (garden (you are in a beautiful garden. there is a well in front of you.)
                              (east door living-room))
                      (attic (you are in the attic of the abandoned house. there is a giant welding torch in the corner.)
                             (downstairs stairway living-room))))

(defparameter *object-locations* '((whiskey-bottle living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *location* 'living-room)


; http://www.lisperati.com/looking.html
(defun describe-location (location map)
  (second (assoc location map)))

(describe-location 'living-room *map*)

(defun describe-path (path)
  `(there is a ,(second path) going ,(first path) from here.))

(describe-path '(west door garden))

(defun describe-paths (location map)
  (apply #'append (mapcar #'describe-path (cddr (assoc location map)))))

(describe-paths 'living-room *map*)

(defun is-at (obj loc obj-loc)
  (eq (second (assoc obj obj-loc)) loc))

(is-at 'whiskey-bottle 'living-room *object-locations*)

(defun describe-floor (loc objs obj-loc)
  (apply #'append (mapcar (lambda (x)
                            `(you see a ,x on the floor.))
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
    (cond (next (setf *location* (third next)) (look))
	      (t    '(you cant go that way.)))))

(walk-direction 'west)


; http://www.lisperati.com/spels.html
(defmacro defspel rest `(defmacro ,@rest))

(defspel walk (direction)
  `(walk-direction ',direction))

(walk east)

(defun pickup-object (object)
  (cond ((is-at object *location* *object-locations*) (push (list object 'body) *object-locations*) `(you are now carrying the ,object))
         (t '(you cannot get that.))))

(defspel pickup (object)
  `(pickup-object ',object))

(pickup whiskey-bottle)

(defun inventory ()
  (remove-if-not (lambda (x)
    (is-at x 'body *object-locations*))
    *objects*))

(defun have (object)
  (member object (inventory)))


; http://www.lisperati.com/actions.html
(defparameter *chain-welded* nil)

(defparameter *bucket-filled* nil)

(defspel game-action (command subj obj place . rest)
  `(defspel ,command (subject object)
     `(cond ((and (eq *location* ',',place)
                  (eq ',subject ',',subj)
                  (eq ',object ',',obj)
                  (have ',',subj))
             ,@',rest)
            (t '(i cant ,',command like that.)))))

(game-action weld chain bucket attic
             (cond ((and (have 'bucket) (setf *chain-welded* 't)) '(the chain is now securely welded to the bucket.))
                   (t '(you do not have a bucket.))))

(game-action dunk bucket well garden
             (cond (*chain-welded* (setf *bucket-filled* 't) '(the bucket is now full of water))
                   (t '(the water level is too low to reach.))))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. he is so upset he banishes you to the netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. he hands you the magic low-carb donut- you win! the end.))))



; Solution:
(pickup bucket)
(walk west)
(pickup chain)
(walk east)
(walk upstairs)
(weld chain bucket)
(walk downstairs)
(walk west)
(dunk bucket well)
(walk east)
(splash bucket wizard)