;;; Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985
;;; p 170

;;; DERIV -- This is the Common Lisp version of a symbolic
;;; derivative benchmark written by Vaughan Pratt.
;;; It uses a simple subset of Lisp and does a lot of
;;; CONSing.

(defun deriv-aux (a) (list '/ (deriv a) a))

(defun deriv (a)
  (cond
    ((atom a)
     (cond ((eq a 'x) 1) (t 0)))
    ((eq (car a) '+)
     (cons '+ (mapcar deriv (cdr a))))
    ((eq (car a) '-)
     (cons '- (mapcar deriv
                      (cdr a))))
    ((eq (car a) '*)
     (list '*
           a
           (cons '+ (mapcar deriv-aux (cdr a)))))
    ((eq (car a) '/)
     (list '-
           (list '/
                 (deriv (cadr a))
                 (caddr a))
           (list '/
                 (cadr a)
                 (list '*
                       (caddr a)
                       (caddr a)
                       (deriv (caddr a))))))
    (t 'error)))

(defun run ()


  (let loop ((i 0))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (if (< i 1000)
          (loop (1+ i)))))

;;; call:  (run)
(run)