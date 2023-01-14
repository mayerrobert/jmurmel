;;;; Implement cons/car/cdr in Lambda-calculus, no cons cells needed
;;;; based on re:  What is the "minimal" set of primitives needed for...
;;;; https://news.ycombinator.com/item?id=8715655
;;;
;;; Run with:
;;;     $ java -jar jmurmel.jar --min+ --no-cons --result conscells-not-needed.lisp

(define cons   (lambda (a b) (lambda (f) (f a b))))
(define car-er (lambda (car-value cdr-value) car-value))
(define cdr-er (lambda (car-value cdr-value) cdr-value))
(define car    (lambda (c) (c car-er)))
(define cdr    (lambda (c) (c cdr-er)))

(car (cons 'a 'b))
; ==> a

(cdr (cons 'a 'b))
; ==> b
