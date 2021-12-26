; ackermann as a recursive function, with Y-combinator and 2 versions of the Z-combinator
; see https://stackoverflow.com/questions/37469378/y-combinator-implementation-scheme/37492667#37492667
; heavily recursive function, works, but takes too long with trace=env

; result: 509.0
; output: (empty)

; recursive function version

(define ackermann
  (lambda (m n)
    (cond
      ((= m 0) (+ n 1))
      ((= n 0) (ackermann (- m 1) 1))
      (t (ackermann (- m 1) (ackermann m (- n 1)))))))

(ackermann 3 6) ; ==> 509



; Y-combinator, only works with lazy evaluation

(define Y
  (lambda (f)
    ((lambda (g)
       (g g))
     (lambda (g)
       (f (g g))))))

;((Y (lambda (ackermann)
;      (lambda (m n)
;        (cond
;          ((= m 0) (+ n 1))
;          ((= n 0) (ackermann (- m 1) 1))
;          (t (ackermann (- m 1) (ackermann m (- n 1))))))))
; 3
; 6) ; ==> 509; stackoverflow



; applicative version of Y - the Z-combinator

(define Z
  (lambda (f)
    ((lambda (g)
       (g g))
     (lambda (g)
       (f (lambda args (apply (g g) args)))))))

((Z (lambda (ackermann)
      (lambda (m n)
        (cond
          ((= m 0) (+ n 1))
          ((= n 0) (ackermann (- m 1) 1))
          (t (ackermann (- m 1) (ackermann m (- n 1))))))))
 3
 6) ; ==> 509



(define Z^
  (lambda (f)
    ((lambda (g)
       (f (lambda args (apply (g g) args))))
     (lambda (g)
       (f (lambda args (apply (g g) args)))))))

((Z^ (lambda (ackermann)
       (lambda (m n)
         (cond
           ((= m 0) (+ n 1))
           ((= n 0) (ackermann (- m 1) 1))
           (t (ackermann (- m 1) (ackermann m (- n 1))))))))
 3
 6) ; ==> 509