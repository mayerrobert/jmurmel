; A very inefficient tail recursive list-reverse function
;
; output: ((1) (2) (3) (4) (5))((5) (4) (3) (2) (1))
; result: t

(labels
  ((A (L Lp)
      (if (eq L ())
          Lp
          (A (cdr L) (cons (car L) Lp))))

   (R (L)
      (A L nil))

   (test (L)
         (write L)
         (write (R L)))

   (make-a-list ()
     (cons (cons (quote 1) ())
     (cons (cons (quote 2) ())
     (cons (cons (quote 3) ())
     (cons (cons (quote 4) ())
     (cons (cons (quote 5) ())
     ())))))))

  (test (make-a-list)))
