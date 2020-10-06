; A very inefficient tail recursive list-reverse function
;
; output: ((a) (b) (c) (d) (e))((e) (d) (c) (b) (a))
; result: t

(labels
  ((R^ (L Lp)
      (if (eq L ())
          Lp
          (R^ (cdr L) (cons (car L) Lp))))

   (R (L)
      (R^ L nil))

   (test (L)
         (write L)
         (write (R L)))

   (make-a-list ()
     (cons (cons (quote a) ())
     (cons (cons (quote b) ())
     (cons (cons (quote c) ())
     (cons (cons (quote d) ())
     (cons (cons (quote e) ())
     ())))))))

  (test (make-a-list)))
