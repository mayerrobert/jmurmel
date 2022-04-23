;;; experiments with macro expansion that jmurmel currently does wrong:
;;; a macro is expanded each time, should be expanded once

#+murmel (require "mlib")
#+murmel (require "expand")


#+murmel (define x 0)
#-murmel (defparameter x 0)

(defmacro m ()
  (setq x (1+ x)))

(defun f()
  (dotimes (i 5)
    (princ "f (m): ")
    (princ (m))
    (terpri)))

#+murmel
(progn
  (pprint f)
  (pprint (expand f))
  (terpri))

(setq x 0)
; f sollte bereits das 1x expandierte macro enthalten und immer dieselbe zahl ausgeben
; in sbcl ist das so, in murmel nicht: murmel expandiert jedesmal und zaehlt hoch
; -> fixed
(f)
(f)
(pprint f) ; after the first application f will have the macro just-in-time expanded and no longer contain the macro application but the expansion
(terpri)


; f wird ersetzt durch eine version mit 1x expandiertem macro
; -> murmel verhaelt sich wie sbcl
(setq x 0)
#+murmel
(progn
  (define f (expand f))
  (f)
  (f))


#-murmel
(progn
(defun f2()
  (macrolet ((m () (setq x (1+ x))))
  (dotimes (i 5)
    (princ "f2 (m): ")
    (princ (m))
    (terpri))))

(f2)
(f2)

(defun f3()
  (dotimes (i 5)
  (macrolet ((m () (setq x (1+ x))))
    (princ "f3 (m): ")
    (princ (m))
    (terpri))))

(f3)
(f3)

(terpri)
(f)
(f2)
(f3))