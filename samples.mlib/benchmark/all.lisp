;;; Run all benchmarks from a single lisp file

#+murmel (progn

(require "mlib")

(defmacro eval-when (sit . body)
  `(progn ,@body))

(defun lisp-implementation-type ()
  ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "getEngineName") ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "new"))))

(defun lisp-implementation-version ()
  ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "getLanguageVersion") ((jmethod "io.github.jmurmel.jsr223.JMurmelScriptEngineFactory" "new"))))

)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "bench" "bench.lisp"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define *allfiles* '("00_tak_apo.lisp"
                       "3.01_tak.lisp"
                       "3.02_stak.lisp"
                       "3.03_ctak.lisp"
                       "3.04_takL.lisp"
                       "3.08_destru.lisp"
                       "3.10_deriv.lisp"
                       "array1.lisp"
                       "dovector.lisp"
                       "q.lisp")))

(defmacro all ()
  `(progn ,@(mapcar (lambda (f) `(load ,f)) *allfiles*)))

(princ (lisp-implementation-type))
(princ #\ )
(princ (lisp-implementation-version))
(terpri)

(all)
