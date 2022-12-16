(require "mlib")  



;; CPS-transformiert "forms" in 
;;
;;   (lambda () (append (butlast forms) (list (last forms) continuation)))
;;
;; aber: jedes vorkommen von yield beendet das aktuelle lambda, und es wird ein neues lambda beginnend mit der continuation erzeugt
#|

1, nil                                     -> (lambda () (list 1 nil))
(f), nil                                   -> (lambda () (list (f) nil))
1 2 (f1) (f2), cont                        -> (lambda () 1 2 (f1) (list (f2) cont))
(f1) (f2) (yield 123) (f3) (f4) (f5), cont -> (lambda () (f1) (f2) (list 123 (lambda () (f3) (f4) (list (f5) cont))))

|#

(defun convert-forms (forms continuation)
  `(lambda () ,@(collecting (let loop ((forms forms))
                             (cond ((null forms)
                                    ;(writeln "nil" nil)
                                    nil)

                                   ;; last form aka "return stmt"
                                   ((null (cdr forms))
                                    ;(write "last form: " nil) (writeln (car forms))
                                    (let ((form (car forms)))
                                      (if (and (consp form) (eq 'yield (car form)))
                                            (collect `'(,(cadr form) ,continuation))
                                        (collect `'(,(car forms) ,continuation)))))

                                   (t
                                    ;(write "else: " nil) (writeln forms)
                                    (let ((form (car forms)))
                                      (if (and (consp form) (eq 'yield (car form)))
                                            (collect `(list ',(cadr form) ,(convert-forms (cdr forms) continuation)))
                                        (progn (collect form) (loop (cdr forms)))))))))))



;;; verpackt `forms` in einen generator, also ein parameterloses lambda,
;;; das bei mehrfachen aufrufen aufeinanderfolgende [value valid] values liefert.
;;;
;;; "forms" darf "yield" enthalten (ausser innerhalb "let dynamic")
;;; define/ defun/ defmacro sind verboten
(defmacro scan-coroutine forms
  (cond ((null forms) '(lambda () (values nil nil)))

        (t `(let ((cont ,(convert-forms forms nil)))
              (lambda ()
                (if cont
                      (let ((old cont))
                        (setq cont (lambda () (error "generator is a zombie")))
                        (destructuring-bind (val newcont) (old)
                          (setq cont newcont)
                          (values val t)))
                  (values nil nil)))))))
