;;;; Python style generators: "yield" may occur in stmt lists

#+murmel (require "mlib")  
#-murmel (ql:quickload "serapeum")



;; (convert-forms forms continuation) -> parameterlsee-coroutine
;;
;; CPS-transformiert "forms" in eine coroutine, also eine parameterlose funktion, die eine liste mit (value continuation) liefert.
;; jedes "yield" wird in so ein "return (value continuation)" transformiert, beim erneuten aufrufen der coroutine gehts mit der continuation des letzten yield weiter.
;; die letzte form von forms ist entweder ein yield oder wird implizit als yield betrachtet.
;; das letzte yield liefert eine liste mit (argument-von-yield continuation-die-convert-forms-uebergeben-wurde).
;;
;; erstmal darf "forms" "yield" innererhalb expliziten oder impliziten progn enthalten (damit ist der returnwert von yield egal).
;; wenn yield innerhalb eines "let dynamic" vorkommt, sehen die stmts vor dem yield den wert den "let dynamic" an eine globale variable gebunden hat,
;; die stmts nach "yield" ggf. einen anderen wert der globalen variable.
;;
;; define/ defun/ defmacro dürfen in "forms" nicht vorkommen.
;;
#|

1, nil                                             -> (lambda () (list 1 nil))
(f), nil                                           -> (lambda () (list (f) nil))
1 2 (f1) (f2), cont                                -> (lambda () 1 2 (f1) (list (f2) cont))
(f1) (f2) (yield 123) (f3) (f4) (f5), cont         -> (lambda () (f1) (f2) (list 123 (lambda () (f3) (f4) (list (f5) cont))))
(progn (f1) (f2) (yield 123) (f3) (f4) (f5)), cont -> (lambda () (f1) (f2) (list 123 (lambda () (f3) (f4) (list (f5) cont))))

(f1)
(f2)
(if t (yield 1) (yield 2))
(f3), cont                                 -> (lambda () (f1) (f2) (if t (lambda () (list 1 (append (f3) cont))) (lambda () (list 2 (append (f3) cont)))))

später beschränkung "yield nur in stmt-liste" aufheben:
(f1 a1 a2)
(f2 a21 a22 (yield a23) a24 a25)
(f3 a31 a32)                        -> (lambda ()
                                         (f1 a1 a2)
                                         (let ((func f2) (_a21 a21) (_a22 a22) (_a23 a23)) ;; funktion und alle argumente bis inkl dem yield zwischenspeichern
                                           (list _a23
                                                 (lambda ()
                                                   (func _a21 _a22 _a23 a24 a25)
                                                   (f3 a31 a32))

später ggf beschränkung "parameterlose funktion" aufheben, d.h. das erste und jedes generierte lambda hat dieselbe parameterliste
|#

(defun convert-forms (forms continuation)
  (collecting (collect 'lambda)
              (collect ())
              (let loop ((collect collect) (forms forms) (continuation continuation))
                (cond ((null forms) (error "forms is null"))

                      ((cdr forms)
                       (let ((form (car forms)))
                         (cond ((and (consp form) (eq 'yield (car form)))
                                (collect `(list ,(cadr form) ,(convert-forms (cdr forms) continuation))))

                               ((and (consp form) (eq 'progn (car form)))
                                (loop collect (append (cdr form) (cdr forms)) continuation))

                               ((and (consp form) (eq 'if (car form)))
                                (collect `(if ,(cadr form)
                                                (progn ,@(collecting (loop collect (append (list (caddr form)) (cdr forms) continuation) nil)))
                                            (progn ,@(collecting     (loop collect (append (list (nth 3 form)) (cdr forms) continuation) nil))))))

                               (t
                                (collect form)
                                (loop collect (cdr forms) continuation)))))

                      ;; last form aka "return stmt"
                      (t
                       (let ((form (car forms)))
                         (cond ((and (consp form) (eq 'yield (car form)))
                                (collect `(list ,(cadr form) ,continuation)))

                               ((and (consp form) (eq 'progn (car form)))
                                (loop collect (append (cdr form) (cdr forms)) continuation))

                               ((and (consp form) (eq 'if (car form)))
                                (collect `(if ,(cadr form)
                                                (progn ,@(collecting (loop collect (append (list (caddr form)) continuation) nil)))
                                            (progn ,@(collecting     (loop collect (append (list (nth 3 form)) continuation) nil))))))

                               (t
                                (if continuation
                                      (collect `(list ,form ,(convert-forms (cdr forms) continuation)))
                                  (collect `(list ,form nil)))))))))))



;;; verpackt `forms` in einen generator, also ein parameterloses lambda,
;;; das bei mehrfachen aufrufen aufeinanderfolgende [value valid] values liefert.
(defmacro scan-coroutine forms
  (let ((cont (gensym "cont")) (old (gensym "old")) (val (gensym "val")) (newcont (gensym "newcont"))) 
    (cond ((null forms) '(lambda () (values nil nil)))

          (t `(let ((,cont ,(convert-forms forms nil)))
                (lambda ()
                  (if ,cont
                        (let ((,old ,cont))
                          (setq ,cont (lambda () (error "generator is a zombie")))
                          (destructuring-bind (,val ,newcont) (,old)
                            (setq ,cont ,newcont)
                            (values ,val t)))
                    (values nil nil))))))))



;; test stuff
(defmacro run (name printconvert-p printexpand-p . forms)
  `(progn (writeln)
          (writeln ,name nil)
          (write "forms:                " nil) (writeln ',forms)
          ,(when printconvert-p
             `(progn (write "converted: " nil) (writeln ',(convert-forms forms nil))))
          ,(when printexpand-p
             `(progn
                (pprint (macroexpand-1 '(scan-coroutine ,@forms)))
                (writeln)))
          (dogenerator (x (scan-coroutine ,@forms))
            (write "dogenerator: " nil)
            (writeln x))
          (write ,name nil) (writeln " done." nil) (writeln)))

(define *errors* 0)
(defmacro compare (forms expected)
  `(let ((actual (convert-forms ,forms nil)))
    (unless (equal ,expected actual)
      (incf *errors*)
      (writeln "for the input forms" nil)
      (writeln ,forms)
      (writeln "Error - expected conversion:" nil)
      (writeln ,expected)
      (writeln "actual conversion:" nil)
      (writeln actual))))


;; end with yield
(run "Test 1" nil nil (writeln 1) (writeln 2) (yield (writeln 10)) (writeln 3) (writeln 4) (yield (writeln 20)))
(compare            '((writeln 1) (writeln 2) (yield (writeln 10)) (writeln 3) (writeln 4) (yield (writeln 20)))
                     '(lambda nil (writeln 1) (writeln 2) (list (writeln 10) (lambda nil (writeln 3) (writeln 4) (list (writeln 20) nil)))))


;; end with normal form
(run "Test 2" nil nil (writeln 1) (writeln 2) (yield 10) (writeln 3) (writeln 4) (yield 20) (writeln 5))
(compare            '((writeln 1) (writeln 2) (yield 10) (writeln 3) (writeln 4) (yield 20) (writeln 5))
                     '(lambda nil (writeln 1) (writeln 2) (list 10 (lambda nil (writeln 3) (writeln 4) (list 20 (lambda nil (list (writeln 5) nil)))))))


;; progn that contains yield
(run "Test 3" nil nil (writeln 1) (progn (writeln 2) (yield 10) (writeln 3)) (writeln 4) (yield 20))
(compare            '((writeln 1) (progn (writeln 2) (yield 10) (writeln 3)) (writeln 4) (yield 20))
                     '(lambda nil (writeln 1) (writeln 2) (list 10 (lambda nil (writeln 3) (writeln 4) (list 20 nil)))))


;; progn that ends with yield
(run "Test 4" nil nil (writeln 1) (progn (writeln 2) (yield (writeln 10))) (writeln 3) (writeln 4) (yield 20) (writeln 5))
(compare            '((writeln 1) (progn (writeln 2) (yield (writeln 10))) (writeln 3) (writeln 4) (yield 20) (writeln 5))
                     '(lambda nil (writeln 1) (writeln 2) (list (writeln 10) (lambda nil (writeln 3) (writeln 4) (list 20 (lambda nil (list (writeln 5) nil)))))))


;; end with progn that ends with yield
(run "Test 5" nil nil (writeln 1) (progn (writeln 2) (yield (writeln 10))) (writeln 3) (writeln 4) (yield 20) (progn (yield (writeln 5))))
(compare            '((writeln 1) (progn (writeln 2) (yield (writeln 10))) (writeln 3) (writeln 4) (yield 20) (progn (yield (writeln 5))))
                    '(lambda nil (writeln 1) (writeln 2) (list (writeln 10) (lambda nil (writeln 3) (writeln 4) (list 20 (lambda nil (list (writeln 5) nil)))))))


;; if clause whose consequent is a single yield, nothing after the if-clause
(compare '((writeln 1)
           (if t (yield (writeln 10))
             (writeln 2)))

         '(lambda ()
            (writeln 1)
            (if t (progn (list (writeln 10) nil))
              (progn (list (writeln 2) nil)))))

(run "Test 6" nil nil (writeln 1) (if t (yield (writeln 10)) (writeln 2)))


;; if clause with a stmt after the if-clause
(compare '((writeln 1)
           (if t (yield (writeln 10))
             (writeln 2))
           (writeln 3))

         '(lambda ()
            (writeln 1)
            (if t (progn (list (writeln 10) (lambda () (list (writeln 3) nil))))
              (progn (writeln 2) (list (writeln 3) nil)))))

(run "Test 7" nil nil (writeln 1) (if t (yield (writeln 10)) (writeln 2)) (writeln 3))

(run "Test 8" nil nil (writeln 1) (if nil (yield (writeln 10)) (writeln 2)) (writeln 3))


(unless (zerop *errors*)
  (writeln)
  (write *errors*)
  (writeln " errors" nil))

nil