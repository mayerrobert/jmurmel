;;;; Expand all macros inside a form.
;;;
;;; See https://legacy.cs.indiana.edu/ftp/scheme-repository/doc/misc/
;;;
;;; Expand is really only useful for writing macros as
;;; a helper similar to macroexpand-1.
;;;
;;; Possible usage:
;;;
;;;     (pprint (expand 'form))
;;;
;;; Only macros already defined before the form will be expanded,
;;; new macros defined by defmacro calls within the form are not considered.

(require "mlib")
(provide "expand")

(defun m%macroexpand-1 (ex)
  (let ((expanded (macroexpand-1 ex)))
    (if (eq ex expanded)
          (list ex nil)
      (list expanded t))))

(defun expand (form)
  (cond ((atom form)
         form)

        ((eq 'quote (car form))
         form)

        ((eq 'lambda (car form))
         `(lambda ,(cadr form)
                  ,(caddr form)
                  ,@(mapcar expand (cdddr form))))

        ((eq 'define (car form))
         `(define ,(cadr form) ,(expand (caddr form))))

        ((eq 'defun (car form))
         `(defun ,(cadr form) ,(caddr form) ,@(mapcar expand (cdddr form))))

        ((eq 'defmacro (car form))
         `(defmacro ,(cadr form) ,(caddr form) ,@(mapcar expand (cdddr form))))

        ((member (car form) '(let let* letrec) eq)
         (if (symbolp (cadr form))
               `(,(car form) ,(cadr form) ,(caddr form) ,@(mapcar expand (cdddr form)))
           `(,(car form) ,(cadr form) ,@(mapcar expand (cddr form)))))

        ((eq 'setq (car form))
         `(setq ,(cadr form) ,(expand (caddr form))))

        (t
         (destructuring-bind (ex changed)
           (m%macroexpand-1 form)
           (if changed
                 (expand ex)
             (mapcar expand form))))))
