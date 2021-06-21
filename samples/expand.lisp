;;;; Expand macros inside a form.
;;;
;;; Only macros defined before the form will be expanded,
;;; defmacro calls within the form are not considered.

(require "mlib")

(defun m%macroexpand-1 (ex)
  (let ((expanded (macroexpand-1 ex)))
    (if (eq ex expanded)
          (list ex nil)
      (list expanded t))))

(defun expand (form)
  (cond ((atom form)
         form)

        ((eq 'quote (car form))
         'form)

        (t
         (let* ((expanded-args (let loop ((l (cdr form)))
                                 (if (atom l)
                                       l
                                   (cons (expand (car l)) (loop (cdr l))))))
                (form-expanded-args (cons (car form) expanded-args)))
           (destructuring-bind
             (ex changed)
             (m%macroexpand-1 form-expanded-args)
             (if changed
                   (expand ex)
               ex))))))
