(defmacro or args
   (if (null args)
         nil
     (if (null (cdr args))
           (car args)
       (let ((temp (gensym)))
         `(let ((,temp ,(car args)))
             (if ,temp
                   ,temp
               (or ,@(cdr args))))))))


(defmacro and args
   (if (null args)
         t
     (if (null (cdr args))
           (car args)
       (let ((temp (gensym)))
         `(let ((,temp ,(car args)))
             (if ,temp
                   (and ,@(cdr args))))))))
