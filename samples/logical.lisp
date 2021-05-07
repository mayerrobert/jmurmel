;;; macros for logical and and or

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

(and (= 1 1)
     (or (< 1 2)
         (> 1 2))
     (and (<= 1 2 3 4)
          (> 5 3 1)))