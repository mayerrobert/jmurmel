;; Experiments with vector functions
;; currently "vector" is really a mix between vector and simple-vector, and has limitations,
;; and is slow because of reflection

(define m%to-int (:: "java.lang.Long" "intValue"))
(define m%to-long (:: "java.lang.Integer" "longValue"))

(define m%svref (:: "java.util.ArrayList" "get"    "int"))
(define m%svset (:: "java.util.ArrayList" "set"    "int" "java.lang.Object"))
(define m%svlen (:: "java.util.ArrayList" "size"))
(define m%svadd (:: "java.util.ArrayList" "add"    "java.lang.Object"))
(define m%svrem (:: "java.util.ArrayList" "remove" "int"))

(define make-simple-vector (:: "java.util.ArrayList" "new"))


; das ist aehnlich (oder gleich?) vector-push-extend, siehe http://www.lispworks.com/documentation/HyperSpec/Body/f_vec_ps.htm
(defun vector-push-extend (vec elem)
  (m%svadd vec elem)
  (m%svlen vec))

(defun vector-pop (vec)
  (m%svrem vec (m%to-int (1- (m%to-long (m%svlen vec))))))

(defun svref (vec idx)
  (m%svref vec (m%to-int idx)))

(defun svset (vec idx value)
  (m%svset vec (m%to-int idx) value))

(defun vector contents
  (let* ((vec (make-simple-vector)))
    (let loop ((contents contents))
     (if contents
         (progn
           (m%svadd vec (car contents))
           (loop (cdr contents)))
       vec))))



(define v (vector 1 2 3))
(writeln (svref v 1))

(svset v 1 'Hello)
(writeln v)
