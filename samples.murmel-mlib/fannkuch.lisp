;;;; Fannkuch-Redux
;;;;
;;;; Modified from https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/fannkuchredux-sbcl-2.html
;;;; which is licensed under the "3-Clause BSD License" https://benchmarksgame-team.pages.debian.net/benchmarksgame/license.html

#+murmel (require "mlib")

(defun fannkuch (n)
  (let ((csum 0)
        (fmax 0)
        (perm (make-array n ;:element-type 'fixnum
                          ))
        (copy (make-array n ;:element-type 'fixnum
                          ))
        (num 0))

      (dotimes (i n) (setf (svref perm i) i))

      (labels ((do-iter (ht)
                 (if (= ht 1)
                     (progn
                       (dotimes (i n) (setf (svref copy i) (svref perm i)))
                       (let ((c 0)
                             (z (svref copy 0)))
                         (do () ((= z 0))
                           (progn
                             (dotimes (i (1+ (truncate z 2)))
                               (let ((temp (svref copy i))
                                     (k (- z i)))
                                 (setf (svref copy i) (svref copy k))
                                 (setf (svref copy k) temp)))
                             (incf c)
                             (setf z (svref copy 0))))
                         (setf csum (+ csum  (if (evenp num) c (- c))))
                         (when (> c fmax)
                           (setf fmax c)))
                       (incf num))
                     (dotimes (i ht)
                       (progn (do-iter (- ht 1))
                              (let ((temp (svref perm 0))
                                    (m (- ht 1)))
                                (dotimes (i m)
                                  (setf (svref perm i) (svref perm (1+ i))))
                                (setf (svref perm m) temp)))))))

        (do-iter n))
      #-murmel (format t "~s~%Pfannkuchen(~s) = ~s~%" csum n fmax)
      #+murmel (format t "%s%nPfannkuchen(%s) = %s%n" csum n fmax)
      ))


;(fannkuch 12) ; takes approx a minute on SBCL, checksum 3968050
(time (fannkuch 10)) ; takes < 1 second on SBCL, checksum 73196
