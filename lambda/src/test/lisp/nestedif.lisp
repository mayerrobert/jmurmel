; outputt: t
; resultt: t



(labels
   ((test (x) (format x))
    (test2 (x) (if (= x 1) (format "x ist 1")))
    (test3 (x) (cond ((= x 1) (format "x ist 1")(format "%n"))
                     ((= x 2) (format "x ist 2")(format "%n"))
                     ((> x 2) (format "x > 2")
                              (format " und nochmal%n")
                              (cond ((=x 3) (format "x > 2") (format "%n"))
                                    (t      (format "x > 2, != 3") (format "%n")))
                         )
                     ))
    )
   
   (test3 3))
   
   
   
