;;;; Super-simple "Hello, World!" using a Swing messagebox
;;;
;;; This will open a messagebox and block until the OK button is clicked.
;;; Then print "Bye." and exit.

((jmethod "javax.swing.JOptionPane" "showMessageDialog" "java.awt.Component" "Object") nil "Hello, World!")

(writeln "Bye." nil)

nil ; avoid printing the return value of writeln