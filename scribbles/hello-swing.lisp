;;;; Preliminary attempt at the Swing "Hello, World!" in Murmel
;;;  see https://docs.oracle.com/javase/tutorial/uiswing/examples/start/HelloWorldSwingProject/src/start/HelloWorldSwing.java
;;;
;;; Currently only works in the interpreter due to issues with compiling Java FFI,
;;; e.g. calls to Java void functions are not handled correctly by the Murmel compiler.

; the next 4 should not be needed, Murmel should convert argument types automatically
(define number->int (:: "Number" "intValue"))
(define string->boolean (:: "Boolean" "valueOf" "String"))

(define +true+ (string->boolean "true"))
(define +false+ (string->boolean "false"))



; dispose the window when clicking X. If all windows are disposed the JVM may end.
(define +dispose-on-close+ (number->int 2))

; call exit(0) when clicking X
(define +exit-on-close+ (number->int 3))

; (add-component container component-to-add) -> component-to-add
(define add-component (:: "java.awt.Container" "add" "java.awt.Component"))

; (make-jframe title) -> javax.swing.JFrame
(define make-jframe (:: "javax.swing.JFrame" "new" "String"))

; (get-content-pane frame) -> content-pane-component
(define get-content-pane (:: "javax.swing.JFrame" "getContentPane"))

; (pack-frame frame) -> void
(define pack-frame (:: "javax.swing.JFrame" "pack"))

; (show-frame frame) -> void
(define show-frame (:: "javax.swing.JFrame" "setVisible" "boolean"))


; (make-label labeltext) -> javax.swing.JLabel
(define make-jlabel (:: "javax.swing.JLabel" "new" "String"))


; (invoke-later runnable) -> void
(define invoke-later (:: "javax.swing.SwingUtilities" "invokeLater" "java.lang.Runnable"))



;;; Swing "Hello, World!" in Murmel

;;; Create the GUI and show it. For thread safety,
;;; this method should be invoked from the
;;; event-dispatching thread.
(defun create-and-show-gui ()
  (let* ((frame (make-jframe "Hello, Swing World!"))
         (label (make-jlabel "Hello, World!"))
         (content-pane (get-content-pane frame)))

    (writeln "casg")
    ((:: "javax.swing.JFrame" "setDefaultCloseOperation" "int") frame +dispose-on-close+)
    (add-component content-pane label) 
    (pack-frame frame)
    (show-frame frame +true+)))

;;; static void main(String[] args)
(invoke-later (proxy "java.lang.Runnable" "run" create-and-show-gui))