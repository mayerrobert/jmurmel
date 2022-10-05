;;;; Preliminary attempt at the Swing "Hello, World!" in Murmel
;;;  see https://docs.oracle.com/javase/tutorial/uiswing/examples/start/HelloWorldSwingProject/src/start/HelloWorldSwing.java
;;;
;;; Currently only works in the interpreter due to issues with compiling Java FFI,
;;; e.g. calls to Java void functions are not handled correctly by the Murmel compiler.


;;; AWT stuff

; (set-component-size component width height) -> void
(define set-component-size
  (jmethod "java.awt.Component" "setSize" "int" "int"))

; (set-component-visible frame) -> void
(define set-component-visible
  (jmethod "java.awt.Component" "setVisible" "boolean"))


; (add-component container component-to-add) -> component-to-add
(define add-component
  (jmethod "java.awt.Container" "add" "java.awt.Component"))

(define set-layout
  (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager"))


(define make-gridbag-layout
  (jmethod "java.awt.GridBagLayout" "new"))


; (make-font name style points) -> java.awt.Font
(define make-font
  (jmethod "java.awt.Font" "new" "String" "int" "int"))


;;; Swing stuff

; dispose the window when clicking X. If all windows are disposed the JVM may end.
(define +dispose-on-close+ 2)

; call exit(0) when clicking X
(define +exit-on-close+ 3)


; helper macro to emit constructor-functions for Swing components.
; Each constructor-function will take one string argument.
; (Why copy&paste one line when you can create a macro that does the same :-)
(defmacro stringarg-constructor (name class)
  `(define ,name (jmethod ,(format nil "javax.swing.%s" class) "new" "String")))

(stringarg-constructor make-jframe "JFrame")
(stringarg-constructor make-jlabel "JLabel")

(define set-font
  (jmethod "javax.swing.JComponent" "setFont" "java.awt.Font"))


; (get-content-pane frame) -> content-pane-component
(define get-content-pane
  (jmethod "javax.swing.JFrame" "getContentPane"))

; (pack-frame frame) -> void
(define pack-frame
  (jmethod "javax.swing.JFrame" "pack"))


; (invoke-later runnable) -> void
(define invoke-later
  (jmethod "javax.swing.SwingUtilities" "invokeLater" "java.lang.Runnable"))



;;; Swing "Hello, World!" in Murmel

;;; Create the GUI and show it. For thread safety,
;;; this method should be invoked from the
;;; event-dispatching thread.
(defun create-and-show-gui ()
  (let* ((frame (make-jframe "Hello, Swing World!"))
         (label (make-jlabel "Hello, World!"))
         (content-pane (get-content-pane frame)))

    ((jmethod "javax.swing.JFrame" "setDefaultCloseOperation" "int") frame +dispose-on-close+)

    (set-font label (make-font nil 0 24))
    (add-component content-pane label)

    (set-layout frame (make-gridbag-layout))
    (pack-frame frame)
    (set-component-size frame 600 400)
    ((jmethod "java.awt.Window" "setLocationRelativeTo" "java.awt.Component") frame nil)
    (set-component-visible frame t)))

;;; static void main(String[] args)
(invoke-later (jproxy "java.lang.Runnable" "run" create-and-show-gui))