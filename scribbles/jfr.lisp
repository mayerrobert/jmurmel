;;;; Functions and macros to submit events to Java Flight Recorder
;;;;
;;;; Run this file with
;;;;
;;;;     java -XX:StartFlightRecording:settings=profile,filename=jm.jfr -jar jmurmel.jar jfr.lisp
;;;;
;;;; and then open the file jm.jfr in Java Management Console (see https://openjdk.java.net/projects/jmc/8/).
;;;; Select "Event Browser", the category "JMurmel" will contain the subcategories "Events" and "Function calls"
;;;; where submitted events should be found.
;;;; Events need to be started and ended. This will record start and end time and additional info.

;;; (jfr-begin-function function-name argument-list) -> jfr-funcall-event
;;;
;;; Create a funcall event and record starttime.
(define jfr-begin-function
        (:: "io.github.jmurmel.LambdaJ$JFRHelper" "beginFunction" "Object" "Object"))

;;; (jfr-end-function jfr-funcall-event return-value)
;;;
;;; End and possibly commit a funcall event.
(define jfr-end-function
        (:: "io.github.jmurmel.LambdaJ$JFRHelper" "endFunction" "io.github.jmurmel.LambdaJ$JFRHelper$JFRFunctionCall" "Object"))


;;; (jfr-begin-event name) -> jfr-event
;;;
;;; Create a generic event and record starttime.
(define jfr-begin-event
        (:: "io.github.jmurmel.LambdaJ$JFRHelper" "beginEvent" "Object"))

;;; (jfr-end-event jfr-event info) -> nil
;;;
;;; End and possibly commit a generic event.
(define jfr-end-event
        (:: "io.github.jmurmel.LambdaJ$JFRHelper" "endEvent" "io.github.jmurmel.LambdaJ$JFRHelper$JFREvent" "Object"))



;;; (call-with-jfr function argument*)
;;;
;;; Perform one function call wrapped with JFR recording
(defmacro call-with-jfr (fun . args)
  (let ((event (gensym)))
    `(let ((,event (jfr-begin-function ',fun ',args)))
       (jfr-end-function ,event (,fun ,@args)))))

;;; (wrap-with-jfr symbol) -> old-function
;;;
;;; Replace `fun` by a wrapper that will record each call to JFR.
;;; Returns original function.
;;;
;;; Note: wrapping builtin primitives will only work with speed=0,
;;; i.e. `(declaim (optimize (speed 0)))` or builtins may be opencoded
;;; bypassing the environment lookup.
(defmacro wrap-with-jfr (fun)
  (let ((old (gensym))
        (args (gensym))
        (event (gensym)))
    `(let ((,old ,fun))
       (setq ,fun
          (lambda ,args
            (let ((,event (jfr-begin-function ',fun ,args)))
              (jfr-end-function ,event (apply ,old ,args)))))
       ,old)))



;;; Test above functions and macros

;;; invoke writeln and submit a "Function call" event
(call-with-jfr writeln "Hello, World!" nil)



;;; create a simple testfunction
(defun testfunc (arg)
  (writeln arg nil))

;;; wrap testfunc and store original definition
(define old (wrap-with-jfr testfunc))

;;; invoke the wrapped function in a loop, the whole loop is wrapped in an event submission
(let ((event (jfr-begin-event "test")))
  (let loop ((i 0))
    (testfunc i)
    (if (< i 10)
         (loop (1+ i))
      (jfr-end-event event (format nil "The loop is done, %d iterations" i)))))



;;; restore original function
(setq testfunc old)

;;; invoke not-wrapped original function, no JFR events will be submitted
(testfunc 15)