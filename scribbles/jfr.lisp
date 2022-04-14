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


;;; (m%jfr-begin-function parent function-name argument-list) -> jfr-funcall-event
;;;
;;; Create a funcall event and record starttime. m%jfr-begin-function and m%jfr-end-function should be paired.
(define m%jfr-begin-function
        (jmethod "io.github.jmurmel.LambdaJ$JFRHelper" "beginFunction" "io.github.jmurmel.LambdaJ$JFRHelper$BaseEvent" "Object" "Object"))

;;; (m%jfr-end-function jfr-funcall-event return-value)
;;;
;;; End and possibly commit a funcall event. m%jfr-begin-function and m%jfr-end-function should be paired.
(define m%jfr-end-function
        (jmethod "io.github.jmurmel.LambdaJ$JFRHelper" "endFunction" "io.github.jmurmel.LambdaJ$JFRHelper$JFRFunctionCall" "Object"))


;;; (m%jfr-begin-event parent name) -> jfr-event
;;;
;;; Create a generic event and record starttime. m%jfr-begin-event and m%jfr-end-event should be paired.
(define m%jfr-begin-event
        (jmethod "io.github.jmurmel.LambdaJ$JFRHelper" "beginEvent" "io.github.jmurmel.LambdaJ$JFRHelper$BaseEvent" "Object"))

;;; (m%jfr-end-event jfr-event info) -> nil
;;;
;;; End and possibly commit a generic event. m%jfr-begin-event and m%jfr-end-event should be paired.
(define m%jfr-end-event
        (jmethod "io.github.jmurmel.LambdaJ$JFRHelper" "endEvent" "io.github.jmurmel.LambdaJ$JFRHelper$JFREvent" "Object"))


;;; (m%jfr-event parent name info) -> nil
;;;
;;; Create and possibly commit a generic event. Will be used on it's own, there is no corresponding "commit" function.
(define m%jfr-event
        (jmethod "io.github.jmurmel.LambdaJ$JFRHelper" "event" "io.github.jmurmel.LambdaJ$JFRHelper$BaseEvent" "Object" "Object"))


(define *m%jfr-parent* nil)

(defun m%jfr-pushparent (event)
  (setq *m%jfr-parent* (cons event *m%jfr-parent*)))

(defun m%jfr-popparent (event)
  (if (eq event (car *m%jfr-parent*)) 
        (setq *m%jfr-parent* (cdr *m%jfr-parent*))
    (fatal "parents not properly nested")))



;;; (call-with-jfr function argument*)
;;;
;;; Perform one function call wrapped with JFR recording
(defmacro call-with-jfr (fun . args)
  (let ((event (gensym)))
    `(let ((,event (m%jfr-begin-function (car *m%jfr-parent*) ',fun ',args)))
       (m%jfr-pushparent ,event)
       ((lambda (a b) a)
          (m%jfr-end-function ,event (,fun ,@args))
          (m%jfr-popparent ,event)))))


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
            (let ((,event (m%jfr-begin-function (car *m%jfr-parent*) ',fun ,args)))
              (m%jfr-pushparent ,event)
              ((lambda (a b)
                 a)
                (m%jfr-end-function ,event (apply ,old ,args))
                (m%jfr-popparent ,event)))))
       ,old)))


;;; (defun jfr-event (name info) -> nil
;;;
;;; Create and possibly commit a generic event.
(defun jfr-event (name info)
  (m%jfr-event (car *m%jfr-parent*) name info))


;;; (defun jfr-begin (name) -> jfr-event
;;;
;;; Create a generic event, record starttime and set the event as the current parent.
;;; jfr-begin and jfr-end must be paired.
(defun jfr-begin (name)
  (car (setq *m%jfr-parent* (cons (m%jfr-begin-event nil name) *m%jfr-parent*))))


;;; (defun jfr-begin (name) -> jfr-event
;;;
;;; End and possibly commit a generic event. Restore previous parent.
;;; jfr-begin and jfr-end must be paired.
(defun jfr-end (event info)
  (m%jfr-end-event event info)
  (m%jfr-popparent event))



;;;
;;; Test above functions and macros
;;;

;;; 1) submit an event
(jfr-event "Testevent" "Hello, Java Management Console!")


;;; 2) invoke writeln and submit a "Function call" event
(call-with-jfr writeln "Hello, World!" nil)



;;; 3a) create a simple testfunction
(defun testfunc (arg)
  (writeln arg nil))

;;; 3b) wrap testfunc and store original definition
(define old (wrap-with-jfr testfunc))

;;; 3c) invoke the wrapped function in a loop, the whole loop is wrapped in an event submission
(let ((event (jfr-begin "test")))
  (let loop ((i 1))
    (testfunc i)
    (if (< i 10)
         (loop (1+ i))
      (jfr-end event (format nil "The loop is done, %d iterations" i)))))



;;; 3d) restore original function
(setq testfunc old)

;;; 3e) invoke not-wrapped original function, no JFR events will be submitted
(testfunc 15)
