; helper to turn off opencoding
; used during testing
; usage:
;   java -jar jmurmel.jar speed0.lisp murmel-test.lisp

(declaim (optimize (speed 0)))