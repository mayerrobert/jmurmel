;;;; Demofile for MurmelHttpServer
;;;
;;; start the MurmelHttpServer like so:
;;;
;;;   java -cp jmurmel.jar MurmelHttpServer.java
;;;
;;; and then in another terminal launch
;;;
;;;   curl http://localhost:8080/murmel/echo.lisp?param1=value1&param2=value%20two
;;;

;; the server will invoke Murmel with the following global variables:
;;
;; *request-method*, *query-string*, *request-uri*, *remote-address*
;;   - simple-string
;;
;; *request-headers*, *response-headers*
;;   - hashtable whose keys are simple-strings, and values must be
;;     adjustable vectors containing simple-strings

(hashset *response-headers* "Content-type" (vector-copy #("text/html; charset=iso-8895-1") t))

; this does less than the bare minimum, don't use for anything important
; or suffer from cross-site-scripting attacks and other fun stuff
(defun write-escaped-string (s)
  (if s
        (let loop ((s (string->list s)))
          (if s
            (progn
              (cond ((eql (car s) #\&) (write "&amp;" nil))
                    ((eql (car s) #\<) (write "&lt;" nil))
                    ((eql (car s) #\>) (write "&gt;" nil))
                    (t (write (car s) nil)))
              (loop (cdr s)))))
    (write "(nil)" nil)))


(writeln "<HTML>" nil)

(format t "<p>Remote address: %s%n" *remote-address*)
(format t "<p>Request method: %s%n" *request-method*)
(format t "<p>Query string: ")
(write-escaped-string *query-string*)
(writeln)

(format t "<p>Request URI: ")
(write-escaped-string *request-uri*)
(writeln)

(format t "<p>Request headers: ")
(write *request-headers*)