;;;; Read a file using JMurmel's FFI and java.nio.file and print it's contents to the screen.
;;;
;;; java.nio.file.Files#readAllLines() returns a java.util.List<String>
;;; which JMurmel will treat as a vector of (readonly) simple strings.

(require "mlib")


(define contents ((jmethod "java.nio.file.Files" "readAllLines" "java.nio.file.Path")
                  ((jmethod "java.nio.file.Paths" "get" "java.net.URI")
                   ((jmethod "java.net.URI" "new" "String") "file:///c:/robert/jmurmel/Dockerfile"))))

(dovector (line contents)
  (princ line)
  (terpri))
(terpri)


(define me (-> "file:///c:/robert/jmurmel/scribbles/readfile-nio.lisp"
            ((jmethod "java.net.URI" "new" "String"))
            ((jmethod "java.nio.file.Paths" "get" "java.net.URI"))
            ((jmethod "java.nio.file.Files" "readAllLines" "java.nio.file.Path"))))

(dovector (line me)
  (writeln line nil))
(writeln)