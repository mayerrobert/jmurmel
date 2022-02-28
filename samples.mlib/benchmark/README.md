=== Benchmarks

This directory contains a few benchmarks and a simple [benchmark facility](bench.lisp).
The files will run the code under test for a given number of seconds and then print timing results.

To run as interpreted Murmel use e.g.:

    C:...\benchmark> java -jar ..\..\lambda\target\jmurmel.jar --libdir .. 3.01_tak.lisp

To run as compiled Murmel use e.g.:

    C:...\benchmark> java -jar ..\..\lambda\target\jmurmel.jar --run --libdir .. 3.01_tak.lisp

To run with SBCL use e.g.:

    C:...\benchmark> sbcl --script 3.01_tak.lisp

To run all benchmarks as compiled Murmel use e.g.:

    C:...\benchmark> for %i in (00*.lisp 3*.lisp q.lisp) do @java -jar ..\..\lambda\target\jmurmel.jar --run --libdir .. %i

To run all benchmarks with SBCL, JMurmel (compiled and interpreted) and with ABCL (interpreted):

    C:...\benchmark> runall.cmd | tee benchresults.txt

== References

"Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985".