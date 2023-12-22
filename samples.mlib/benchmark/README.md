# Benchmarks

This directory contains a few benchmarks and a simple [benchmark facility](bench.lisp).
The files will run the code under test for a given number of seconds and then print timing results.

**Note:** in their current form these benchmarks
won't give a 100% fair performance comparison across the Lisp-implementations under test,
so (as usual with benchmarks) take the numbers with a grain of salt.

The purpose of the programs in this directory is to find performance issues in JMurmel and/ or experiment with different JVM settings.

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

Alternatively all.lisp can be used to run all benchmarks in one go and print a total result
where each individual benchmark is weighted to contribute equally:

    C:...\benchmark> java -jar ..\..\lambda\target\jmurmel.jar --run --libdir .. all.lisp

The total result (which is the sum of weighted averages of each individual benchmark)
is also compared to a "reference result" which was computed using sbcl 2.2.9.

## References

"Performance and Evaluation of Lisp Systems, Richard P. Gabriel, 1985".