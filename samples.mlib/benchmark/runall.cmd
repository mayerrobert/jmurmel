@echo off

REM run all benchmarks with sbcl and jmurmel
REM Usage: runall | tee benchresults.txt

set SBCL=c:\apps\sbcl\2.2.0\bin\sbcl.exe
set JMURMEL=java -jar ..\..\lambda\target\jmurmel.jar --libdir ..

echo Benchmark results
date /t
%SBCL% --version
%JMURMEL% --version

for %%i in (00*.lisp 3*.lisp q.lisp) do @echo. & @echo *** sbcl: %%i *** & @%SBCL% --script %%i & @echo. & @echo *** jmurmel: %%i *** & @%JMURMEL% --run %%i
