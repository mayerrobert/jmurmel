@echo off

REM run all benchmarks with sbcl and jmurmel

for %%i in (00*.lisp 3*.lisp q.lisp) do @echo. & @echo *** sbcl: %%i *** & @sbcl --script %%i & @echo. & @echo *** jmurmel: %%i *** & @java -jar ..\..\lambda\target\jmurmel.jar --run --libdir .. %%i
