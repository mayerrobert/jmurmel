@echo off

REM run all benchmarks with sbcl and jmurmel
REM Usage: runall | tee benchresults.txt

REM config
set SBCL=c:\apps\sbcl\2.2.0\bin\sbcl.exe
set JMURMEL=C:\Apps\Java\X64\jdk8u252-b09\bin\java -Dfile.encoding=UTF8 -jar ..\..\lambda\target\jmurmel.jar --libdir ..
set ABCL=C:\Apps\Java\X64\jdk8u252-b09\bin\java -jar -XX:+AggressiveOpts -XX:CompileThreshold=1000 C:\Apps\abcl-bin-1.8.0\abcl.jar --noinform --batch
REM end config


echo Benchmark results
date /t
echo.

%JMURMEL% --version

for %%i in (00*.lisp 3*.lisp q.lisp) do call :all %%i
goto :EOF

:all
echo.
echo *** jmurmel (compiled): %1 ***
%JMURMEL% --run %1

echo.
echo *** jmurmel (interpreted): %1 ***
%JMURMEL% %1
