@echo off

REM run all benchmarks with sbcl, jmurmel and abcl
REM Usage: runall | tee benchresults.txt

REM config
set SBCL=c:\apps\sbcl\2.2.0\bin\sbcl.exe
set JMURMEL=C:\Apps\Java\X64\jdk8u322-b06\bin\java -Dfile.encoding=UTF8 -jar ..\..\lambda\target\jmurmel.jar --libdir ..
set ABCL=C:\Apps\Java\X64\jdk8u322-b06\bin\java -XX:+AggressiveOpts -XX:CompileThreshold=1000 -jar C:\Apps\abcl-bin-1.8.0\abcl.jar --noinform --batch
REM end config


echo Benchmark results
date /t
echo.

%SBCL% --version
%JMURMEL% --version
%ABCL% --eval "(progn (princ (lisp-implementation-type)) (princ #\ ) (princ (lisp-implementation-version)))"

for %%i in (00*.lisp 3*.lisp a*.lisp dovector.lisp q.lisp) do call :all %%i
goto :EOF

:all
echo.
echo *** sbcl: %1 ***
%SBCL% --script %1

echo.
echo *** jmurmel (compiled): %1 ***
%JMURMEL% --run %1

echo.
echo *** jmurmel (interpreted): %1 ***
%JMURMEL% %1

echo.
echo *** abcl (interpreted): %1 ***
%ABCL% --load %1
