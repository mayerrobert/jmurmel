@echo off

REM run all benchmarks with jmurmel
REM Usage: runmurmel | tee murmelresults.txt

REM config
set JMURMEL=C:\Apps\Java\X64\jdk8u322-b06\bin\java -Dfile.encoding=UTF8 -jar ..\..\lambda\target\jmurmel.jar --libdir ..
REM end config


echo Benchmark results
date /t
echo.

%JMURMEL% --version

for %%i in (00*.lisp 3*.lisp a*.lisp dovector.lisp q.lisp) do call :all %%i
goto :EOF

:all
echo.
echo *** jmurmel (compiled): %1 ***
%JMURMEL% --run %1

echo.
echo *** jmurmel (interpreted): %1 ***
%JMURMEL% %1
