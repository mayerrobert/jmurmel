@echo off

REM run all benchmarks with jmurmel
REM Usage: runmurmel | tee murmelresults.txt

REM config
set JMURMEL=java -Dfile.encoding=UTF8 -jar ..\..\lambda\target\jmurmel.jar --libdir ..
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
