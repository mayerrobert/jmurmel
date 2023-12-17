@echo off

REM run all benchmarks with sbcl
REM Usage: runsbcl | tee sbclresults.txt

REM config
set SBCL=sbcl
REM end config


echo Benchmark results
date /t
echo.

%SBCL% --version

for %%i in (00*.lisp 3*.lisp a*.lisp dovector.lisp q.lisp) do call :all %%i
goto :EOF

:all
echo.
echo *** sbcl: %1 ***
%SBCL% --script %1
