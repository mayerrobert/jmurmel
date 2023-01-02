@echo off
goto :start

jpackage-w64 - create JMurmel app-image

Needs jpackage which is included in Java16 and above.

This will create an "app-image" aka a directory hierachy
that contains everything needed to run JMurmel
including a Java runtime.

The end result will be the directory "jmurmel" in "target".

:start

REM begin config

REM this is where the directory "jmurmel" will be created. Could be changed to e.g. "C:\Program files"
set DESTDIR=target

REM limit the parts of the JDK that will go into the packaged runtime. Comment out to include the complete JDK,
REM or use e.g. "set MODULES=java.base" to create a reduced size image w/o graphics or support for compilation.
set MODULES=--add-modules java.base,java.desktop,jdk.compiler,jdk.zipfs,jdk.jfr,jdk.localedata,java.management

REM Java options that will be used at runtime
set JOPTIONS=--java-options -Xmx1G --java-options -Xss2m --java-options -XX:+UseParallelGC

REM end config



rd /s /q %DESTDIR%\jmurmel
rd /s /q target\jpackage-input

md target\jpackage-input
copy ..\lambda\target\jmurmel.jar target\jpackage-input

jpackage --type app-image -i target\jpackage-input -d %DESTDIR% -n jmurmel --main-class io.github.jmurmel.LambdaJ --main-jar jmurmel.jar --win-console %MODULES% %JOPTIONS%

copy ..\samples.mlib\mlib.lisp %DESTDIR%\jmurmel
copy ..\LICENSE                %DESTDIR%\jmurmel
copy ..\murmel-langref.html    %DESTDIR%\jmurmel
copy ..\mlib.html              %DESTDIR%\jmurmel
