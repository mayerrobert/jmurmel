@echo off
goto :start

jpackage-w64 - create JMurmel app-image with a Windows .exe launcher

Must be run from within the .../scripts directory.
Needs jpackage, jlink & friends which are included in Java16 and above.

This will create an "app-image" aka a directory hierachy
that contains everything needed to run JMurmel
including a Java runtime.

The end result will be the directory "jmurmel" in "target".
Use ...\jmurmel\jmurmel.exe to launch a REPL.

:start

REM begin config

REM this is where the directory "jmurmel" will be created. Could be changed to e.g. "C:\Program files"
set DESTDIR=target

REM limit the parts of the JDK that will go into the packaged runtime. Comment out to include the complete JDK,
REM or use e.g. "set MODULES=java.base" to create a reduced size image w/o graphics or support for compilation.
set MODULES=--add-modules java.base,java.desktop,jdk.compiler,jdk.zipfs,jdk.jfr,jdk.localedata,java.management

REM Java options that will be used at runtime
set JOPTIONS=--java-options -Xmx1G --java-options -Xss2m --java-options -XX:+UseParallelGC
REM enable class loading debugging for CDS testing
REM set JOPTIONS=%JOPTIONS% --java-options -Xshare:on --java-options -Xlog:class+load

REM end config



rd /s /q %DESTDIR%\jmurmel
rd /s /q target\jpackage-input

md target\jpackage-input
copy ..\lambda\target\jmurmel.jar target\jpackage-input\.
copy ..\samples.mlib\mlib.lisp    target\jpackage-input\.


REM create a directory with jlinked JDK, Murmel files and launcher .exe, see https://docs.oracle.com/en/java/javase/17/docs/specs/man/jpackage.html
set JLINK=--jlink-options --strip-native-commands --jlink-options --strip-debug --jlink-options --no-man-pages --jlink-options --no-header-files --jlink-options --compress=2
jpackage %JLINK% --type app-image -i target\jpackage-input -d %DESTDIR% -n jmurmel --main-class io.github.jmurmel.LambdaJ --main-jar jmurmel.jar --win-console %MODULES% %JOPTIONS%

copy ..\LICENSE                %DESTDIR%\jmurmel\.
copy ..\murmel-langref.md      %DESTDIR%\jmurmel\.
copy ..\mlib.md                %DESTDIR%\jmurmel\.
copy ..\murmel-langref.html    %DESTDIR%\jmurmel\.
copy ..\mlib.html              %DESTDIR%\jmurmel\.


REM configure application class data sharing, see https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html#application-class-data-sharing

set JAVACMD=java %MODULES%

REM run jmurmel to create a classlist
echo (load "../samples.murmel/murmel-test")| %JAVACMD% -Xshare:off  -XX:DumpLoadedClassList=target\jmurmel.classlist -cp %DESTDIR%\jmurmel\app\jmurmel.jar io.github.jmurmel.LambdaJ

REM create classes.jsa from classlist
%JAVACMD% -Xshare:dump -XX:SharedClassListFile=target\jmurmel.classlist -XX:SharedArchiveFile=%DESTDIR%\jmurmel\runtime\bin\server\classes.jsa -cp %DESTDIR%\jmurmel\app\jmurmel.jar
