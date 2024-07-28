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

REM limit the parts of the JDK that will go into the packaged runtime. Comment out to include the complete JDK,
REM or use e.g. "set MODULES=java.base" to create a reduced size image w/o graphics or support for compilation.
set MODULES=--add-modules java.base,java.desktop,jdk.compiler,jdk.zipfs,jdk.jfr,jdk.localedata,java.management

REM Java options that will be used at runtime
set JOPTIONS=--java-options -Xmx1G --java-options -Xss2m --java-options -XX:+UseParallelGC

REM end config



rd /s /q target\jmurmel
rd /s /q target\jpackage-input

md target\jpackage-input
copy ..\lambda\target\jmurmel.jar target\jpackage-input\.
copy ..\samples.murmel-mlib\mlib.lisp    target\jpackage-input\.
copy ..\LICENSE                   target\jpackage-input\.
copy ..\murmel-langref.md         target\jpackage-input\.
copy ..\mlib.md                   target\jpackage-input\.
copy ..\murmel-langref.html       target\jpackage-input\.
copy ..\mlib.html                 target\jpackage-input\.



set JLINK=--jlink-options --strip-debug --jlink-options --strip-native-commands --jlink-options --no-man-pages --jlink-options --no-header-files --jlink-options --compress=2
set JPACKAGE=-i target\jpackage-input -d target -n JMurmel --main-class io.github.jmurmel.LambdaJ --main-jar jmurmel.jar --win-console %MODULES% %JOPTIONS%

goto :inst

REM create a directory with jlinked JDK, Murmel files and launcher .exe, see https://docs.oracle.com/en/java/javase/17/docs/specs/man/jpackage.html
jpackage %JLINK% --type app-image %JPACKAGE%

REM see if it works
target\jmurmel\jmurmel.exe --version
goto :EOF


:inst
REM this needs the WiX v3 Toolset, download and unzip from https://github.com/wixtoolset/wix3/releases/download/wix3112rtm/wix311-binaries.zip and add to the PATH

set INST=--app-version "1.5.0.0" --copyright "Copyright (C) 2020-2024 by Robert Mayer" --description "Murmel interpreter/ compiler https://jmurmel.github.io" --license-file target\jpackage-input\LICENSE --name "JMurmel"
set INST=%INST% --win-dir-chooser --win-shortcut --win-shortcut-prompt --win-per-user-install
jpackage --verbose %JLINK% --type msi %INST% %JPACKAGE%
