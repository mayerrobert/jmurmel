#!/bin/sh

# 
# jpackage-linux - create JMurmel app-image with a linux launcher
# 
# Must be run from within the .../scripts directory.
# Needs jpackage, jlink & friends which are included in Java16 and above.
# 
# This will create an "app-image" aka a directory hierachy
# that contains everything needed to run JMurmel
# including a Java runtime.
# 
# The end result will be the directory "jmurmel" in "target".
# Use .../jmurmel/bin/jmurmel to launch a REPL.

# begin config

# this is where the directory "jmurmel" will be created. Could be changed to e.g. "/usr/local"
export DESTDIR=target

# limit the parts of the JDK that will go into the packaged runtime. Comment out to include the complete JDK,
# or use e.g. "export MODULES=java.base" to create a reduced size image w/o graphics or support for compilation.
export MODULES="--add-modules java.base,java.desktop,jdk.compiler,jdk.zipfs,jdk.jfr,jdk.localedata,java.management"

# Java options that will be used at runtime
export JOPTIONS="--java-options -Xmx1G --java-options -Xss2m --java-options -XX:+UseParallelGC"
# enable class loading debugging for CDS testing
#export JOPTIONS="$JOPTIONS --java-options -Xshare:on --java-options -Xlog:class+load --java-options -Xlog:cds=debug::none --java-options -Xlog:cds+map=trace:file=target/cds.map:none:filesize=0"

# end config



if ! [ -x "$(command -v java)" ]; then
    echo "java could not be found. Make sure Java16 or higher is installed and on the PATH."  >&2
    exit 1
fi

if ! [ -x "$(command -v jpackage)" ]; then
    echo "jpackage could not be found. Make sure Java16 or higher is installed and on the PATH."  >&2
    exit 1
fi



rm -rf $DESTDIR/jmurmel
rm -rf target/jpackage-input

mkdir target/jpackage-input
cp ../lambda/target/jmurmel.jar target/jpackage-input/.
cp ../samples.mlib/mlib.lisp    target/jpackage-input/.


# create a directory with jlinked JDK, Murmel files and launcher .exe, see https://docs.oracle.com/en/java/javase/17/docs/specs/man/jpackage.html
export JLINK="--jlink-options --strip-debug --jlink-options --no-man-pages --jlink-options --no-header-files --jlink-options --compress=2"
jpackage $JLINK --type app-image -i target/jpackage-input -d $DESTDIR -n jmurmel --main-class io.github.jmurmel.LambdaJ --main-jar jmurmel.jar $MODULES $JOPTIONS

cp ../LICENSE                $DESTDIR/jmurmel/.
cp ../murmel-langref.md      $DESTDIR/jmurmel/.
cp ../mlib.md                $DESTDIR/jmurmel/.
cp ../murmel-langref.html    $DESTDIR/jmurmel/.
cp ../mlib.html              $DESTDIR/jmurmel/.


# configure application class data sharing, see https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html#application-class-data-sharing

export JAVACMD="java $MODULES"

# run jmurmel to create a classlist
echo '(load "../samples.murmel/murmel-test")' | $JAVACMD -Xshare:off  -XX:DumpLoadedClassList=target/jmurmel.classlist -cp $DESTDIR/jmurmel/lib/app/jmurmel.jar io.github.jmurmel.LambdaJ

# create classes.jsa from classlist
$DESTDIR/jmurmel/lib/runtime/bin/java -Xshare:dump -XX:SharedClassListFile=target/jmurmel.classlist -XX:SharedArchiveFile=$DESTDIR/jmurmel/lib/runtime/lib/server/classes.jsa -cp $DESTDIR/jmurmel/lib/app/jmurmel.jar

# java, javac & friends are no longer needed
rm -rf $DESTDIR/jmurmel/lib/runtime/bin/


# see if it works
$DESTDIR/jmurmel/bin/jmurmel --version

exit 0
