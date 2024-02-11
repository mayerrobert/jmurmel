#!/bin/sh

#
# Compile jmurmel.jar into a standalone Linux executable
# (probably and/ or Windows and/ or MacOS executables but only Linux was tested)
#
# Needs a local installation of GraalVM:
# install a C compiler that will be used by GraalVM's native_image,
# download an appropriate GraalVM distribution
# e.g. from https://github.com/graalvm/graalvm-ce-builds/releases/tag/vm-22.3.1
# unzip it somewhere, adjust GRAAL_HOME accordingly and then (if using an older
# version of GraalVM that does not come with native-image already included) do
#
# $ sudo $GRAAL_HOME/bin/gu install native-image
#
# $ mvn package -pl lambda
# $ mvn package -f graalvm/pom.xml
# $ cd graalvm
# $ sh jmurmel-native.sh
#
# This will create graalvm/target/jmurmel.
# The generated jmurmel executable won't support graphics,
# won't be able to run compiled code, i.e. ":r" won't work,
# "jmethod" is limited to the classes listed in the file "reflectconfig",
# and "jproxy" is limited to the classes listed in the file "proxyconfig".
#
# In addition to jmurmel's commandline arguments the generated executable
# will support some Java specific arguments such as -Xms and -Xmx
# (as all GraalVM native binaries do).
#

export GRAAL_HOME=/usr/local/graalvm-jdk-21.0.2+13.1
export PATH=$GRAAL_HOME/bin:$PATH

test -d target || mkdir target


NI_OPTS="\
  -H:IncludeResources=\"META-INF/.*\" \
  -H:ReflectionConfigurationFiles=./src/main/graalvm/reflectconfig \
  -H:DynamicProxyConfigurationFiles=./src/main/graalvm/proxyconfig \
  -H:SerializationConfigurationFiles=./src/main/graalvm/serializationconfig \
  --no-fallback \
  --initialize-at-build-time=io.github.jmurmel \
  --initialize-at-run-time=io.github.jmurmel.InstallDir \
"

# to have reports written during compilation:
#  -H:+PrintClassInitialization \

# to have verbose info printed during build
#  -H:+PrintUniverse \

# in case you want to use "jmurmel -XX:+DumpHeapAndExit"
#  --enable-monitoring=heapdump \


# comment the next line if using Windows and/ or GraalVM CE
NI_OPTS="--gc=G1 $NI_OPTS"


# build using profile-guided-optimization (not supported by GraalVM CE)
native-image --pgo-instrument $NI_OPTS -cp ./target/unsupported.jar -jar ../lambda/target/jmurmel.jar -o target/jmurmel
target/jmurmel -XX:ProfilesDumpFile=target/jmurmel.iprof ../samples.murmel/murmel-test.lisp ../samples.murmel-mlib/mlib-test.lisp
mv target/jmurmel target/jmurmel-pgo
native-image --pgo=target/jmurmel.iprof $NI_OPTS -cp ./target/unsupported.jar -jar ../lambda/target/jmurmel.jar -o target/jmurmel


# or build w/o PGO
#native-image $NI_OPTS -cp ./target/unsupported.jar -jar ../lambda/target/jmurmel.jar -o target/jmurmel
