#!/bin/sh

#
# Compile jmurmel.jar into a standalone Linux executable
# (probably and/ or Windows and/ or MacOS executables but only Linux was tested)
#
# Needs a local installation of GraalVM:
# install a C compiler that will be used by GraalVM's native_image,
# download an appropriate GraalVM distribution
# e.g. from https://github.com/graalvm/graalvm-ce-builds/releases/tag/vm-22.3.1
# unzip it somewhere, adjust GRAAL_HOME accordingly and then do
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

export GRAAL_HOME=/usr/local/graalvm-ce-java19-22.3.1
export PATH=$GRAAL_HOME/bin:$PATH

test -d target || mkdir target

native-image \
  -H:IncludeResources="META-INF/.*" \
  -H:ReflectionConfigurationFiles=./src/main/graalvm/reflectconfig \
  -H:DynamicProxyConfigurationFiles=./src/main/graalvm/proxyconfig \
  --no-fallback \
  --report-unsupported-elements-at-runtime \
  -cp ./target/unsupported.jar \
  -jar ../lambda/target/jmurmel.jar \
  -o target/jmurmel

# in case you want to use "jmurmel -XX:+DumpHeapAndExit"
#  --enable-monitoring=heapdump \