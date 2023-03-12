#!/bin/sh

#
# Compile jmurmel.jar into a standalone Linux executable
# (probably and/ or Windows and/ or MacOS executables but only Linux was tested)
#
# Needs a local installation of GraalVM:
# download an appropriate GraalVM distribution
# e.g. from https://github.com/graalvm/graalvm-ce-builds/releases/tag/vm-22.3.1
# unzip it somewhere, adjust GRAAL_HOME accordingly and then do
#
# $ sudo $GRAAL_HOME/bin/gu install native-image
#
# $ mvn install
# $ cd scripts
# $ sh jmurmel-native.sh
#
# This will create scripts/target/jmurmel.
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

native-image \
  -H:IncludeResources="META-INF/.*" \
  -H:ReflectionConfigurationFiles=../lambda/src/main/graalvm/reflectconfig \
  -H:DynamicProxyConfigurationFiles=../lambda/src/main/graalvm/proxyconfig \
  --report-unsupported-elements-at-runtime \
  -jar ../lambda/target/jmurmel.jar \
  -o target/jmurmel
