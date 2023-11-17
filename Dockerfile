#
# Dockerfile for JMurmel with X11 graphics
#
# Will build a docker image from a local clone of https://github.com/mayerrobert/jmurmel.git
# Needs docker or podman and a local git repo containing the jmurmel files.
# A local Java- or maven installation is not needed.
#
# Do something like
#
#    $ git clone https://github.com/mayerrobert/jmurmel.git
#    $ cd jmurmel
#
# and then:
#
# Usage:
#
#    $ podman build -t jmurmel .
#    $ podman run -it --rm jmurmel
#
# The "podman build..." command will build a docker image from the local jmurmel git repo.
# The "podman run..." command will launch an interactive REPL session.
#
# You may want to mount your current working directory into the container so that JMurmel
# will "see" your local files:
#
#    $ podman run -it --rm -v .:/work -w /work jmurmel
#
# The above command will make the current working directory available as "/work"
# inside the container and cd to it before launching JMurmel.
#
# For graphics to work you will probably need to set the environment variable DISPLAY inside
# the container like so (replacing 12.34.56.78 by an appropriate IP-address, of course):
#
#    $ podman run -it --rm --env DISPLAY=12.34.56.78:0.0 jmurmel
#
#
#
# Optional: after the build command some docker images could be deleted
# unless you want to keep them around to rebuild again and/ or use them for other purposes:
#
#    $ podman rmi maven:3.8.6-openjdk-18-slim
#
# maybe followed by
#
#    $ podman image prune
#


# maven:3.8.6-openjdk-18-slim is based on "Debian 11.3" aka "Bullseye"
FROM maven:3.8-openjdk-18-slim AS builder
# binutils are needed for "jlink ... --strip-debug". Saves 4MB in the final image.
RUN apt-get update && apt-get install -y --no-install-recommends binutils && rm -rf /var/lib/apt/lists/*

WORKDIR /jmurmel
COPY . .
RUN mvn -B package -pl lambda -DskipTests && \
    jlink --output jdkbuild/jdk --compress=2 --no-header-files --no-man-pages --strip-debug --add-modules java.base,java.desktop,jdk.compiler,jdk.zipfs,jdk.jfr,jdk.localedata,java.management


#FROM oraclelinux:8-slim
## remove/ comment the next line if you don't want/ need turtle graphics.
## If you do remove X11 then you might also want to remove "java.desktop" in the jlink-command above, because without X11 there is no need/ use for AWT and Swing.
#RUN microdnf -y --nodocs install libX11 libXext libXrender libXtst freetype && microdnf -y clean all

FROM debian:bullseye-slim
# remove/ comment the next line if you don't want/ need turtle graphics.
# If you do remove X11 then you might also want to remove "java.desktop" in the jlink-command above, because without X11 there is no need/ use for AWT and Swing.
RUN apt-get update && apt-get install -y --no-install-recommends libx11-6 libxext6 libxrender1 libxtst6 libfreetype6 fontconfig && rm -rf /var/lib/apt/lists/*


WORKDIR /jmurmel
COPY --from=builder /jmurmel/jdkbuild /jmurmel/lambda/target/jmurmel.jar /jmurmel/samples.mlib/mlib.lisp ./

# set X11 environment variables in order to make turtle frames work.
# This will probably not work and you will need to specify the X-server on the commandline e.g.
# $ podman -it --rm --env DISPLAY=12.34.56.78:0.0 jmurmel
ENV DISPLAY=localhost:0.0 \
    LIBGL_ALWAYS_INDIRECT=1

# might want to add Java heap options e.g. -Xms500m -Xmx4G
ENTRYPOINT [ "/jmurmel/jdk/bin/java", "-Xss2m", "-XX:+UseParallelGC", "-Dsun.java2d.opengl=true", "-jar", "/jmurmel/jmurmel.jar" ]
