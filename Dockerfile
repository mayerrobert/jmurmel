#
# Dockerfile for JMurmel w/o graphics
#
# Usage:
#
#    $ podman build -t jmurmel .
#    $ podman run -it --rm jmurmel
#
# The above will build a docker image and launch an interactive REPL session.
#
# Optional: after the build command some docker images could be deleted
# unless you want to keep them around to rebuild again and/ or use them for other purposes:
#
#    $ podman rmi maven:3.8.5-openjdk-18
#    $ podman rmi oraclelinux:8
#
# maybe followed by
#
#    $ podman image prune
#

FROM maven:3.8.5-openjdk-18 AS builder

WORKDIR jmurmel
COPY . .

RUN mvn -B package -pl lambda -DskipTests && \
    jlink --output jm --compress=2 --no-header-files --no-man-pages --strip-debug --add-modules java.base,jdk.compiler,jdk.zipfs,jdk.jfr,jdk.localedata


FROM oraclelinux:8-slim

WORKDIR jmurmel
COPY --from=builder /jmurmel/jm /jmurmel/lambda/target/jmurmel.jar /jmurmel/samples.mlib/mlib.lisp ./

ENTRYPOINT [ "./bin/java", "-jar", "jmurmel.jar" ]
