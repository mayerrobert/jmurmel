# litprog.sed
#
# extract special line comments to emulate a poor man's form of literate programming
#
# usage:
# sed -nf src\main\shell\litprog.sed src\main\java\io\github\jmurmel\LambdaJ.java

s@^[ ]*/// \?\(.*\)$@\1  @p