# strings.sed
#
# list string literals in a java file
#
# usage:
# sed -nf src\main\shell\strings.sed src\main\java\com\robertmayer\lambdaj\LambdaJ.java | sort | uniq

s/[^"]*["]\([^"]*\)["].*/"\1"/gp