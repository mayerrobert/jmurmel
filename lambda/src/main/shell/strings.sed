# strings.sed
#
# list string literals in a java file
#
# usage:
# sed -nf src\main\shell\strings.sed src\main\java\io\github\jmurmel\LambdaJ.java | sort | uniq -c

# remove comments
sx\(.*\)[ ]*//.*x\1xg

# print string literals
s/[^"]*["]\([^"]*\)["].*/"\1"/gp