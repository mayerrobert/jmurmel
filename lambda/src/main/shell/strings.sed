# strings.sed
#
# list string literals in a java file
#
# usage:
# sed -nf src\main\shell\strings.sed src\main\java\io\github\jmurmel\LambdaJ.java | sort | uniq -c

# remove comments
sx\(.*\)[ ]*//.*x\1x

sx/\*.*\*/xx

# remove annotations
sx@Suppress.*xx

# print each string literal on it's own line
s/[^"]*["]\([^"]*\)["][^"]*/"\1"\n/gp
