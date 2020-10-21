# usage:
# sed -nf strings.sed < src\main\java\com\robertmayer\lambdaj\LambdaJ.java | sort
s/[^"]*["]\([^"]*\)["].*/\1/gp