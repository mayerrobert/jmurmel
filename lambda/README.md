# LambdaJ

A customizable Lisp-1-ish interpreter written in Java8+ that can be used
standalone as well as embedded.

**Standalone use:**

    $ java -jar lambdaj-1.0-SNAPSHOT.jar
    Enter a Lisp expression:
    LambdaJ>

The command above will wait for you to enter an expression, interpret it and print it's result.

    C:\> echo (write (quote Hello,\ World!))| java -jar lambdaj-1.0-SNAPSHOT.jar
    Hello, World!
    C:\>

or

    $ echo "(write (quote Hello,\ World!))" | java -jar lambdaj-1.0-SNAPSHOT.jar
    Hello, World!
    $

The commands above will read an expression from stdin and interpret it.

Command line parameters in standalone mode:

* `--result` ... print the result to stdout, this is the default when reading an expression from the console 
* `--trace` ... print lexing/ parsing/ eval info to stderr

**Embedded use:**

    LambdaJ interpreter = new LambdaJ();
    String result = interpreter.interpret(System.in, System.out);

or pass a `new ByteArrayInputStream()` or something,
see e.g. `LambdaJTest.java` for an embedded use example.

## Features
The environment contains the symbols `nil` and `t` and the functions

* `quote, cons, car, cdr`
* `cond, if`
* `apply`
* `lambda`
* `labels`
 
* `eq, atom, consp, listp, symbolp, numberp, null?`

* `assoc`
* `read`
* `write, writeln` ... write expects one argument, writeln expects zero or one argument(s)

* `=, <, <=, >, >=`
* `+, -, *, /, mod`

Most (maybe all) tail calls including tail recursive calls are optimized away.

Variables and functions share one namespace, symbols names must not start with a digit.

Numbers must start with a digit, a `+` or a `-`.
E.g. the expression `(/ 1 -0)` is valid and will yield `-Infinity`.

Math operators are implemented using Java operators for double. This is probably different to Common Lisp
esp. around +/-NaN, +/-Infinity, division by zero and so on.

The only data types currently supported are symbols, pairs (i.e. lists) and numbers (represented as Java Double).

Lambdas are dynamic, i.e. no lexical closures (yet?).

## Customization

LambdaJ comes with:
* a parser that reads S-expressions composed of lists, symbols and Doubles.
* Math support for Doubles

You could substitute the default parsers for reading data and/ or programs by your own parsers that reads e.g. XML or JSON
instead of S-expressions, and/ or support additional data types.

You could extend LambdaJ's environment with your own builtin functions and/ or data types e.g. for supporting BigDecimals
or reading/ writing from/ to a JDBC Datasource.

Datentypen kann man im Parser, in Builtins, oder beidem unterstuetzen. LambdaJ sieht nur Atoms.
Wenns nur im Parser supported ist, kann man die datentypen nur in lists reinstecken und als result zurueckgeben.

Wenns nur in builtins unterstuetzt ist, kann der source code die datentypen nicht enthalten, aber builtins koennten sie erzeugen und damit rechnen, z.B. (+ (big-decimal 1.0) (big-decimal 2.0))

Beispiel: function "read-csv" die liest CSV und liefert eine List of Lists,
darin sind Zahlen als BigDecimal representiert, und Datum/Zeit/Timestamp als java.time.*

## References

Based on [micro-lisp](https://github.com/carld/micro-lisp)
with some additional inspiration from [Implementing Lisp (wiki.c2.com)](https://wiki.c2.com/?ImplementingLisp).

And, of course:
[Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I](http://www-formal.stanford.edu/jmc/recursive.pdf), John McCarthy's famous as well as brilliant paper.
