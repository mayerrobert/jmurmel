# LambdaJ

A Lisp-1-ish interpreter written in Java8+ that can be used
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
* `write, writeln`

* `=, <, <=, >, >=`
* `+, -, *, /, mod`

Variables and functions share one namespace, symbols names must not start with a digit.

Numbers must start with a digit, a `+` or a `-`.
E.g. the expression `(/ 1 -0)` is valid and will yield `-Infinity`.

Math operators are implemented using Java operators for double. This is probably different to Common Lisp
esp. around +/-NaN, +/-Infinity, division by zero and so on.

The only data types currently supported are symbols, pairs (i.e. lists) and numbers (represented as Java Double).

Lambdas are dynamic, i.e. no lexical closures (yet?).

No tail recursion optimization (yet?), sorry.

Based on [micro-lisp](https://github.com/carld/micro-lisp).
