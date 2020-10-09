# LambdaJ

A customizable Lisp-1-ish interpreter written in Java8+ that can be used
standalone as well as embedded.

LambdaJ has garbage collection, tail call optimization,
and except read/ write/ writeln it is purely functional (but see Customization below).

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
* `--help` ..... show all the available commandline parameters and quit

**Embedded use:**

    LambdaJ interpreter = new LambdaJ();
    Object result = interpreter.interpretExpressions(System.in, System.out);
    if (result instanceof LambdaJ.ConsCell) {
        LambdaJ.ConsCell listResult = (LambdaJ.ConsCell)result;
        for (Object car: listResult) {
            System.out.println("received: " + car);
        }
    }

or pass a in `new ByteArrayInputStream()` or something,
see e.g. `LambdaJTest.java` for an embedded use example.

## Examples

write, string-format

    LambdaJ> (write (string-format "%s, World!%n" "Hello"))
    Hello, World!
    result: t

Tail recursion

    LambdaJ> (labels ((factTR (n a)
                     (if (= n 0)
                         a
                         (factTR (- n 1) (* n a)))))
     (write (string-format "Factorial of 50 is %g" (factTR 50 1))))
    Factorial of 50 is 3,04141e+64
    result: t

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

* `=, <, <=, >, >=, +, -, *, /, mod` ... Java Double
* `stringp, string-format` ... `string-format` works like Java `String#format(String format, Object... args)`

Most of these features can be disabled using commandline arguments.
If you want to experiment with a bare-bones-Lisp use `--help` for details.
 
Most (maybe all) tail calls including tail recursive calls are optimized away.

Variables and functions share one namespace.
Symbols names must not start with a digit, only the first 2000 characters are significant.

Numbers must start with a digit, a `+` or a `-`.
E.g. the expression `(/ 1 -0)` is valid and will yield `-Infinity`.

Math operators are implemented using Java operators for double. This is probably different to Common Lisp
esp. around +/-NaN, +/-Infinity, division by zero and so on.

The only data types currently supported are symbols, pairs (i.e. lists), numbers (represented as Java Double)
and strings. String literals are 2000 chars max length. (Strings have very limited support.)

Lambdas are dynamic, i.e. no lexical closures (yet?).

## References

Based on [micro-lisp](https://github.com/carld/micro-lisp)
with some additional inspiration from [Implementing Lisp (wiki.c2.com)](https://wiki.c2.com/?ImplementingLisp).

And, of course:
[Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I](http://www-formal.stanford.edu/jmc/recursive.pdf), John McCarthy's famous as well as brilliant paper.
