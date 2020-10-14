# LambdaJ

A customizable Lisp-1-ish interpreter written in Java8+ that can be used
standalone as well as embedded.

LambdaJ has garbage collection c/o Java, tail call optimization,
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
    |Hello, World!|
    $

The commands above will read an expression from stdin and interpret it.

Command line parameters in standalone mode:

* `--result` ... print the result to stdout, this is the default when reading an expression from the console 
* `--help` ..... show all the available commandline parameters and quit

**Embedded use:**

Minimal "Hello, World!" example:

    @Test
    public void testMinimal() {
        Object result = new LambdaJ().interpretExpression(
                new StringReader("(cons 'Hello,\\ World! nil)")::read,
                (s) -> { return; }
                );
        assertEquals("(Hello, World!)", result.toString());
    }

Slightly more advanced example:

    @Test
    public void testCons() {
        // run a Lisp program
        LambdaJ interpreter = new LambdaJ();
        StringBuffer program = new StringReader("(cons 'a 'b)")
        StringBuffer output = new StringBuffer();
        Object result = interpreter.interpretExpression(program::read, output::append);
        // done, that was it!
        
        // check results
        assertEquals("(a . b)", result.toString());
        assertEquals(0, out.length());

        assertTrue(result instanceof LambdaJ.ConsCell); // type of result depends on the Lisp program
                                                        // could be String, Double or ConsCell (i.e. list)
        LambdaJ.ConsCell list = (LambdaJ.ConsCell)result;

        String s = "";
        for (Object car: list) { // the iterator will return subsequent car
                                 // and - if nonnull - the cdr of the last cons cell
            s += car.toString();
        }
        assertEquals("ab", s);
    }

See `EmbeddedTest.java` or `LambdaJTest.java` for more embedded use examples
including an example of how to hook up your own Lisp primitives written in Java.

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
* `apply` ... works more like Scheme and expects a single argument list, e.g. `(apply + '(1 2 3))` or `(apply + (cons 2 (cons 3 nil)))`
* `lambda`
* `labels`
 
* `eq, atom, consp, listp, symbolp, numberp, null?`

* `assoc`
* `read`
* `write, writeln` ... write expects one argument, writeln expects zero or one argument(s)

* `=, <, <=, >, >=, +, -, *, /, mod` ... Java Double
* `stringp, string-format, string-format-locale`
    - `string-format` works like Java `String#format(String format, Object... args)`
    - `string-format-locale` works like Java `String#format(Locale loc, String format, Object... args)`, e.g. `(string-format-locale "en-US" "Hello Number %g" 1)` 

Tail calls includingtail recursive calls are optimized, also tail calls out of the body of a `labels` clause.

Variables and functions share one namespace.
Symbols names must not start with a digit, only the first 2000 characters are significant.

Numbers must start with a digit, a `+` or a `-`.
E.g. the expression `(/ 1 -0)` is valid and will yield `-Infinity`.

Math operators are implemented using Java operators for double. This is probably different to Common Lisp
esp. around +/-NaN, +/-Infinity, division by zero and so on.

The only data types currently supported are symbols, pairs (i.e. lists), numbers (represented as Java Double)
and strings. String literals are 2000 chars max length. (Strings have very limited support.)

Lambdas are dynamic, i.e. no lexical closures (yet?).

Most of these features can be disabled using commandline arguments.
You can disable pretty much everything except S-expressions, symbols, cons cells and lambda.
If you want to experiment with a bare-bones-Lisp use `--help` for details.
 
## Customization

LambdaJ comes with:

* a parser that reads S-expressions composed of lists, symbols, Doubles and Strings.
* Math support for Doubles

You could substitute the default parser for reading data and/ or programs by your own parsers that reads e.g. XML or JSON
instead of S-expressions, and/ or support additional data types.

You could extend LambdaJ's environment with your own builtin functions and/ or data types e.g. for supporting BigDecimals
or reading/ writing from/ to a JDBC Datasource.

Additional datatypes can be supported in your custom Parser, in your custom primitives or in both.

LambdaJ is based on values of the Java type Object. It will see your custom datatypes as atoms and should handle them
without any need for change.
See `EmbeddedTest#testCustomEnv()` for an example.

## References

Based on [micro-lisp](https://github.com/carld/micro-lisp)
with some additional inspiration from [Implementing Lisp (wiki.c2.com)](https://wiki.c2.com/?ImplementingLisp).

And, of course:
[Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I](http://www-formal.stanford.edu/jmc/recursive.pdf), John McCarthy's famous as well as brilliant paper.
