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

The commands above will read the expression from stdin and interpret it.

**Embedded use:**

    LambdaJ interpreter = new LambdaJ();
    String result = interpreter.interpret(System.in, System.out);

or pass a `new ByteArrayInputStream()` or something,
see e.g. `LambdaJTest.java` for an embedded use example.

## Features
The environment contains the symbol `nil` and the functions

* quote
* apply
* cond, if
* lambda
* labels
 
* cons
* car
* cdr
* eq
* pair?
* symbol?
* null?
* read
* write
* writeln

Variables and functions share one namespace.

The only data types currently supported are symbols and pairs (i.e. lists).

Lambdas are dynamic, i.e. no lexical closures (yet?).

No tail recursion optimization (yet?), sorry.

Based on [micro-lisp](https://github.com/carld/micro-lisp).
