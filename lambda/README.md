# LambdaJ

A Lisp-1-ish interpreter written in Java8+ that can be used
standalone as well as embedded.

**Standalone use:**

    java -jar lambda-1.0-SNAPSHOT.jar

The above will wait for an expression which then will be interpreted and it's result will be printed.

    C:\> echo (write (quote hello))| java -jar lambda-1.0-SNAPSHOT.jar
    hello(quote t)

or

    $ echo "(write (quote hello))" | java -jar lambda-1.0-SNAPSHOT.jar
    hello(quote t)

**Embedded use:**

    Lambda interpreter = new Lambda();
    String result = interpreter.interpret(System.in, System.out);

or pass a `new ByteArrayInputStream()` or something,
see e.g. `LambdaTest.java` for an embedded use example.

## Features
The environment contains `nil` and the functions

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

The only data types currently supported are symbols and pairs (i.e. lists).

Lambdas are dynamic, i.e. no lexical closures (yet?).

No tail recursion optimization (yet?), sorry.

Based on [micro-lisp](https://github.com/carld/micro-lisp).
