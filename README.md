# JMurmel

[![Java CI with Maven](https://github.com/mayerrobert/jmurmel/actions/workflows/maven.yml/badge.svg)](https://github.com/mayerrobert/jmurmel/actions/workflows/maven.yml)
[![CodeQL](https://github.com/mayerrobert/jmurmel/actions/workflows/codeql-analysis.yml/badge.svg)](https://github.com/mayerrobert/jmurmel/actions/workflows/codeql-analysis.yml)

**JMurmel is a lightweight Lisp-1-ish (mostly based on a small subset of Common Lisp with a side of Scheme)
interpreter/ compiler written in Java8 that can be used standalone as well as embedded.**

Currently weighing in at ~200kB (size of the compiled jmurmel.jar file
containing the interpreter + compiler + runtime + REPL),
or one single Java source file, compatible with Java 8 through Java 19-ea.

Murmel is the name of the programming language (which is a Lisp dialect),
JMurmel is the name of the interpreter/ compiler that implements Murmel.
For more details on the language see [murmel-langref.md](murmel-langref.md),
and [mlib.md](mlib.md) for Murmel's default library.

The interpreter, REPL, compiler as well as Jars with compiled Murmel all run on top of the JVM.
Compilation is done as Murmel to Java source
which is then compiled to .class files using the JDK compiler.
JMurmel as well as compiled Murmel programs should run on all platforms supported by Java8+.

JMurmel features a REPL with a trace facility (trace and untrace function calls),
tail call optimization,
lexical environments,
a macro facility,
backquote expansion including nested backquotes,
JSR223 support,
support for JFR (Java Flight Recorder)/ JMC (Java Mission Control),
turtle- and bitmap graphics
and garbage collection c/o JVM.

**Goals, Priorities**

Murmel is inspired by Common Lisp, i.e. when in doubt try to do it the Common Lisp way.
It should be relatively easy to port a program from Murmel to Common Lisp,
and Murmel knowledge should transfer to Common Lisp.

**Status**

At this time Murmel is a personal project,
don't expect it to be an industrial strength Lisp in the near future.

Both the language Murmel as well as the interpreter/ compiler JMurmel
currently are work in progress.
There may be some incompatible language changes in the future.
See also "Known Issues" in [murmel-langref.md](murmel-langref.md).

**Copyright**

Murmel and JMurmel are Copyright (C) 2020-2022 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see [LICENSE](LICENSE).


## Getting started

**Quickstart for Java 11+ users**

Instead of cloning the repo and building the jarfile
Java 11+ users can download [LambdaJ.java](lambda/src/main/java/io/github/jmurmel/LambdaJ.java)
and run

    C:\> java Lambdaj.java

for a first peek at JMurmel (compiling Murmel won't work, though, only the interpreter works that way).

**Optional: building from source**

Clone the repo and build using maven:

    C:\> git clone https://github.com/mayerrobert/jmurmel.git
    ...
    C:\> cd jmurmel
    C:\jmurmel> mvn package

The resulting jar will end up in `lambda\target\jmurmel.jar`

**Quickstart using a precompiled version**

Make sure you have Java 8+ installed (Java 8 is minimum, Java 17 is preferred).

`java -version` will tell (GNU/linux or Unix or Mac users please adjust as appropriate):

    C:\> java -version
    openjdk version "1.8.0_252"
    OpenJDK Runtime Environment (AdoptOpenJDK)(build 1.8.0_252-b09)
    OpenJDK 64-Bit Server VM (AdoptOpenJDK)(build 25.252-b09, mixed mode)

With `jmurmel.jar` in the current directory start JMurmel with the following command

    C:\> java -jar jmurmel.jar

At the REPL prompt enter e.g. `(write "Hello, World!")`, your screen should look like this:

    D:\jmurmel\lambda\target>java -jar jmurmel.jar
    Enter a Murmel form or :command (or enter :h for command help or :q to exit):

    JMurmel> (write "Hello, World!")
    "Hello, World!"
    ==> t
    JMurmel>

You just wrote and ran a Murmel program!

Now take it from there,
e.g. read [murmel-langref.md](murmel-langref.md) and [mlib.md](mlib.md).  
Or take a look at the example code in [samples.murmel/](samples.murmel)
and [samples.mlib/](samples.mlib)
Or type `:h` at the REPL prompt.  
Or run `java -jar jmurmel.jar --help`.  


## Standalone use

    $ java -jar jmurmel.jar
    Enter a Murmel form or :command (or enter :h for command help or :q to exit):
    JMurmel>

The command above will wait for you to enter an S-expression, interpret it and print it's result.
Try e.g.

    $ java -jar jmurmel.jar
    Enter a Murmel form or :command (or enter :h for command help or :q to exit):
    JMurmel> (+ 1 2)

    ==> 3.0
    JMurmel>

JMurmel also can read program text from stdin:

    C:\> echo (write (quote Hello,\ World!))| java -jar jmurmel.jar
    |Hello, World!|
    ==> t
    
    C:\>

or

    $ echo "(write (quote Hello,\ World!))" | java -jar jmurmel.jar
    |Hello, World!|
    ==> t
    
    $

The commands above will read an S-expression from stdin and interpret it.

Command line parameters in standalone mode:

* `--result` ... print the result to stdout, this is the default when reading an S-expression from the console 
* `--help` ..... show all the available commandline parameters and quit

**Installation**

Currently there is no setup.exe or .msi or .rpm or .deb file. Just copy `jmurmel.jar` and (optional but recommended) `mlib.lisp` somewhere convenient,
and maybe create a batchfile along these lines (Windows .cmd-style shown here):

    jm.cmd:
    --- snip ---
    @echo off
    setlocal
    set JAVA_HOME=C:\Apps\Java\X64\jdk8u252-b09
    set JMURMEL=C:\jmurmel\lambda\target\jmurmel.jar
    %JAVA_HOME%\bin\java -jar %JMURMEL% %*
    endlocal
    --- snip ---

or setup an alias (again Windows style shown here):

    set JAVA_HOME=c:\Program files\jdk-17.0.2
    set JAVA_OPTS=-Xms200M -Xmx1G -Xss2m -XX:+UseZGC
    doskey jm="%JAVA_HOME%\bin\java" %JAVA_OPTS% -jar C:\jmurmel\lambda\target\jmurmel.jar --libdir C:\jmurmel\samples.mlib $*

That way to run e.g. the file `hanoi.lisp` you can use the following command:

    C:\> jm hanoi.lisp

or if you want to interpret a file and then go into REPL:

    C:\> jm --repl hanoi.lisp

Or use JMurmel from within Emacs: add `(setq inferior-lisp-program "jm --tty")`
to your `.emacs` file and do `M-x (run-lisp)`
and use `C-M-x` to eval Murmel S-expressions.

Or see [HACKING](HACKING) for how to use [rlwrap](https://github.com/hanslub42/rlwrap)
to get a command history and TAB-completion at the JMurmel REPL.

## Embedded use

JMurmel can also be used embedded in another Java program.
JMurmel uses Java8 only, but should run on higher versions as well
(Java 17 is lightly tested, and Github CI builds and tests 8, 17, 18-ea and 19-ea).
It comes as one self contained jar, no further dependencies needed.

Minimal "Hello, World!" example:

    @Test
    public void testMinimal() {
        Object result = new LambdaJ()
            .interpretExpression(new StringReader("(cons 'Hello,\\ World! nil)")::read, s -> {});
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
                                 // and - for dotted lists - the cdr of the last cons cell
            s += car.toString();
        }
        assertEquals("ab", s);
    }

Java calls JMurmel function:

    @Test
    public void testCallMurmelFromJava() {
        LambdaJ interp = new LambdaJ();
        interp.interpretExpression(new StringReader("(defun f (p1 p2) (* p1 p2))")::read, s -> {});

        MurmelFunction add = interp.getFunction("f");
        Object result = add.apply(2, 3);
        assertEquals(6.0, result);
    }

See `EmbeddedTest.java` or `FFITest.java` for more embedded use examples
including an example of how to hook up your own Lisp primitives written in Java.

Or see `JSR223Test.java` for an example on how to use JMurmel through the
[Java Scripting API](https://docs.oracle.com/javase/8/docs/technotes/guides/scripting/prog_guide/about.html)
(setting/ accessing Java objects via JSR223 from Murmel code is not supported yet.)

## Examples
A [Quine](http://rosettacode.org/wiki/Quine#Lisp)

    JMurmel> ((lambda (x) (list x (list (quote quote) x))) (quote (lambda (x) (list x (list (quote quote) x)))))
    
    ==> ((lambda (x) (list x (list (quote quote) x))) (quote (lambda (x) (list x (list (quote quote) x)))))
    JMurmel>

write, format

    JMurmel> (write (format nil "%s, World!%n" "Hello"))
    "Hello, World!
    "
    ==> t

Tail recursion, locale dependent number formatting

    JMurmel> (labels ((factTR (n a)
                              (if (= n 0)
                                  a
                                  (factTR (- n 1) (* n a)))))
     (write (format-locale nil "en-US" "Factorial of 50 is %g" (factTR 50 1))))
    "Factorial of 50 is 3.04141e+64"
    ==> t

See [samples.murmel/](samples.murmel) and [samples.mlib/](samples.mlib)
for more Murmel example code
including usage of Murmel's turtle graphics.

## JMurmel Features

JMurmel supports the special forms:

* `quote, cond, if`
* `lambda` ... create a lexical closure
* `labels` ... define local functions

* `define` ... inserts a (symbol value) pair into the top level environment, returns `value`.
  E.g.
    - `(define *answer* 42)`
    - or `(define print-answer (lambda () (write (string-format "%2.2g" *answer*))))`
* `defun` ... `(<symbol> (<params>*) <bodyform>*)`, e.g. `(defun addone (n) (+ n 1))`
* `let, let <symbol> ` (i.e. named let)`, let*, letrec, progn`
* `setq`
* `defmacro`
* `throw, catch, unwind-protect`
* `multiple-value-call, multiple-value-bind`
* `declaim, load, require, provide`

The environment contains the symbols `nil`, `t`, `pi`, `*command-line-argument-list*`
and the functions

* `cons, car, cdr, rplaca, rplacd`
* `apply` ... works similar to Scheme, e.g. `(apply + '(1 2 3))` or `(apply + (cons 2 (cons 3 nil)))`
* `eval, eq, eql, null, atom`
* `consp, listp, functionp, floatp, integerp, numberp, characterp, stringp, symbolp, vectorp`

* `assq, assoc, append, list, list*`
* `vector, svref, svset, svlength, make-array, simple-vector->list, list->simple-vector`
* `read, write, writeln`

* `=, <, <=, >, >=, /=, +, -, *, /, 1+, 1-`
* `sqrt, log, log10, exp, expt, signum, mod, rem`
* `ceiling, floor, round, truncate, fceiling, ffloor, fround, ftruncate`

* `char-code, code-char, string=, string->list, list->string`

* `gensym, macroexpand-1, trace, untrace`

For more functions (including turtle graphics and functions to call Java)
and more details on the language supported see [murmel-langref.md](murmel-langref.md).

Tail calls including tail recursive calls are optimized.

Lambdas can be dynamic or lexical, this can be selected via a keyword parameter to `lambda`.

Variables and functions share one namespace.
Symbols names must not start with a digit, only the first 2000 characters are significant.

Numbers must start with a digit, a `+` or a `-`.
E.g. the S-expression `(/ 1 -0)` is valid and will yield `-Infinity`.

Math operators are implemented using Java operators for double. This is probably different to Common Lisp
esp. around +/-NaN, +/-Infinity, division by zero and so on.

The only data types currently supported are symbols, pairs (i.e. lists), numbers (represented as Java Double or Long),
characters and strings. String literals are 2000 chars max length.

Most of these features can be disabled using commandline arguments.
You can disable pretty much everything except S-expressions, symbols, cons cells and lambda.
If you want to experiment with a bare-bones-Lisp use `--help-features` for details.

## Customization

JMurmel comes with:

* a parser that reads S-expressions composed of lists, symbols, doubles, longs, characters and Strings.
* Math support for Longs and Doubles
* (Very) simple I/O
* Some support for strings

However:

You could substitute the default parser for reading data and/ or programs by your own parsers that reads e.g. XML or JSON
instead of S-expressions, and/ or support additional data types.

See `SerializeTest`: this uses Java's serialization to deserialize Lisp objects as forms from a stream
and feeds then to the interpreter instead of parsing S-expressions.

You could extend LambdaJ's environment with your own builtin functions and/ or data types e.g. for supporting BigDecimals
or reading/ writing from/ to a JDBC Datasource.

See `EmbeddedTest#testCustomEnv()` for an example.

Additional datatypes can be supported in your custom Parser, in your custom primitives or in both.

JMurmel is based on values of the Java type Object. It will see your custom datatypes as atoms and should handle them
without any need for change.
