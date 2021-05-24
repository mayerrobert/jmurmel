/* LambdaJ is Copyright (C) 2020 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

/**

<p>This package contains the implementation of JMurmel, an interpreter/ compiler for the Lisp-dialect Murmel,
these API-docs are for using JMurmel as an embedded interpreter and/ or compiler (as-is or customized),
and for compiling and using interpreted or compiled Murmel programs from within another Java application.

<p>{@link LambdaJ} is the main class for
<ul>
<li>standalone commandline use
<li>embedding a Murmel interpreter
<li>embedding interpreted Murmel programs.
</ul>

<p>{@link LambdaJ.MurmelJavaCompiler} is the main class for compiling Murmel programs,
{@link LambdaJ.MurmelJavaProgram} is the base class of compiled Murmel programs.


<p><b>Characterset and line separator summary</b>

<p>Murmel and JMurmel internally use Unicode strings and characters.

<p>Murmel's surface representation is S-expressions in UTF-8 files,
any of "\n", "\r\n", "\r" are acceptable line separators.
However, when JMurmel's REPL reads and prints from/ to the console then the console characterset will be used.

<p>Murmel files given as arguments on JMurmel's commandline will be read as UTF-8,
any line separator.

<p>S-expressions written to files will be UTF-8 with platform line separators.

<p>JMurmel's REPL reads and prints in the console characterset with OS-dependent line separators.
Note that this goes for input that is typed on the keyboard as well as input redirected from a file.
I.e.<pre>    jmurmel &lt; program.lisp
is different from
    jmurmel --repl &lt; program.lisp</pre>
The former command (no REPL involved) will read UTF-8,
the latter command (which forces REPL and is not too useful anyways) will read console characterset.

<p>JMurmel writes Java sourcefiles as UTF-8 with Unix-style "\n" line separators.


<p><b>Charactersets</b></p>
<p>JMurmel internally uses Unicode strings and characters.
The translation from "external" to "internal" is somewhat messy and involved.

<p>"External" can be ...tbd

<p>The default Lisp reader {@link LambdaJ.SExpressionParser} reads bytes from input,
interprets these bytes according to the UTF-8 characterset
and subsequently transforms bytes to Unicode strings.

<p>EXCEPT: the Repl interprets input according to the characterset specified by the Java system property 'sun.stdout.encoding').

<p>The default Lisp writer {@link LambdaJ.SExpressionWriter} sends Unicode strings to Java's System.out
which in turn writes bytes to the console or a redirected file.
Java's System.out does this translation according to Java's system default characterset (which can be configured by the Java system property 'sun.stdout.encoding').

<p>JMurmel does not use the system property 'file.encoding'. TODO except LispReader for JSR223 programs.
Note that e.g. on Windows the default value of file.encoding is 'Cp1252' but input read from System.in is encoded in 'cp850'.

<p>The compiler writes files in the UTF-8 characterset.


<p><b>Line separators</b></p>

<p>The internal representation of line separators is Unix-style, i.e. a character with the value 10.
The external representation of line line separators is platform dependent,
i.e. Java's System.lineSeparator will be used.
(Except: generated Java source, see below.)

<p>In the surface representation of Murmel any of "\n", "\r\n", "\r" are acceptable line separators.
Note that line separators in Murmel source files only play a role inside multiline string literals and in counting lines for error messages,
otherwise they are just whitespace.

<p>The REPL will translate any of "\n", "\r\n", "\r" to "\n" when reading from either the console or from a redirected input.
The REPL will however print or echo data using the system default line separator.

<p>The Lisp reader will translate any of "\n", "\r\n", "\r" to "\n".
This means that Murmel programs will only see "\n" as a line separator.

<p>The Lisp printer will print data using the system default line separator.

<p>The compiler writes Java files with '\n' line separators.

<p>Exception message texts will use the platform default line separator.


<p><b>Copyright</b></p>
<p>Murmel and JMurmel are Copyright (C) 2020-2021 Robert Mayer.

<p>This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT.

*/
package io.github.jmurmel;
