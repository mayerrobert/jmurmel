/* LambdaJ is Copyright (C) 2020 Robert Mayer. All rights reserved.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

/**

<p>This package contains the implementation of JMurmel, an interpreter/ compiler for the Lisp-dialect Murmel,
these API-docs are for using JMurmel as an embedded interpreter and/ or compiler (as-is or customized),
and for compiling and using interpreted or compiled Murmel programs from within another Java application.

<p>{@link com.robertmayer.lambdaj.LambdaJ LambdaJ} is the main class for embedding interpreted murmel programs.

<p>{@link com.robertmayer.lambdaj.LambdaJ.MurmelJavaCompiler LambdaJ.MurmelJavaCompiler} is the main class for compiling Murmel programs,
{@link com.robertmayer.lambdaj.LambdaJ.MurmelJavaProgram LambdaJ.MurmelJavaProgram} is the base class of compiled Murmel programs.


<p><b>Character sets</b></p>
<p>JMurmel internally uses Unicode characters.

<p>The default Lisp reader {@link LambdaJ.SExpressionParser} reads bytes from input,
interprets these bytes according to the system default charset which can be configured using the Java system property 'file.encoding'
and subsequently transforms bytes to strings.

<p>The default Lisp writer {@link LambdaJ.SExpressionWriter} sends Unicode strings to Java's System.out which in turn writes bytes to the console.
System.out does this translation according to the character set specified by the Java system property 'sun.stdout.encoding'.


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
<p>Murmel and JMurmel are Copyright (C) 2020 Robert Mayer. All rights reserved.

<p>This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT.

*/
package com.robertmayer.lambdaj;
