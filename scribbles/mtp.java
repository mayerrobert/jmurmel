/*

Using Murmel as a template processor (preview feature in Java21)
to make Java callable Objects from inline-Murmel-code.

Usage: make sure jmurmel.jar and mlib.lisp are in the current directory and then:

  C:> java -cp jmurmel.jar --enable-preview --source 21 mtp.java


*/

import io.github.jmurmel.LambdaJ;


class TemplateProcessorDemo {

    void main() {

        // create a Murmel interpreter object
        // and a template processor that will turn Murmel code into Java Objects
        var murmel = new LambdaJ();
        var MTP = StringTemplate.Processor.of((StringTemplate st) -> { return murmel.formsToInterpretedProgram(st.interpolate(), null, null); } );

        LambdaJ.MurmelProgram hello = MTP."""
        
        ;;;; Hello, World! example
        ;;;
        ;;; Demo that shows how Murmel code can be written directly in a Java source file.
        
        (require "mlib")
        
        (princ "Hello, ")
        (princ "World!")
        
        42 ; the answer, obviously
        
        """;

        // create and assign a buffer for Murmel's stdout
        var sb = new StringBuffer();
        hello.setReaderPrinter(null, LambdaJ.makeWriter(sb::append));

        // run the Murmelprogram and display it's result and stdout
        var answer = hello.body();
        System.out.format("Murmel answers %s%n", answer);

        System.out.format("Murmel's stdout was: %s%n", sb);
    }
}
