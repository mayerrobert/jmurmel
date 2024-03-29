/*

Using JMurmel in a template processor (preview feature in Java21)
to make Java callable Objects from inline-Murmel-code.

Usage: make sure jmurmel.jar and mlib.lisp are in the current directory and then:

  C:> java -cp jmurmel.jar --enable-preview --source 21 mtp.java

See mtp.java for an alternative version that uses the interpreter.

*/

import java.io.*;
import java.nio.file.*;

import io.github.jmurmel.LambdaJ;
import static io.github.jmurmel.LambdaJ.ConsCell.*;

void main() throws Exception {

    // could use both Primitive or CompilerPrimitive
    //LambdaJ.Primitive callback = args -> "hi from Java";
    LambdaJ.MurmelJavaProgram.CompilerPrimitive callback = args -> "hi from Java";

    var murmelStdout = new StringBuffer();

    // create a template processor that will use a Murmel compiler to turn Murmel code into Java Objects
    var MTP = StringTemplate.Processor.of((StringTemplate st) -> {
        try {
            Path tmpDir = Files.createTempDirectory("jmurmeltest");
            tmpDir.toFile().deleteOnExit();
            var murmelCompiler = new LambdaJ.MurmelJavaCompiler(null, null, tmpDir);

            var additionalEnvironment = list(cons(murmelCompiler.intern("callback"), callback));

            murmelCompiler.setCustomEnvironment(additionalEnvironment);
            LambdaJ.ReadSupplier reader = new StringReader(st.interpolate())::read;
            Class<LambdaJ.MurmelProgram> murmelProgramClass = murmelCompiler.formsToJavaClass("demo", reader, null);

            var murmelProgram = murmelProgramClass.getDeclaredConstructor().newInstance();
            murmelProgram.setReaderPrinter(null, murmelStdout::append);

            murmelProgram.body();
            return murmelProgram;
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    } );

    LambdaJ.MurmelProgram hello = MTP."""
        
    ;;;; Hello, World! example
    ;;;
    ;;; Demo that shows how Murmel code can be written directly in a Java source file.
    
    (require "mlib")
    
    (defun greet (name)
      (princ "Hello, ")
      (princ name)
      (princ "!")
      (terpri)
      
      (princ "Java says: ")
      (princ (callback))
      
      ;; the answer, obviously
      42)
    
    """;

    // get and run the Murmel function "greet" and display it's result and stdout
    LambdaJ.MurmelFunction greet = hello.getFunction("greet");
    var answer = greet.apply("your name");
    System.out.format("Murmel answers %s%n", answer);

    System.out.format("Murmel's stdout was: %s%n", murmelStdout);
}
