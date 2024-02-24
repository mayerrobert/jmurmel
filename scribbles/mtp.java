/*

Using JMurmel in a template processor (preview feature in Java21)
to make Java callable Objects from inline-Murmel-code.

Usage: make sure jmurmel.jar and mlib.lisp are in the current directory and then:

  C:> java -cp jmurmel.jar --enable-preview --source 21 mtp.java

*/

import io.github.jmurmel.LambdaJ;
import static io.github.jmurmel.LambdaJ.ConsCell.*;

void main() throws Exception {

    LambdaJ.Primitive callback = args -> "hi from Java";

    var murmelStdout = new StringBuffer();

    // create a template processor that will use a Murmel interpreter the turn Murmel code into Java Objects
    var MTP = StringTemplate.Processor.of((StringTemplate st) -> {
        var murmel = new LambdaJ();
        var additionalEnvironment = list(cons(murmel.getSymbolTable().intern("callback"), callback));
        murmel.init((LambdaJ.ReadSupplier)null, null, additionalEnvironment);
        var murmelProgram = murmel.formsToInterpretedProgram(st.interpolate(), null, murmelStdout::append);
        murmelProgram.body();
        return murmelProgram;
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
