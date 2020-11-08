package com.robertmayer.lambdaj.custom;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNull;

import java.io.StringReader;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;
import com.robertmayer.lambdaj.LambdaJ.ConsCell;
import com.robertmayer.lambdaj.LambdaJ.ObjectReader;
import com.robertmayer.lambdaj.LambdaJ.ObjectWriter;
import com.robertmayer.lambdaj.LambdaJ.Parser;
import com.robertmayer.lambdaj.LambdaJ.Primitive;
import com.robertmayer.lambdaj.LambdaJ.TraceLevel;

public class EmbeddedCustomEnvTest {

    @Test
    public void testCustomEnv() {
        // "create a Lisp"
        final LambdaJ interpreter = new LambdaJ(LambdaJ.HAVE_ALL_LEXC, LambdaJ.TraceLevel.TRC_FUNC, null); // turn on logging of eval and function invocations

        // our Lisp "program"
        StringReader program = new StringReader("(writeln *answer*)(greet \"Robert\")");
        Parser parser = interpreter.new SExpressionParser(program::read);

        // empty "file" for stdin, simply return EOF
        ObjectReader inReader = () -> -1;

        // collect stdout of the Lisp program in a StringBuilder
        StringBuilder outBuffer = new StringBuilder();
        ObjectWriter outWriter = new ObjectWriter() {
            @Override public void printObj(Object o) {
                outBuffer.append(o); // if a cons cell was to be printed this will invoke it's toString() method
                                     // which will print it's contents as an S-expression
            }
            @Override public void printEol()         { outBuffer.append("\n"); }
        };

        // invoke the interpreter with
        // * an S-Expression parser that reads from the Stringreader "program"
        // * an "empty" ObjectReader that returns EOF
        // * an ObjectWriter that writes to the StringBuffer "outBuffer"
        // * a Java-lambda that creates our custom environment
        Object result = interpreter.interpretExpressions(parser, inReader, outWriter, s -> makeEnvironment(s, interpreter));

        // check results
        assertNull(result);
        assertEquals("42.0\nHello, Robert!\n", outBuffer.toString());
    }

    // this will be invoked as first thing in interpretExpressions()
    // create a list containing our custom primitive, i.e. a list containing (symbol function) or (symbol value) pairs
    //
    // here a list will be created that contains ((greet . <Java code for greet>) (*answer* . 42.0))
    private static LambdaJ.ConsCell makeEnvironment(LambdaJ.SymbolTable symtab, LambdaJ intp) {
        return cons(cons(symtab.intern(new LambdaJ.LambdaJSymbol("greet")),    (Primitive)(a -> greet(a, intp))),
               cons(cons(symtab.intern(new LambdaJ.LambdaJSymbol("*answer*")), 42.0),
               null));
    }

    private static Object greet(ConsCell a, LambdaJ intp) {
        if (a == null) throw new RuntimeException("expected 1 parameter, got none, so there!");
        final String msg = "Hello, " + car(a) + '!';
        intp.getLispPrinter().printObj(msg);
        intp.getLispPrinter().printEol();
        return null;
    };


    private static ConsCell cons(Object car, Object cdr) {
        return new ConsCell(car, cdr);
    }

    private static Object car(Object l) {
        return ((ConsCell)l).car;
    }
}
