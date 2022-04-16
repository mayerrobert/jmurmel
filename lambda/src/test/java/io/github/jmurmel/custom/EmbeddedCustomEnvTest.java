package io.github.jmurmel.custom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.Reader;
import java.io.StringReader;

import org.junit.Test;

import io.github.jmurmel.LambdaJ;
import io.github.jmurmel.LambdaJ.ConsCell;
import io.github.jmurmel.LambdaJ.ObjectReader;
import io.github.jmurmel.LambdaJ.ObjectWriter;
import io.github.jmurmel.LambdaJ.Primitive;

public class EmbeddedCustomEnvTest {

    @Test
    public void testCustomEnv() {
        // "create a Lisp"
        final LambdaJ interpreter = new LambdaJ(LambdaJ.Features.HAVE_ALL_LEXC.bits(), LambdaJ.TraceLevel.TRC_FUNC, null, null, null); // turn on logging of eval and function invocations

        // our Lisp "program"
        final Reader program = new StringReader("(writeln *answer*)(greet \"Robert\")");
        final ObjectReader parser = LambdaJ.makeReader(program::read, interpreter.getSymbolTable(), null);

        // empty "file" for stdin, simply return EOF
        final ObjectReader inReader = eof -> eof;

        // collect stdout of the Lisp program in a StringBuilder
        final StringBuilder outBuffer = new StringBuilder();
        final ObjectWriter outWriter = new ObjectWriter() {
            @Override public void printObj(Object o, boolean printEscape) {
                outBuffer.append(o); // if a cons cell was to be printed this will invoke it's toString() method
                                     // which will print it's contents as an S-expression
            }
            @Override public void printEol()         { outBuffer.append("\n"); }
        };

        // invoke the interpreter with
        // * an S-Expression parser that reads from the Stringreader "program"
        // * an "empty" ObjectReader that returns EOF
        // * an ObjectWriter that writes to the StringBuilder "outBuffer"
        // * a Java-lambda that creates our custom environment
        final Object result = interpreter.interpretExpressions(parser, inReader, outWriter, s -> makeEnvironment(s, interpreter));

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
    }


    private static ConsCell cons(Object car, Object cdr) {
        return ConsCell.cons(car, cdr);
    }

    private static Object car(Object l) {
        return ((ConsCell)l).car();
    }
}
