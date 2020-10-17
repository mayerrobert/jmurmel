package com.robertmayer.lambdaj.custom;

import java.io.StringReader;
import java.util.Locale;

import static junit.framework.Assert.*;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;
import com.robertmayer.lambdaj.LambdaJ.ConsCell;
import com.robertmayer.lambdaj.LambdaJ.ObjectReader;
import com.robertmayer.lambdaj.LambdaJ.ObjectWriter;
import com.robertmayer.lambdaj.LambdaJ.Parser;
import com.robertmayer.lambdaj.LambdaJ.Primitive;

public class EmbeddedTest {
    private static Locale prev;

    @BeforeClass
    public static void setup() {
        prev = Locale.getDefault();
        Locale.setDefault(Locale.US);
    }

    @AfterClass
    public static void teardown() {
        Locale.setDefault(prev);
    }

    @Test
    public void testMinimal() {
        Object result = new LambdaJ()
                .interpretExpression(new StringReader("(cons 'Hello,\\ World! nil)")::read, (s) -> { return; });
        assertEquals("(Hello, World!)", result.toString());
    }

    @Test
    public void testCons() {
        final LambdaJ interpreter = new LambdaJ();
        final StringBuffer out = new StringBuffer();
        final Object result = interpreter.interpretExpression(new StringReader("(cons 'a 'b)")::read, out::append);

        assertEquals("(a . b)", result.toString());
        assertEquals(0, out.length());

        assertTrue(result instanceof LambdaJ.ConsCell);
        LambdaJ.ConsCell list = (LambdaJ.ConsCell)result;

        String s = "";
        for (Object car: list) { // the iterator will return subsequent car and - if nonnull - the cdr of the last cons cell
            s += car.toString();
        }
        assertEquals("ab", s);
    }

    @Test
    public void testString() {
        final LambdaJ interpreter = new LambdaJ();
        final StringBuffer out = new StringBuffer();
        final Object result = interpreter.interpretExpression(new StringReader("(string-format \"%g\" 1)")::read, out::append);

        assertEquals("1.00000", result.toString()); // will be formatted according to default Locale
        assertEquals(0, out.length());

        assertTrue(result instanceof LambdaJ.LambdaJString);
    }

    @Test
    public void testCustomEnv() {
        // "create a Lisp"
        final LambdaJ interpreter = new LambdaJ();
        interpreter.trace = LambdaJ.TRC_PRIM; // turn on logging of eval and primitive invocations

        // our Lisp "program"
        StringReader program = new StringReader("(writeln *answer*)(greet \"Robert\")");
        Parser parser = interpreter.new SExpressionParser(program::read);

        // empty "file" for stdin, simply return EOF
        ObjectReader inReader = () -> -1;

        // collect stdout of the Lisp program in a StringBuffer
        StringBuffer outBuffer = new StringBuffer();
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
        Object result = interpreter.interpretExpressions(parser, inReader, outWriter, (s, in, out) -> makeEnvironment(s, in, out));

        // check results
        assertNull(result);
        assertEquals("42.0\nHello, Robert!\n", outBuffer.toString());
    }

    // this will be invoked as first thing in interpretExpressions()
    // create a list containing our custom primitive, i.e. a list containing (symbol function) or (symbol value) pairs
    //
    // here a list will be created that contains ((greet . <Java code for greet>) (*answer* . 42.0))
    private static LambdaJ.ConsCell makeEnvironment(LambdaJ.SymbolTable symtab, ObjectReader in, ObjectWriter out) {
        return cons(cons(symtab.intern("greet"),    (Primitive)(a -> greet(a, in, out))),
               cons(cons(symtab.intern("*answer*"), 42.0),
               null));
    }

    private static Object greet(ConsCell a, ObjectReader in, ObjectWriter out) {
        if (a == null) throw new RuntimeException("expected 1 parameter, got none, so there!");
        final String msg = "Hello, " + car(a) + '!';
        out.printObj(msg);
        out.printEol();
        return null;
    };


    private static ConsCell cons(Object car, Object cdr) {
        return new ConsCell(car, cdr);
    }

    private static Object car(Object l) {
        return ((ConsCell)l).car;
    }
}
