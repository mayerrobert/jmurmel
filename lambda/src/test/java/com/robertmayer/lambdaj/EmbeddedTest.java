package com.robertmayer.lambdaj;

import java.io.StringReader;

import static junit.framework.Assert.*;

import org.junit.Test;

public class EmbeddedTest {

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

        assertEquals("1,00000", result.toString());
        assertEquals(0, out.length());

        assertTrue(result instanceof String);
    }
}
