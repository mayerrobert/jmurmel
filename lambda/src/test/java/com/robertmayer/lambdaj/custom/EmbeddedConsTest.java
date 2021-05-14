package com.robertmayer.lambdaj.custom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.StringReader;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;

public class EmbeddedConsTest {

    @Test
    public void testCons() {
        final LambdaJ interpreter = new LambdaJ();
        final StringBuilder out = new StringBuilder();
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
}
