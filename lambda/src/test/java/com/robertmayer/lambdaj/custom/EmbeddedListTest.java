package com.robertmayer.lambdaj.custom;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;

import java.io.StringReader;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;

public class EmbeddedListTest {

    @Test
    public void testList() {
        final LambdaJ interpreter = new LambdaJ();
        final StringBuffer out = new StringBuffer();
        final Object result = interpreter.interpretExpression(new StringReader("(cons 'a (cons 'b nil))")::read, out::append);

        assertEquals("(a b)", result.toString());
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
