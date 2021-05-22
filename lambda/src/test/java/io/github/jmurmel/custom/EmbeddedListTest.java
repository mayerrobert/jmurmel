package io.github.jmurmel.custom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.StringReader;

import io.github.jmurmel.LambdaJ;
import org.junit.Test;

public class EmbeddedListTest {

    @Test
    public void testList() {
        final LambdaJ interpreter = new LambdaJ();
        final StringBuilder out = new StringBuilder();
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
