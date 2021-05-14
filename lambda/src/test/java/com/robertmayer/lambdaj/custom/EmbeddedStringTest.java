package com.robertmayer.lambdaj.custom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.StringReader;
import java.util.Locale;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;

public class EmbeddedStringTest {
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
    public void testString() {
        final LambdaJ interpreter = new LambdaJ();
        final StringBuilder out = new StringBuilder();
        final Object result = interpreter.interpretExpression(new StringReader("(format nil \"%g\" 1.)")::read, out::append);

        assertEquals("1.00000", result.toString()); // will be formatted according to default Locale
        assertEquals(0, out.length());

        assertTrue(result instanceof String);
    }
}
