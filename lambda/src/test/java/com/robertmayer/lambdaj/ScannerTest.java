package com.robertmayer.lambdaj;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.regex.Pattern;

import org.junit.Test;

public class ScannerTest {
    @Test
    public void testEmptyList() {
        LambdaJTest.runTest("cons", "'()", "nil", null);
    }
    @Test
    public void testEscapedParens() {
        LambdaJTest.runTest("cons", "'(\\( b)", "(|(| b)", null);
    }

    @Test
    public void testQuoteList() {
        LambdaJTest.runTest("cons", "'(a b c)", "(a b c)", null);
    }

    @Test
    public void testQuoteSymbol() {
        LambdaJTest.runTest("cons", "'('a b c)", "((quote a) b c)", null);
    }

    @Test
    public void testEscapedQuoteSymbol() {
        LambdaJTest.runTest("cons", "'(\\'a b c)", "(|'a| b c)", null);
    }

    @Test
    public void testPair() {
        LambdaJTest.runTest("cons", "'(a . b)", "(a . b)", null);
    }

    @Test
    public void testDottedList() {
        LambdaJTest.runTest("dotted list", "'(a . (b . (c . d)))", "(a b c . d)", null);
    }

    @Test
    public void testDottedListShort() {
        LambdaJTest.runTest("dotted list short", "'(a b c . d)", "(a b c . d)", null);
    }

    @Test
    public void testListDotListShort() {
        LambdaJTest.runTest("dotted list short", "'((a b) . (c d))", "((a b) c d)", null);
    }


    @Test
    public void testTwoElemList() {
        LambdaJTest.runTest("cons", "'(a b)", "(a b)", null);
    }

    @Test
    public void testEscapedDot() {
        LambdaJTest.runTest("cons", "'(a \\. b)", "(a . b)", null); // ok, aber hier wird nicht gecheckt ob cons oder list
    }

    @Test
    public void testExtraParens() {
        LambdaJTest.runErrorTest("openparen", "\n\n  )  \n\n", "line 3:4: unexpected ')'");
    }

    @Test
    public void testMissingParens() {
        LambdaJTest.runErrorTest("openparen", "(1\n"
                + "(2\n"
                + "\n"
                + "(3\n"
                + "\n"
                + "  )", "line 6:3: cannot read list. missing ')'?\nerror occurred in S-expression line 2:1..6:3");
    }


    @Test
    public void testListDotListShort2() {
        LambdaJTest.runErrorTest("dotted list short", "'(a b . c d)", "line 1:12: illegal end of dotted list: (c d)");
    }


    private static final Pattern longPattern = Pattern.compile("[-+]?([0-9]|([1-9][0-9]*))");

    private static boolean isLong(String s) {
        if (s == null || s.isEmpty()) return false;
        return longPattern.matcher(s).matches();
    }

    private static final Pattern doublePattern = Pattern.compile(
            "[-+]?"                              // optional-sign
          + "((([0-9]+\\.)[0-9]*)|\\.[0-9]+)"    // either: one-or-more-digits '.' zero-or-more-digits  or: '.' one-or-more-digits
          + "([eE][-+]?[0-9]+)?");               // optional: e-or-E optional-sign one-or-more-digits

    private static boolean isDouble(String s) {
        if (s == null || s.isEmpty()) return false;
        return doublePattern.matcher(s).matches();
    }

    @Test
    public void testIsLong() {
        assertFalse(isLong(null));
        assertFalse(isLong(""));
        assertFalse(isLong("1."));
        assertFalse(isLong("1+"));

        assertTrue(isLong("1"));
    }

    @Test
    public void testIsDouble() {
        assertFalse(isDouble(null));
        assertFalse(isDouble(""));
        assertFalse(isDouble("."));
        assertFalse(isDouble("1"));
        assertFalse(isDouble("1e"));
        assertFalse(isDouble("1+"));

        assertTrue(isDouble("1."));
        assertTrue(isDouble("1.1"));
        assertTrue(isDouble(".123"));
    }
}
