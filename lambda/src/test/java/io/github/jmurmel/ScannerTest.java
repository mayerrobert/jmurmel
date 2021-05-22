package io.github.jmurmel;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.regex.Pattern;

import org.junit.Test;

public class ScannerTest {

    @Test
    public void testDouble() {
        LambdaJTest.runTest("double", "42.0", "42.0", null);
    }

    @Test
    public void testScientificDouble() {
        LambdaJTest.runTest("double", "1.0e3", "1000.0", null);
    }

    @Test
    public void testScientificNoDot() {
        LambdaJTest.runTest("double", "1e3", "1000.0", null);
    }

    @Test
    public void testEmptySymbol() {
        LambdaJTest.runTest("empty", "(quote ||)", "||", null);
    }

    @Test
    public void testEmptySymbol2() {
        LambdaJTest.runTest("empty", "'||", "||", null);
    }

    @Test
    public void testPipe() {
        LambdaJTest.runTest("pipe", "'\\|", "\\|", null);
    }

    @Test
    public void testOpenParen() {
        LambdaJTest.runTest("paren", "(quote \\()", "|(|", null);
    }

    @Test
    public void testOpenParen2() {
        LambdaJTest.runTest("paren", "'|(|", "|(|", null);
    }



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
        LambdaJTest.runTest("cons", "'(a \\. b)", "(a |.| b)", null);
    }

    @Test
    public void testSymbolDot() {
        LambdaJTest.runTest("symboldot", "'a.", "a.", null);
    }

    @Test
    public void testSymbolDot2() {
        LambdaJTest.runTest("symboldot2", "'(a.b)", "(a.b)", null);
    }

    @Test
    public void testSymbolDot3() {
        LambdaJTest.runTest("symboldot3", "'(.b)", "(.b)", null);
    }

    @Test
    public void testWeird() {
        LambdaJTest.runTest("weird", "((lambda([])((lambda(x |()| |(| |)|)(+ x |(| 1 |)| |()| |(| |(| |(|)) [] [] [] [])) 0)", "1.0", null);
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
