package io.github.jmurmel;

import org.junit.Test;
import org.testng.Assert;

import java.io.StringReader;

public class ScannerTest {

    @Test
    public void testIsLong() {
        //Assert.assertFalse(LambdaJ.isLong(""));
        Assert.assertFalse(LambdaJ.isLong(" 1"));
        Assert.assertFalse(LambdaJ.isLong("1."));
        Assert.assertFalse(LambdaJ.isLong("1.0"));
        Assert.assertFalse(LambdaJ.isLong("+"));

        Assert.assertTrue(LambdaJ.isLong("1"));
    }

    @Test
    public void testIsCLDecimalLong() {
        //Assert.assertFalse(LambdaJ.isCLDecimalLong(""));
        Assert.assertFalse(LambdaJ.isCLDecimalLong("."));
        Assert.assertFalse(LambdaJ.isCLDecimalLong(" 1"));
        Assert.assertFalse(LambdaJ.isCLDecimalLong(" 1."));
        Assert.assertFalse(LambdaJ.isCLDecimalLong("1.0"));
        Assert.assertFalse(LambdaJ.isCLDecimalLong("+"));
        Assert.assertFalse(LambdaJ.isCLDecimalLong("+."));
        Assert.assertFalse(LambdaJ.isCLDecimalLong("1"));

        Assert.assertTrue(LambdaJ.isCLDecimalLong("1."));
    }

    // behaves somewhat similar to LambdaJTest.runTest() but form is only "read" not eval'd, and line ends are Unix style (as are Murmel's internal line ends)
    private static void runTest(String name, String form, String expectedResult, Object ignored) {
        final LambdaJ.ObjectReader reader = LambdaJ.makeReader(new StringReader(form)::read);
        final Object result = reader.readObj("eof");
        Assert.assertEquals(stringify(result), expectedResult, name + "failed: ");
    }

    // fake behaviour of LambdaJTest.runTest() which returns strings in dbl quotes
    // this method does not change line ends, LambdaJTest.runTest() does
    private static String stringify(Object result) {
        if (result == null) return "nil";
        if (result instanceof String) return "\"" + result + "\"";
        return String.valueOf(result);
    }

    @Test
    public void testDouble() {
        runTest("double", "42.0", "42.0", null);
    }

    @Test
    public void testScientificDouble() {
        runTest("double", "1.0e3", "1000.0", null);
    }

    @Test
    public void testScientificNoDot() {
        runTest("double", "1e3", "1000.0", null);
    }

    @Test
    public void testEmptySymbol() {
        LambdaJTest.runTest("empty", "(quote ||)", "||", null);
        LambdaJTest.runTest("empty", "'||", "||", null);
    }

    @Test
    public void testNil() {
        runTest("nil", "nil", "nil", null);
    }

    @Test
    public void testSymbol() {
        LambdaJTest.runTest("symbol starting with dbl quote", "'\\\"123", "|\"123|", null);
        LambdaJTest.runTest("symbol with embedded backslash", "'12\\34", "|1234|", null);

        LambdaJTest.runErrorTest("unclosed |quoted symbol", "'|123", "|-quoted symbol is missing closing |");
    }

    @Test
    public void testCharacter() {
        LambdaJTest.runTest("char as char",   "'#\\a", "#\\a", null);       // quoted character evals to character
        LambdaJTest.runTest("char as symbol", "'\\#\\a", "|#a|", null);     // if the # is escaped then that's really a symbol and must be printed within ||
    }

    @Test
    public void testHash() {
        LambdaJTest.runTest("hash space", "'#\\ ", "#\\ ", null);
        LambdaJTest.runTest("hash Tab", "'#\\Tab", "#\\Tab", null);
        LambdaJTest.runTest("hash Us", "'#\\Us", "#\\Us", null);
        LambdaJTest.runTest("character specified with ascii value", "'#\\228", "#\\228", null);
        LambdaJTest.runTest("character 10", "'#\\10", "#\\Newline", null);

        LambdaJTest.runErrorTest("inv character name", "'#\\bla", "unrecognized character");
        LambdaJTest.runErrorTest("hash invalid symbol", "'#sdf", "no dispatch function defined");
    }

    @Test
    public void testComment() {
        LambdaJTest.runTest("multiline comment",           "#| one\ntwo|\nthree|#\n1.0", "1.0", null);
        LambdaJTest.runErrorTest("open multiline comment", "#| one\ntwo\nthree#\n1.0", "line 1:3: EOF in multiline comment\nerror occurred in line 4:3");
    }

    @Test
    public void testPipe() {
        LambdaJTest.runTest("pipe", "'\\|", "|\\||", null);
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
    public void testStringOpenParen() {
        LambdaJTest.runTest("stringopenparen", "\"(\"", "\"(\"", null);
    }

    @Test
    public void testStringCloseParen() {
        LambdaJTest.runTest("stringcloseparen", "\")\"", "\")\"", null);
    }

    @Test
    public void testString() {
        runTest("cr-lf", "\"one\rtwo\nbla\r\nbla2\"", "\"one\ntwo\nbla\nbla2\"", null);

        LambdaJTest.runErrorTest("open string", "\"blabla", "string literal is missing closing");
        // check if different line ends mess up linenumbers in error messages
        LambdaJTest.runErrorTest("cr-lf", "\r\r \r\n\r\n \n\n(list 'one\r'two\n'three\r\nfour)",
                                 "eval: 'four' is not bound\n" +
                                 "error occurred in line 7:1..10:5: (list (quote one) (quote two) (quote three) four)");
    }


    @Test
    public void testEmptyList() {
        LambdaJTest.runTest("nil", "'()", "nil", null);
    }

    @Test
    public void testEscapedParens() {
        LambdaJTest.runTest("escaped paren", "'(\\( b)", "(|(| b)", null);
    }

    @Test
    public void testQuoteList() {
        LambdaJTest.runTest("list", "'(a b c)", "(a b c)", null);
    }

    @Test
    public void testQuoteSymbol() {
        LambdaJTest.runTest("symbol list", "'('a b c)", "((quote a) b c)", null);
    }

    @Test
    public void testEscapedQuoteSymbol() {
        LambdaJTest.runTest("symbolname with quote", "'(\\'a b c)", "(|'a| b c)", null);
    }

    @Test
    public void testPair() {
        LambdaJTest.runTest("dotted pair", "'(a . b)", "(a . b)", null);
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
        LambdaJTest.runTest("list", "'(a b)", "(a b)", null);
    }

    @Test
    public void testEscapedDot() {
        LambdaJTest.runTest("symbolname is dot", "'(a \\. b)", "(a |.| b)", null);
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
    public void testSymbolDotString() {
        LambdaJTest.runTest("symboldotstring", "'(a \".\" b)", "(a \".\" b)", null);
    }

    @Test
    public void testWeird() {
        // sbcl: (funcall (lambda([])(funcall (lambda(x |()| |(| |)|)(+ x |(| 1 |)| |()| |(| |(| |(|)) [] [] [] [])) 0) ; => 1
        LambdaJTest.runTest("weird", "((lambda([])((lambda(x |()| |(| |)|)(+ x |(| 1 |)| |()| |(| |(| |(|)) [] [] [] [])) 0)", "1.0", null);
    }

    @Test
    public void testExtraParens() {
        LambdaJTest.runErrorTest("openparen", "\n\n  )  \n\n", "unexpected ')'\nerror occurred in line 3:4");
    }

    @Test
    public void testMissingParens() {
        LambdaJTest.runErrorTest("openparen", "(1\n"
                + "(2\n"
                + "\n"
                + "(3\n"
                + "\n"
                + "  )", "cannot read list. missing ')'?\nerror occurred in line 2:1..6:3");
    }


    @Test
    public void testListDotListShort2() {
        LambdaJTest.runErrorTest("dotted list short", "'(a b . c d)", "illegal end of dotted list: (c d)\nerror occurred in line 1:2..1:12");
    }
    
    @Test
    public void testIsDouble2() {
        Assert.assertTrue(LambdaJ.isDouble("+.0"));
        Assert.assertTrue(LambdaJ.isDouble("-.0"));
        Assert.assertTrue(LambdaJ.isDouble(".0"));
        Assert.assertTrue(LambdaJ.isDouble(".0123"));
        Assert.assertTrue(LambdaJ.isDouble(".0123e5"));
        Assert.assertTrue(LambdaJ.isDouble(".0123e556"));
        Assert.assertTrue(LambdaJ.isDouble(".0123e+556"));

        Assert.assertTrue(LambdaJ.isDouble("+1.0"));
        Assert.assertTrue(LambdaJ.isDouble("-1.0"));
        Assert.assertTrue(LambdaJ.isDouble("1.0"));
        Assert.assertTrue(LambdaJ.isDouble("123.0"));
        Assert.assertTrue(LambdaJ.isDouble("1.0123"));
        Assert.assertTrue(LambdaJ.isDouble("123.0123"));
        Assert.assertTrue(LambdaJ.isDouble("1.0123e5"));
        Assert.assertTrue(LambdaJ.isDouble("1.0123e556"));
        Assert.assertTrue(LambdaJ.isDouble("1.0123e+556"));

        Assert.assertFalse(LambdaJ.isDouble("+.0 "));
        Assert.assertFalse(LambdaJ.isDouble("+.0d0"));
    }
}
