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
        if (LambdaJ.vectorp(result)) return LambdaJ.printSEx(result, false);
        if (LambdaJ.hashtablep(result)) return LambdaJ.printSEx(result, false);
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
    public void testVector() {
        runTest("vector", "#(1 2 3)", "#(1 2 3)", null);
        runTest("vector", "#5(1 2 3)", "#(1 2 3 3 3)", null);
        runTest("vector", "#5(1 2 nil)", "#(1 2 nil nil nil)", null);
        runTest("vector", "#0()", "#()", null);
        LambdaJTest.runErrorTest("vector", "#0(1 2 3)", "vector is longer");
        LambdaJTest.runErrorTest("vector", "#1()", "vector of length 1 cannot be initialized from ()");

        runTest("char vector", "#(#\\a #\\b #\\c)", "#(a b c)", null);
        runTest("char vector", "#3(#\\a #\\b #\\c)", "#(a b c)", null);

        runTest("bitvector", "#*0101", "#*0101", null);
        runTest("bitvector", "#10*0101", "#*0101111111", null);
        runTest("bitvector", "#10*010", "#*0100000000", null);
        runTest("bitvector", "#0*", "#*", null);
        LambdaJTest.runErrorTest("bitvector", "#0*01", "too many bits");
        LambdaJTest.runErrorTest("bitvector", "#1*", "#1* requires at least 1 bit of input");
        LambdaJTest.runErrorTest("bitvector", "#*012", "not a valid value for bitvector: 2");
    }

    @Test
    public void testHashtable() {
        runTest("hash", " #H(compare-eql 1.0 11.0 2.0 22.0 3.0 33.0)", "#H(compare-eql 1.0 11.0 2.0 22.0 3.0 33.0)", null);
        LambdaJTest.runErrorTest("hash", " #H (compare-eql 1.0 11.0 2.0 22.0 3.0 33.0)", "expected '(' after '#H'");
    }

    @Test
    public void testFeature() {
        runTest("feature nil",   "#+nil 1 2", "2", null);
        runTest("feature nil",   "#-nil 1 2", "1", null);
        runTest("feature (and)", "#+(and) 1 2", "1", null);
        runTest("feature (or)",  "#+(or) 1 2", "2", null);

        LambdaJTest.runTest("+feature", "#+murmel 1.0 2.0", "1.0", null);
        LambdaJTest.runTest("+feature", "#+xyxxy  1.0 2.0", "2.0", null);

        LambdaJTest.runTest("-feature", "#-murmel 1.0 2.0", "2.0", null);
        LambdaJTest.runTest("-feature", "#-xyxxy  1.0 2.0", "1.0", null);

        LambdaJTest.runTest("and features", "#+(and murmel jvm)   1.0 2.0", "1.0", null);
        LambdaJTest.runTest("and features", "#+(and murmel xyxxy) 1.0 2.0", "2.0", null);
        LambdaJTest.runTest("or features",  "#+(or murmel xyxxy)  1.0 2.0", "1.0", null);
        LambdaJTest.runTest("not features",  "#+(not murmel)  1.0 2.0", "2.0", null);
        LambdaJTest.runTest("not features",  "#+(not xyxxy)   1.0 2.0", "1.0", null);

        LambdaJTest.runErrorTest("not features",  "#+(not)   1.0 2.0", "feature expression not: not enough subexpressions");
        LambdaJTest.runErrorTest("not features",  "#+(not murmel jvm)   1.0 2.0", "feature expression not: too many subexpressions");

        LambdaJTest.runErrorTest("not features",  "#+1   1.0 2.0", "unsupported feature expression");
        LambdaJTest.runErrorTest("not features",  "#+(bla)   1.0 2.0", "unsupported feature expression");
    }

    @Test
    public void testSymbol() {
        LambdaJTest.runTest("symbol starting with dbl quote", "'\\\"123", "|\"123|", null);
        LambdaJTest.runTest("symbol with embedded backslash", "'12\\34", "|1234|", null);
        LambdaJTest.runTest("symbol with embedded bar", "'ab\\|cd", "ab\\|cd", null);
        LambdaJTest.runTest("|-symbol with embedded bar", "'|ab\\|cd|", "ab\\|cd", null);

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
        runTest("comment", "; bla\nasdf", "asdf", null);
        runTest("comment", "(a ; bla\nasdf)", "(a asdf)", null);
        LambdaJTest.runErrorTest("comment", "(a ; bla", "cannot read list. missing ')'");

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
        runTest("trailing stuff", "\"one\rtwo\nbla\r\nbla2\" 1 2 3", "\"one\ntwo\nbla\nbla2\"", null);
        runTest("escaped quote", "\"one\rtwo\nbla \\\" bla\r\nbla2\" 1 2 3", "\"one\ntwo\nbla \" bla\nbla2\"", null);
        runTest("embedded semi", "\"one\rtwo\nbla; \\\" bla\r\nbla2\" 1 2 3", "\"one\ntwo\nbla; \" bla\nbla2\"", null);

        LambdaJTest.runErrorTest("unterminated string with semi", "\"blabla; asdf", "string literal is missing closing");

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
    public void testFunction() {
        runTest("sharp apo", "#'func", "func", null);
        runTest("sharp apo", "#'|123|", "123", null);
    }

    @Test
    public void testNumbers() {
        runTest("bin", "#b0101", "5", null);
        runTest("bin", " #b 0101 ", "5", null);
        runTest("oct", "#o42", "34", null);
        runTest("oct", " #o 42 ", "34", null);
        runTest("hex", "#xdead", "57005", null);
        runTest("hex", " #x dead ", "57005", null);
    }

    @Test
    public void testUninterned() {
        runTest("uninterned", "#:qwer", "qwer", null);
        runTest("uninterned", "#:|123|", "123", null);
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
