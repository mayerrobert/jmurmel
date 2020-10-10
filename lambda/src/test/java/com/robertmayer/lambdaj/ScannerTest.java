package com.robertmayer.lambdaj;

import org.junit.Test;

public class ScannerTest {
    @Test
    public void testEscapedParens() {
        LambdaJTest.runTest("cons", "'(\\( b)", "(( b)", null); // todo sollte (|(| b) sein
    }

    @Test
    public void testQuoteList() {
        LambdaJTest.runTest("cons", "'(a b c)", "(a b c)", null); // ok
    }

    @Test
    public void testQuoteSymbol() {
        LambdaJTest.runTest("cons", "'('a b c)", "((quote a) b c)", null); // ok
    }

    @Test
    public void testEscapedQuoteSymbol() {
        LambdaJTest.runTest("cons", "'(\\'a b c)", "('a b c)", null); // todo sollte (|'a| b c) sein
    }

    @Test
    public void testPair() {
        LambdaJTest.runTest("cons", "'(a . b)", "(a . b)", null); // ok
    }

    @Test
    public void testTwoElemList() {
        LambdaJTest.runTest("cons", "'(a b)", "(a b)", null); // ok
    }

    @Test
    public void testEscapedDot() {
        LambdaJTest.runTest("cons", "'(a \\. b)", "(a . b)", null); // ok, aber hier wird nicht gecheckt ob cons oder list
    }
}
