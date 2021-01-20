package com.robertmayer.lambdaj;

import org.junit.Test;

public class AppendTest {

    @Test
    public void noArg() {
        LambdaJTest.runTest("noArg", "(append)", "nil", null);
    }

    @Test
    public void digit() {
        LambdaJTest.runTest("digit", "(append 3)", "3.0", null);
    }

    @Test
    public void lists() {
        LambdaJTest.runTest("lists", "(append '(a) '(b) '(c))", "(a b c)", null);
    }

    @Test
    public void listSymbol() {
        LambdaJTest.runTest("listSymbol", "(append '(a b) 'c)", "(a b . c)", null);
    }

    // CL gibt den fehler "the value b ist not of type list"
    @Test
    public void dottedListSymbol() {
        LambdaJTest.runTest("dottedListSymbol", "(append '(a . b) 'c)", "(a b . c)", null);
    }
}
