package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ConsTest {

    private static LambdaJ.ListBuilder listBuilder() { return new LambdaJ.ListBuilder(); }

    // '("a" "b" "c")
    @Test
    public void build() {
        Object l = listBuilder().append("a").append("b").append("c").first();
        assertEquals("(\"a\" \"b\" \"c\")", TestUtils.sexp(l));
    }

    // '()
    @Test
    public void buildNothing() {
        Object l = listBuilder().first();
        assertEquals(null, l);
        assertEquals("nil", TestUtils.sexp(l));
    }

    // '(nil)
    @Test
    public void buildNil() {
        Object l = listBuilder().append(null).first();
        assertEquals("(nil)", TestUtils.sexp(l));
    }

    // '(nil nil)
    @Test
    public void buildNilNil() {
        Object l = listBuilder().append(null).append(null).first();
        assertEquals("(nil nil)", TestUtils.sexp(l));
    }

    @Test
    public void buildNilListNil() {
        Object l = listBuilder().append(null).append(listBuilder().append(null).first()).first();
        assertEquals("(nil (nil))", TestUtils.sexp(l));
    }

    // '("a" "b" . "c")
    @Test
    public void buildDottedList() {
        Object l = listBuilder().append("a").append("b").appendLast("c").first();
        assertEquals("(\"a\" \"b\" . \"c\")", TestUtils.sexp(l));
    }

    // '(nil nil . nil)
    @Test
    public void buildDottedListNil() {
        Object l = listBuilder().append(null).append(null).appendLast(null).first();
        assertEquals("(nil nil)", TestUtils.sexp(l));
    }

    // '(nil . nil)
    @Test
    public void buildNilDotNil() {
        Object l = listBuilder().append(null).appendLast(null).first();
        assertEquals("(nil)", TestUtils.sexp(l));
    }



    @Test
    public void consNilNil() throws Exception {
        LambdaJTest.runTest("macro1.lisp", "(cons nil nil)", "(nil)", null);
    }
}
