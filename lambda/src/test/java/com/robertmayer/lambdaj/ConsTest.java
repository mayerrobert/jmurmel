package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import com.robertmayer.lambdaj.LambdaJ.ConsCell;

public class ConsTest {

    // '("a" "b" "c")
    @Test
    public void cons() {
        Object l = listBuilder().append("a").append("b").append("c").first();
        assertEquals("(\"a\" \"b\" \"c\")", TestUtils.sexp(l));
    }

    // '()
    @Test
    public void consNothing() {
        Object l = listBuilder().first();
        assertEquals(null, l);
        assertEquals("nil", TestUtils.sexp(l));
    }

    // '(nil)
    @Test
    public void consNil() {
        Object l = listBuilder().append(null).first();
        assertEquals("(nil)", TestUtils.sexp(l));
    }

    // '(nil nil)
    @Test
    public void consNilNil() {
        Object l = listBuilder().append(null).append(null).first();
        assertEquals("(nil nil)", TestUtils.sexp(l));
    }

    @Test
    public void consNilListNil() {
        Object l = listBuilder().append(null).append(listBuilder().append(null).first()).first();
        assertEquals("(nil (nil))", TestUtils.sexp(l));
    }

    // '("a" "b" . "c")
    @Test
    public void consDottedList() {
        Object l = listBuilder().append("a").append("b").appendLast("c").first();
        assertEquals("(\"a\" \"b\" . \"c\")", TestUtils.sexp(l));
    }

    // '(nil nil . nil)
    @Test
    public void consDottedListNil() {
        Object l = listBuilder().append(null).append(null).appendLast(null).first();
        assertEquals("(nil nil)", TestUtils.sexp(l));
    }



    static LambdaJ.ListBuilder listBuilder() { return new LambdaJ.ListBuilder(); }
}
