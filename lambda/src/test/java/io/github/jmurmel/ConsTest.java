package io.github.jmurmel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

public class ConsTest {

    private static LambdaJ.ListBuilder listBuilder() { return new LambdaJ.ListBuilder(); }

    // '("a" "b" "c")
    @Test
    public void build() {
        final Object l = listBuilder().append("a").append("b").append("c").first();
        assertEquals("(\"a\" \"b\" \"c\")", TestUtils.sexp(l));
    }

    // '()
    @Test
    public void buildNothing() {
        final Object l = listBuilder().first();
        assertNull(l);
        assertEquals("nil", TestUtils.sexp(l));
    }

    // '(nil)
    @Test
    public void buildNil() {
        final Object l = listBuilder().append(null).first();
        assertEquals("(nil)", TestUtils.sexp(l));
    }

    // '(nil nil)
    @Test
    public void buildNilNil() {
        final Object l = listBuilder().append(null).append(null).first();
        assertEquals("(nil nil)", TestUtils.sexp(l));
    }

    @Test
    public void buildNilListNil() {
        final Object l = listBuilder().append(null).append(listBuilder().append(null).first()).first();
        assertEquals("(nil (nil))", TestUtils.sexp(l));
    }

    // '("a" "b" . "c")
    @Test
    public void buildDottedList() {
        final Object l = listBuilder().append("a").append("b").appendLast("c").first();
        assertEquals("(\"a\" \"b\" . \"c\")", TestUtils.sexp(l));
    }

    // '(nil nil . nil)
    @Test
    public void buildDottedListNil() {
        final Object l = listBuilder().append(null).append(null).appendLast(null).first();
        assertEquals("(nil nil)", TestUtils.sexp(l));
    }

    // '(nil . nil)
    @Test
    public void buildNilDotNil() {
        final Object l = listBuilder().append(null).appendLast(null).first();
        assertEquals("(nil)", TestUtils.sexp(l));
    }

    @Test
    public void testAppendConsSlice() {
        final Object l = listBuilder().append(1).append(2).appendLast(LambdaJ.arraySlice(3, 4, 5)).first();
        assertEquals("(1 2 3 4 5)", TestUtils.sexp(l));
    }

    @Test
    public void testConsSlice() {
        final Object l = listBuilder().append(1).append(2).append(LambdaJ.arraySlice(3, 4, 5)).first();
        assertEquals("(1 2 (3 4 5))", TestUtils.sexp(l));
    }

    @Test
    public void testSliceCons() {
        final Object l = LambdaJ.arraySlice(1, 2, 3, listBuilder().append(4).append(5).first());
        assertEquals("(1 2 3 (4 5))", TestUtils.sexp(l));
    }

    @Test
    public void testAppendConsSliceIterator() {
        final LambdaJ.ConsCell l = (LambdaJ.ConsCell)listBuilder().append(1).append(2).appendLast(LambdaJ.arraySlice(3, 4, 5)).first();
        final StringBuilder sb = new StringBuilder();
        l.forEach(sb::append);
        assertEquals("12345", sb.toString());
    }



    @Test
    public void testElt() {
        final LambdaJ.ConsCell lst = LambdaJ.arraySlice(11, 22, 33);
        assertEquals(lst.elt(2), 33);

        final LambdaJ.ConsCell lst2 = (LambdaJ.ConsCell)LambdaJ.ConsCell.listStar(1, lst);
        assertEquals(lst2.elt(2), 22);
    }



    @Test
    public void consNilNil() throws Exception {
        LambdaJTest.runTest("macro1.lisp", "(cons nil nil)", "(nil)", null);
    }
}
