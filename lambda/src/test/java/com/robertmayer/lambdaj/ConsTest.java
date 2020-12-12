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



    static ListBuilder listBuilder() { return new ListBuilder(); }
}

class ListBuilder {
    Object first = null;
    Object last = null;

    ListBuilder append(Object elem) {
        ConsCell newCell = ConsCell.cons(elem, null);
        if (first == null) {
            last = first = newCell;
        }
        else if (last instanceof ConsCell) {
            ((ConsCell) last).rplacd(newCell);
            last = newCell;
        }
        else throw new LambdaJ.LambdaJError("can't append list element to dotted list");
        return this;
    }

    ListBuilder appendLast(Object lastElem) {
        if (first == null) {
            last = first = lastElem;
        }
        else if (last instanceof ConsCell) {
            ((ConsCell) last).rplacd(lastElem);
            last = lastElem;
        }
        else throw new LambdaJ.LambdaJError("can't append last list element to dotted list");
        return this;
    }

    Object first() { return first; }
}
