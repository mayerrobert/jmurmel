package com.robertmayer.lambdaj;

import org.junit.Test;

import static junit.framework.Assert.*;

import static com.robertmayer.lambdaj.LambdaJ.ConsCell;

public class PrintObjTest {

    @Test
    public void listContainsNil() {
        ConsCell l = ConsCell.cons(null, null);
        assertEquals("(nil)", l.toString());
    }

    @Test
    public void listContainsThreeNils() {
        ConsCell l = ConsCell.cons(null, ConsCell.cons(null, ConsCell.cons(null, null)));
        assertEquals("(nil nil nil)", l.toString());
    }

    @Test
    public void listContainsTwoSymbols() {
        ConsCell l = ConsCell.cons("a", ConsCell.cons("b", null));
        assertEquals("(a b)", l.toString());
    }

    @Test
    public void listContainsThreeSymbols() {
        ConsCell l = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c", null)));
        assertEquals("(a b c)", l.toString());
    }

    @Test
    public void dottedList() {
        ConsCell l = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c1", "c2")));
        assertEquals("(a b c1 . c2)", l.toString());
    }

    @Test
    public void normalCons() {
        ConsCell l = ConsCell.cons("a", "b");
        assertEquals("(a . b)", l.toString());
    }

    @Test
    public void listContainsSelf() {
        ConsCell l = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c", null)));
        cdr(l).rplaca(l);
        assertEquals("(a #<this list> c)", l.toString());
    }

    @Test
    public void circularList() {
        ConsCell l = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c", null)));
        cdr(cdr(l)).rplacd(l);
        assertEquals("(a b c #<circular list>)", l.toString());
    }

    @Test
    public void consContainsSelfCar() {
        ConsCell c = ConsCell.cons("a", "b");
        c.rplaca(c);
        assertEquals("(#<this cons> . b)", c.toString());
    }

    @Test
    public void consContainsSelfCdr() {
        ConsCell c = ConsCell.cons("a", "b");
        c.rplacd(c);
        // if cdr is a cons then it's really a list
        assertEquals("(a #<circular list>)", c.toString());
    }

    static ConsCell cdr(Object l) {
        return (ConsCell)((ConsCell)l).cdr();
    }
}
