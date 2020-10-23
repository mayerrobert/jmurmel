package com.robertmayer.lambdaj;

import org.junit.Test;

import static junit.framework.Assert.*;

import static com.robertmayer.lambdaj.LambdaJ.*;

public class PrintObjTest {

    @Test
    public void listContainsNil() {
        ConsCell l = new ConsCell(null, null);
        assertEquals("(nil)", l.toString());
    }

    @Test
    public void listContainsThreeNils() {
        ConsCell l = new ConsCell(null, new ConsCell(null, new ConsCell(null, null)));
        assertEquals("(nil nil nil)", l.toString());
    }

    @Test
    public void listContainsTwoSymbols() {
        ConsCell l = new ConsCell("a", new ConsCell("b", null));
        assertEquals("(a b)", l.toString());
    }

    @Test
    public void listContainsThreeSymbols() {
        ConsCell l = new ConsCell("a", new ConsCell("b", new ConsCell("c", null)));
        assertEquals("(a b c)", l.toString());
    }

    @Test
    public void dottedList() {
        ConsCell l = new ConsCell("a", new ConsCell("b", new ConsCell("c1", "c2")));
        assertEquals("(a b c1 . c2)", l.toString());
    }

    @Test
    public void normalCons() {
        ConsCell l = new ConsCell("a", "b");
        assertEquals("(a . b)", l.toString());
    }

    @Test
    public void listContainsSelf() {
        ConsCell l = new ConsCell("a", new ConsCell("b", new ConsCell("c", null)));
        cdr(l).car = l;
        assertEquals("(a #<this list> c)", l.toString());
    }

    @Test
    public void circularList() {
        ConsCell l = new ConsCell("a", new ConsCell("b", new ConsCell("c", null)));
        cdr(cdr(l)).cdr = l;
        assertEquals("(a b c #<circular list>)", l.toString());
    }

    @Test
    public void consContainsSelfCar() {
        ConsCell c = new ConsCell("a", "b");
        c.car = c;
        assertEquals("(#<this cons> . b)", c.toString());
    }

    @Test
    public void consContainsSelfCdr() {
        ConsCell c = new ConsCell("a", "b");
        c.cdr = c;
        // if cdr is a cons then it's really a list
        assertEquals("(a #<circular list>)", c.toString());
    }

    static ConsCell cdr(Object l) {
        return (ConsCell)((ConsCell)l).cdr;
    }
}
