package com.robertmayer.lambdaj;

import org.junit.Test;

import static junit.framework.Assert.*;

import static com.robertmayer.lambdaj.LambdaJ.*;

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
        cdr(l).car = l;
        assertEquals("(a #<this list> c)", l.toString());
    }

    @Test
    public void circularList() {
        ConsCell l = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c", null)));
        cdr(cdr(l)).cdr = l;
        assertEquals("(a b c #<circular list>)", l.toString());
    }

    @Test
    public void consContainsSelfCar() {
        ListConsCell c = ConsCell.cons("a", "b");
        c.car = c;
        assertEquals("(#<this cons> . b)", c.toString());
    }

    @Test
    public void consContainsSelfCdr() {
        ListConsCell c = ConsCell.cons("a", "b");
        c.cdr = c;
        // if cdr is a cons then it's really a list
        assertEquals("(a #<circular list>)", c.toString());
    }

    static ListConsCell cdr(Object l) {
        return (ListConsCell)((ListConsCell)l).cdr;
    }
}
