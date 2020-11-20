package com.robertmayer.lambdaj;

import static junit.framework.Assert.*;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ.ConsCell;
import com.robertmayer.lambdaj.LambdaJ.ListConsCell;

import static com.robertmayer.lambdaj.PrintObjTest.cdr;

public class ConsCellIteratorTest {

    @Test
    public void circularList() {
        ListConsCell list = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c", null)));
        cdr(cdr(list)).cdr = list;
        assertEquals("(a b c #<circular list>)", list.toString());

        int n = 0;
        for (Object o: list) {
            assertNotNull(o);
            n++;
        }
        assertEquals(3, n);

        StringBuilder b = new StringBuilder();
        list.forEach(car -> b.append(car));
        assertEquals("abc", b.toString());
    }

    @Test
    public void consCell() {
        ConsCell cell = ConsCell.cons("a", "b");
        assertEquals("(a . b)", cell.toString());

        int n = 0;
        for (Object o: cell) {
            assertNotNull(o);
            n++;
        }
        assertEquals(2, n);

        StringBuilder b = new StringBuilder();
        cell.forEach(car -> b.append(car));
        assertEquals("ab", b.toString());
    }
}
