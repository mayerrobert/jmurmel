package io.github.jmurmel;

import static org.junit.Assert.*;

import org.junit.Test;

import io.github.jmurmel.LambdaJ.ConsCell;
import static io.github.jmurmel.TestUtils.cdr;

public class ConsCellIteratorTest {

    @Test
    public void circularList() {
        ConsCell list = ConsCell.cons("a", ConsCell.cons("b", ConsCell.cons("c", null)));
        cdr(cdr(list)).rplacd(list);
        assertEquals("(a b c #<circular list>)", list.toString());

        int n = 0;
        for (Object o: list) {
            assertNotNull(o);
            n++;
        }
        assertEquals(3, n);

        StringBuilder b = new StringBuilder();
        list.forEach(b::append);
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
        cell.forEach(b::append);
        assertEquals("ab", b.toString());
    }
}
