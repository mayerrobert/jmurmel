package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import com.robertmayer.lambdaj.LambdaJ.ConsCell;

public class ConsTest {

    @Test
    public void cons() {
        ConsCell l = new ListBuilder().append("a").append("b").append("c").first();
        assertEquals("(\"a\" \"b\" \"c\")", TestUtils.sexp(l));
    }
}

class ListBuilder {
    ConsCell first = null;
    ConsCell last = null;

    ListBuilder append(Object car) {
        ConsCell newCell = ConsCell.cons(car, null);
        if (first == null) {
            last = first = newCell;
        }
        else {
            last.rplacd(newCell);
            last = newCell;
        }
        return this;
    }

    ConsCell first() { return first; }
}
