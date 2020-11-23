package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class RecursionManualTest {

    Object f(Object... args) {
        Object a, n;
        recur:
        for (;;) {
            a = args[0];
            n = args[1];
            for (;;) {
                if ((Integer)a == 0) return n;
                args[0] = (Integer)a-1; args[1] = (Integer)n+1;
                continue recur;
            }
        }
    }

    @Test
    public void testEndRecursion() {
        assertEquals(5, f(5, 0));
    }

    interface MurmelProgn { Object call(); }
    /*
    Object fProgn(Object... args) {
        Object a, n;
        recur:
        while (true) {
            a = args[0];
            n = args[1];
            while (true) {
                Object result = ((MurmelProgn) () -> {
                    if ((Integer)a == 0) return n;
                    args[0] = (Integer)a-1; args[1] = (Integer)n+1;
                    continue recur;
                    throw new IllegalStateException();
                }).call();
            }
        }
    }
    */

    Object fProgn(Object... args) {
        Object a, n;
        recur:
        for (;;) {
            a = args[0];
            n = args[1];
            for (;;) {
                Object result = ((MurmelProgn) () -> {
                    return "Hallo";
                }).call();
                if ((Integer)a == 0) return n;
                args[0] = (Integer)a-1; args[1] = (Integer)n+1;
                continue recur;
            }
        }
    }

    @Test
    public void testEndRecursionProgn() {
        assertEquals(5, fProgn(5, 0));
    }
}
