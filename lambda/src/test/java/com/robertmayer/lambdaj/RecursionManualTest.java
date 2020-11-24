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
    /* geht nicht weil im lambda ist das label nicht sichtbar
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



    // rekursion geht nicht im lambda
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



    private static class Recursion extends RuntimeException { public static final long serialVersionUID = 1L; Recursion() { super("recursion", null, false, false); } }
    private static final Recursion recursion = new Recursion();

    Object fPrognThrow(Object... args) {
        //Object a, n;
        recur:
        for (;;) {
            //a = args[0];
            //n = args[1];
            for (;;) {
                Object result = null;
                try {
                    result = ((MurmelProgn) () -> {
                        if ((Integer)(args[0]) == 0) return args[1];
                        else {
                            args[0] = (Integer)(args[0])-1; args[1] = (Integer)(args[1])+1;
                            throw recursion;
                        }
                    }).call();
                }
                catch (Recursion r) { continue recur; }
                return result;
            }
        }
    }

    @Test
    public void testEndRecursionPrognThrow() {
        assertEquals(5, fProgn(5, 0));
    }
}
