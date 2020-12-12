package com.robertmayer.lambdaj.recurs;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ.LambdaJError;

// Das ist weniger CPS als vielmehr TCO.
// Die statische Methode funcall() ist ein thunk, der die Tailcalls macht (und damit ist TCO umgesetzt),
// anstatt dass der Tailcall in der Funktion selbst passiert und damit Stack verbraucht.
// Hier ist vieles umstaendlich und fishy, ggf. falsch, hoffentlich bessere V2 ist "TCOManualTest.java"
public class CPSManualTest {

    interface MurmelCPSFunction { MurmelCPSResult apply(Object... args) throws LambdaJError; }

    static final MurmelCPSFunction EARLY_RETURN = null;

    static class MurmelCPSResult {
        Object result;
        MurmelCPSFunction next;
        Object[] args;

        MurmelCPSResult(Object result, MurmelCPSFunction next, Object[] args) {
            this.result = result;
            this.next = next;
            this.args = args;
        }
    }

    private static Object funcall(MurmelCPSFunction f, Object... args) {
        MurmelCPSResult r = new MurmelCPSResult(null, f, args);
        do {
            r = r.next.apply(r.args);
        } while (r.next != null);
        return r.result;
    }

    private static MurmelCPSResult makeResult(Object result) {
        return new MurmelCPSResult(result, null, null);
    }

    // das koennt gehen, fuer (return form), fuer (return-from name form...) ists aber zuwenig
    private static MurmelCPSResult makeEarlyReturn(Object result) {
        return new MurmelCPSResult(result, EARLY_RETURN, null);
    }

    private static MurmelCPSResult makeTailCall(MurmelCPSFunction func, Object... args) {
        return new MurmelCPSResult(null, func, args);
    }

    // das geht so nicht, weil eine continuation ist mehr, hat state
    private static MurmelCPSResult makeContCall(Object result, MurmelCPSFunction next, Object... args) {
        return new MurmelCPSResult(result, next, args);
    }

    private static double dbl(Object o) { return ((Number)o).doubleValue(); }



    /*

    (defun f1 (a n)
      (if (= a n)
            a
        (f1 (+ a 1) n)))

    (defun f2 () ())

    (defun f3 () ())

    (f2)
    (f3)
    (f1 0 5)

     */

    MurmelCPSResult f1(Object... args) {
        Object a = args[0]; Object n = args[1];
        if (a == n) {
            return makeResult(a);
        }
        else {
            return makeTailCall(this::f1, (Integer)a+1, n);
        }
    }

    MurmelCPSResult f2(Object... args) {
        return makeResult(null);
    }

    MurmelCPSResult f3(Object... args) {
        return makeResult(null);
    }

    @Test
    public void testCps() {
        Object result;

        result = funcall(this::f2);
        result = funcall(this::f3);
        result = funcall(this::f1, new Object[] { 0, 5 });

        assertEquals(Integer.valueOf(5), result);
    }



    /*
    (defun factorial (n product)
      (if (<= n 1)
            product
        (factorial (- n 1) (* product n))))

    (factorial 5 1) ; ==> 120
    */
    MurmelCPSResult factorial(Object... args) {
        Object n = args[0];
        Object product = args[1];
        MurmelCPSResult result = null;

        result = (dbl(n) <= 1)
        ? makeResult(product)
        : makeTailCall(this::factorial, dbl(n) - 1, dbl(product) * dbl(n));
        if (result.next != null) return result;

        return result;
    }

    @Test
    public void testFac() {
        Object res = funcall(this::factorial, 5, 1);
        assertEquals(120.0, dbl(res), 1e-35);
    }



    /*
    (defun ackermann (m n)
      (cond
        ((= m 0) (+ n 1))
        ((= n 0) (ackermann (- m 1) 1))
        (t (ackermann (- m 1) (ackermann m (- n 1))))))

    (ackermann 3 6) ; ==> 509
     */
    MurmelCPSResult ackermann(Object... args) {
        Object m = args[0];
        Object n = args[1];
        MurmelCPSResult result = null;

        result = false ? null
        : (dbl(m) == 0.0) ? makeResult(dbl(n) + 1)
        : (dbl(n) == 0.0) ? makeTailCall(this::ackermann, dbl(m) - 1.0, 1.0)
        : makeTailCall(this::ackermann, dbl(m) - 1, funcall(this::ackermann, m, dbl(n) - 1.0));
        if (result.next != null) return result; // das ist eigentlich nicht notwendig, weil nur das letze stmt darf
                                                // makeTailCall() aufrufen, danach MUSS "return result" kommen

        return result;
    }

    @Test
    public void testAckermann() {
        Object res = funcall(this::ackermann, 3.0, 6.0);
        assertEquals(509.0, dbl(res), 1e-35);
    }
}
