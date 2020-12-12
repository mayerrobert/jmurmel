package com.robertmayer.lambdaj.recurs;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ.MurmelFunction;

public class TCOManualTest {

    static class MurmelFunctionCall {
        MurmelFunction next;
        Object[] args;

        MurmelFunctionCall(MurmelFunction next, Object[] args) {
            this.next = next;
            this.args = args;
        }
    }

    private static Object funcall(MurmelFunction f, Object... args) {
        Object r = new MurmelFunctionCall(f, args);
        do {
            MurmelFunctionCall functionCall = (MurmelFunctionCall)r;
            r = functionCall.next.apply(functionCall.args);
        } while (r instanceof MurmelFunctionCall);
        return r;
    }

    private static MurmelFunctionCall makeTailCall(MurmelFunction func, Object... args) {
        return new MurmelFunctionCall(func, args);
    }

    private static double dbl(Object o) { return ((Number)o).doubleValue(); }



    /*
    (defun factorial (n product)
      (if (<= n 1)
            product
        (factorial (- n 1) (* product n))))

    (factorial 5 1) ; ==> 120
    */
    Object factorial(Object... args) {
        Object n = args[0];
        Object product = args[1];
        Object result = null;

        result = (dbl(n) <= 1)
        ? product
        : makeTailCall(this::factorial, dbl(n) - 1, dbl(product) * dbl(n));

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
    Object ackermann(Object... args) {
        Object m = args[0];
        Object n = args[1];
        Object result = null;

        result = false ? null
        : (dbl(m) == 0.0) ? (dbl(n) + 1)
        : (dbl(n) == 0.0) ? makeTailCall(this::ackermann, dbl(m) - 1.0, 1.0)
        : makeTailCall(this::ackermann, dbl(m) - 1, funcall(this::ackermann, m, dbl(n) - 1.0));

        return result;
    }

    @Test
    public void testAckermann() {
        Object res = funcall(this::ackermann, 3.0, 6.0);
        assertEquals(509.0, dbl(res), 1e-35);
    }
}
