package com.robertmayer.lambdaj.recurs;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ.LambdaJError;

public class CPSManualTest {

    interface MurmelCPSFunction { Result apply(Object... args) throws LambdaJError; }

    static class Result {
        Object result;
        MurmelCPSFunction next;
        Object[] args;

        Result() {}
        Result(Object result, MurmelCPSFunction next, Object[] args) {
            this.result = result;
            this.next = next;
            this.args = args;
        }
    }

    static Result makeResult(Object result) {
        return new Result(result, null, null);
    }

    static Result makeEarlyReturn(Object result, MurmelCPSFunction next) {
        return new Result(result, next, null);
    }

    static Result makeTailCall(MurmelCPSFunction func, Object... args) {
        return new Result(null, func, args);
    }

    Object apply(MurmelCPSFunction f, Object... args) {
        Result r = new Result();
        r.next = f;
        r.args = args;
        do {
            r = r.next.apply(r.args);
        } while (r.next != null);
        return r.result;
    }



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

    Result f1(Object... args) {
        Object a = args[0]; Object n = args[1];
        if (a == n) {
            return makeResult(a);
        }
        else {
            return makeTailCall(this::f1, (Integer)a+1, n);
        }
    }

    Result f2(Object... args) {
        return makeResult(null);
    }

    Result f3(Object... args) {
        return makeResult(null);
    }

    @Test
    public void testCps() {
        Object result;

        result = apply(this::f2);
        result = apply(this::f3);
        result = apply(this::f1, new Object[] { 0, 5 });

        assertEquals(Integer.valueOf(5), result);
    }


    double dbl(Object o) { return ((Number)o).doubleValue(); }

    /*
    (defun factorial (n product)
      (if (<= n 1)
            product
        (factorial (- n 1) (* product n))))

    (factorial 5 1) ; ==> 120
   */
    Result factorial(Object... args) {
        Object n = args[0];
        Object product = args[1];
        if (dbl(n) <= 1) return makeResult(product);
        else return makeTailCall(this::factorial, dbl(n) - 1, dbl(product) * dbl(n));
    }

    @Test
    public void testFac() {
        Object res = apply(this::factorial, 5, 1);
        assertEquals(120.0, dbl(res), 1e-35);
    }
}
