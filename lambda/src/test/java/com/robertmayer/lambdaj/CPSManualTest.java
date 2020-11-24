package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ.LambdaJError;

public class CPSManualTest {

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

    interface MurmelCPSFunction { Result apply(MurmelCPSFunction next, Object... args) throws LambdaJError; }

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

    static Result makeResult(Object result, MurmelCPSFunction next) {
        return new Result(result, next, null);
    }

    static Result makeTailCall(MurmelCPSFunction func, Object... args) {
        return new Result(null, func, args);
    }



    Result f1(MurmelCPSFunction next, Object... args) {
        Object a = args[0]; Object n = args[1];
        if (a == n) {
            return makeResult(a, next);
        }
        else {
            return makeTailCall(this::f1, new Object[] { (Integer)a+1, n });
        }
    }

    Result f2(MurmelCPSFunction next, Object... args) {
        return makeResult(null, next);
    }

    Result f3(MurmelCPSFunction next, Object... args) {
        return makeResult(null, next);
    }

    @Test
    public void testCps() {
        Object result;

        result = apply(this::f2, null, this::f3);
        result = apply(this::f3, null, this::f1);
        result = apply(this::f1, new Object[] { 0, 5 }, null);

        assertEquals(Integer.valueOf(5), result);
    }

    Object apply(MurmelCPSFunction f, Object[] args, MurmelCPSFunction next) {
        Result r = new Result();
        r.next = f;
        r.args = args;
        do {
            r = r.next.apply(next, r.args);
        } while (r.next != next);
        return r.result;
    }
}
