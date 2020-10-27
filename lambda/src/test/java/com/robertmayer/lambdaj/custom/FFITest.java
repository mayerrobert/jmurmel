package com.robertmayer.lambdaj.custom;

import static org.junit.Assert.*;

import java.io.StringReader;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;
import com.robertmayer.lambdaj.LambdaJ.MurmelFunction;

public class FFITest {

    @Test
    public void testGetValue() {
        LambdaJ interp = new LambdaJ();
        interp.interpretExpression(new StringReader("(define *global* 42.0)")::read, (s) -> { return; });

        Object value = interp.getValue("*global*");
        assertEquals(42.0, value);
    }

    @Test
    public void testPrimitive() {
        LambdaJ interp = new LambdaJ();
        // must interpret *something* so that environment will be set up. "()->-1" is a ReadSupplier that returns EOF
        interp.interpretExpression(() -> -1, (s) -> { return; });

        MurmelFunction add = interp.getFunction("+");
        Object result = add.apply(1, 2);
        assertEquals(3.0, result);
    }

    @Test
    public void testLambda() {
        LambdaJ interp = new LambdaJ();
        interp.interpretExpression(new StringReader("(defun f (p1 p2) (* p1 p2))")::read, (s) -> { return; });

        MurmelFunction add = interp.getFunction("f");
        Object result = add.apply(2, 3);
        assertEquals(6.0, result);
    }
}
