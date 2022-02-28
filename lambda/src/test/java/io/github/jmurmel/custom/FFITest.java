package io.github.jmurmel.custom;

import static org.junit.Assert.*;

import java.io.StringReader;

import io.github.jmurmel.LambdaJ;
import org.junit.Test;

public class FFITest {

    @Test
    public void testGetValue() {
        LambdaJ interp = new LambdaJ();
        interp.interpretExpression(new StringReader("(define *global* 42.0)")::read, s -> {});

        Object value = interp.getValue("*global*");
        assertEquals(42.0, value);
    }

    @Test
    public void testPrimitive() {
        LambdaJ interp = new LambdaJ();
        // must interpret *something* so that environment will be set up. "()->-1" is a ReadSupplier that returns EOF
        interp.interpretExpression(() -> -1, s -> {});

        LambdaJ.MurmelFunction add = interp.getFunction("+");
        Object result = add.apply(1L, 2L);
        assertEquals(3.0, result);
    }

    @Test
    public void testLambda() {
        LambdaJ interp = new LambdaJ();
        interp.interpretExpression(new StringReader("(defun f (p1 p2) (* p1 p2))")::read, s -> {});

        LambdaJ.MurmelFunction add = interp.getFunction("f");
        Object result = add.apply(2L, 3L);
        assertEquals(6.0, result);
    }
}
