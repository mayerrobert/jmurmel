package io.github.jmurmel;

import org.junit.Test;

public class ClosureTest {

    @Test
    public void defineClosure() {
        LambdaJTest.runTest("defineLambda", "(define x (lambda (arg1 arg2) (body1 1) (body2 2)))",
                "x",
                null);
    }

    @Test
    public void defineAndInvokeClosure() {
        LambdaJTest.runTest("defineLambda.lisp", "(define x (lambda (p1 p2) (write 1) (write 2))) (x 3 4)",
                "t", "1.02.0");
    }
}
