package com.robertmayer.lambdaj;

import org.junit.Test;

public class SingleExpressionManualTest {

    @Test
    public void runExpression() {
        LambdaJTest.runTest("quote", "(+ 1 (/ 1 (- 0)))", "-Infinity", null);
    }
}
