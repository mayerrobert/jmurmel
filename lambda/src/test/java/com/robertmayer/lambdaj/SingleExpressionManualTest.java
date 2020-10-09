package com.robertmayer.lambdaj;

import org.junit.Test;

public class SingleExpressionManualTest {

    @Test
    public void runExpression() {
        LambdaJTest.runTest("quote", "'(1 2 3)", "(1.0 2.0 3.0)", null);
    }
}
