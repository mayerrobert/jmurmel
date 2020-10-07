package com.robertmayer.lambdaj;

import org.junit.Test;

public class SingleExpressionManualTest {

    @Test
    public void runExpression() {
        LambdaJTest.runTest("quote", "(apply (assoc (quote x) ()) ())", "-Infinity", null);
    }
}
