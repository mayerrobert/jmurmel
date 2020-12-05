package com.robertmayer.lambdaj;

import org.junit.Test;

public class SingleExpressionManualTest {

    @Test
    public void runExpression() {
        LambdaJTest.runTest("String", "(let ((cons 42)) cons)", "t", "\"aaa\"");
    }
}
