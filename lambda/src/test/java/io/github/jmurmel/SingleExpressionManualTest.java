package io.github.jmurmel;

import org.junit.Test;

public class SingleExpressionManualTest {

    @Test
    public void runExpression() {
        LambdaJTest.runTest("String", "(let ((cons 42)) cons)", "42.0", "");
    }
}
