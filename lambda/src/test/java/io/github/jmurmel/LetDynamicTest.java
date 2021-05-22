package io.github.jmurmel;

import org.junit.Test;

public class LetDynamicTest {

    @Test
    public void test1() throws Exception {
        LambdaJTest.runTest("letdynamic1.lisp",
                "(define *g* 'init-global) "
                        + "(defun f() (write *g*)) "
                        + "(let dynamic ((*g* 'dyn)) (f)) "
                        + "*g*",
                "init-global", "dyn");
    }
}
