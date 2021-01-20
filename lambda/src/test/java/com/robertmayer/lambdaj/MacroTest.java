package com.robertmayer.lambdaj;

import org.junit.Test;

public class MacroTest {

    @Test
    public void test1() throws Exception {
        LambdaJTest.runTest("macro1.lisp",
                "(defmacro m () 1)"
                + "(m)",
                "1.0", null);
    }

    @Test
    public void test2() throws Exception {
        LambdaJTest.runTest("macro2.lisp",
                "(defmacro m () ''a)"
                + "(m)",
                "a", null);
    }

    @Test
    public void test3() throws Exception {
        LambdaJTest.runTest("macro3.lisp",
                "(defmacro m (a) a)"
                + "(m 2)",
                "2.0", null);
    }

    @Test
    public void test4() throws Exception {
        LambdaJTest.runTest("macro4.lisp",
                "(defmacro m (a) a)"
                + "(m 'x)",
                "x", null);
    }

    @Test
    public void test5() throws Exception {
        LambdaJTest.runTest("macro5.lisp",
                "(defmacro defparameter (sym val) `(define ,sym ,val)) "
                + "(defparameter x nil) "
                + "x",
                "nil", null);
    }
}
