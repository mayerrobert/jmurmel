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

    // from http://clhs.lisp.se/Body/m_defmac.htm
    @Test
    public void clhs1() throws Exception {
        LambdaJTest.runTest("clhs1.lisp",
                "(defmacro mac1 (a b) `(+ ,a (* ,b 3))) "
              + "(mac1 4 5)",
                "19.0", null);
    }

    @Test
    public void testMacroexpand1() throws Exception {
        LambdaJTest.runTest("macroexpand1.lisp",
                "(defmacro mac1 (a b) `(+ ,a (* ,b 3))) "
              + "(macroexpand-1 '(mac1 4 5))",
                "(+ 4.0 (* 5.0 3.0))", null);
    }

    @Test
    public void testMacro2() throws Exception {
        LambdaJTest.runTest("macro2.lisp",
                "(defmacro add1 (a b) `(+ ,a ,b)) (defmacro add2 (a b) `(add1 ,a ,b)) "
              + "(add2 1 2)",
                "3.0", null);
    }

    @Test
    public void testMacroexpand2() throws Exception {
        LambdaJTest.runTest("macroexpand2.lisp",
                "(defmacro add1 (a b) `(+ ,a ,b)) (defmacro add2 (a b) `(add1 ,a ,b)) "
              + "(macroexpand-1 (macroexpand-1 '(add2 1 2)))",
                "(+ 1.0 2.0)", null);
    }
}
