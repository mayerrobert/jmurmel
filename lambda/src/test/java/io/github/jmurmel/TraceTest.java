package io.github.jmurmel;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.Test;

public class TraceTest {

    @Test
    public void testTailRec() throws Exception {
        final ByteArrayOutputStream myOut = new ByteArrayOutputStream();
        System.setErr(new PrintStream(myOut));

        LambdaJTest.runTest("tailrec.lisp",
                          "(defun f (n) (if (> n 0) (f (- n 1)))) "
                        + "(trace 'f) "
                        + "(f 2.0)",
                          "nil", null);

        final String expectedTrace
                        = "(1 enter f: 2.0)\n"
                        + "  (2 enter f: 1.0)\n"
                        + "    (3 enter f: 0.0)\n"
                        + "    (3 exit  f: nil)\n"
                        + "  (2 exit  f: nil)\n"
                        + "(1 exit  f: nil)\n";

        assertEquals(expectedTrace, EolUtil.anyToUnixEol(myOut.toString()));
    }

    @Test
    public void testTailCall2() throws Exception {
        final ByteArrayOutputStream myOut = new ByteArrayOutputStream();
        System.setErr(new PrintStream(myOut));

        LambdaJTest.runTest("tailcall2.lisp",
                          "(defun f1 (n) n) (defun f2 (n) (f1 n)) "
                        + "(trace 'f1 'f2) "
                        + "(f2 2.0)",
                          "2.0", null);

        final String expectedTrace
                = "(1 enter f2: 2.0)\n"
                + "  (2 enter f1: 2.0)\n"
                + "  (2 exit  f1: 2.0)\n"
                + "(1 exit  f2: 2.0)\n";

        assertEquals(expectedTrace, EolUtil.anyToUnixEol(myOut.toString()));
    }

    @Test
    public void testTailCall3() throws Exception {
        final ByteArrayOutputStream myOut = new ByteArrayOutputStream();
        System.setErr(new PrintStream(myOut));

        LambdaJTest.runTest("tailcall2.lisp",
                          "(defun f1 (n) n) (defun f2 (n) (f1 n)) (defun f3 (n) (f2 n)) "
                        + "(trace 'f1 'f2 'f3) "
                        + "(f3 2.0)",
                          "2.0", null);

        final String expectedTrace
                = "(1 enter f3: 2.0)\n"
                + "  (2 enter f2: 2.0)\n"
                + "    (3 enter f1: 2.0)\n"
                + "    (3 exit  f1: 2.0)\n"
                + "  (2 exit  f2: 2.0)\n"
                + "(1 exit  f3: 2.0)\n";

        assertEquals(expectedTrace, EolUtil.anyToUnixEol(myOut.toString()));
    }

    @Test
    public void testCall() throws Exception {
        final ByteArrayOutputStream myOut = new ByteArrayOutputStream();
        System.setErr(new PrintStream(myOut));

        LambdaJTest.runTest("call.lisp",
                          "(defun f1 (n) n) (defun f2 (n) (f1 n)) (defun f3 () (f1 1.0) (f2 2.0)) "
                        + "(trace 'f1 'f2 'f3) "
                        + "(f3)",
                          "2.0", null);

        final String expectedTrace
                        = "(1 enter f3)\n"
                        + "  (2 enter f1: 1.0)\n"
                        + "  (2 exit  f1: 1.0)\n"
                        + "  (2 enter f2: 2.0)\n"
                        + "    (3 enter f1: 2.0)\n"
                        + "    (3 exit  f1: 2.0)\n"
                        + "  (2 exit  f2: 2.0)\n"
                        + "(1 exit  f3: 2.0)\n";

        assertEquals(expectedTrace, EolUtil.anyToUnixEol(myOut.toString()));
    }
}
