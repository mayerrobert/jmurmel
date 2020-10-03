package com.robertmayer.lambda;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import org.junit.Test;

public class LambdaTest {

    private String[][] tests = {
        /*  0 */ { "write", "#<primitive>", null },

        // application of builtins
        /*  1 */ { "(write (quote (Hello, world!)))", "(quote t)", "(Hello, world!)" },
        /*  2 */ { "(write (quote HELLO))", "(quote t)", "HELLO" },
        /*  3 */ { "(cons (quote HELLO) (quote HELLO))", "(HELLO . HELLO)", null },
        /*  4 */ { "(assoc (quote write) ())", "nil", null },
        /*  5 */ { "(assoc (quote b) (cons (cons (quote a) (quote 1)) (cons (cons (quote b) (quote 2)) (cons (cons (quote c) (quote 3)) ()))))", "(b . 2)", null },

        // comments
        /*  6 */ { "; comment\n(write (quote HELLO))", "(quote t)", "HELLO" },
        /*  7 */ { "; comment\n(write (quote HELLO)) ; comment", "(quote t)", "HELLO" },

        // quoted chars
        /*  8 */ { "(write (quote HELLO\\ ))", "(quote t)", "HELLO " },
        /*  9 */ { "(write (quote HELLO\\\\))", "(quote t)", "HELLO\\" },
        /* 10 */ { "(write (quote HELLO\\)))", "(quote t)", "HELLO)" },
        /* 11 */ { "(write (quote HELLO\\;))", "(quote t)", "HELLO;" },

        // apply
        /* 12 */ { "(apply write (cons (quote HELLO) nil))", "(quote t)", "HELLO" },
        /* 13 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) nil)) nil))", "(quote t)", "(HELLO HELLO)" },
        /* 14 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) ())) ()))", "(quote t)", "(HELLO HELLO)" },
        /* 15 */ { "(apply write (cons (cons (quote HELLO) (quote HELLO)) nil))", "(quote t)", "(HELLO . HELLO)" },

        // lambda
        //* 16 */ { "(lambda () (write (quote noparam)))", "#<function>", null }, // todo
        //* 17 */ { "(write (lambda () (write (quote noparam))))", "(quote t)", "#<function>" }, // todo
        /* 18 */ { "((lambda () (write (quote noparam))))", "(quote t)", "noparam" },
        /* 19 */ { "((lambda (x) (write x)) (quote hello))", "(quote t)", "hello" },
        /* 20 */ { "((lambda (x y) (write (cons x y))) (quote p1) (quote p2))", "(quote t)", "(p1 . p2)" },
        /* 21 */ { "(write ((lambda () (write (quote 1)) (write (quote 2)))))", "(quote t)", "12(quote t)" },

        // eq
        /* 22 */ { "(write ((lambda () (eq (quote 1) (quote 2)))))", "(quote t)", "nil" },
        /* 23 */ { "(write ((lambda () (eq (quote 1) (quote 1)))))", "(quote t)", "(quote t)" },

        // cond
        /* 24 */ { "((lambda (x) (cond ((eq x (quote 1)) (write (quote 1))) ((eq x (quote 2)) (write (quote 2))) ((eq x (quote 3)) (write (quote 3))))) (quote 3))", "(quote t)", "3" },
        /* 25 */ { "((lambda (x) (cond ((eq x (quote 1)) (write (quote 1))) ((eq x (quote 2)) (write (quote 2))) ((eq x (quote 3)) (write (quote 3))))) (quote 4))", "nil", null },

        // labels
        /* 26 */ { "(labels () (write (quote 1)) (write (quote 2)))", "(quote t)", "12" },
        //* 27 */ { "(labels ((w1 (x) (write (cons (quote 1) x))) (w2 (x) (write (cons (quote 2) x)))) (w1 (quote 3)) (w2 (quote 4)))", "(1 . 3)(2 . 4)", null }, // todo
    };

    //@Test
    public void runTest() {
        runTest(21);
    }

    @Test
    public void allTests() {
        for (int n = 0; n < tests.length; n++) {
            runTest(n);
        }
    }

    private void runTest(int n) {
        String[] test = tests[n];
        String prog = test[0];
        String expected = test[1];
        String expectedOutput = test[2];

        InputStream in = new ByteArrayInputStream(prog.getBytes());
        ByteArrayOutputStream actualOutput = new ByteArrayOutputStream();
        PrintStream out = new PrintStream(actualOutput);

        Lambda interpreter = new Lambda();
        interpreter.trace = Lambda.TRC_PRIM;

        System.out.println("***** running program:");
        System.out.println("-------------------------------------------------------");
        System.out.println(prog);
        System.out.println("-------------------------------------------------------");
        String actual = interpreter.interpret(in, out);
        out.flush();
        System.out.println("***** done program, result: " + actual);
        System.out.println();

        assertEquals("program " + prog + " produced unexpected result", expected, actual);

        if (expectedOutput == null) {
            assertEquals("program " + prog + " produced unexpected output", 0, actualOutput.size());
        } else {
            assertEquals("program " + prog + " produced unexpected output", expectedOutput, actualOutput.toString());
        }
    }
}
