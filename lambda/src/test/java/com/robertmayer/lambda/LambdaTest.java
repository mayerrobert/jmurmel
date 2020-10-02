package com.robertmayer.lambda;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import org.junit.Test;

public class LambdaTest {

    private String[][] tests = {
            { "write", "#<primitive>", null },

        // application of builtins
        /*  0 */ { "(write (quote (Hello, world!)))", "(quote t)", "(Hello, world!)" },
        /*  1 */ { "(write (quote HELLO))", "(quote t)", "HELLO" },
        /*  2 */ { "(cons (quote HELLO) (quote HELLO))", "(HELLO . HELLO)", null },

        // comments
        /*  3 */ { "; comment\n(write (quote HELLO))", "(quote t)", "HELLO" },
        /*  4 */ { "; comment\n(write (quote HELLO)) ; comment", "(quote t)", "HELLO" },

        // quoted chars
        /*  5 */ { "(write (quote HELLO\\ ))", "(quote t)", "HELLO " },
        /*  6 */ { "(write (quote HELLO\\\\))", "(quote t)", "HELLO\\" },
        /*  7 */ { "(write (quote HELLO\\)))", "(quote t)", "HELLO)" },
        /*  8 */ { "(write (quote HELLO\\;))", "(quote t)", "HELLO;" },

        // apply
        /*  9 */ { "(apply write (cons (quote HELLO) nil))", "(quote t)", "HELLO" },
        /* 10 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) nil)) nil))", "(quote t)", "(HELLO HELLO)" },
        /* 11 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) ())) ()))", "(quote t)", "(HELLO HELLO)" },
        /* 12 */ { "(apply write (cons (cons (quote HELLO) (quote HELLO)) nil))", "(quote t)", "(HELLO . HELLO)" },

        // lambda
        /* 13 */ { "((lambda (x) (write x)) (quote hello))", "(quote t)", "hello" },

    };

    //@Test
    public void runTest() {
        runTest(2);
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
        interpreter.debug = 0;

        System.out.println("***** running program:");
        System.out.println("-------------------------------------------------------");
        System.out.println(prog);
        System.out.println("-------------------------------------------------------");
        String actual = interpreter.interpret(in, out);
        out.flush();
        System.out.println("***** done program, result: " + actual);
        System.out.println();

        assertEquals("program " + prog + " produced unexpected result", expected, actual);

        if (expectedOutput != null) {
            assertEquals("program " + prog + " produced unexpected output",
                         expectedOutput, actualOutput.toString());
        }
    }
}
