package com.robertmayer.lambda;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import org.junit.Test;

public class LambdaTest {

    @Test
    public void runTests() {
        for (String[] test: tests) {
            String prog = test[0];
            String expected = test[1];
            String expectedOutput = test[2];

            InputStream in = new ByteArrayInputStream(prog.getBytes());
            ByteArrayOutputStream actualOutput = new ByteArrayOutputStream();
            PrintStream out = new PrintStream(actualOutput);

            Lambda interpreter = new Lambda();
            //interpreter.debug = 0;

            String actual = interpreter.interpret(in, out);
            out.flush();

            assertEquals("program " + prog + " produced unexpected result", expected, actual);

            if (expectedOutput != null) {
                assertEquals("program " + prog + " produced unexpected output",
                             expectedOutput, actualOutput.toString().trim());
            }
        }
    }

    private String[][] tests = {
        { "(write (quote (Hello, world!)))", "(QUOTE T)", "(HELLO, WORLD!)" },
        { "(write (quote HELLO))", "(QUOTE T)", "HELLO" },
        { "(apply write (cons (quote HELLO) nil))", "(QUOTE T)", "HELLO" },
        { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) nil)) nil))", "(QUOTE T)", "(HELLO HELLO)" },
        { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) ())) ()))", "(QUOTE T)", "(HELLO HELLO)" },

        { "((lambda (x) (write x)) (quote hello))", "(QUOTE T)", "HELLO" },
        // geht nicht weil cons(string,string) den zweiten string auf pair casten will { "(apply (quote write) (cons (cons (quote HELLO) (quote HELLO)) nil))", "(QUOTE T)", "HELLOHELLO" },
    };
}
