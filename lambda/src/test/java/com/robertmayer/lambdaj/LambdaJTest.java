package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

public class LambdaJTest {

    public static final String[][] tests = {
        /*  0 */ { "write", "#<primitive>", null },

        // application of builtins
        /*  1 */ { "(write (quote (Hello, world!)))", "t", "(Hello, world!)" },
        /*  2 */ { "(write (quote HELLO))", "t", "HELLO" },
        /*  3 */ { "(cons (quote HELLO) (quote HELLO))", "(HELLO . HELLO)", null },
        /*  4 */ { "(assoc (quote write) ())", "nil", null },
        /*  5 */ { "(assoc (quote b) (cons (cons (quote a) (quote 1)) (cons (cons (quote b) (quote 2)) (cons (cons (quote c) (quote 3)) ()))))", "(b . 2)", null },

        // comments
        /*  6 */ { "; comment\n(write (quote HELLO))", "t", "HELLO" },
        /*  7 */ { "; comment\n(write (quote HELLO)) ; comment", "t", "HELLO" },

        // quoted chars
        /*  8 */ { "(write (quote HELLO\\ ))", "t", "HELLO " },
        /*  9 */ { "(write (quote HELLO\\\\))", "t", "HELLO\\" },
        /* 10 */ { "(write (quote HELLO\\)))", "t", "HELLO)" },
        /* 11 */ { "(write (quote HELLO\\;))", "t", "HELLO;" },

        // apply
        /* 12 */ { "(apply write (cons (quote HELLO) nil))", "t", "HELLO" },
        /* 13 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) nil)) nil))", "t", "(HELLO HELLO)" },
        /* 14 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) ())) ()))", "t", "(HELLO HELLO)" },
        /* 15 */ { "(apply write (cons (cons (quote HELLO) (quote HELLO)) nil))", "t", "(HELLO . HELLO)" },

        // lambda
        /* 16 */ { "(lambda () (write (quote noparam)))", "(lambda nil (write (quote noparam)))", null },
        /* 17 */ { "(write (lambda () (write (quote noparam))))", "t", "(lambda nil (write (quote noparam)))" },
        /* 18 */ { "((lambda () (write (quote noparam))))", "t", "noparam" },
        /* 19 */ { "((lambda (x) (write x)) (quote hello))", "t", "hello" },
        /* 20 */ { "((lambda (x y) (write (cons x y))) (quote p1) (quote p2))", "t", "(p1 . p2)" },
        /* 21 */ { "(write ((lambda () (write (quote 1)) (write (quote 2)))))", "t", "12t" },

        // eq
        /* 22 */ { "(write ((lambda () (eq (quote 1) (quote 2)))))", "t", "nil" },
        /* 23 */ { "(write ((lambda () (eq (quote 1) (quote 1)))))", "t", "t" },

        // cond
        /* 24 */ { "((lambda (x) (cond ((eq x (quote 1)) (write (quote 1))) ((eq x (quote 2)) (write (quote 2))) ((eq x (quote 3)) (write (quote 3))))) (quote 3))", "t", "3" },
        /* 25 */ { "((lambda (x) (cond ((eq x (quote 1)) (write (quote 1))) ((eq x (quote 2)) (write (quote 2))) ((eq x (quote 3)) (write (quote 3))))) (quote 4))", "nil", null },

        // labels
        /* 26 */ { "(labels () (write (quote 1)) (write (quote 2)))", "t", "12" },
        /* 27 */ { "(labels ((w1 (x) (write (cons (quote 1) x))) (w2 (x) (write (cons (quote 2) x)))) (w1 (quote 3)) (w2 (quote 4)))", "t", "(1 . 3)(2 . 4)" }, // todo
    };

    //@Test
    public void runTest() {
        runTest(27);
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
        String expectedResult = test[1];
        String expectedOutput = test[2];

        runTest(getClass().getSimpleName() + ".tests[" + n + "]", prog, expectedResult, expectedOutput);
    }



    @Test
    public void runAllFiles() throws Exception {
        Path cwd = Paths.get(".").toRealPath();
        System.out.println("cwd: " + cwd.toString());
        Path lispDir = Paths.get("src", "test", "lisp");
        Files.walk(lispDir).filter(path -> path.toString()
                .endsWith(".lisp")).forEach(path -> runTest(path));
    }

    private static Pattern outputPattern = Pattern.compile("[^;]*; output: (.*)");
    private static Pattern resultPattern = Pattern.compile("[^;]*; result: (.*)");
    private static Pattern errorPattern = Pattern.compile("[^;]*; error: (.*)");

    static void runTest(Path fileName) {
        try {
            final String contents = new String(Files.readAllBytes(fileName));

            String expectedOutput = findMatch(outputPattern, contents);
            String expectedResult = findMatch(resultPattern, contents);
            final String expectedError = findMatch(errorPattern, contents);

            if (expectedOutput != null && expectedResult != null || expectedError != null) {
                expectedOutput = transform(expectedOutput);
                expectedResult = transform(expectedResult);
                try {
                    runTest(fileName.toString(), contents, expectedResult, expectedOutput);
                }
                catch (LambdaJ.Error e) {
                    if (expectedError != null && e.getMessage().contains(expectedError)) {
                        // thats fine
                    } else {
                        throw e;
                    }
                }
            }
            else {
                System.out.println("***** skipping " + fileName.toString());
        }
        } catch (LambdaJ.Error e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static String findMatch(Pattern pattern, String contents) {
        final Matcher outputMatcher = pattern.matcher(contents);
        if (outputMatcher.find()) {
            return outputMatcher.group(1);
        }
        else {
            return null;
        }
    }

    private static String transform(String s) {
        if (s == null) return null;
        if ("nil".equals(s)) return null;
        if ("(empty)".equals(s)) return "";
        return s;
    }

    static void runTest(String fileName, String prog, String expectedResult, String expectedOutput) {
        InputStream in = new ByteArrayInputStream(prog.getBytes());

        ByteArrayOutputStream actualOutput = new ByteArrayOutputStream();
        PrintStream out = new PrintStream(actualOutput);

        LambdaJ interpreter = new LambdaJ();
        interpreter.trace = LambdaJ.TRC_LEX;

        System.out.println("***** running program:");
        System.out.println("-------------------------------------------------------");
        System.out.println(prog);
        System.out.println("-------------------------------------------------------");

        String actualResult;
        if (fileName.endsWith(".lisp")) {
            actualResult = interpreter.interpretExpressions(in, out);
        } else {
            actualResult = interpreter.interpretExpression(in, out);
        }
        out.flush();
        System.out.println("***** done program, result: " + actualResult);
        System.out.println();

        assertEquals("program " + fileName + " produced unexpected result", expectedResult, actualResult);

        if (expectedOutput == null) {
            assertEquals("program " + fileName + " produced unexpected output", 0, actualOutput.size());
        } else {
            assertEquals("program " + fileName + " produced unexpected output", expectedOutput, actualOutput.toString());
        }
    }
}
