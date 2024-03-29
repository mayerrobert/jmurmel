package io.github.jmurmel;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

import static io.github.jmurmel.TestUtils.sexp;
import static org.junit.Assert.*;

public class LambdaJTest {

    private static final String[][] tests = {
        /*  0 */ { "write", "#<primitive>", null },

        // application of builtins
        /*  1 */ { "(write (quote (Hello\\, world!)))", "(|Hello,| world!)", "(|Hello,| world!)" },
        /*  2 */ { "(write (quote HELLO))", "HELLO", "HELLO" },
        /*  3 */ { "(cons (quote HELLO) (quote HELLO))", "(HELLO . HELLO)", null },
        /*  4 */ { "(assoc (quote write) ())", "nil", null },
        /*  5 */ { "(assoc (quote b) (cons (cons (quote a) (quote a1)) (cons (cons (quote b) (quote b2)) (cons (cons (quote c) (quote c3)) ()))))", "(b . b2)", null },
        /*  6 */ { "(write (car (quote (v1 v2))))", "v1", "v1" },
        /*  7 */ { "(write (cdr (quote (v1 v2))))", "(v2)", "(v2)" },
        /*  8 */ { "(write (car (cons (quote v1) (quote v2))))", "v1", "v1" },
        /*  9 */ { "(write (cdr (cons (quote v1) (quote v2))))", "v2", "v2" },

        // comments
        /* 10 */ { "; comment\n(write (quote HELLO))", "HELLO", "HELLO" },
        /* 11 */ { "; comment\n(write (quote HELLO)) ; comment", "HELLO", "HELLO" },

        // quoted chars
        /* 12 */ { "(write (quote HELLO\\ ))", "|HELLO |", "|HELLO |" },
        /* 13 */ { "(write (quote HELLO\\\\))", "|HELLO\\\\|", "|HELLO\\\\|" },
        /* 14 */ { "(write (quote HELLO\\)))", "|HELLO)|", "|HELLO)|" },
        /* 15 */ { "(write (quote HELLO\\;))", "HELLO;", "HELLO;" },

        // apply
        /* 16 */ { "(apply write (cons (quote HELLO) nil))", "HELLO", "HELLO" },
        /* 17 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) nil)) nil))", "(HELLO HELLO)", "(HELLO HELLO)" },
        /* 18 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) ())) ()))", "(HELLO HELLO)", "(HELLO HELLO)" },
        /* 19 */ { "(apply write (cons (cons (quote HELLO) (quote HELLO)) nil))", "(HELLO . HELLO)", "(HELLO . HELLO)" },

        // lambda
        /* 20 */ { "(lambda () (write (quote noparam)))", "#<interpreted closure>", null },
        /* 21 */ { "(write (lambda () (write (quote noparam))))", "#<interpreted closure>", "#<interpreted closure>" },
        /* 22 */ { "((lambda () (write (quote noparam))))", "noparam", "noparam" },
        /* 23 */ { "((lambda (x) (write x)) (quote hello))", "hello", "hello" },
        /* 24 */ { "((lambda (x y) (write (cons x y))) (quote p1) (quote p2))", "(p1 . p2)", "(p1 . p2)" },
        /* 25 */ { "(write ((lambda () (write (quote s1)) (write (quote s2)))))", "s2", "s1s2s2" },

        // eq
        /* 26 */ { "(write ((lambda () (eq (quote s1) (quote s2)))))", "nil", "nil" },
        /* 27 */ { "(write ((lambda () (eq (quote s1) (quote s1)))))", "t", "t" },

        // cond
        /* 28 */ { "((lambda (x) (cond ((eq x (quote s1)) (write (quote s1))) ((eq x (quote s2)) (write (quote s2))) ((eq x (quote s3)) (write (quote s3))))) (quote s3))", "s3", "s3" },
        /* 29 */ { "((lambda (x) (cond ((eq x (quote s1)) (write (quote s1))) ((eq x (quote s2)) (write (quote s2))) ((eq x (quote s3)) (write (quote s3))))) (quote s4))", "nil", null },

        /* 30 */ { "(if (eq (quote a) (quote b)) (quote a))", "nil", null },
        /* 31 */ { "(if (eq (quote a) (quote a)) (quote a))", "a", null },
        /* 32 */ { "(if (eq (quote a) (quote b)) (quote a) (quote b))", "b", null },
        /* 33 */ { "(if (eq (quote a) (quote a)) (quote a) (quote b))", "a", null },

        // labels
        /* 34 */ { "(labels () (write (quote s1)) (write (quote s2)))", "s2", "s1s2" },
        /* 35 */ { "(labels ((w1 (x) (write (cons (quote s1) x))) (w2 (x) (write (cons (quote s2) x)))) (w1 (quote s3)) (w2 (quote s4)))", "(s2 . s4)", "(s1 . s3)(s2 . s4)" },

        // numbers, numeric operators
        /* 36 */ { "1", "1.0", null },
        /* 37 */ { "(write 1)", "1.0", "1.0" },

        /* 38 */ { "(= 2 2)", "t", null },
        /* 39 */ { "(= 2 3)", "nil", null },

        /* 40 */ { "(< 2 3)", "t", null },
        /* 41 */ { "(< 2 2)", "nil", null },

        /* 42 */ { "(<= 3 3)", "t", null },
        /* 43 */ { "(<= 4 2)", "nil", null },

        /* 44 */ { "(> 4 3)", "t", null },
        /* 45 */ { "(> 2 2)", "nil", null },

        /* 46 */ { "(>= 3 3)", "t", null },
        /* 47 */ { "(>= 3 4)", "nil", null },

        /* 48 */ { "(+)", "0.0", null },
        /* 49 */ { "(+ 3)", "3.0", null },
        /* 50 */ { "(+ 3 4)", "7.0", null },
        /* 51 */ { "(+ 3 4 5)", "12.0", null },

        /* 52 */ { "(- 3)", "-3.0", null },
        /* 53 */ { "(- 3 4)", "-1.0", null },
        /* 54 */ { "(- 3 4 5)", "-6.0", null },

        /* 55 */ { "(*)", "1.0", null },
        /* 56 */ { "(* 3)", "3.0", null },
        /* 57 */ { "(* 3 4)", "12.0", null },
        /* 58 */ { "(* 3 4 5)", "60.0", null },

        /* 59 */ { "(/ 5)", "0.2", null },
        /* 60 */ { "(/ 12 2)", "6.0", null },
        /* 61 */ { "(/ 12 2 3)", "2.0", null },

        /* 62 */ { "(mod 12 5)", "2.0", null },

        /* 63 */ { "(atom 1)",              "t", null },
        /* 64 */ { "(atom (quote x))",      "t", null },
        /* 65 */ { "(atom ())",             "t", null },
        /* 66 */ { "(atom nil)",            "t", null },
        /* 67 */ { "(atom (quote (a)))",    "nil", null },

        /* 68 */ { "(symbolp 1)",           "nil", null },
        /* 69 */ { "(symbolp (quote x))",   "t", null },
        /* 70 */ { "(symbolp ())",          "t", null },
        /* 71 */ { "(symbolp nil)",         "t", null },
        /* 72 */ { "(symbolp (quote (a)))", "nil", null },

        /* 73 */ { "(consp 1)",             "nil", null },
        /* 74 */ { "(consp (quote x))",     "nil", null },
        /* 75 */ { "(consp ())",            "nil", null },
        /* 76 */ { "(consp nil)",           "nil", null },
        /* 77 */ { "(consp (quote (a)))",   "t", null },

        /* 78 */ { "(listp 1)",             "nil", null },
        /* 79 */ { "(listp (quote x))",     "nil", null },
        /* 80 */ { "(listp ())",            "t", null },
        /* 81 */ { "(listp nil)",           "t", null },
        /* 82 */ { "(listp (quote (a)))",   "t", null },

        /* 83 */ { "(numberp 1)",           "t", null },
        /* 84 */ { "(numberp (quote x))",   "nil", null },
        /* 85 */ { "(numberp ())",          "nil", null },
        /* 86 */ { "(numberp nil)",         "nil", null },
        /* 87 */ { "(numberp (quote (a)))", "nil", null },

        /* 88 */ { "(cdr (cdr (cdr (quote (a (b c) (d (e f)))))))", "nil", null },
        /* 89 */ { "()", "nil", null },
        /* 90 */ { "(quote ())", "nil", null },
        /* 91 */ { "nil", "nil", null },
        /* 92 */ { "t", "t", null },

        /* 93 */ { "(define *xyxxy* (lambda () ()))", "*xyxxy*", null },
    };

    @Test
    public void runAllTests() {
        for (int n = 0; n < tests.length; n++) {
            runTest(n);
        }
    }

    @Test
    public void runAllFiles() throws Exception {
        Path cwd = Paths.get(".").toRealPath();
        System.out.println("LambdaJTest.runAllFiles() cwd: " + cwd);
        Path lispDir = Paths.get("src", "test", "lisp");
        int skipped = Files.walk(lispDir).filter(path -> path.toString()
                .endsWith(".lisp")).mapToInt(LambdaJTest::runTest).sum();

        // files that contain neither "result:" nor "error:" will be skipped
        // grep -L -E "(result:|error:)" *.lisp
        // make sure that files won't be skipped because of a typo or extra space or something
        assertEquals("number of skipped .lisp files has changed", 1, skipped);
    }



    static void runTest(int n) {
        String[] test = tests[n];
        String prog = test[0];
        String expectedResult = test[1];
        String expectedOutput = test[2];

        try {
            runTest("LambdaJTest.tests[" + n + "]", prog, expectedResult, expectedOutput);
        } catch (Exception e) {
            fail("LambdaJTest.tests[" + n + "]: " + prog + " threw Exception " + e.getMessage());
        }
    }

    private static final Pattern outputPattern = Pattern.compile("[^;]*; output: (.*)");
    private static final Pattern resultPattern = Pattern.compile("[^;]*; result: (.*)");
    private static final Pattern errorPattern = Pattern.compile("[^;]*; error: (.*)");

    static int runTest(Path fileName) {
        try {
            final String contents = new String(Files.readAllBytes(fileName));

            String expectedOutput = findMatch(outputPattern, contents);
            String expectedResult = findMatch(resultPattern, contents);
            String expectedError = findMatch(errorPattern, contents);

            if (expectedOutput != null && expectedResult != null || expectedError != null) {
                expectedOutput = transform(expectedOutput);
                expectedResult = transform(expectedResult);
                try {
                    runTest(fileName.toString(), contents, expectedResult, expectedOutput);
                    if (expectedError != null) fail(fileName + ": expected error \"" + expectedError + "\" but no error occurred");
                }
                catch (Exception e) {
                    final String message = messageAndLocation(e);
                    assertTrue("unexpected exception " + e.getClass().getSimpleName() + ": " + message, e instanceof LambdaJ.LambdaJError || e instanceof LambdaJ.ReaderError);
                    if (expectedError != null) {
                        expectedError = EolUtil.anyToUnixEol(expectedError).toString(); // todo findMatch sollte gleich \n liefern dann brauchts hier kein anyToUnixEol
                        if (message.contains(expectedError)) {
                            // thats fine
                        }
                        else {
                            fail(fileName + ": expected error \"" + expectedError + '\"'
                                    + " but got error \"" + message + '\"');
                        }
                    } else {
                        fail(fileName + " threw exception " + message);
                    }
                }
                return 0;
            }
            else {
                System.out.println("***** skipping " + fileName);
                return 1;
            }
        } catch (LambdaJ.LambdaJError | AssertionError e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static String findMatch(Pattern pattern, String contents) {
        final Matcher outputMatcher = pattern.matcher(contents);
        if (outputMatcher.find()) {
            return outputMatcher.group(1).replaceAll(Matcher.quoteReplacement("\\n"), System.lineSeparator());
        }
        else {
            return null;
        }
    }

    private static String transform(String s) {
        if (s == null) return null;
        if ("(empty)".equals(s)) return "";
        return s;
    }

    static void runErrorTest(String fileName, String prog, String expectedExceptionMsgPfx) {
        try {
            runTest(fileName, prog, "ignored", "ignored");
            fail("was expecting error: " + expectedExceptionMsgPfx);
        }
        catch (Exception e) {
            assertTrue("unexpected exception " + e.getClass().getSimpleName() + ": " + e.getMessage(), e instanceof LambdaJ.LambdaJError || e instanceof LambdaJ.ReaderError);
            String msg = messageAndLocation(e);

            final String expected = EolUtil.anyToUnixEol(expectedExceptionMsgPfx).toString();
            assertTrue("got wrong exception message: " + e.getMessage(), msg.startsWith(expected));
        }
    }

    private static String messageAndLocation(Exception e) {
        String msg = e.getMessage();
        if (e instanceof LambdaJ.LambdaJError) msg += "\n" + ((LambdaJ.LambdaJError)e).getLocation();
        msg = EolUtil.anyToUnixEol(msg).toString();
        return msg;
    }

    /**
     * <p>Eval the expression(s) {@code prog} and check it's result and output.
     *
     * <p>If {@code fileName} ends with ".lisp" then a list of expressions will be eval'd.
     *
     * @param fileName          name used for messages
     * @param prog              the program to eval
     * @param expectedResult    expected expression result, "nil" for null result
     * @param expectedOutput    expected contents of stdout, null or "" for no output
     */
    static void runTest(String fileName, String prog, String expectedResult, String expectedOutput) {
        final StringBuilder out = new StringBuilder();

        // turn off long so I don't have to adjust all tests
        final int features = LambdaJ.Features.HAVE_ALL_LEXC.bits() & ~LambdaJ.Features.HAVE_LONG.bits() | LambdaJ.Features.HAVE_DOUBLE.bits();
        final LambdaJ intp = new LambdaJ(features, LambdaJ.TraceLevel.TRC_NONE /*TRC_ENV*/, null);

        System.out.println("***** running program '" + fileName + "':");
        System.out.println("-------------------------------------------------------");
        System.out.println(prog);
        System.out.println("-------------------------------------------------------");

        final String actualResult;
        if (fileName.endsWith(".lisp")) {
            actualResult = sexp(intp.interpretExpressions(new StringReader(prog)::read, () -> -1, out::append));
        } else {
            actualResult = sexp(intp.interpretExpression(new StringReader(prog)::read, out::append));
        }

        System.out.println("***** done program, result: " + actualResult);
        System.out.println();

        assertEquals("program " + fileName + " produced unexpected result", expectedResult, actualResult);

        if (expectedOutput == null) {
            assertEquals("program " + fileName + " produced unexpected output", 0, out.length());
        } else {
            final String outputStr = out.toString();
            assertEquals("program " + fileName + " produced unexpected output", expectedOutput, outputStr);
        }
    }
}
