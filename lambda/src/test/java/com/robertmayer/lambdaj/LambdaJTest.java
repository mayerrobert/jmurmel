package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Assert;
import org.junit.Test;

public class LambdaJTest {

    private static final String[][] tests = {
        /*  0 */ { "write", "#<primitive>", null },

        // application of builtins
        /*  1 */ { "(write (quote (Hello, world!)))", "t", "(Hello, world!)" },
        /*  2 */ { "(write (quote HELLO))", "t", "HELLO" },
        /*  3 */ { "(cons (quote HELLO) (quote HELLO))", "(HELLO . HELLO)", null },
        /*  4 */ { "(assoc (quote write) ())", "nil", null },
        /*  5 */ { "(assoc (quote b) (cons (cons (quote a) (quote a1)) (cons (cons (quote b) (quote b2)) (cons (cons (quote c) (quote c3)) ()))))", "(b . b2)", null },
        /*  6 */ { "(write (car (quote (v1 v2))))", "t", "v1" },
        /*  7 */ { "(write (cdr (quote (v1 v2))))", "t", "(v2)" },
        /*  8 */ { "(write (car (cons (quote v1) (quote v2))))", "t", "v1" },
        /*  9 */ { "(write (cdr (cons (quote v1) (quote v2))))", "t", "v2" },

        // comments
        /* 10 */ { "; comment\n(write (quote HELLO))", "t", "HELLO" },
        /* 11 */ { "; comment\n(write (quote HELLO)) ; comment", "t", "HELLO" },

        // quoted chars
        /* 12 */ { "(write (quote HELLO\\ ))", "t", "|HELLO |" },
        /* 13 */ { "(write (quote HELLO\\\\))", "t", "HELLO\\" },
        /* 14 */ { "(write (quote HELLO\\)))", "t", "|HELLO)|" },
        /* 15 */ { "(write (quote HELLO\\;))", "t", "HELLO;" },

        // apply
        /* 16 */ { "(apply write (cons (quote HELLO) nil))", "t", "HELLO" },
        /* 17 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) nil)) nil))", "t", "(HELLO HELLO)" },
        /* 18 */ { "(apply write (cons (cons (quote HELLO) (cons (quote HELLO) ())) ()))", "t", "(HELLO HELLO)" },
        /* 19 */ { "(apply write (cons (cons (quote HELLO) (quote HELLO)) nil))", "t", "(HELLO . HELLO)" },

        // lambda
        /* 20 */ { "(lambda () (write (quote noparam)))", "(lambda nil (write (quote noparam)))", null },
        /* 21 */ { "(write (lambda () (write (quote noparam))))", "t", "(lambda nil (write (quote noparam)))" },
        /* 22 */ { "((lambda () (write (quote noparam))))", "t", "noparam" },
        /* 23 */ { "((lambda (x) (write x)) (quote hello))", "t", "hello" },
        /* 24 */ { "((lambda (x y) (write (cons x y))) (quote p1) (quote p2))", "t", "(p1 . p2)" },
        /* 25 */ { "(write ((lambda () (write (quote s1)) (write (quote s2)))))", "t", "s1s2t" },

        // eq
        /* 26 */ { "(write ((lambda () (eq (quote s1) (quote s2)))))", "t", "nil" },
        /* 27 */ { "(write ((lambda () (eq (quote s1) (quote s1)))))", "t", "t" },

        // cond
        /* 28 */ { "((lambda (x) (cond ((eq x (quote s1)) (write (quote s1))) ((eq x (quote s2)) (write (quote s2))) ((eq x (quote s3)) (write (quote s3))))) (quote s3))", "t", "s3" },
        /* 29 */ { "((lambda (x) (cond ((eq x (quote s1)) (write (quote s1))) ((eq x (quote s2)) (write (quote s2))) ((eq x (quote s3)) (write (quote s3))))) (quote s4))", "nil", null },

        /* 30 */ { "(if (eq (quote a) (quote b)) (quote a))", "nil", null },
        /* 30 */ { "(if (eq (quote a) (quote a)) (quote a))", "a", null },
        /* 30 */ { "(if (eq (quote a) (quote b)) (quote a) (quote b))", "b", null },
        /* 30 */ { "(if (eq (quote a) (quote a)) (quote a) (quote b))", "a", null },

        // labels
        /* 30 */ { "(labels () (write (quote s1)) (write (quote s2)))", "t", "s1s2" },
        /* 31 */ { "(labels ((w1 (x) (write (cons (quote s1) x))) (w2 (x) (write (cons (quote s2) x)))) (w1 (quote s3)) (w2 (quote s4)))", "t", "(s1 . s3)(s2 . s4)" },

        // numbers, numeric operators
        /* 32 */ { "1", "1.0", null },
        /* 33 */ { "(write 1)", "t", "1.0" },

        /* 34 */ { "(= 2 2)", "t", null },
        /* 35 */ { "(= 2 3)", "nil", null },

        /* 36 */ { "(< 2 3)", "t", null },
        /* 37 */ { "(< 2 2)", "nil", null },

        /* 38 */ { "(<= 3 3)", "t", null },
        /* 39 */ { "(<= 4 2)", "nil", null },

        /* 40 */ { "(> 4 3)", "t", null },
        /* 41 */ { "(> 2 2)", "nil", null },

        /* 42 */ { "(>= 3 3)", "t", null },
        /* 43 */ { "(>= 3 4)", "nil", null },

        /* 44 */ { "(+)", "0.0", null },
        /* 45 */ { "(+ 3)", "3.0", null },
        /* 46 */ { "(+ 3 4)", "7.0", null },
        /* 47 */ { "(+ 3 4 5)", "12.0", null },

        /* 48 */ { "(- 3)", "-3.0", null },
        /* 49 */ { "(- 3 4)", "-1.0", null },
        /* 50 */ { "(- 3 4 5)", "-6.0", null },

        /* 51 */ { "(*)", "1.0", null },
        /* 52 */ { "(* 3)", "3.0", null },
        /* 53 */ { "(* 3 4)", "12.0", null },
        /* 54 */ { "(* 3 4 5)", "60.0", null },

        /* 55 */ { "(/ 5)", "0.2", null },
        /* 56 */ { "(/ 12 2)", "6.0", null },
        /* 57 */ { "(/ 12 2 3)", "2.0", null },

        /* 58 */ { "(mod 12 5)", "2.0", null },

        /* 59 */ { "(atom 1)",              "t", null },
        /* 60 */ { "(atom (quote x))",      "t", null },
        /* 61 */ { "(atom ())",             "t", null },
        /* 62 */ { "(atom nil)",            "t", null },
        /* 63 */ { "(atom (quote (a)))",    "nil", null },

        /* 64 */ { "(symbolp 1)",           "nil", null },
        /* 65 */ { "(symbolp (quote x))",   "t", null },
        /* 66 */ { "(symbolp ())",          "t", null },
        /* 67 */ { "(symbolp nil)",         "t", null },
        /* 68 */ { "(symbolp (quote (a)))", "nil", null },

        /* 69 */ { "(consp 1)",             "nil", null },
        /* 70 */ { "(consp (quote x))",     "nil", null },
        /* 71 */ { "(consp ())",            "nil", null },
        /* 72 */ { "(consp nil)",           "nil", null },
        /* 73 */ { "(consp (quote (a)))",   "t", null },

        /* 74 */ { "(listp 1)",             "nil", null },
        /* 75 */ { "(listp (quote x))",     "nil", null },
        /* 76 */ { "(listp ())",            "t", null },
        /* 77 */ { "(listp nil)",           "t", null },
        /* 78 */ { "(listp (quote (a)))",   "t", null },

        /* 79 */ { "(numberp 1)",           "t", null },
        /* 80 */ { "(numberp (quote x))",   "nil", null },
        /* 81 */ { "(numberp ())",          "nil", null },
        /* 82 */ { "(numberp nil)",         "nil", null },
        /* 83 */ { "(numberp (quote (a)))", "nil", null },

        /* https://graham.main.nc.us/~bhammel/graham/lisp.html

        It is a convention, and perhaps a logically infelicitous one, of most LISP interpreters
        to equate the empty list with a selfevaluating atom called "nil". Thus,
        */

        /* 83 */ { "(cdr (cdr (cdr (quote (a (b c) (d (e f)))))))", "nil", null },
        /* 83 */ { "()", "nil", null },
        /* 83 */ { "(quote ())", "nil", null },
        /* 83 */ { "nil", "nil", null },
        /* 84 */ { "t", "t", null },

        /*
        The values that atom returns sets the stage for the two internally defined selfevaluating
        boolean values "nil" and "t".
        Examples:

        -> nil
           nil

        -> t
           t

        -> (atom ())
           t

        -> (atom ())
           t

        -> (atom nil)
           t

        -> (atom t)
           t

        -> (atom (quote a))
           t

        -> (atom (quote (a b)))
           nil
         */

        /* 85 */ { "(define *xyxxy* (lambda () ()))", "(lambda nil nil)", null },
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
        System.out.println("cwd: " + cwd.toString());
        Path lispDir = Paths.get("src", "test", "lisp");
        Files.walk(lispDir).filter(path -> path.toString()
                .endsWith(".lisp")).forEach(path -> runTest(path));
    }



    static void runTest(int n) {
        String[] test = tests[n];
        String prog = test[0];
        String expectedResult = test[1];
        String expectedOutput = test[2];

        try {
            runTest("LambdaJTest.tests[" + n + "]", prog, expectedResult, expectedOutput);
        } catch (Exception e) {
            Assert.fail("LambdaJTest.tests[" + n + "]: " + prog + " threw Exception " + e.getMessage());
        }
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
                catch (LambdaJ.LambdaJError e) {
                    if (expectedError != null) {
                        if (e.getMessage().contains(expectedError)) {
                            // thats fine
                        }
                        else {
                            Assert.fail(fileName.toString() + ": expected error \"" + expectedError + '\"'
                                    + " but got error \"" + e.getMessage() + '\"');
                        }
                    } else {
                        Assert.fail(fileName.toString() + " threw exception " + e.getMessage());
                    }
                }
            }
            else {
                System.out.println("***** skipping " + fileName.toString());
        }
        } catch (LambdaJ.LambdaJError e) {
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
        if ("(empty)".equals(s)) return "";
        return s.replaceAll("\\\\n", "\n");
    }

    static void runErrorTest(String fileName, String prog, String expectedExceptionMsgPfx) {
        try {
            LambdaJTest.runTest(fileName, prog, "ignored", "ignored");
            fail("was expecting error: " + expectedExceptionMsgPfx);
        }
        catch (LambdaJ.LambdaJError e) {
            assertTrue("got wrong exception message: " + e.getMessage(), e.getMessage().startsWith(expectedExceptionMsgPfx));
        }
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
        StringBuffer out = new StringBuffer();

        LambdaJ interpreter = new LambdaJ();
        interpreter.trace = LambdaJ.TRC_EVAL;

        System.out.println("***** running program '" + fileName + ':');
        System.out.println("-------------------------------------------------------");
        System.out.println(prog);
        System.out.println("-------------------------------------------------------");

        String actualResult;
        if (fileName.endsWith(".lisp")) {
            actualResult = lispObjectToString(interpreter.interpretExpressions(new StringReader(prog)::read, () -> -1, out::append));
        } else {
            actualResult = lispObjectToString(interpreter.interpretExpression(new StringReader(prog)::read, out::append));
        }
        System.out.println("***** done program, result: " + actualResult);
        System.out.println();

        assertEquals("program " + fileName + " produced unexpected result", expectedResult, actualResult);

        if (expectedOutput == null) {
            assertEquals("program " + fileName + " produced unexpected output", 0, out.length());
        } else {
            final String outputStr = out.toString().replaceAll("\r", "");
            assertEquals("program " + fileName + " produced unexpected output", expectedOutput, outputStr);
        }
    }

    private static String lispObjectToString(Object exp) {
        /*
        if (exp == null) return "nil";
        if (exp instanceof LambdaJ.Primitive) return "#<primitive>";

        // else it's either a ConsCell which has an appropriate toString() method
        // or an Atom which really is a Java Object such as String (Lisp symbols) or Double (Lisp numbers)
        return exp.toString();
        */
        StringBuffer sExp = new StringBuffer();
        new LambdaJ.SExpressionWriter(sExp::append).printObj(exp);
        return sExp.toString();
    }
}
