package io.github.jmurmel;

import org.junit.Test;

public class ApplyTest {

    @Test
    public void primitiveOneArg() {
        LambdaJTest.runTest("primitiveOneArg", "(null nil)", "t", null);
    }

    @Test
    public void primitiveOneArgApply() {
        LambdaJTest.runTest("primitiveOneArg", "(apply null '(nil))", "t", null);
    }

    @Test
    public void primitiveOneArgApplyError() {
        LambdaJTest.runErrorTest("primitiveOneArg", "(apply null 'a)",
                "apply: malformed apply: expected a list but got a");
    }

    @Test
    public void lambdaOneArgApply() {
        LambdaJTest.runTest("primitiveOneArg", "(apply (lambda (x) (null x)) '(nil))", "t", null);
    }

    @Test
    public void lambdaOneArgApplyError() {
        LambdaJTest.runErrorTest("lambdaOneArg", "(apply (lambda (x) (null x)) 'a)",
                "apply: malformed apply: expected a list but got a");
    }


    @Test
    public void primitiveTwoArgs() {
        LambdaJTest.runTest("primitiveTwoArgs", "(cons 1 2)", "(1.0 . 2.0)", null);
    }

    @Test
    public void primitiveTwoArgsApply() {
        LambdaJTest.runTest("primitiveTwoArgs", "(apply cons '(1 2))", "(1.0 . 2.0)", null);
    }


    @Test
    public void primitiveThreeArgs() {
        LambdaJTest.runTest("primitiveThreeArgs", "(+ 1 2 3)", "6.0", null);
    }

    @Test
    public void primitiveThreeArgsApply() {
        LambdaJTest.runTest("primitiveThreeArgs", "(apply + '(1 2 3))", "6.0", null);
    }

    @Test
    public void primitiveThreeExpApplyError() {
        LambdaJTest.runErrorTest("primitiveThreeArgs", "(apply + '((+ 1 1) (+ 2 2) 3))",
                                 "+: expected a number argument but got (+ 1.0 1.0)");
    }

    @Test
    public void primitiveThreeExpApply() {
        LambdaJTest.runTest("primitiveThreeArgs", "(apply + (cons (+ 1 1) (cons (+ 2 2) (cons 3 ()))))", "9.0", null);
    }

    @Test
    public void notASymbol() {
        LambdaJTest.runErrorTest("String", "(apply xyxxy '(1 2))", "eval: 'xyxxy' is not bound");
    }

    @Test
    public void cannotApplyNil() {
        LambdaJTest.runErrorTest("String", "(apply nil '(1 2))", "function application: not a primitive or lambda: nil");
    }

    @Test
    public void cannotAddCons() {
        LambdaJTest.runErrorTest("String", "(apply + (cons 2 3))", "+: expected a proper list of numbers but got (2.0 . 3.0)");
    }


    @Test
    public void lambdaZeroArgs() {
        LambdaJTest.runTest("lambdaZeroArgs", "(write (apply ((lambda () (lambda () (quote (a b c))))) ()))", "(a b c)", "(a b c)");
        LambdaJTest.runTest("lambdaZeroArgs", "(apply ((lambda () (lambda () (quote (a b c))))) ())", "(a b c)", null);
    }


    @Test
    public void lambdaExtraArgs() {
        LambdaJTest.runErrorTest("lambdaExtraArgs", "(apply (lambda (a b) (writeln a) (writeln b)) '(1 2 3))",
                                 "function application: too many arguments. Remaining arguments: (3.0)");
    }

    @Test
    public void hello() {
        LambdaJTest.runTest("String", "(write (quote HELLO))", "HELLO", "HELLO");
    }

    @Test
    public void helloApply() {
        LambdaJTest.runTest("String", "(apply write (cons (quote HELLO) nil))", "HELLO", "HELLO");
    }

    @Test
    public void helloApply2() {
        LambdaJTest.runTest("String", "(apply write (cons 'HELLO nil))", "HELLO", "HELLO");
    }

    @Test
    public void helloApply3() {
        LambdaJTest.runTest("String", "(apply write (cons 1 nil))", "1.0", "1.0");
    }



    @Test
    public void applyEval() {
        LambdaJTest.runTest("String", "(apply eval '(1))", "1.0", null);
    }

    @Test
    public void applyEvalAdd() {
        LambdaJTest.runTest("String", "(apply eval (list (quote (+ 1 2))))", "3.0", null);
    }

    @Test
    public void evalEnv() {
        LambdaJTest.runTest("String", "(eval 'x (list '(x . 2)))", "2.0", null);
    }

    @Test
    public void evalEnvError() {
        LambdaJTest.runErrorTest("String", "(eval 'x '2)", "eval: malformed eval: expected 'env' to be a list but got 2.0");
    }



    @Test
    public void applyApply() {
        LambdaJTest.runTest("String", "(apply (apply (lambda (a b) (lambda () nil)) '(1 2)) '())", "nil", null);
    }

    @Test
    public void x() {
        LambdaJTest.runTest("String", "(apply (apply (lambda (a b) (lambda nil nil)) '(1 2)) '())",
                            "nil", null);
    }

    @Test
    public void y() {
        LambdaJTest.runErrorTest("String", "(apply (apply (lambda (a b) (lambda () nil)) '(1 2)) 'a)",
                                 "apply: malformed apply: expected a list but got a");
    }
}
