package com.robertmayer.lambdaj;

import org.junit.Test;

public class ApplyTest {

    @Test
    public void primitiveOneArg() {
        LambdaJTest.runTest("primitiveOneArg", "(null? nil)", "t", null);
    }

    @Test
    public void primitiveOneArgApply() {
        LambdaJTest.runTest("primitiveOneArg", "(apply null? '(nil))", "nil", null);
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
    public void primitiveThreeExpApply() {
        LambdaJTest.runErrorTest("primitiveThreeArgs", "(apply + '((+ 1 1) (+ 2 2) 3))", "+: expected only number arguments but got ((+ 1.0 1.0) (+ 2.0 2.0) 3.0)");
    }



    @Test
    public void notASymbol() {
        LambdaJTest.runErrorTest("String", "(apply xyxxy '(1 2))", "'xyxxy' is undefined");
    }

    @Test
    public void cannotApplyNil() {
        LambdaJTest.runErrorTest("String", "(apply nil '(1 2))", "apply: cannot apply function nil");
    }

    @Test
    public void cannotAddCons() {
        LambdaJTest.runErrorTest("String", "(apply + (cons 2 3))", "+: expected only number arguments but got (2.0 . 3.0)");
    }


    @Test
    public void lambdaZeroArgs() {
        LambdaJTest.runTest("lambdaZeroArgs", "(write (apply ((lambda () (lambda () (quote (a b c))))) ()))", "t", "(a b c)");
        LambdaJTest.runTest("lambdaZeroArgs", "(apply ((lambda () (lambda () (quote (a b c))))) ())", "(a b c)", null);
    }


    // todo could make this work by passing extra args as a list aka variadic...
    @Test
    public void lambdaExtraArgs() {
        LambdaJTest.runErrorTest("lambdaExtraArgs", "(apply (lambda (a b) (writeln a) (writeln b)) '(1 2 3))",
                                 "lambda: too many arguments. remaining arguments: (3.0)");
    }

    @Test
    public void hello() {
        LambdaJTest.runTest("String", "(write (quote HELLO))", "t", "HELLO");
    }

    @Test
    public void helloApply() {
        LambdaJTest.runTest("String", "(apply write (cons (quote HELLO) nil))", "t", "HELLO");
    }

    @Test
    public void helloApply2() {
        LambdaJTest.runTest("String", "(apply write (cons 'HELLO nil))", "t", "HELLO");
    }

    @Test
    public void helloApply3() {
        LambdaJTest.runTest("String", "(apply write (cons 1 nil))", "t", "1.0");
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
                                 "lambda invocation: expected an argument list but got a");
    }
}
