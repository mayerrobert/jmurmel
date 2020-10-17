package com.robertmayer.lambdaj;

import org.junit.Test;

public class ClosureTest {

    // this will fail when primitives are added/ changed
    //@Test
    public void defineClosure() {
        LambdaJTest.runTest("defineLambda", "(define x (lambda (arg1 arg2) (body1 1) (body2 2)))",
                "(lambda ((arg1 arg2) (body1 1.0) (body2 2.0)) (nil) (x lambda ((arg1 arg2) (body1 1.0) (body2 2.0)) (nil) #<this list> (cdr . #<primitive>) (car . #<primitive>) (cons . #<primitive>) (eq . #<primitive>) (+ . #<primitive>) (- . #<primitive>) (* . #<primitive>) (/ . #<primitive>) (= . #<primitive>) (> . #<primitive>) (>= . #<primitive>) (< . #<primitive>) (<= . #<primitive>) (mod . #<primitive>) (numberp . #<primitive>) (atom . #<primitive>) (consp . #<primitive>) (symbolp . #<primitive>) (listp . #<primitive>) (null? . #<primitive>) (assoc . #<primitive>) (nil) (t . t) (stringp . #<primitive>) (string-format . #<primitive>) (string-format-locale . #<primitive>) (read . #<primitive>) (write . #<primitive>) (writeln . #<primitive>)) (cdr . #<primitive>) (car . #<primitive>) (cons . #<primitive>) (eq . #<primitive>) (+ . #<primitive>) (- . #<primitive>) (* . #<primitive>) (/ . #<primitive>) (= . #<primitive>) (> . #<primitive>) (>= . #<primitive>) (< . #<primitive>) (<= . #<primitive>) (mod . #<primitive>) (numberp . #<primitive>) (atom . #<primitive>) (consp . #<primitive>) (symbolp . #<primitive>) (listp . #<primitive>) (null? . #<primitive>) (assoc . #<primitive>) (nil) (t . t) (stringp . #<primitive>) (string-format . #<primitive>) (string-format-locale . #<primitive>) (read . #<primitive>) (write . #<primitive>) (writeln . #<primitive>))",
                null);
    }

    @Test
    public void defineAndInvokeClosure() {
        LambdaJTest.runTest("defineLambda.lisp", "(define x (lambda (p1 p2) (write 1) (write 2))) (x 3 4)",
                "t", "1.02.0");
    }
}
