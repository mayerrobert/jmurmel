package com.robertmayer.lambdaj;

import java.io.StringReader;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static com.robertmayer.lambdaj.TestUtils.sexp;

public class BackquoteTest {

    @Test
    public void testNumber() {
        expandOnce("1", "1");
    }

    @Test
    public void testSymbol() {
        expandOnce("aaa", "aaa");
    }

    @Test
    public void testList() {
        expandOnce("(aaa bbb ccc)", "(aaa bbb ccc)");
    }



    @Test
    public void testQuotedNumber() {
        expandOnce("'1", "(quote 1)");
    }

    @Test
    public void testQuotedSymbol() {
        expandOnce("'aaa", "(quote aaa)");
    }

    @Test
    public void testQuotedList() {
        expandOnce("'(aaa bbb ccc)", "(quote (aaa bbb ccc))");
    }

    @Test
    public void testListQuotedAtoms() {
        expandOnce("('aaa bbb 'ccc)", "((quote aaa) bbb (quote ccc))");
    }

    @Test
    public void testListQuotedList() {
        expandOnce("('aaa bbb '(ccc ddd))", "((quote aaa) bbb (quote (ccc ddd)))");
    }

    @Test
    public void testQuotedListQuotedList() {
        expandOnce("'('aaa bbb '(ccc ddd))", "(quote ((quote aaa) bbb (quote (ccc ddd))))");
    }



    @Test
    public void testBQuotedSymbol() {
        eval("`aaa", "aaa");
        expandOnce("`aaa", "(quote aaa)");
    }

    @Test
    public void testBQuotedQuotedSymbol() {
        eval("`'aaa", "(quote aaa)");
        expandOnce("`'aaa", "(cons (quote quote) (cons (quote aaa) nil))");
    }

    @Test
    public void testBQuotedUnquotedSymbol() {
        eval("(define aaa 'aval) `,aaa", "aval");
        expandOnce("`,aaa", "aaa");
    }



    @Test
    public void testBQuotedList() {
        eval("`(aaa bbb ccc)", "(aaa bbb ccc)");
        expandOnce("`(aaa bbb ccc)", "(cons (quote aaa) (cons (quote bbb) (cons (quote ccc) nil)))");
    }

    @Test
    public void testBQuotedDottedList() {
        eval("`(aaa bbb . ccc)", "(aaa bbb . ccc)");
        expandOnce("`(aaa bbb . ccc)", "(cons (quote aaa) (cons (quote bbb) (quote ccc)))");
    }

    @Test
    public void testBQuotedListSlicedList() {
        eval("(define l '(1.0 2.0)) `(a ,@l b)", "(a 1.0 2.0 b)");
        expandOnce("`(a ,@l b)", "(cons (quote a) (append l (cons (quote b) nil)))");
    }

    // sample from CLHS
    // http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm
    // Murmel: (define       a "A") (define       c "C") (define       d '("D" "DD"))   `((,a b) ,c ,@d)  ==> (("A" b) "C" "D" "DD")
    // CL:     (defparameter a "A") (defparameter c "C") (defparameter d '("D" "DD"))   `((,a b) ,c ,@d)  ==> (("A" B) "C" "D" "DD")
    @Test
    public void testCHLSBackQuote() {
        eval("(define a \"A\") (define c \"C\") (define d '(\"D\" \"DD\")) `((,a b) ,c ,@d)", "((\"A\" b) \"C\" \"D\" \"DD\")");
        expandOnce("`((,a b) ,c ,@d)", "(cons (cons a (cons (quote b) nil)) (cons c (append d nil)))");
    }

    @Test
    public void testCHLSMod() {
        eval("(define a \"A\") (define c \"C\") (define d '(\"D\" \"DD\")) `((,a b) ,@d ,c)", "((\"A\" b) \"D\" \"DD\" \"C\")");
        expandOnce("`((,a b) ,@d ,c)", "(cons (cons a (cons (quote b) nil)) (append d (cons c nil)))");
    }



//    @Test
//    public void testBBquotedSymbol() {
//        expandOnce("``aaa", "(quasiquote (cons (quote aaa) nil))");
//        //expandTwice("``aaa", "aaa");
//        eval("``aaa", "(quasiquote aaa)");
//    }

//    // ``(aaa ,bbb ,,ccc) =>
//    @Test
//    public void testX() {
//        expandOnce("``(aaa ,bbb ,,ccc)", "falsch (quasiquote (cons (cons (quote aaa) (cons (quasiquote (cons (quote bbb) nil)) (cons (quasiquote (cons ccc nil)) nil))) nil))");
//    }



    @Test
    public void errortestUnquote() {
        expandError("(,b)", "comma not inside a backquote");
    }

    @Test
    public void errortestUnquoteSplice() {
        expandError("`,@b", "can't splice here");
    }



    private void expandOnce(String expression, String expectedExpansion) {
        final Object expanded = expand(expression);
        final String expandedSexp = sexp(expanded);
        assertEquals(expectedExpansion, expandedSexp);
    }

//    private void expandTwice(String expression, String expectedExpansion) {
//        final Object expanded = expand(expression);
//        final Object expanded2 = expand(sexp(expanded));
//        final String expandedSexp = sexp(expanded2);
//        assertEquals(expectedExpansion, expandedSexp);
//    }

    private void expandError(String s, String expectedError) {
        try {
            expand(s);
            fail("expected error " + expectedError);
        }
        catch (LambdaJ.LambdaJError e) {
            if (expectedError != null)
                assertTrue("expected <" + expectedError + "> but got <" + e.getMessage() + '>', e.getMessage().startsWith(expectedError));
        }
    }

    private Object expand(String s) {
        final LambdaJ intp = new LambdaJ();
        intp.init(new StringReader(s)::read, x -> { return; });
        final LambdaJ.ObjectReader reader = intp.getLispReader();
        Object o = reader.readObj();
        return o;
    }

    private void eval(String exp, String expectedResult) {
        LambdaJTest.runTest("backquotetest.lisp", exp, expectedResult, null);
    }
}
