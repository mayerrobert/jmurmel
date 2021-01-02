package com.robertmayer.lambdaj;

import java.io.StringReader;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static com.robertmayer.lambdaj.TestUtils.sexp;

public class BackquoteTest {

    @Test
    public void testNumber() {
        test("1", "1");
    }

    @Test
    public void testSymbol() {
        test("aaa", "aaa");
    }

    @Test
    public void testList() {
        test("(aaa bbb ccc)", "(aaa bbb ccc)");
    }



    @Test
    public void testQuotedNumber() {
        test("'1", "(quote 1)");
    }

    @Test
    public void testQuotedSymbol() {
        test("'aaa", "(quote aaa)");
    }

    @Test
    public void testQuotedList() {
        test("'(aaa bbb ccc)", "(quote (aaa bbb ccc))");
    }

    @Test
    public void testListQuotedAtoms() {
        test("('aaa bbb 'ccc)", "((quote aaa) bbb (quote ccc))");
    }

    @Test
    public void testListQuotedList() {
        test("('aaa bbb '(ccc ddd))", "((quote aaa) bbb (quote (ccc ddd)))");
    }

    @Test
    public void testQuotedListQuotedList() {
        test("'('aaa bbb '(ccc ddd))", "(quote ((quote aaa) bbb (quote (ccc ddd))))");
    }



    @Test
    public void testBQuotedSymbol() {
        test("`aaa", "(quote aaa)");
    }

    @Test
    public void testBQuotedUnquotedSymbol() {
        test("`,aaa", "aaa");
    }



    @Test
    public void testBQuotedList() {
        test("`(aaa bbb ccc)", "(cons (quote aaa) (cons (quote bbb) (cons (quote ccc) nil)))");
    }

    @Test
    public void testBQuotedListSlicedList() {
        test("`(a ,@l b)", "(cons (quote a) (append l (cons (quote b) nil)))");
    }

    // sample from CLHS
    // http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm
    // Murmel: (define a "A")       (define c "C")       (define d '("D" "DD"))       (eval (macroexpand-1 '`((,a b) ,c ,@d))) ==> (("A" b) "C" "D" "DD")
    // CL:     (defparameter a "A") (defparameter c "C") (defparameter d '("D" "DD")) (eval (macroexpand-1 '`((,a b) ,c ,@d))) ==> (("A" B) "C" "D" "DD")
    @Test
    public void testCHLSBackQuote() {
        test("`((,a b) ,c ,@d)", "(cons (cons a (cons (quote b) nil)) (cons c (append d nil)))");
    }



    private void test(String expression, String expectedExpansion) {
        final Object expanded = expand(expression);
        final String expandedSexp = sexp(expanded);
        assertEquals(expectedExpansion, expandedSexp);
    }

    private Object expand(String s) {
        final LambdaJ intp = new LambdaJ();
        intp.init(new StringReader(s)::read, x -> { return; });
        final LambdaJ.ObjectReader reader = intp.getLispReader();
        Object o = reader.readObj();
        return o;
    }
}
