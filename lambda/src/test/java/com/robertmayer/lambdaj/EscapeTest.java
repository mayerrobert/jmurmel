package com.robertmayer.lambdaj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import com.robertmayer.lambdaj.LambdaJ.ConsCell;

public class EscapeTest {

    @Test
    public void testStringDQuote() {
        // create a list that contains the string literal he"lo
        LambdaJ.ConsCell cell = ConsCell.cons("he\"lo", null);

        // ConsCell#toString() should return (he"lo)
        assertEquals("(he\"lo)", cell.toString());

        // SExpressionWriter#printObj() should write ("he\"lo")
        StringBuilder sExp = new StringBuilder();
        new LambdaJ.SExpressionWriter(sExp::append).printObj(cell);
        assertEquals("(\"he\\\"lo\")", sExp.toString());
    }

    @Test
    public void testStringBSlash() {
        // create a list that contains the string literal he\lo
        LambdaJ.ConsCell cell = ConsCell.cons("he\\lo", null);

        // ConsCell#toString() should return (he\lo)
        assertEquals("(he\\lo)", cell.toString());

        // SExpressionWriter#printObj() should write ("he\\lo")
        StringBuilder sExp = new StringBuilder();
        new LambdaJ.SExpressionWriter(sExp::append).printObj(cell);
        assertEquals("(\"he\\\\lo\")", sExp.toString());
    }

    @Test // todo
    public void testSymbolSpace() {
        // create a list that contains a symbol with a space
        LambdaJ.ConsCell cell = ConsCell.cons(new LambdaJ.LambdaJSymbol("he lo"), null);

        // ConsCell#toString() should return (he lo)
        assertEquals("(he lo)", cell.toString());

        // SExpressionWriter#printObj() should write (|he lo|)
        StringBuilder sExp = new StringBuilder();
        new LambdaJ.SExpressionWriter(sExp::append).printObj(cell);
        assertEquals("(|he lo|)", sExp.toString());
    }

    @Test
    public void testSymbolParen() {
        // create a list that contains a symbol with a (
        LambdaJ.ConsCell cell = ConsCell.cons(new LambdaJ.LambdaJSymbol("he(lo"), null);

        // ConsCell#toString() should return (he(lo)
        assertEquals("(he(lo)", cell.toString());

        // SExpressionWriter#printObj() should write (|he(lo|)
        StringBuilder sExp = new StringBuilder();
        new LambdaJ.SExpressionWriter(sExp::append).printObj(cell);
        assertEquals("(|he(lo|)", sExp.toString());
    }
}
