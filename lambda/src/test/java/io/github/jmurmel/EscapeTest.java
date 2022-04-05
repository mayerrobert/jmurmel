package io.github.jmurmel;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import static io.github.jmurmel.LambdaJ.ConsCell.cons;

public class EscapeTest {

    @Test
    public void testStringDQuote() {
        // create a list that contains the string literal he"lo
        LambdaJ.ConsCell cell = cons("he\"lo", null);

        // ConsCell#toString() should return (he"lo)
        assertEquals("(he\"lo)", cell.toString());

        // SExpressionWriter#printObj() should write ("he\"lo")
        StringBuilder sExp = new StringBuilder();
        LambdaJ.makeWriter(sExp::append).printObj(cell);
        assertEquals("(\"he\\\"lo\")", sExp.toString());
    }

    @Test
    public void testStringBSlash() {
        // create a list that contains the string literal he\lo
        LambdaJ.ConsCell cell = cons("he\\lo", null);

        // ConsCell#toString() should return (he\lo)
        assertEquals("(he\\lo)", cell.toString());

        // SExpressionWriter#printObj() should write ("he\\lo")
        StringBuilder sExp = new StringBuilder();
        LambdaJ.makeWriter(sExp::append).printObj(cell);
        assertEquals("(\"he\\\\lo\")", sExp.toString());
    }

    @Test
    public void testSymbolSpace() {
        // create a list that contains a symbol with a space
        LambdaJ.ConsCell cell = cons(new LambdaJ.LambdaJSymbol("he lo"), null);

        // ConsCell#toString() should return (he lo)
        assertEquals("(he lo)", cell.toString());

        // SExpressionWriter#printObj() should write (|he lo|)
        StringBuilder sExp = new StringBuilder();
        LambdaJ.makeWriter(sExp::append).printObj(cell);
        assertEquals("(|he lo|)", sExp.toString());
    }

    @Test
    public void testSymbolParen() {
        // create a list that contains a symbol with a (
        LambdaJ.ConsCell cell = cons(new LambdaJ.LambdaJSymbol("he(lo"), null);

        // ConsCell#toString() should return (he(lo)
        assertEquals("(he(lo)", cell.toString());

        // SExpressionWriter#printObj() should write (|he(lo|)
        StringBuilder sExp = new StringBuilder();
        LambdaJ.makeWriter(sExp::append).printObj(cell);
        assertEquals("(|he(lo|)", sExp.toString());
    }
}
