package io.github.jmurmel;

import org.junit.Assert;
import org.junit.Test;

public class FormattableTest {

    private static final String DEF = "%3d sym";

    private static void test(String fmt, String s) {
        Assert.assertEquals(String.format(fmt, s),
                            String.format(fmt, new LambdaJ.LambdaJSymbol(s)));
    }

    private static void test(String fmt) {
        test(fmt, DEF);
    }

    @Test
    public void testSimple() {
        test("%s");
        test("%S");
    }

    @Test
    public void testWidth() {
        test("%2s");
        test("%-1s");
        test("%-2s");
        test("%1s", "");
        test("%1s", "x");
    }

    @Test
    public void testWidthRight() {
        test("%6s");
        test("%7s");
        test("%8s");
        test("%10s");
    }

    @Test
    public void testWidthLeft() {
        test("%-6s");
        test("%-7s");
        test("%-8s");
        test("%-10s");
    }

    @Test
    public void testPrecision() {
        test("%.10s");
    }

    @Test
    public void testPrecisionTruncate() {
        final String s = String.format("%.5s", new LambdaJ.LambdaJSymbol("%3d sym"));
        Assert.assertEquals("%3...", s);

        final String s2 = String.format("%.6s", new LambdaJ.LambdaJSymbol("%3d sym"));
        Assert.assertEquals("%3d...", s2);

        final String s3 = String.format("%.7s", new LambdaJ.LambdaJSymbol("%3d sym"));
        Assert.assertEquals("%3d sym", s3);
    }

    @Test
    public void testAlternate() {
        final String s = String.format("%#s", new LambdaJ.LambdaJSymbol("%3d sym"));
        Assert.assertEquals("#:|%3d sym|", s);
    }
}
