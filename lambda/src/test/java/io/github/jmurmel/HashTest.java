package io.github.jmurmel;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigInteger;

import static io.github.jmurmel.LambdaJ.Subr.sxhash;

public class HashTest {

    @Test
    public void testString() {
        final int stringHash = sxhash("abc");
        Assert.assertEquals(sxhash(new StringBuilder("abc")), stringHash);
        Assert.assertEquals(sxhash(new StringBuffer("abc")), stringHash);
        Assert.assertEquals(sxhash("abc".toCharArray()), stringHash);
    }

    @Test
    public void testList() {
        final LambdaJ.ConsCell list = (LambdaJ.ConsCell)new LambdaJ.ListBuilder().appendElements(1, 2, 3, 4, 5, 6).first();
        final LambdaJ.ConsCell arry = LambdaJ.arraySlice(1, 2, 3, 4, 5, 6);
        final LambdaJ.ConsCell mixed = (LambdaJ.ConsCell)new LambdaJ.ListBuilder()
                                                         .appendElements(1, 2, 3)
                                                         .appendLast(LambdaJ.arraySlice(4, 5, 6)).first();

        final int sxhashList = sxhash(list);
        final int sxhashArry = sxhash(arry);
        Assert.assertEquals(sxhashList, sxhashArry);

        final int sxhashMixed = sxhash(mixed);
        Assert.assertEquals(sxhashList, sxhashMixed);
    }

    @Test
    public void testEmptyList() {
        final LambdaJ.ConsCell list = null;
        final LambdaJ.ConsCell arry = LambdaJ.arraySlice();

        final int sxhashList = sxhash(list);
        final int sxhashArry = sxhash(arry);
        Assert.assertEquals(sxhashList, sxhashArry);
    }

    @Test
    public void testDottedList() {
        final LambdaJ.ConsCell list = (LambdaJ.ConsCell)new LambdaJ.ListBuilder().appendElements(1, 2).appendLast(3).first();;

        Assert.assertNotEquals(sxhash(list), 0); // make sure it doesn't crash with a CCE or something
    }

    @Test
    public void testInteger() {
        final int byteHash = sxhash((byte)97);

        Assert.assertEquals(sxhash((short)97), byteHash);
        Assert.assertEquals(sxhash((int)97), byteHash);
        Assert.assertEquals(sxhash((long)97), byteHash);
        Assert.assertEquals(sxhash(new BigInteger("97")), byteHash);
    }

    @Test
    public void testNegInteger() {
        final int byteHash = sxhash((byte)-97);

        Assert.assertEquals(sxhash((short)-97), byteHash);
        Assert.assertTrue(sxhash((short)-97) > 0);

        Assert.assertEquals(sxhash((int)-97), byteHash);
        Assert.assertTrue(sxhash((int)-97) > 0);

        Assert.assertEquals(sxhash((long)-97), byteHash);

        Assert.assertEquals(sxhash(new BigInteger("-97")), byteHash);
    }

    @Test
    public void testBitvector() {
        final boolean[] bv = new boolean[] { false, true, true, false };
        final int bvhash = sxhash(bv);
        Assert.assertEquals(sxhash(LambdaJ.Subr.Bitvector.of(bv)), bvhash);
    }
}
