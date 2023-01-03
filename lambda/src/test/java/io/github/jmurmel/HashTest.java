package io.github.jmurmel;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigInteger;

import static io.github.jmurmel.LambdaJ.sxhash;

public class HashTest {

    @Test
    public void testSxhashString() {
        final int stringHash = sxhash("abc");
        Assert.assertEquals(sxhash(new StringBuilder("abc")), stringHash);
        Assert.assertEquals(sxhash(new StringBuffer("abc")), stringHash);
        Assert.assertEquals(sxhash("abc".toCharArray()), stringHash);
    }

    @Test
    public void testSxhashList() {
        final LambdaJ.ConsCell list = (LambdaJ.ConsCell)new LambdaJ.ListBuilder().appendElements(1, 2, 3, 4, 5).first();
        final LambdaJ.ConsCell arry = LambdaJ.arraySlice(1, 2, 3, 4, 5);
        final LambdaJ.ConsCell mixed = (LambdaJ.ConsCell)new LambdaJ.ListBuilder()
                                                         .appendElements(1, 2, 3)
                                                         .appendLast(LambdaJ.arraySlice(4, 5)).first();    

        Assert.assertEquals(sxhash(list), sxhash(arry));
        Assert.assertEquals(sxhash(list), sxhash(mixed));
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
        Assert.assertEquals(sxhash((int)-97), byteHash);
        Assert.assertEquals(sxhash((long)-97), byteHash);
        Assert.assertEquals(sxhash(new BigInteger("-97")), byteHash);
    }
}
