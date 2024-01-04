package io.github.jmurmel;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ArraySliceTest {

    @Test
    public void testFor() {
        final Object[] numbers = new Object[] { 1, 2, 3, 4, 5 };
        final LambdaJ.ConsCell slice = LambdaJ.arraySlice(numbers, 2);
        int sum = 0;
        for (Object o: slice) {
            sum += (Integer)o;
        }
        assertEquals(sum, 12);
    }
}
