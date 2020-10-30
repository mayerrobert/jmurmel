package com.robertmayer.lambdaj.custom;

import java.io.StringReader;

import static junit.framework.Assert.*;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;

public class EmbeddedMinimalTest {

    @Test
    public void testMinimal() {
        Object result = new LambdaJ().interpretExpression(new StringReader("\"Hello, World!\"")::read, s -> { return; });
        assertEquals("Hello, World!", result.toString());
    }
}
