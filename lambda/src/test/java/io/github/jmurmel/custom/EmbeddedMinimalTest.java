package io.github.jmurmel.custom;

import java.io.StringReader;

import static org.junit.Assert.*;

import io.github.jmurmel.LambdaJ;
import org.junit.Test;

public class EmbeddedMinimalTest {

    @Test
    public void testMinimal() {
        Object result = new LambdaJ().interpretExpression(new StringReader("\"Hello, World!\"")::read, s -> {});
        assertEquals("Hello, World!", result.toString());
    }
}
