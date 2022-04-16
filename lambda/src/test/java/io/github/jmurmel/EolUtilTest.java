package io.github.jmurmel;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class EolUtilTest {

    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
        { null, null },
        { "", "" },
        { "bla", "bla" },

        { "\r",   "\n" },
        { "\n",   "\n" },
        { "\r\n", "\n" },

        { "hello\rworld",   "hello\nworld" },
        { "hello\nworld",   "hello\nworld" },
        { "hello\r\nworld", "hello\nworld" },

        { "hello world\r",   "hello world\n" },
        { "hello world\n",   "hello world\n" },
        { "hello world\r\n", "hello world\n" },

        { "\rhello world\r",     "\nhello world\n" },
        { "\nhello world\n",     "\nhello world\n" },
        { "\r\nhello world\r\n", "\nhello world\n" },
        };
    }

    @Test(dataProvider = "test")
    public void testEolUtil(String arg, String expected) {
        assertEquals(EolUtil.anyToUnixEol(arg), expected);
    }
}
