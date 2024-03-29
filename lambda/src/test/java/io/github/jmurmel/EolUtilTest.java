package io.github.jmurmel;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class EolUtilTest {

    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
        { null, "null" },
        { "", "" },
        { "bla", "bla" },

        { "\r",   "\n" },
        { "\n",   "\n" },
        { "\r\n", "\n" },

        { "hello\rworld",   "hello\nworld" },
        { "hello\r\rworld", "hello\n\nworld" },
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
        assertEquals(String.valueOf(EolUtil.anyToUnixEol(arg)), expected);
    }

    @DataProvider(name = "test2")
    public Object[][] createdata2() {
        final String eol = System.lineSeparator();

        return new Object[][] {
        { null, "null" },
        { "", "" },
        { "bla", "bla" },

        { "\r",   "\r" },
        { "\n",   eol },
        { "\r\n", "\r" + eol },

        { "hello\rworld",   "hello\rworld" },
        { "hello\r\rworld", "hello\r\rworld" },
        { "hello\nworld",   "hello" + eol + "world" },
        { "hello\r\nworld", "hello\r" + eol + "world" },

        { "hello world\r",   "hello world\r" },
        { "hello world\n",   "hello world" + eol },
        { "hello world\r\n", "hello world\r" + eol },

        { "\rhello world\r",     "\rhello world\r" },
        { "\nhello world\n",     eol + "hello world" + eol },
        { "\r\nhello world\r\n", "\r" + eol + "hello world\r" + eol },
        };
    }

    @Test(dataProvider = "test2")
    public void testEolUtil2(String arg, String expected) {
        assertEquals(String.valueOf(EolUtil.unixToJavaEol(arg == null ? null : new StringBuilder(arg))), expected);
    }
}
