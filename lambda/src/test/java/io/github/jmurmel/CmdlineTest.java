package io.github.jmurmel;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/** Test for invoking {@link LambdaJ#main} */
public class CmdlineTest {
    private static final Pattern NEWLINE_PATTERN = Pattern.compile("\\n|(\\r\\n)");

    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
            { new String[] { "--repl" }, ":q", 0, "Enter a Murmel form(?s).*", ""},

            { new String[] { "--bla" }, null, 1, "", "LambdaJ: unknown commandline argument(?s).*LambdaJ: exiting because of previous errors\\.\n"},

            { new String[] {}, "", 0, "", ""},

            { new String[] { "--tty" }, "", 0, "Enter a Murmel form(?s).*JMurmel> bye\\.\n\n", ""},
        };
    }

    @Test(dataProvider = "test")
    public void testMain(String[] args, String stdIn, int expectRc, String expectStdout, String expectStderr) {
        final InputStream oldStdin = System.in;
        if (stdIn != null) System.setIn(new ByteArrayInputStream(stdIn.getBytes(StandardCharsets.UTF_8)));
        
        final PrintStream oldStdout = System.out;
        final ByteArrayOutputStream captureStdout = new ByteArrayOutputStream();
        System.setOut(new PrintStream(captureStdout));

        final PrintStream oldStderr = System.err;
        final ByteArrayOutputStream captureStderr = new ByteArrayOutputStream();
        System.setErr(new PrintStream(captureStderr));

        try {
            LambdaJ.mainInternal(args);
        }
        catch (LambdaJ.Exit e) {
            Assert.assertEquals(e.rc, expectRc);
        }

        final String actualStdout = captureStdout.toString();
        Assert.assertTrue(expectStdout.isEmpty() && actualStdout.isEmpty() || EolUtil.anyToUnixEol(actualStdout).matches(expectStdout), failMsg("stdout", expectStdout, actualStdout));

        final String actualStderr = captureStderr.toString();
        Assert.assertTrue(expectStderr.isEmpty() && actualStderr.isEmpty() || EolUtil.anyToUnixEol(actualStderr).matches(expectStderr), failMsg("stderr", expectStderr, actualStderr));

        if (stdIn != null) System.setIn(oldStdin);
        System.setOut(oldStdout);
        System.setErr(oldStderr);
    }
    
    private static String failMsg(String chan, String expect, String actual) {
        return "\nexpected " + chan + ": '" + replaceNewlines(expect) + "'"
             + "\nactual " + chan + ":   '" + replaceNewlines(actual) + "'\n";
    }

    private static String replaceNewlines(String expect) {
        return NEWLINE_PATTERN.matcher(expect).replaceAll("\\\\n");
    }
}
