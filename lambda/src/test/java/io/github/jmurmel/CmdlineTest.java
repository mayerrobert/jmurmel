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

    // normal operation: welcome, some lines of output, the last prompt and bye.
    private static final String STDOUT_WELCOME_OUTPUT_BYE = re("Enter a Murmel form", "\nJMurmel> bye.\n\n");

    // error in --tty mode: errors will be caught, everything goes to stdout
    private static final String STDOUT_WELCOME_OUTPUT_ERROR_BYE = re("Enter a Murmel form", "\nError: ", "JMurmel> bye.\n\n");

    // error in --repl mode: welcome, some lines of output, the last prompt and nothing (because errors go to stderr)
    private static final String STDOUT_WELCOME_OUTPUT = re("Enter a Murmel form", "\nJMurmel> ");

    // interpreter error messages: newline, "Error: ", some lines followed by a newline
    private static final String STDERR_ERROR = re("\nError: ", "\n");



    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
            // "null test": piping an empty file into jmurmel
            { "nulltest", new String[] {}, "", 0, "", ""},
            { "error form", new String[] {}, "errorform", 1, "", STDERR_ERROR },
            { "load invalid", new String[] {}, "(load \"invalid\")", 1, "", STDERR_ERROR },

            { "nulltest --repl", new String[] { "--repl" }, ":q", 0, STDOUT_WELCOME_OUTPUT_BYE, ""},
            { "error --repl", new String[] { "--repl" }, "errorform", 1, STDOUT_WELCOME_OUTPUT, STDERR_ERROR },

            { "nulltest --tty", new String[] { "--tty" },  "",   0, STDOUT_WELCOME_OUTPUT_BYE, ""},
            { "error --tty", new String[] { "--tty" }, "errorform\n:q", 0, STDOUT_WELCOME_OUTPUT_ERROR_BYE, "" },

            { "invalid cmdline arg", new String[] { "--bla" }, null, 1, "", re("LambdaJ: unknown commandline argument", "LambdaJ: exiting because of previous errors.\n") },
            { "--version", new String[] { "--version" }, null, 0, re("Version", "Built from jmurmel-all", "Built by", "\n"), "" },
            { "--help", new String[] { "--help" }, null, 0, re("Version", "Built from jmurmel-all", "Built by", "Usage:\n\n", "\n"), "" },
        };
    }

    @Test(dataProvider = "test")
    public void testMain(String name, String[] args, String stdIn, int expectRc, String expectStdout, String expectStderr) {
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
            Assert.assertEquals(e.rc, expectRc, name + " wrong exitlevel");
        }

        final String actualStdout = captureStdout.toString();
        Assert.assertTrue(expectStdout.isEmpty() && actualStdout.isEmpty() || EolUtil.anyToUnixEol(actualStdout).matches(expectStdout),
                          name + " " + failMsg("stdout", expectStdout, actualStdout));

        final String actualStderr = captureStderr.toString();
        Assert.assertTrue(expectStderr.isEmpty() && actualStderr.isEmpty() || EolUtil.anyToUnixEol(actualStderr).matches(expectStderr),
                          name + " " + failMsg("stderr", expectStderr, actualStderr));

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

    /** construct a regex by appending quoted parts, with "(?s).*" in between */
    private static String re(String... parts) {
        if (parts == null || parts.length == 0) return null;
        if (parts.length == 1 && parts[0] == null) return null;
        if (parts.length == 1 && parts[0].isEmpty()) return "";

        final StringBuilder ret = new StringBuilder();
        boolean first = true;
        for (String part: parts) {
            if (first) first = false;
            else ret.append("(?s).*");
            ret.append(quote(part));
        }
        return ret.toString();
    }

    /** quote (some of the) regex special characters by a '\' */
    public static String quote(String s) {
        if (s == null || s.isEmpty()) return s;
        final StringBuilder sb = new StringBuilder();
        for (int i=0; i<s.length(); i++) {
            final char c = s.charAt(i);
            if (c == '\\' || c == '$' || c == '.' || c == '(' || c == ')') {
                sb.append('\\');
            }
            sb.append(c);
        }
        return sb.toString();
    }
}
