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
            { "error form", new String[] {}, "errorform", 255, "", STDERR_ERROR },
            { "load invalid", new String[] {}, "(load \"invalid\")", 255, "", STDERR_ERROR },
            { "add 1+2", new String[] {}, "(+ 1 2)", 0, "\n==> 3.0\n", "" },

            { "one file", new String[] { "./src/test/lisp/hello.lisp" }, "", 0, re("|Hello, World!|", "==> t\n"), "" },
            { "two files", new String[] { "./src/test/lisp/empty.lisp", "./src/test/lisp/hello.lisp" }, "", 0, re("|Hello, World!|", "==> t\n"), "" },

            { "run one file", new String[] { "--run", "./src/test/lisp/hello.lisp" }, "", 0, re("|Hello, World!|", "==> t\n"), "" },
            { "run two files", new String[] { "--run", "./src/test/lisp/empty.lisp", "./src/test/lisp/hello.lisp" }, "", 0, re("|Hello, World!|", "==> t\n"), "" },
            { "run add 1+2", new String[] { "--run" }, "(+ 1 2)", 0, "\n==> 3.0\n", "" },

            { "nulltest --repl", new String[] { "--repl" }, ":q", 0, STDOUT_WELCOME_OUTPUT_BYE, ""},
            { "error --repl",    new String[] { "--repl" }, "errorform", 255, STDOUT_WELCOME_OUTPUT, STDERR_ERROR },
            { "runform --repl",  new String[] { "--repl" }, "(write 'hello)\n:r\n", 0, re("Enter a Murmel form", "hello\n==> t\nJMurmel> hello\n==> t\n", "bye.\n\n"), "" },

            { "nulltest --tty", new String[] { "--tty" },  "",   0, STDOUT_WELCOME_OUTPUT_BYE, ""},
            { "error --tty", new String[] { "--tty" }, "errorform\n:q", 0, STDOUT_WELCOME_OUTPUT_ERROR_BYE, "" },

            { "invalid cmdline arg", new String[] { "--bla" }, null, 128, "", re("LambdaJ: unknown commandline argument", "LambdaJ: exiting because of previous errors.\n") },
            { "--version", new String[] { "--version" }, null, 0, re("Version", "Built from ", "Built by", "\n"), "" },
            { "--help", new String[] { "--help" }, null, 0, re("Version", "Built from ", "Built by", "Usage:\n\n", "\n"), "" },
            { "--help-features", new String[] { "--help-features" }, null, 0, re("Version", "Built from ", "Built by", "Feature flags:\n\n", "\n"), "" },
            
            { "cmdline args", new String[] {          "--", "arg1" }, "(write (car *command-line-argument-list*) nil)", 0, "arg1\n==> t\n", "" },
            { "cmdline args", new String[] { "--run", "--", "arg1" }, "(write (car *command-line-argument-list*) nil)", 0, "arg1\n==> t\n", "" },
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

        final int rc = LambdaJ.mainInternal(args);

        final String actualStderr = EolUtil.anyToUnixEol(captureStderr.toString());
        Assert.assertTrue(expectStderr.isEmpty() && actualStderr.isEmpty() || actualStderr.matches(expectStderr),
        name + " " + failMsg("stderr", expectStderr, actualStderr));

        final String actualStdout = EolUtil.anyToUnixEol(captureStdout.toString());
        Assert.assertTrue(expectStdout.isEmpty() && actualStdout.isEmpty() || actualStdout.matches(expectStdout),
                          name + " " + failMsg("stdout", expectStdout, actualStdout));

        Assert.assertEquals(rc, expectRc, name + " wrong exitlevel");

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
            if (c == '\\' || c == '$' || c == '.' || c == '(' || c == ')' || c == '|') {
                sb.append('\\');
            }
            sb.append(c);
        }
        return sb.toString();
    }
}
