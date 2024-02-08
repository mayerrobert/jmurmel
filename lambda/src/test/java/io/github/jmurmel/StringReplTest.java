package io.github.jmurmel;

import org.junit.Assert;
import org.junit.Test;

public class StringReplTest {
    static LambdaJ.StringRepl r;

    @Test
    public void test1() {
        r = LambdaJ.StringRepl.makeStringRepl();
        String stdout = r.evalString("1 2 3");
        Assert.assertNotNull(stdout);

        stdout = r.evalString("4 5 6");
        Assert.assertNotNull(stdout);
    }
}
