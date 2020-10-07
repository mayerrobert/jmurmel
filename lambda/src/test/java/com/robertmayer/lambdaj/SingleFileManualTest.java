package com.robertmayer.lambdaj;

import java.nio.file.Paths;

import org.junit.Test;

public class SingleFileManualTest {

    @Test
    public void runFile() throws Exception {
        LambdaJTest.runTest(Paths.get("src", "test", "lisp", "tailrec2.lisp"));
    }
}
