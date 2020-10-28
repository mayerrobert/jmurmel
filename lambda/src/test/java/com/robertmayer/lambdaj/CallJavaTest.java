package com.robertmayer.lambdaj;

import org.junit.Test;

public class CallJavaTest {

    @Test
    public void staticMethod() {
        System.setProperty("testprop", "testvalue");
        LambdaJTest.runTest("staticmethod.lisp",
                "(define get-property (:: \"java.lang.System\" \"getProperty\" \"java.lang.String\"))"
                + "(get-property nil \"testprop\")", "\"testvalue\"", null);
    }

    @Test
    public void mapConstructor() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (:: \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))", "my-hash", null);
    }

    @Test
    public void mapToString() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (:: \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (:: \"java.util.HashMap\" \"toString\"))", "hash-tostring", null);
    }

    @Test
    public void invokeMapToString() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (:: \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (:: \"java.util.HashMap\" \"toString\"))"
                + "(hash-tostring my-hash)", "\"{}\"", null);
    }

    @Test
    public void invokeMapPutGet() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (:: \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (:: \"java.util.HashMap\" \"toString\"))"
                + "((:: \"java.util.HashMap\" \"put\" \"java.lang.Object\" \"java.lang.Object\") my-hash \"key\" \"value\")"
                + "(write ((:: \"java.util.HashMap\" \"get\" \"java.lang.Object\") my-hash \"key\"))"
                + "(hash-tostring my-hash)", "\"{key=value}\"", "\"value\"");
    }
}
