package io.github.jmurmel;

import org.junit.Test;

public class CallJavaTest {

    @Test
    public void staticMethod() {
        System.setProperty("testprop", "testvalue");
        LambdaJTest.runTest("staticmethod.lisp",
                "(define get-property (jmethod \"java.lang.System\" \"getProperty\" \"java.lang.String\"))"
                + "(get-property \"testprop\")", "\"testvalue\"", null);
    }

    @Test
    public void mapConstructor() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))", "my-hash", null);
    }

    @Test
    public void mapToString() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (jmethod \"java.util.HashMap\" \"toString\"))", "hash-tostring", null);
    }

    @Test
    public void invokeMapToString() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (jmethod \"java.util.HashMap\" \"toString\"))"
                + "(hash-tostring my-hash)", "\"{}\"", null);
    }

    @Test
    public void invokeMapPutGet() {
        LambdaJTest.runTest("HashMap.lisp",
                "(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (jmethod \"java.util.HashMap\" \"toString\"))"
                + "((jmethod \"java.util.HashMap\" \"put\" \"java.lang.Object\" \"java.lang.Object\") my-hash \"key\" \"value\")"
                + "(write ((jmethod \"java.util.HashMap\" \"get\" \"java.lang.Object\") my-hash \"key\"))"
                + "(hash-tostring my-hash)", "\"{key=value}\"", "\"value\"");
    }
}
