package io.github.jmurmel;

import org.junit.Test;

public class CallJavaTest {

    public static Object[] echoArgs(String s, double d, int... values) {
        final Object[] ret = new Object[values.length + 2];
        ret[0] = s;
        ret[1] = d;
        for (int i = 0; i < values.length; i++) {
            ret[i+2] = values[i];
        }
        return ret;
    }

    @Test
    public void staticMethodNoArgs() throws Exception {
        runTest("(define ctm (jmethod \"java.lang.System\" \"currentTimeMillis\"))"
                + "(> (ctm) 0)", "t");
    }

    @Test
    public void staticMethod() throws Exception {
        System.setProperty("testprop", "testvalue");
        runTest("(define get-property (jmethod \"java.lang.System\" \"getProperty\" \"java.lang.String\"))"
                + "(get-property \"testprop\")", "\"testvalue\"");
    }

    @Test
    public void staticMethodInline() throws Exception {
        System.setProperty("testprop", "testvalue");
        runTest("((jmethod \"java.lang.System\" \"getProperty\" \"java.lang.String\") \"testprop\")",
                "\"testvalue\"");
    }

    @Test
    public void staticMethodInlineTooManyArgs() {
        System.setProperty("testprop", "testvalue");
        LambdaJTest.runErrorTest("toomanyargs", "((jmethod \"java.lang.System\" \"getProperty\" \"java.lang.String\") \"testprop\" 1)",
                                 "getProperty: expected one argument but got extra arg(s)");
    }

    @Test
    public void staticMethodVarargs() throws Exception {
        runTest("(define join (jmethod \"String\" \"join\" \"CharSequence\" \"CharSequence...\"))" 
                + "(join  \"@\" \"foo\" \"bar\" \"baz\")",
                "\"foo@bar@baz\"");
    }

    @Test
    public void staticMethodVarargsInline() throws Exception {
        runTest("((jmethod \"String\" \"join\" \"CharSequence\" \"CharSequence...\") \"@\" \"foo\" \"bar\" \"baz\")",
                "\"foo@bar@baz\"");
    }

    @Test
    public void staticMethodVarargsPrimitive() throws Exception {
        runTest("(define echo (jmethod \"io.github.jmurmel.CallJavaTest\" \"echoArgs\" \"String\" \"double\" \"int...\"))"
                + "(echo \"foo\" -0.0 1 2 3)",
                "#(\"foo\" -0.0 1 2 3)");
    }

    @Test
    public void staticMethodVarargsInlinePrimitive() throws Exception {
        runTest("((jmethod \"io.github.jmurmel.CallJavaTest\" \"echoArgs\" \"String\" \"double\" \"int...\") \"foo\" -0.0 1 2 3)",
                "#(\"foo\" -0.0 1 2 3)");
    }

    @Test
    public void mapConstructor() throws Exception {
        runTest("(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))" 
                + "my-hash", "{}");
    }

    @Test
    public void invokeMapToString() throws Exception {
        runTest("(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "(define hash-tostring (jmethod \"java.util.HashMap\" \"toString\"))"
                + "(hash-tostring my-hash)", "\"{}\"");
    }

    @Test
    public void invokeMapToStringInline() throws Exception {
        runTest("(define create-hash (jmethod \"java.util.HashMap\" \"new\"))"
                + "(define my-hash (create-hash))"
                + "((jmethod \"java.util.HashMap\" \"toString\") my-hash)",
                "\"{}\"");
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
    
    private static void runTest(String source, String expectedResult) throws Exception {
        LambdaJTest.runTest("calljavatest.lisp", source, expectedResult, null);
        MurmelJavaCompilerTest.compileAndRun(source, expectedResult);
    }
}
