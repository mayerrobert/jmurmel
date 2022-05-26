package io.github.jmurmel;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

@Test
public class NonlocalReturnTest {

    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
            { "simple catch/throw", "(catch 'tag 1.0 (throw 'tag 11.0) 2.0)", 11.0 },
            { "empty unwind-protect", "(catch 'tag (unwind-protect (throw 'tag 'outer)))", new LambdaJ.LambdaJSymbol("outer") },
            { "unwind-protect", "(catch 'tag (unwind-protect 123 (throw 'tag 'outer)))", new LambdaJ.LambdaJSymbol("outer") },
            { "nested", "(catch 'tag (unwind-protect (unwind-protect (unwind-protect 'result (throw 'tag \"inner\")) (throw 'tag \"middle\")) (throw 'tag \"outer\")))", "outer" },
        };
    }

    @Test(dataProvider = "test")
    public void testInterpreter(String name, String program, Object result) {
        LambdaJTest.runTest(name, program, TestUtils.sexp(result), "");
    }

    @Test(dataProvider = "test")
    public void testCompiler(String name, String program, Object result) throws Exception {
        MurmelJavaCompilerTest.compileAndRun(program, result);
    }
}
