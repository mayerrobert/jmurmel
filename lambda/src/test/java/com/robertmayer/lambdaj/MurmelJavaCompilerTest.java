package com.robertmayer.lambdaj;

import static org.junit.Assert.*;

import java.io.StringReader;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Test;
import com.robertmayer.lambdaj.LambdaJ.*;

public class MurmelJavaCompilerTest {

    @Test
    public void testSimpleClass() throws Exception {
        MurmelJavaCompiler c = new MurmelJavaCompiler(Paths.get("target"));
        Class<?> clazz = c.javaToClass("Test", "class Test { int i; }");
        assertNotNull(clazz);
    }

    @Test
    public void testForm() throws Exception {
        MurmelJavaCompiler c = new MurmelJavaCompiler(Paths.get("target"));
        StringReader reader = new StringReader("(define a 2)");
        final SExpressionParser parser = new LambdaJ().new SExpressionParser(reader::read);
        Object sexp = parser.readObj();
        String java = c.formsToJavaProgram("Test", Collections.singletonList(sexp));
        assertNotNull(java);
    }

    @Test
    public void testNative() throws Exception {
        MurmelJavaCompiler c = new MurmelJavaCompiler(Paths.get("target"));
        String source = "(define f (lambda (a b) (write a) (write b)))"
                      + "(f \"Hello, \" \"World!\")";
        StringReader reader = new StringReader(source);
        final SExpressionParser parser = new LambdaJ().new SExpressionParser(reader::read);
        final ArrayList<Object> program = new ArrayList<>();
        while (true) {
            Object sexp = parser.readObj();
            if (sexp == null) break;
            program.add(sexp);
        }

        Class<LambdaJ.MurmelJavaProgram> murmelClass = c.formsToApplicationClass("Test", program, "target/test-1.0.zip");
        assertNotNull("class to be compiled is null", murmelClass);

        LambdaJ.MurmelJavaProgram compiled = murmelClass.newInstance();
        Object result = compiled.body();
        assertEquals("t", result.toString());

        LambdaJ.MurmelFunction f = compiled.getFunction("f");
        f.apply("The answer is: ", 42);
    }
}
