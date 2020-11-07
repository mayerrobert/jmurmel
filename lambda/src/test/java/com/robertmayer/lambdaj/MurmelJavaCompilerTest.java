package com.robertmayer.lambdaj;

import static org.junit.Assert.*;

import java.io.StringReader;
import java.util.Collections;

import org.junit.Test;
import com.robertmayer.lambdaj.LambdaJ.*;

public class MurmelJavaCompilerTest {

    @Test
    public void testSimpleClass() throws Exception {
        MurmelJavaCompiler c = new MurmelJavaCompiler();
        Class<?> clazz = c.javaToJvm("Test", "class Test { int i; }");
        assertNotNull(clazz);
    }

    @Test
    public void testForm() throws Exception {
        MurmelJavaCompiler c = new MurmelJavaCompiler();
        StringReader program = new StringReader("(define a 2)");
        final SExpressionParser parser = new LambdaJ().new SExpressionParser(program::read);
        Object sexp = parser.readObj();
        String java = c.murmelFormsToJava("Test", Collections.singletonList(sexp));
        assertNotNull(java);
    }
}
