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
        MurmelJavaCompiler c = new MurmelJavaCompiler(null, Paths.get("target"));
        Class<?> clazz = c.javaToClass("Test", "class Test { int i; }");
        assertNotNull("failed to compile Java to class", clazz);
    }

    @Test
    public void testForm() throws Exception {
        StringReader reader = new StringReader("(define a 2)");
        final SExpressionParser parser = new SExpressionParser(reader::read);
        Object sexp = parser.readObj();

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        String java = c.formsToJavaProgram("Test", Collections.singletonList(sexp));
        assertNotNull("failed to compile Murmel to Java", java);
    }

    @Test
    public void testNativeHelloWorld() throws Exception {
        String source = "(define f (lambda (a b) (write a) (write b)))"
                + "(f \"Hello, \" \"World!\")";

        StringReader reader = new StringReader(source);
        final SExpressionParser parser = new SExpressionParser(reader::read);
        final ArrayList<Object> program = new ArrayList<>();
        while (true) {
            Object sexp = parser.readObj(true);
            if (sexp == null) break;
            program.add(sexp);
        }

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        Class<MurmelJavaProgram> murmelClass = c.formsToApplicationClass("Test", program, "target/test-1.0.zip");
        assertNotNull("failed to compile Murmel to class", murmelClass);

        MurmelJavaProgram compiled = murmelClass.newInstance();
        Object result = compiled.body();
        assertEquals("wrong result", "t", result.toString());

        MurmelFunction f = compiled.getFunction("f");
        result = f.apply("The answer is: ", 42);
        assertEquals("wrong result", "t", result.toString());
    }

    @Test
    public void testArith() throws Exception {
        MurmelJavaProgram program = compile("(+ 1 2 3 (* 4 5 6))");
        assertNotNull("failed to compile arith to class", program);
        assertEquals("arith produced wrong result", 126.0, program.body());
    }

    @Test
    public void testCons() throws Exception {
        MurmelJavaProgram program = compile("(car (cons 1 2))");
        assertNotNull("failed to compile cons to class", program);
        assertEquals("cons produced wrong result", 1, program.body());
    }

    @Test
    public void testEq() throws Exception {
        MurmelJavaProgram program = compile("(eq 1 1)");
        assertNotNull("failed to compile eq to class", program);
        assertEquals("eq produced wrong result", Boolean.TRUE, program.body());
    }

    @Test
    public void testQuoteSymbol() throws Exception {
        MurmelJavaProgram program = compile("'a");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "a", sexp(program.body()));
    }

    @Test
    public void testQuoteAtom() throws Exception {
        MurmelJavaProgram program = compile("'1");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "1", sexp(program.body()));
    }

    @Test
    public void testQuoteList() throws Exception {
        MurmelJavaProgram program = compile("'(1 2 3)");
        assertNotNull("failed to compile quoteList to class", program);
        assertEquals("quoteList produced wrong result", "(1 2 3)", sexp(program.body()));
    }

    @Test
    public void testQuoteNil() throws Exception {
        MurmelJavaProgram program = compile("'()");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "nil", sexp(program.body()));
    }

    @Test
    public void testReverse() throws Exception {
        String source = "((lambda (reverse)\r\n"
                + "    (reverse (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 nil)))))))))))\r\n"
                + "\r\n"
                + " (lambda (list)\r\n"
                + "   ((lambda (rev)\r\n"
                + "      (rev rev nil list))\r\n"
                + "    (lambda (rev_ a l)\r\n"
                + "      (if\r\n"
                + "        (not l) a\r\n"
                + "        (rev_ rev_ (cons (car l) a) (cdr l )))))))";
        MurmelJavaProgram program = compile(source);
        assertNotNull("failed to compile reverse to class:", program);
        assertEquals("reverse produced wrong result", "(9 8 7 6 5 4 3 2 1)", sexp(program.body()));
    }



    private MurmelJavaProgram compile(String source) throws Exception {
        StringReader reader = new StringReader(source);
        final SExpressionParser parser = new SExpressionParser(reader::read);
        final ArrayList<Object> program = new ArrayList<>();
        while (true) {
            Object sexp = parser.readObj();
            if (sexp == null) break;
            program.add(sexp);
        }

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        Class<MurmelJavaProgram> murmelClass = c.formsToApplicationClass("Test", program, null);
        assertNotNull("failed to compile Murmel to class:\n\n" + c.formsToJavaProgram("Test", program), murmelClass);

        return murmelClass.newInstance();
    }

    private String sexp(Object obj) {
        StringBuilder b = new StringBuilder();
        new LambdaJ.SExpressionWriter(b::append).printObj(obj);;
        return b.toString();
    }
}
