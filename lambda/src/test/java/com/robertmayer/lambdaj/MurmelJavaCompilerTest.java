package com.robertmayer.lambdaj;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;

import org.junit.Test;
import com.robertmayer.lambdaj.LambdaJ.*;

public class MurmelJavaCompilerTest {

    @Test
    public void testSimpleClass() throws Exception {
        JavaCompilerUtils c = new JavaCompilerUtils(Paths.get("target"));
        Class<?> clazz = c.javaToClass("Test", "class Test { int i; }");
        assertNotNull("failed to compile Java to class", clazz);
    }

    @Test
    public void testForm() throws Exception {
        InputStream reader = new ByteArrayInputStream("(define a 2)".getBytes(StandardCharsets.UTF_8));
        final SExpressionParser parser = new SExpressionParser(reader::read);

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        StringWriter w = new StringWriter();
        c.formsToJavaSource(w, "Test", parser);
        String java = w.toString();
        assertNotNull("failed to compile Murmel to Java", java);
    }

    @Test
    public void testNativeHelloWorld() throws Exception {
        String source = "(define f (lambda (a b) (write a) (write b)))"
                      + "(f \"Hello, \" \"World!\")";

        InputStream reader = new ByteArrayInputStream(source.getBytes(StandardCharsets.UTF_8));
        final SExpressionParser parser = new SExpressionParser(reader::read);

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        Class<MurmelProgram> murmelClass = c.formsToJavaClass("Test", parser, "target/test-1.0.zip");
        assertNotNull("failed to compile Murmel to class", murmelClass);

        MurmelProgram instance = murmelClass.newInstance();
        Object result = instance.body();
        assertEquals("wrong result", "t", sexp(result));

        MurmelFunction f = instance.getFunction("f");
        result = f.apply("The answer is: ", 42);
        assertEquals("wrong result", "t", sexp(result));
    }

    // murmel compiler should throw error "reserved word"
    @Test
    public void testDefineNil() throws Exception {
        String msg = compileError("(define nil 42)");
        assertNotNull("expected error", msg);
        String expected = "compile: can't use reserved";
        assertEquals("got wrong error", expected, msg.substring(0, expected.length()));
    }

    // murmel compiler should throw error "reserved word"
    @Test
    public void testDefineT() throws Exception {
        String msg = compileError("(define t 42)");
        assertNotNull("expected error", msg);
        String expected = "compile: can't use reserved";
        assertEquals("got wrong error", expected, msg.substring(0, expected.length()));
    }

    @Test
    public void testDefun() throws Exception {
        MurmelProgram program = compile("(define f1 (lambda (a) a)) (defun f2 (a) a)");
        assertNotNull("failed to compile defun to class", program);
        assertEquals("defun produced wrong result", "f2", sexp(program.body()));
    }

    @Test
    public void testArith() throws Exception {
        MurmelProgram program = compile("(+ 1 2 3 (* 4 5 6))");
        assertNotNull("failed to compile arith to class", program);
        assertEquals("arith produced wrong result", 126.0, program.body());
    }

    @Test
    public void testSub() throws Exception {
        MurmelProgram program = compile("((lambda (n) (- n 1)) 1)");
        assertNotNull("failed to compile sub to class", program);
        assertEquals("sub produced wrong result", 0.0, program.body());
    }

    @Test
    public void testCompare() throws Exception {
        MurmelProgram program = compile("((lambda (n) (<= n 1)) 2)");
        assertNotNull("failed to compile compare to class", program);
        assertEquals("compare produced wrong result", null, program.body());
    }

    @Test
    public void testCons() throws Exception {
        MurmelProgram program = compile("(car (cons 1 2))");
        assertNotNull("failed to compile cons to class", program);
        assertEquals("cons produced wrong result", 1L, program.body());
    }

    @Test
    public void testEq() throws Exception {
        MurmelProgram program = compile("(eq 1 1)");
        assertNotNull("failed to compile eq to class", program);
        assertEquals("eq produced wrong result", "t", sexp(program.body()));
    }

    @Test
    public void testNumberEq() throws Exception {
        MurmelProgram program = compile("(= 1 1)");
        assertNotNull("failed to compile numberEq to class", program);
        assertEquals("numberEq produced wrong result", "t", sexp(program.body()));
    }

    @Test
    public void testLe() throws Exception {
        MurmelProgram program = compile("((lambda (n) (<= n 1)) 2)");
        assertNotNull("failed to compile le to class", program);
        assertEquals("le produced wrong result", null, program.body());
    }

    @Test
    public void testNumberEq2() throws Exception {
        MurmelProgram program = compile("((lambda (n) (= n 1)) 1)");
        assertNotNull("failed to compile compare to class", program);
        assertEquals("compare produced wrong result", "t", sexp(program.body()));
    }

    @Test
    public void testQuoteSymbol() throws Exception {
        MurmelProgram program = compile("'a");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "a", sexp(program.body()));
    }

    @Test
    public void testQuoteAtom() throws Exception {
        MurmelProgram program = compile("'1");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "1", sexp(program.body()));
    }

    @Test
    public void testQuoteList() throws Exception {
        MurmelProgram program = compile("'(1 2 3)");
        assertNotNull("failed to compile quoteList to class", program);
        assertEquals("quoteList produced wrong result", "(1 2 3)", sexp(program.body()));
    }

    @Test
    public void testQuoteNil() throws Exception {
        MurmelProgram program = compile("'()");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "nil", sexp(program.body()));
    }

    @Test
    public void testApply() throws Exception {
        MurmelProgram program = compile("(apply + '(1 2 3))");
        assertNotNull("failed to compile apply to class", program);
        assertEquals("apply produced wrong result", 6.0, program.body());
    }

    @Test
    public void testEval() throws Exception {
        MurmelProgram program = compile("(eval (list 'cdr (car '((quote (a . b)) c))))");
        assertNotNull("failed to compile eval to class", program);
        assertEquals("eval produced wrong result", "b", sexp(program.body()));
    }

    @Test
    public void testEval2() throws Exception {
        MurmelProgram program = compile("(apply eval (list (quote (+ 1 2))))");
        assertNotNull("failed to compile eval2 to class", program);
        assertEquals("eval2 produced wrong result", 3.0, program.body());
    }

    @Test
    public void testCond() throws Exception {
        MurmelProgram program = compile("((lambda (x) (cond ((eq x 's1) 's1) ((eq x 's2) 's2) ((eq x 's3) 's3))) 's3)");
        assertNotNull("failed to compile cond to class", program);
        assertEquals("cond produced wrong result", "s3", sexp(program.body()));
    }

    @Test
    public void testProgn1() throws Exception {
        MurmelProgram program = compile("(progn)");
        assertNotNull("failed to compile progn1 to class", program);
        assertEquals("progn produced wrong result", "nil", sexp(program.body()));
    }

    @Test
    public void testProgn2() throws Exception {
        MurmelProgram program = compile("(progn 1 2 3)");
        assertNotNull("failed to compile progn2 to class", program);
        assertEquals("progn2 produced wrong result", "3", sexp(program.body()));
    }

    @Test
    public void testVarargs() throws Exception {
        MurmelProgram program = compile("((lambda a (apply + a)) 2 3)");
        assertNotNull("failed to compile varargs to class", program);
        assertEquals("varargs produced wrong result", 5.0, program.body());
    }

    @Test
    public void testVarargsCar() throws Exception {
        MurmelProgram program = compile("((lambda args (car args)) 'a 'b 'c)");
        assertNotNull("failed to compile varargscar to class", program);
        assertEquals("varargscar produced wrong result", "a", sexp(program.body()));
    }

    @Test
    public void testVarargsCdr() throws Exception {
        MurmelProgram program = compile("((lambda args (cdr args)) 'a 'b 'c)");
        assertNotNull("failed to compile varargscdr to class", program);
        assertEquals("varargscdr produced wrong result", "(b c)", sexp(program.body()));
    }

    @Test
    public void testVarargsCdddr() throws Exception {
        MurmelProgram program = compile("((lambda args (cdr (cdr (cdr args)))) 'a 'b 'c)");
        assertNotNull("failed to compile varargscdddr to class", program);
        assertEquals("varargscdddr produced wrong result", "nil", sexp(program.body()));
    }

    @Test
    public void testFormat() throws Exception {
        MurmelProgram program = compile("(format-locale nil \"de-DE\" \"Hello, World!\")");
        assertNotNull("failed to compile format to class", program);
        assertEquals("format produced wrong result", "\"Hello, World!\"", sexp(program.body()));
    }

    @Test
    public void testLet() throws Exception {
        MurmelProgram program = compile("(let ((a 1) (b 2) (c 3)) c)");
        assertNotNull("failed to compile let to class", program);
        assertEquals("let produced wrong result", "3", sexp(program.body()));
    }

    @Test
    public void testLabels() throws Exception {
        MurmelProgram program = compile("(labels ((a (p1 p2 p3) (+ p1 p2 p3))"
                                                  + " (b (p1 p2 p3) (* p1 p2 p3))"
                                                  + " (c (p1 p2 p3) (- p1 p2 p3)))"
                                                  + "(b 2 3 4))");
        assertNotNull("failed to compile labels to class", program);
        assertEquals("labels produced wrong result", 24.0, program.body());
    }



    @Test
    public void testReverse() throws Exception {
        String source = "((lambda (reverse)\n"
                + "    (reverse (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 nil)))))))))))\n"
                + "\n"
                + " (lambda (list)\n"
                + "   ((lambda (rev)\n"
                + "      (rev rev nil list))\n"
                + "    (lambda (rev^ a l)\n"
                + "      (if\n"
                + "        (null l) a\n"
                + "        (rev^ rev^ (cons (car l) a) (cdr l )))))))";
        MurmelProgram program = compile(source);
        assertNotNull("failed to compile reverse to class:", program);
        assertEquals("reverse produced wrong result", "(9 8 7 6 5 4 3 2 1)", sexp(program.body()));
    }

    @Test
    public void testFibonacci() throws Exception {
        String source = "(define iterative-fib-tr (lambda (n i previous current)\n"
                + "       (if (>= i n)\n"
                + "           current\n"
                + "           (iterative-fib-tr n (+ i 1) current (+ previous current)))))\n"
                + "\n"
                + "(define iterative-fib (lambda (n) (iterative-fib-tr n 1 1 1)))\n"
                + "\n"
                + "(iterative-fib 30)";
        MurmelProgram program = compile(source);
        assertNotNull("failed to compile fibonacci to class:", program);
        assertEquals("fibonacci produced wrong result", 1346269.0, program.body());
    }

    @Test
    public void testAckermannZ() throws Exception {
        String source = "(define Z^\n"
                + "  (lambda (f)\n"
                + "    ((lambda (g)\n"
                + "       (f (lambda args (apply (g g) args))))\n"
                + "     (lambda (g)\n"
                + "       (f (lambda args (apply (g g) args)))))))\n"
                + "\n"
                + "((Z^ (lambda (ackermann)\n"
                + "       (lambda (m n)\n"
                + "         (if (= m 0)\n"
                + "               (+ n 1)\n"
                + "           (if (= n 0)\n"
                + "                 (ackermann (- m 1) 1)\n"
                + "             (ackermann (- m 1) (ackermann m (- n 1))))))))\n"
                + " 3\n"
                + " 6) ; ==> 509";
        MurmelProgram program = compile(source);
        assertNotNull("failed to compile ackermann to class:", program);
        assertEquals("ackermann produced wrong result", 509.0, program.body());
    }



    private MurmelProgram compile(String source) throws Exception {
        InputStream reader = new ByteArrayInputStream(source.getBytes(StandardCharsets.UTF_8));
        final SExpressionParser parser = new SExpressionParser(reader::read);

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        Class<MurmelProgram> murmelClass = c.formsToJavaClass("Test", parser, null);
        StringWriter w = new StringWriter();
        c.formsToJavaSource(w, "Test", parser);
        String s = w.toString();
        assertNotNull("failed to compile Murmel to class:\n\n" + s, murmelClass);

        return murmelClass.newInstance();
    }

    private String compileError(String source) throws Exception {
        InputStream reader = new ByteArrayInputStream(source.getBytes(StandardCharsets.UTF_8));
        final SExpressionParser parser = new SExpressionParser(reader::read);

        MurmelJavaCompiler c = new MurmelJavaCompiler(parser, Paths.get("target"));
        try {
            c.formsToJavaClass("Test", parser, null);
        }
        catch (LambdaJError e) {
            return e.getMessage();
        }
        return null;
    }

    private String sexp(Object obj) {
        StringBuilder b = new StringBuilder();
        new LambdaJ.SExpressionWriter(b::append).printObj(obj);;
        return b.toString();
    }
}
