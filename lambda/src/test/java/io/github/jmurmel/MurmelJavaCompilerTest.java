package io.github.jmurmel;

import static org.junit.Assert.*;

import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Test;
import io.github.jmurmel.LambdaJ.*;

public class MurmelJavaCompilerTest {

    @Test
    public void testSimpleClass() throws Exception {
        final JavaCompilerHelper c = new JavaCompilerHelper(TestUtils.getTmpDir());
        final Class<?> clazz = c.javaToClass("Test", "class Test { int i; }");
        assertNotNull("failed to compile Java to class", clazz);
        c.cleanup();
    }

    @Test
    public void testForm() throws Exception {
        final MurmelJavaCompiler c = new MurmelJavaCompiler(null, null, TestUtils.getTmpDir());
        final Reader reader = new StringReader("(define a 2)");
        final ObjectReader parser = LambdaJ.makeReader(reader::read, c.getSymbolTable(), null);

        final StringWriter w = new StringWriter();
        c.formsToJavaSource(w, "Test", parser);
        final String java = w.toString();
        assertNotNull("failed to compile Murmel to Java", java);
    }

    @Test
    public void testNativeHelloWorld() throws Exception {
        final MurmelJavaCompiler c = new MurmelJavaCompiler(null, null, TestUtils.getTmpDir());

        final String source = "(define f (lambda (a b) (write a) (write b)))"
                              + "(f \"Hello, \" \"World!\")";

        final ReadSupplier reader = new StringReader(source)::read;

        final Class<MurmelProgram> murmelClass = c.formsToJavaClass("Test", reader, "target/test-1.0.zip");
        assertNotNull("failed to compile Murmel to class", murmelClass);

        final MurmelProgram instance = murmelClass.getDeclaredConstructor().newInstance();
        Object result = instance.body();
        assertEquals("wrong result", "\"World!\"", TestUtils.sexp(result));

        final MurmelFunction f = instance.getFunction("f");
        result = f.apply("The answer is: ", 42);
        assertEquals("wrong result", "42", TestUtils.sexp(result));
    }

    // murmel compiler should throw error "reserved word"
    @Test
    public void testDefineNil() throws Exception {
        compileError("(define nil 42)", "define: malformed define: can't use reserved");
    }

    // murmel compiler should throw error "reserved word"
    @Test
    public void testDefineT() throws Exception {
        compileError("(define t 42)", "define: malformed define: can't use reserved");
    }

    @Test
    public void testDefun() throws Exception {
        final MurmelProgram program = compile("(define f1 (lambda (a) a)) (defun f2 (a) a)");
        assertNotNull("failed to compile defun to class", program);
        assertEquals("defun produced wrong result", "f2", TestUtils.sexp(program.body()));
    }

    @Test
    public void testDefunNonToplevel() throws Exception {
        final MurmelProgram program = compile("(let ((a 3)) (defun f () a))");
        assertNotNull("failed to compile defun to class", program);
        assertEquals("defun produced wrong result", "f", TestUtils.sexp(program.body()));
    }

    @Test
    public void testTestFuncallError() throws Exception {
        runtimeError("((lambda (a) a))", "(lambda (a) ...): not enough arguments");
    }

    @Test
    public void testDeclaimDotted() throws Exception {
        compileError("(declaim . 1)", "eval: malformed eval: expected a list but got 1");
    }

    // function uses a variable that is defined later
    @Test
    public void testForwardVariable() throws Exception {
        final MurmelProgram program = compile("(defun f () x) (define x 1) (f)");
        assertEquals(1L, program.body());
    }

    // function uses a variable that is not defined
    @Test
    public void testUndefinedVariable() throws Exception {
        compileError("(defun f () x) (f)", "compilation unit: malformed compilation unit: undefined symbols: [x]");
    }

    // function uses a function that is defined later
    @Test
    public void testForwardFunction() throws Exception {
        final MurmelProgram program = compile("(defun f () (x)) (defun x() 1) (f)");
        assertEquals(1L, program.body());
    }

    // function uses a function that is not defined
    @Test
    public void testUndefinedFunction() throws Exception {
        compileError("(defun f () (x)) (f)", "compilation unit: malformed compilation unit: undefined symbols: [x]");
    }



    @Test
    public void testArith() throws Exception {
        final MurmelProgram program = compile("(+ 1 2 3 (* 4 5 6))");
        assertNotNull("failed to compile arith to class", program);
        assertEquals("arith produced wrong result", 126.0, program.body());
    }

    @Test
    public void testSub() throws Exception {
        final MurmelProgram program = compile("((lambda (n) (- n 1)) 1)");
        assertNotNull("failed to compile sub to class", program);
        assertEquals("sub produced wrong result", 0.0, program.body());
    }

    @Test
    public void testCompare() throws Exception {
        final MurmelProgram program = compile("((lambda (n) (<= n 1)) 2)");
        assertNotNull("failed to compile compare to class", program);
        assertNull("compare produced wrong result", program.body());
    }

    @Test
    public void testCons() throws Exception {
        final MurmelProgram program = compile("(car (cons 1 2))");
        assertNotNull("failed to compile cons to class", program);
        assertEquals("cons produced wrong result", 1L, program.body());
    }

    @Test
    public void testCons2() throws Exception {
        final MurmelProgram program = compile("'((1 2) 3 4 5)");
        assertNotNull("failed to compile cons2 to class", program);
        assertEquals("cons2 produced wrong result", "((1 2) 3 4 5)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testCons3() throws Exception {
        final MurmelProgram program = compile("'(1 . 2)");
        assertNotNull("failed to compile cons3 to class", program);
        assertEquals("cons3 produced wrong result", "(1 . 2)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testCons4() throws Exception {
        final MurmelProgram program = compile("'a");
        assertNotNull("failed to compile cons4 to class", program);
        assertEquals("cons4 produced wrong result", "a", TestUtils.sexp(program.body()));
    }

    @Test
    public void testCons5() throws Exception {
        final MurmelProgram program = compile("'((1 . 2) 3 4 . 5)");
        assertNotNull("failed to compile cons5 to class", program);
        assertEquals("cons5 produced wrong result", "((1 . 2) 3 4 . 5)", TestUtils.sexp(program.body()));
    }

    // a will be an ArrayList, test rplaca
    @Test
    public void testRplaca() throws Exception {
        final MurmelProgram program = compile("(defun f a (rplaca (cdr a) 1) a) (f 11 22 33)");
        assertNotNull("failed to compile rplacd to class", program);
        assertEquals("rplaca produced wrong result", "(11 1 33)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testRplacd() throws Exception {
        final MurmelProgram program = compile("(define l '(0 . 0)) (rplacd l 1)");
        assertNotNull("failed to compile rplacd to class", program);
        assertEquals("rplacd produced wrong result", "(0 . 1)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testRplacd2() throws Exception {
        final MurmelProgram program = compile("(define l '(0 . 0)) (rplacd l 1) l");
        assertNotNull("failed to compile rplacd2 to class", program);
        assertEquals("rplacd2 produced wrong result", "(0 . 1)", TestUtils.sexp(program.body()));
    }



    @Test
    public void testEq() throws Exception {
        final MurmelProgram program = compile("(eq 1 1)");
        assertNotNull("failed to compile eq to class", program);
        assertEquals("eq produced wrong result", "t", TestUtils.sexp(program.body()));
    }

    @Test
    public void testNumberEq() throws Exception {
        final MurmelProgram program = compile("(= 1 1)");
        assertNotNull("failed to compile numberEq to class", program);
        assertEquals("numberEq produced wrong result", "t", TestUtils.sexp(program.body()));
    }

    @Test
    public void testLe() throws Exception {
        final MurmelProgram program = compile("((lambda (n) (<= n 1)) 2)");
        assertNotNull("failed to compile le to class", program);
        assertNull("le produced wrong result", program.body());
    }

    @Test
    public void testNumberEq2() throws Exception {
        final MurmelProgram program = compile("((lambda (n) (= n 1)) 1)");
        assertNotNull("failed to compile compare to class", program);
        assertEquals("compare produced wrong result", "t", TestUtils.sexp(program.body()));
    }

    @Test
    public void testQuoteSymbol() throws Exception {
        final MurmelProgram program = compile("'a");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "a", TestUtils.sexp(program.body()));
    }

    @Test
    public void testQuoteAtom() throws Exception {
        final MurmelProgram program = compile("'1");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "1", TestUtils.sexp(program.body()));
    }

    @Test
    public void testQuoteList() throws Exception {
        final MurmelProgram program = compile("'(1 2 3)");
        assertNotNull("failed to compile quoteList to class", program);
        assertEquals("quoteList produced wrong result", "(1 2 3)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testQuoteNil() throws Exception {
        final MurmelProgram program = compile("'()");
        assertNotNull("failed to compile quoteSymbol to class", program);
        assertEquals("quoteSymbol produced wrong result", "nil", TestUtils.sexp(program.body()));
    }

    @Test
    public void testApply() throws Exception {
        final MurmelProgram program = compile("(apply + '(1 2 3))");
        assertNotNull("failed to compile apply to class", program);
        assertEquals("apply produced wrong result", 6.0, program.body());
    }

    @Test
    public void testEval() throws Exception {
        final MurmelProgram program = compile("(eval (list 'cdr (car '((quote (a . b)) c))))");
        assertNotNull("failed to compile eval to class", program);
        assertEquals("eval produced wrong result", "b", TestUtils.sexp(program.body()));
    }

    @Test
    public void testEval2() throws Exception {
        final MurmelProgram program = compile("(apply eval (list (quote (+ 1 2))))");
        assertNotNull("failed to compile eval2 to class", program);
        assertEquals("eval2 produced wrong result", 3.0, program.body());
    }

    // interpreter runs compiled code
    @Test
    public void testEval3() throws Exception {
        final MurmelProgram program = compile("(eval '(f v) (list (cons 'f write) (cons 'v 1)))");
        assertNotNull("failed to compile eval3 to class", program);
        assertEquals("eval3 produced wrong result", "1", TestUtils.sexp(program.body()));
    }

    @Test
    public void testEval4() throws Exception {
        final MurmelProgram program = compile("(apply (eval 'write) '(1))");
        assertNotNull("failed to compile eval4 to class", program);
        assertEquals("eval4 produced wrong result", "1", TestUtils.sexp(program.body()));
    }

    @Test
    public void testEvalCompileError() throws Exception {
        // compiletime error because #\a is not a valid argument to +
        compileError("(defun add (a b) (+ #\\a b))" 
                     + "(eval '(add 1 2) (list (cons 'add add)))", "+: expected a number argument but got #\\a");
    }

    @Test
    public void testEvalRuntimeError() throws Exception {
        // runtime error because #\a is not a valid argument to +
        runtimeError("(defun add (a b) (+ a b))"
                     + "(eval '(add 1 #\\b) (list (cons 'add add)))", "?: expected a number argument but got #\\b");
    }

    @Test
    public void testCond() throws Exception {
        final MurmelProgram program = compile("((lambda (x) (cond ((eq x 's1) 's1) ((eq x 's2) 's2) ((eq x 's3) 's3))) 's3)");
        assertNotNull("failed to compile cond to class", program);
        assertEquals("cond produced wrong result", "s3", TestUtils.sexp(program.body()));
    }

    @Test
    public void testProgn1() throws Exception {
        final MurmelProgram program = compile("(progn)");
        assertNotNull("failed to compile progn1 to class", program);
        assertEquals("progn produced wrong result", "nil", TestUtils.sexp(program.body()));
    }

    @Test
    public void testProgn2() throws Exception {
        final MurmelProgram program = compile("(progn 1 2 3)");
        assertNotNull("failed to compile progn2 to class", program);
        assertEquals("progn2 produced wrong result", "3", TestUtils.sexp(program.body()));
    }


    @Test
    public void testVarargs() throws Exception {
        final MurmelProgram program = compile("((lambda a (apply + a)) 2 3)");
        assertNotNull("failed to compile varargs to class", program);
        assertEquals("varargs produced wrong result", 5.0, program.body());
    }

    @Test
    public void testVarargs0() throws Exception {
        final MurmelProgram program = compile("((lambda (a . b) b))");
        assertNotNull("failed to compile varargs0 to class", program);
        try {
            program.body();
            fail("expected rt error");
        }
        catch (LambdaJError e) {
            assertTrue(e.getMessage().startsWith("(lambda (a . b) ...): not enough arguments"));
        }
    }

    @Test
    public void testVarargs1() throws Exception {
        final MurmelProgram program = compile("((lambda (a . b) b) 1)");
        assertNotNull("failed to compile varargs1 to class", program);
        assertNull("varargs1 produced wrong result", program.body());
    }

    @Test
    public void testVarargs2() throws Exception {
        final MurmelProgram program = compile("((lambda (a . b) b) 1 2)");
        assertNotNull("failed to compile varargs2 to class", program);
        assertEquals("varargs2 produced wrong result", "(2)", program.body().toString());
    }

    @Test
    public void testVarargs3() throws Exception {
        final MurmelProgram program = compile("((lambda (a . b) b) 1 2 3)");
        assertNotNull("failed to compile varargs3 to class", program);
        assertEquals("varargs3 produced wrong result", "(2 3)", program.body().toString());
    }

    @Test
    public void testVarargsCar() throws Exception {
        final MurmelProgram program = compile("((lambda args (car args)) 'a 'b 'c)");
        assertNotNull("failed to compile varargscar to class", program);
        assertEquals("varargscar produced wrong result", "a", TestUtils.sexp(program.body()));
    }

    @Test
    public void testVarargsCdr() throws Exception {
        final MurmelProgram program = compile("((lambda args (cdr args)) 'a 'b 'c)");
        assertNotNull("failed to compile varargscdr to class", program);
        assertEquals("varargscdr produced wrong result", "(b c)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testVarargsCdddr() throws Exception {
        final MurmelProgram program = compile("((lambda args (cdr (cdr (cdr args)))) 'a 'b 'c)");
        assertNotNull("failed to compile varargscdddr to class", program);
        assertEquals("varargscdddr produced wrong result", "nil", TestUtils.sexp(program.body()));
    }


    @Test
    public void testFormat() throws Exception {
        final MurmelProgram program = compile("(format-locale nil \"de-DE\" \"Hello, World!\")");
        assertNotNull("failed to compile format to class", program);
        assertEquals("format produced wrong result", "\"Hello, World!\"", TestUtils.sexp(program.body()));
    }

    @Test
    public void testLet() throws Exception {
        final MurmelProgram program = compile("(let ((a 1) (b 2) (c 3)) c)");
        assertNotNull("failed to compile let to class", program);
        assertEquals("let produced wrong result", "3", TestUtils.sexp(program.body()));
    }

    @Test
    public void testNamedLet() throws Exception {
        final MurmelProgram program = compile("(let loop ((n 0) (max 3)) (if (< n max) (loop (+ n 1) max) n))");
        assertNotNull("failed to compile named let to class", program);
        assertEquals("named let produced wrong result", 3.0, program.body());
    }

    @Test
    public void testLetStar() throws Exception {
        final MurmelProgram program = compile("(let* ((a 1) (b a)) b)");
        assertNotNull("failed to compile let* to class", program);
        assertEquals("let* produced wrong result", 1L, program.body());
    }

    @Test
    public void testLetStarSetq() throws Exception {
        final MurmelProgram program = compile("(let* (a) (progn (setq a 1) a))");
        assertNotNull("failed to compile let* to class", program);
        assertEquals("let* produced wrong result", 1L, program.body());
    }

    @Test
    public void testLetStarError() throws Exception {
        compileError("(let* ((a b) (b 2)) a)", "compilation unit: malformed compilation unit: undefined symbol");
    }

    @Test
    public void testNamedLetStar() throws Exception {
        final MurmelProgram program = compile("(let* loop ((a 1) (b a)) (if (> b a) (loop b a)) b)");
        assertNotNull("failed to compile namd let* to class", program);
        assertEquals("named let* produced wrong result", 1L, program.body());
    }

    @Test
    public void testNamedLetStar2() throws Exception {
        final MurmelProgram program = compile("(let* loop ((a 1) (b a) (b 2)) (if (< b a) (loop b a)) b)");
        assertNotNull("failed to compile namd let*2 to class", program);
        assertEquals("named let*2 produced wrong result", 2L, program.body());
    }

    @Test
    public void testLetrec() throws Exception {
        final MurmelProgram program = compile("(letrec ((a 1) (b 2)) (+ a b))");
        assertNotNull("failed to compile letrec to class", program);
        assertEquals("letrec produced wrong result", 3.0, program.body());
    }

    @Test
    public void testLetrec2() throws Exception {
        final MurmelProgram program = compile("(letrec ((a (lambda () b)) (b 1)) (a))");
        assertNotNull("failed to compile letrec2 to class", program);
        assertEquals("letrec2 produced wrong result", 1L, program.body());
    }

    @Test
    public void testLetDynamic() throws Exception {
        final MurmelProgram program = compile("(define a 1) (define b 2) (defun f () (write (cons a b))) (f) (let dynamic ((a 11) (b a)) (f)) (f)");
        assertNotNull("failed to compile let dynamic to class", program);

        final StringBuilder out = new StringBuilder();
        program.setReaderPrinter(null, out::append);
        program.body();
        assertEquals("let dynamic produced wrong output", "(1 . 2)(11 . 1)(1 . 2)", out.toString());
    }

    @Test
    public void testLetStarDynamic() throws Exception {
        final MurmelProgram program = compile("(define a 1) (define b 2) (defun f () (write a) (write b)) (let* dynamic ((a 33) (b a)) (f)) (f)");
        assertNotNull("failed to compile let* dynamic to class", program);

        final StringBuilder out = new StringBuilder();
        program.setReaderPrinter(null, out::append);
        program.body();
        assertEquals("let* dynamic produced wrong output", "333312", out.toString());
    }

    // let* dynamic, 1 global, 1 local
    @Test
    public void testLetStarDynamic2() throws Exception {
        final MurmelProgram program = compile("(define a nil) (defun f (x) (write (+ a x))) (let* dynamic ((a 11) (b 22)) (f b))");
        assertNotNull("failed to compile let* dynamic to class", program);

        final StringBuilder out = new StringBuilder();
        program.setReaderPrinter(null, out::append);
        program.body();
        assertEquals("let* dynamic produced wrong output", "33.0", out.toString());
    }

    @Test
    public void testLetDynamicLambda() throws Exception {
        final MurmelProgram program = compile("(define *g* 1)\n"
                                              + "(define *g-getter* (let dynamic ((*g* 2))\n"
                                              + "                     (lambda () *g*)))\n"
                                              + "(*g-getter*)");
        assertNotNull("failed to compile let dynamic to class", program);
        assertEquals("let dynamic produced wrong output", 1L, program.body());
    }

    @Test
    public void testLetStarDynamicLambda() throws Exception {
        final MurmelProgram program = compile("(define *g* 1)\n"
                                              + "(define *g-getter* (let* dynamic ((*g* 2))\n"
                                              + "                     (lambda () *g*)))\n"
                                              + "(*g-getter*)");
        assertNotNull("failed to compile let dynamic to class", program);
        assertEquals("let dynamic produced wrong output", 1L, program.body());
    }



    // body calls one local function
    @Test
    public void testLabels() throws Exception {
        final MurmelProgram program = compile("(labels ((a (p1 p2 p3) (+ p1 p2 p3))"
                                              + "         (b (p1 p2 p3) (* p1 p2 p3))"
                                              + "         (c (p1 p2 p3) (- p1 p2 p3)))"
                                              + "  (b 2 3 4))");
        assertNotNull("failed to compile labels to class", program);
        assertEquals("labels produced wrong result", 24.0, program.body());
    }

    // body calls one local recursive function
    @Test
    public void testLabelsRec() throws Exception {
        final MurmelProgram program = compile("(labels\n"
                                              + "  ((loop (n max)\n"
                                              + "     (if (< n max)\n"
                                              + "           (loop (+ n 1) max)\n"
                                              + "       n)))\n"
                                              + "  (loop 0 3))");
        assertNotNull("failed to compile labelsrec to class", program);
        assertEquals("labelsrec produced wrong result", 3.0, program.body());
    }

    // body calls a local function which calls another local function
    @Test
    public void testLabelsMutual() throws Exception {
        final MurmelProgram program = compile("(labels ((f1 (n) (f2 n))\n"
                                              + "         (f2 (n) n))\n"
                                              + "  (f1 5))");
        assertNotNull("failed to compile labelsmutual to class", program);
        assertEquals("labelsmutual produced wrong result", 5L, program.body());
    }



    @Test
    public void testJavaStatic() throws Exception {
        final MurmelProgram program = compile("((jmethod \"java.lang.System\" \"currentTimeMillis\"))");
        assertNotNull("failed to compile javastatic to class", program);
        assertEquals("javastatic produced wrong result", Long.class, program.body().getClass());
    }

    @Test
    public void testJavaInstance() throws Exception {
        final MurmelProgram program = compile("(define my-hash ((jmethod \"java.util.HashMap\" \"new\")))"
                                      + "(write ((jmethod \"java.util.HashMap\" \"toString\") my-hash))");
        assertNotNull("failed to compile javainstance to class", program);
        assertEquals("javainstance produced wrong result", "\"{}\"", TestUtils.sexp(program.body()));
    }

    @Test
    public void testString() throws Exception {
        final MurmelProgram program = compile("(defun string-split (str sep)\n"
                                              + "  (vector->list ((jmethod \"java.lang.String\" \"split\" \"String\") str sep)))" 
                                              + "(string-split \"1 2 3\" \" \")");
        assertNotNull("failed to compile string-split to class", program);
        assertEquals("string-split produced wrong result", "(\"1\" \"2\" \"3\")", TestUtils.sexp(program.body()));
    }

    @Test
    public void testString2() throws Exception {
        final MurmelProgram program = compile("(defun string-split (str sep) ((jmethod \"java.lang.String\" \"split\" \"String\") str sep))\n"
                                              + "(define s (make-array 1 (quote character) t))\n"
                                              + "(string-split s \" \")");
        assertNotNull("failed to compile string-split2 to class", program);
        assertEquals("string-split2 produced wrong result", "#(\"\0\")", TestUtils.sexp(program.body()));
    }


    @Test
    public void testMacroexpand() throws Exception {
        final MurmelProgram program = compile("(defmacro add2 (a) `(+ ,a 2))"
                                              + "(macroexpand-1 '(add2 3))");
        assertNotNull("failed to compile macroexpand to class", program);
        assertEquals("macroexpand produced wrong result", "(+ 3 2)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testMacro() throws Exception {
        final MurmelProgram program = compile("(defmacro add2 (a) `(+ ,a 2))"
                                              + "(add2 3)");
        assertNotNull("failed to compile macro to class", program);
        assertEquals("macro produced wrong result", "5.0", TestUtils.sexp(program.body()));
    }

    @Test
    public void testMacro2() throws Exception {
        final MurmelProgram program = compile("(define *g* 3)"
                                              + "(defmacro addg (a) `(+ ,a *g*))"
                                              + "(addg 3)");
        assertNotNull("failed to compile macro2 to class", program);
        assertEquals("macro2 produced wrong result", "6.0", TestUtils.sexp(program.body()));
    }

    @Test
    public void testMacroInDefine() throws Exception {
        final MurmelProgram program = compile("(defmacro m () 3)"
                                              + "(define *g* (m))"
                                              + "*g*");
        assertNotNull("failed to compile macroInDefine to class", program);
        assertEquals("macroInDefine produced wrong result", "3", TestUtils.sexp(program.body()));
    }

    @Test
    public void testMacroInDefun() throws Exception {
        final MurmelProgram program = compile("(define *g* 3)"
                                              + "(defmacro addg (a) `(+ ,a *g*))"
                                              + "(defun f () (addg 3))"
                                              + "(f)");
        assertNotNull("failed to compile macroInDefun to class", program);
        assertEquals("macroInDefun produced wrong result", "6.0", TestUtils.sexp(program.body()));
    }

    // macro defines a function
    @Test
    public void testMacroDefun() throws Exception {
        final MurmelProgram program = compile("(defmacro m () `(defun f () 1)) (m) (f)");
        assertNotNull("failed to compile macroDefun to class", program);
        assertEquals("macroDefun produced wrong result", "1", TestUtils.sexp(program.body()));
    }

    // macro is defined and used in a progn
    @Test
    public void testMacroProgn() throws Exception {
        final MurmelProgram program = compile("(progn (defmacro add2 (a) `(+ ,a 2))"
                                              + "(add2 3))");
        assertNotNull("failed to compile macro to class", program);
        assertEquals("macro produced wrong result", "5.0", TestUtils.sexp(program.body()));
    }

    @Test
    public void testReverse() throws Exception {
        final String source = "((lambda (reverse)\n"
                              + "    (reverse (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 nil)))))))))))\n"
                              + "\n"
                              + " (lambda (list)\n"
                              + "   ((lambda (rev)\n"
                              + "      (rev rev nil list))\n"
                              + "    (lambda (rev^ a l)\n"
                              + "      (if\n"
                              + "        (null l) a\n"
                              + "        (rev^ rev^ (cons (car l) a) (cdr l )))))))";
        final MurmelProgram program = compile(source);
        assertNotNull("failed to compile reverse to class:", program);
        assertEquals("reverse produced wrong result", "(9 8 7 6 5 4 3 2 1)", TestUtils.sexp(program.body()));
    }

    @Test
    public void testFibonacci() throws Exception {
        final String source = "(define iterative-fib-tr (lambda (n i previous current)\n"
                              + "       (if (>= i n)\n"
                              + "           current\n"
                              + "           (iterative-fib-tr n (+ i 1) current (+ previous current)))))\n"
                              + "\n"
                              + "(define iterative-fib (lambda (n) (iterative-fib-tr n 1 1 1)))\n"
                              + "\n"
                              + "(iterative-fib 30)";
        final MurmelProgram program = compile(source);
        assertNotNull("failed to compile fibonacci to class:", program);
        assertEquals("fibonacci produced wrong result", 1346269.0, program.body());
    }

    @Test
    public void testAckermannZ() throws Exception {
        final String source = "(define Z^\n"
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
        final MurmelProgram program = compile(source);
        assertNotNull("failed to compile ackermann to class:", program);
        assertEquals("ackermann produced wrong result", 509.0, program.body());
    }



    @Test
    public void testGensym() throws Exception {
        final String source = "(defmacro m (x1 x2) (let ((y1 (gensym)) (y2 (gensym))) `(let ((,y1 ,x1) (,y2 ,x2)) (+ ,y1 ,y2)))) "
                              + "(macroexpand-1 '(m 2 3)) "
                              + "(m 2 3)";
        final MurmelProgram program = compile(source);
        assertNotNull("failed to compile gensym to class:", program);
        assertEquals("gensym produced wrong result", 5.0, program.body());
    }



    @Test
    public void testCustomEnv() throws Exception {
        final String source = "(1+ (javaprim))";
        final MurmelProgram program = compile(source, true);
        assertNotNull("failed to compile customenv to class:", program);
        assertEquals("customenv produced wrong result", 42L, program.body());
    }

    static void compileAndRun(String source, Object expectedResult) throws Exception {
        final MurmelProgram program = compile(source);
        final Object actualResult = program.body();
        assertEquals(expectedResult, actualResult);
    }

    static void compileAndRun(String source, String expectedResultSexp) throws Exception {
        final MurmelProgram program = compile(source);
        final Object actualResult = program.body();
        assertEquals(expectedResultSexp, TestUtils.sexp(actualResult));
    }

    private static MurmelProgram compile(String source) throws Exception {
        return compile(source, false);
    }

    private static MurmelProgram compile(String source, boolean doCustomEnv) throws Exception {
        final MurmelJavaCompiler c = new MurmelJavaCompiler(null, null, TestUtils.getTmpDir());
        if (doCustomEnv) {
            final MurmelJavaProgram.CompilerPrimitive p = args -> 41;
            final ConsCell customEnv = ConsCell.list(ConsCell.cons(c.getSymbolTable().intern("javaprim"), p));
            c.setCustomEnvironment(customEnv);
        }

        final ReadSupplier reader = new StringReader(source)::read;

        try {
            final Class<MurmelProgram> murmelClass = c.formsToJavaClass("Test", reader, null);
            return murmelClass.getDeclaredConstructor().newInstance();
        }
        catch (LambdaJError le) {
            final StringWriter w = new StringWriter();
            final Reader reader2 = new StringReader(source);
            final ObjectReader parser2 = LambdaJ.makeReader(reader2::read, c.getSymbolTable(), null);
            c.formsToJavaSource(w, "Test", parser2);
            final String s = w.toString();
            fail("failed to compile Murmel to class:\n\n" + s);
        }
        throw new Exception("cannot happen"); // notreached
    }

    private static void compileError(String source, String expected) throws Exception {
        final MurmelJavaCompiler c = new MurmelJavaCompiler(null, null, TestUtils.getTmpDir());
        final ReadSupplier reader = new StringReader(source)::read;

        try {
            c.formsToJavaClass("Test", reader, null);
            fail("expected error " + expected + " but got no error");
        }
        catch (LambdaJError e) {
            final String actualMsg = e.getMessage();
            assertNotNull("expected error", actualMsg);
            assertEquals("got wrong error", expected, cutToLength(actualMsg, expected.length()));
        }
    }

    private static void runtimeError(String source, String expected) throws Exception {
        final MurmelJavaCompiler c = new MurmelJavaCompiler(null, null, TestUtils.getTmpDir());
        final ReadSupplier reader = new StringReader(source)::read;

        try {
            final Class<MurmelProgram> murmelClass = c.formsToJavaClass("Test", reader, null);
            murmelClass.getDeclaredConstructor().newInstance().body();
            fail("expected error " + expected + " but got no error");
        }
        catch (LambdaJError e) {
            final String actualMsg = e.getMessage();
            assertNotNull("expected error", actualMsg);
            assertEquals("got wrong error", expected, cutToLength(actualMsg, expected.length()));
        }
    }

    private static String cutToLength(String s, int maxLength) {
        if (s.length() < maxLength) return s;
        return s.substring(0, maxLength);
    }
}
