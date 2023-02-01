package io.github.jmurmel.jsr223;

import static org.junit.Assert.assertEquals;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import io.github.jmurmel.LambdaJ;
import org.junit.Test;

public class JSR223Test {

    @Test
    public void testEval() throws Exception {
        final ScriptEngine se = new JMurmelScriptEngine();

        final Object result = se.eval("(+ 4 5)");

        assertEquals(9.0, result);
    }

    @Test
    public void testEvalLambda() throws Exception {
        final ScriptEngine se = new JMurmelScriptEngine();

        Object result = se.eval("(defun f (p1 p2) (* p1 p2))");
        assertEquals("f", result.toString());

        result = se.eval("(f 2 3)");
        assertEquals(6.0, result);
    }

    //@Test
    public void testScriptEngineManagerNashorn() throws Exception {
        final ScriptEngineManager manager = new ScriptEngineManager();
        final ScriptEngine engine = manager.getEngineByName("nashorn");

        // evaluate JavaScript code
        engine.eval("print('Hello, World')");
    }

    @Test
    public void testScriptEngineManagerEval() throws Exception {
        final ScriptEngineManager manager = new ScriptEngineManager();
        final ScriptEngine engine = manager.getEngineByName("jmurmel");

        // evaluate Murmel code
        final Object result = engine.eval("(writeln \"Hello, World!\")");

        // result is a LambdaJSymbol, invoke toString() to get the symbol name
        assertEquals("Hello, World!", result.toString());
    }

    @Test
    public void testScriptEngineManagerTwoExp() throws Exception {
        final ScriptEngineManager manager = new ScriptEngineManager();
        final ScriptEngine engine = manager.getEngineByName("jmurmel");

        // evaluate Murmel code
        final Object result = engine.eval("(writeln \"Hello, World!\") (writeln \"Hello again, World!\")");

        // result is a LambdaJSymbol, invoke toString() to get the symbol name
        assertEquals("Hello again, World!", result.toString());
    }

    @Test
    public void testScriptEngineManagerMurmelName() {
        final ScriptEngineManager manager = new ScriptEngineManager();
        final ScriptEngine engine = manager.getEngineByName("jmurmel");

        assertEquals("JMurmel", engine.getFactory().getParameter(ScriptEngine.NAME));
    }

    @Test
    public void testScriptEnginePut() throws Exception {
        final ScriptEngineManager manager = new ScriptEngineManager();
        final ScriptEngine engine = manager.getEngineByName("jmurmel");

        engine.put("test", 1);
        final Object result = engine.eval("test");
        assertEquals(1, result);

        engine.put("javafunc", (LambdaJ.Primitive)JSR223Test::callback);
        final Object result2 = engine.eval("(javafunc \"me\")");
        assertEquals("Hello me", result2);
    }

    private static String callback(LambdaJ.ConsCell args) {
        return "Hello " + args.car();
    }

    @Test
    public void testInvokeFunction() throws Exception {
        final ScriptEngineManager manager = new ScriptEngineManager();
        final ScriptEngine engine = manager.getEngineByName("jmurmel");

        //engine.eval("1");
        final Object result = ((Invocable)engine).invokeFunction("+", 1, 2, 3);
        assertEquals(6.0, result);
    }
}
