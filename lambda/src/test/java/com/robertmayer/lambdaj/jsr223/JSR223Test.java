package com.robertmayer.lambdaj.jsr223;

import static org.junit.Assert.assertEquals;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.junit.Test;

public class JSR223Test {

    @Test
    public void testEval() throws Exception {
        ScriptEngine se = new JMurmelScriptEngine();

        Object result = se.eval("(+ 4 5)");

        assertEquals(9.0, result);
    }

    @Test
    public void testLambda() throws Exception {
        ScriptEngine se = new JMurmelScriptEngine();

        Object result = se.eval("(defun f (p1 p2) (* p1 p2))");
        assertEquals("f", result.toString());

        result = se.eval("(f 2 3)");
        assertEquals(6.0, result);
    }

    //@Test
    public void testScriptEngineManagerNashorn() throws Exception {
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("nashorn");

        // evaluate JavaScript code
        engine.eval("print('Hello, World')");
    }

    @Test
    public void testScriptEngineManagerMurmel() throws Exception {
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("jmurmel");

        // evaluate Murmel code
        engine.eval("(writeln \"Hello, World!\")");
    }

    @Test
    public void testScriptEngineManagerMurmelName() throws Exception {
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("jmurmel");

        assertEquals("JMurmel", engine.getFactory().getParameter(ScriptEngine.NAME));
    }
}
