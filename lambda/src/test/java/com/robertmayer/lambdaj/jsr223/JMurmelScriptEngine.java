package com.robertmayer.lambdaj.jsr223;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import com.robertmayer.lambdaj.LambdaJ;

/** JSR-223 wraper for JMurmel */
public class JMurmelScriptEngine extends AbstractScriptEngine implements ScriptEngine {

    final JMurmelScriptEngineFactory factory;
    final LambdaJ murmel;
    boolean isInit;

    public JMurmelScriptEngine() {
        this(null);
    }

    public JMurmelScriptEngine(JMurmelScriptEngineFactory factory) {
        this.factory = factory;
        murmel = new LambdaJ();
    }

    @Override
    public Object eval(String script, ScriptContext context) throws ScriptException {
        return eval(new StringReader(script), context);
    }

    @Override
    public Object eval(Reader reader, ScriptContext context) throws ScriptException {
        if (!isInit) {
            isInit = true;
            murmel.interpretExpression(() -> -1, s -> { return; });
        }
        final Reader stdIn = context.getReader();
        final Writer stdOut = context.getWriter();
        Object ret = murmel.evalScript(reader::read, stdIn::read, s -> doWrite(stdOut, s));
        try {
            stdOut.flush();
        } catch (IOException e) {
            throw new ScriptException("I/O exception during final flush");
        }
        return ret;
    }

    private static void doWrite(Writer w, String s) {
        try { w.write(s); }
        catch (IOException e) { throw new RuntimeException(e); }
    }

    @Override
    public Bindings createBindings() {
        // todo das wird wohl fuer setzen von Java Objekten gebraucht werden
        return null;
    }

    @Override
    public ScriptEngineFactory getFactory() {
        return factory;
    }
}
