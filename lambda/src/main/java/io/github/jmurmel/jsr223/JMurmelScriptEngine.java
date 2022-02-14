package io.github.jmurmel.jsr223;

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

import io.github.jmurmel.LambdaJ;

/** JSR-223 wrapper for JMurmel */
public class JMurmelScriptEngine extends AbstractScriptEngine implements ScriptEngine {

    final JMurmelScriptEngineFactory factory;
    final LambdaJ murmel;

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
        final Reader stdIn = context.getReader();
        final Writer stdOut = context.getWriter();
        final Object ret = murmel.evalScript(reader, stdIn, stdOut);
        try {
            stdOut.flush();
        } catch (IOException e) {
            throw new ScriptException("I/O exception during final flush");
        }
        return ret;
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
