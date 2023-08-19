package io.github.jmurmel.jsr223;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

import javax.script.*;

import io.github.jmurmel.LambdaJ;

/** JSR-223 wrapper for JMurmel */
public class JMurmelScriptEngine extends AbstractScriptEngine implements ScriptEngine, Invocable {

    private final JMurmelScriptEngineFactory factory;
    private final LambdaJ murmel;

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
        final Object ret = murmel.evalScript(reader, stdIn, stdOut, context.getBindings(ScriptContext.ENGINE_SCOPE));
        try {
            stdOut.flush();
        }
        catch (IOException e) {
            throw new ScriptException("I/O exception during final flush");
        }
        return ret;
    }

    @Override
    public Object invokeMethod(Object thiz, String name, Object... args) {
        throw new UnsupportedOperationException("methods are not supported, use invokeFunction instead");
    }

    @Override
    public Object invokeFunction(String name, Object... args) throws ScriptException, NoSuchMethodException {
        try {
            return murmel.getFunction(name).apply(args);
        }
        catch (Exception e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public <T> T getInterface(Class<T> clasz) {
        throw new UnsupportedOperationException("getInterface(Class) is not yet supported"); // todo
    }

    @Override
    public <T> T getInterface(Object thiz, Class<T> clasz) {
        throw new UnsupportedOperationException("objects are not supported, use getInterface(Class) instead");
    }

    @Override
    public Bindings createBindings() {
        return new SimpleBindings();
    }

    @Override
    public ScriptEngineFactory getFactory() {
        return factory;
    }
}
