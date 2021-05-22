package io.github.jmurmel.jsr223;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

import io.github.jmurmel.LambdaJ;

/** JSR-223 wrapper for JMurmel */
public class JMurmelScriptEngineFactory implements ScriptEngineFactory {
    private final List<String> extensions = Collections.unmodifiableList(Arrays.asList(".lisp", ".lsp"));
    private final List<String> names      = Collections.unmodifiableList(Arrays.asList("JMurmel", "jmurmel"));
    private final List<String> mimetypes  = Collections.unmodifiableList(Arrays.asList("application/x-lisp", "text/x-script.lisp"));
    private final Map<String, String> parameters;

    {
        final Map<String, String> _parameters = new HashMap<>();
        _parameters.put(ScriptEngine.ENGINE, getEngineName());
        _parameters.put(ScriptEngine.ENGINE_VERSION, getEngineVersion());
        _parameters.put(ScriptEngine.LANGUAGE, getLanguageName());
        _parameters.put(ScriptEngine.LANGUAGE_VERSION, getLanguageVersion());
        _parameters.put(ScriptEngine.NAME, names.get(0));

        parameters = Collections.unmodifiableMap(_parameters);
    }

    @Override
    public String getEngineName() {
        return LambdaJ.ENGINE_NAME;
    }

    @Override
    public String getEngineVersion() {
        return LambdaJ.ENGINE_VERSION;
    }

    @Override
    public List<String> getExtensions() {
        return extensions;
    }

    @Override
    public List<String> getMimeTypes() {
        return mimetypes;
    }

    @Override
    public List<String> getNames() {
        return names;
    }

    @Override
    public String getLanguageName() {
        return "Murmel";
    }

    @Override
    public String getLanguageVersion() {
        return LambdaJ.LANGUAGE_VERSION;
    }

    @Override
    public Object getParameter(String key) {
        return parameters.get(key);
    }

    @Override
    public String getMethodCallSyntax(String obj, String m, String... args) {
        return null;
    }

    @Override
    public String getOutputStatement(String toDisplay) {
        return "(write " + toDisplay + ")";
    }

    @Override
    public String getProgram(String... statements) {
        return null;
    }

    @Override
    public ScriptEngine getScriptEngine() {
        return new JMurmelScriptEngine(this);
    }
}
