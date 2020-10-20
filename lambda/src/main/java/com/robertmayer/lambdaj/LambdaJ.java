/* LambdaJ is Copyright (C) 2020 Robert Mayer. All rights reserved.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

package com.robertmayer.lambdaj;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.IllegalFormatException;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.function.DoubleBinaryOperator;
import java.util.function.IntPredicate;
import java.util.regex.Pattern;

public class LambdaJ {

    @FunctionalInterface public interface ReadSupplier { int read() throws IOException; }
    @FunctionalInterface public interface WriteConsumer { void print(String s); }
    @FunctionalInterface public interface Tracer { void println(String msg); }

    @FunctionalInterface public interface ObjectReader { Object readObj(); }
    public interface SymbolTable { Object intern(String symbol); }
    public interface Parser extends ObjectReader, SymbolTable {}

    public interface ObjectWriter { void printObj(Object o); default void printString(String s) { printObj(s); } void printEol(); }

    @FunctionalInterface public interface Primitive { Object apply(ConsCell x); }

    public interface CustomBuiltinsSupplier {
        ConsCell customEnvironment(SymbolTable symtab, ObjectReader lispStdin, ObjectWriter lispStdout);
    }



    public static class LambdaJError extends RuntimeException {
        public static final long serialVersionUID = 1L;

        public LambdaJError(String msg) { super(msg, null, false, false); }
        @Override public String toString() { return "Error: " + getMessage(); }
    }



    /// data type used by interpreter program as well as interpreted programs
    public static class ConsCell implements Iterable<Object>, Serializable {
        private static class ConsCellIterator implements Iterator<Object> {
            private final ConsCell coll;
            private Object cursor;

            private ConsCellIterator(ConsCell coll) { this.coll = coll; this.cursor = coll; }
            @Override public boolean hasNext() { return cursor != null; }

            @Override
            public Object next() {
                if (cursor == null) throw new NoSuchElementException();
                if (cursor instanceof ConsCell) {
                    final ConsCell list = (ConsCell)cursor;
                    final Object ret = list.car;
                    if (list.cdr == coll) cursor = null; // circle detected, stop here
                    else cursor = list.cdr;
                    return ret;
                }
                final Object ret = cursor;
                cursor = null;
                return ret;
            }
        }

        private static final long serialVersionUID = 1L;
        public Object car, cdr;
        private ConsCell closure; // only used for Lambdas with lexical environments. doesn't waste space because Java object sizes are multiples of 8 and this uses an otherwise unused slot
        public ConsCell(Object car, Object cdr)    { nCells++; this.car = car; this.cdr = cdr; }
        public ConsCell(Object car, Object cdr, ConsCell closure)    { this(car, cdr); this.closure = closure; }

        @Override public String toString() { return printObj(this); }
        @Override public Iterator<Object> iterator() { return new ConsCellIterator(this); }
    }


    public static class LambdaJString implements Serializable {
        private static final long serialVersionUID = 1L;
        private final String value;
        public LambdaJString(String value) { this.value = value; }
        @Override public String toString() { return value.toString(); }
    }



    /// infrastructure
    public static final int EOF = -1;
    public static final int TOKEN_MAX = 2000; // max length of symbols and literals

    public static final int TRC_NONE = 0, TRC_STATS = 1, TRC_EVAL = 2, TRC_ENV = 3, TRC_FUNC = 4, TRC_PARSE = 5, TRC_TOK = 6, TRC_LEX = 7;
    private final int trace;

    private final Tracer tracer;

    public static final int
    HAVE_LABELS = 1,                   // use Y-combinator instead
    HAVE_NIL    = 1<<2, HAVE_T = 1<<3, // use () and (quote t) instead. printObj will print nil regardless
    HAVE_XTRA   = 1<<4,                // no extra special forms such as if
    HAVE_DOUBLE = 1<<5,                // no numbers, +-<>..., numberp, remaining datatypes are symbls and cons-cells (lists)
    HAVE_STRING = 1<<6,                // no strings, string literals of string related functions
    HAVE_IO     = 1<<7,                // no read/ write, result only
    HAVE_UTIL   = 1<<8,                // no null?, consp, listp, symbolp, assoc
    HAVE_APPLY  = 1<<9,                // McCarthy didn't list apply
    HAVE_CONS   = 1<<10,
    HAVE_COND   = 1<<11,
    HAVE_ATOM   = 1<<12,
    HAVE_EQ     = 1<<13,
    HAVE_QUOTE  = 1<<14,

    HAVE_LEXC   = 1<<15,


    HAVE_LAMBDA     = 0,
    HAVE_LAMBDAPLUS = HAVE_LAMBDA | HAVE_ATOM | HAVE_QUOTE | HAVE_EQ,
    HAVE_MIN        = HAVE_LAMBDAPLUS | HAVE_CONS | HAVE_COND,
    HAVE_MINPLUS    = HAVE_MIN | HAVE_APPLY | HAVE_LABELS,
    HAVE_ALL_DYN    = HAVE_MINPLUS | HAVE_NIL | HAVE_T | HAVE_XTRA | HAVE_DOUBLE | HAVE_STRING | HAVE_IO | HAVE_UTIL,

    HAVE_ALL_LEXC   = HAVE_ALL_DYN | HAVE_LEXC;
    ;
    private final int features;

    private boolean haveLabels() { return (features & HAVE_LABELS) != 0; }
    private boolean haveNil()    { return (features & HAVE_NIL)    != 0; }
    private boolean haveT()      { return (features & HAVE_T)      != 0; }
    private boolean haveXtra()   { return (features & HAVE_XTRA)   != 0; }
    private boolean haveDouble() { return (features & HAVE_DOUBLE) != 0; }
    private boolean haveString() { return (features & HAVE_STRING) != 0; }
    private boolean haveIO()     { return (features & HAVE_IO)     != 0; }
    private boolean haveUtil()   { return (features & HAVE_UTIL)   != 0; }
    private boolean haveApply()  { return (features & HAVE_APPLY)  != 0; }
    private boolean haveCons()   { return (features & HAVE_CONS)   != 0; }
    private boolean haveCond()   { return (features & HAVE_COND)   != 0; }
    private boolean haveAtom()   { return (features & HAVE_ATOM)   != 0; }
    private boolean haveEq()     { return (features & HAVE_EQ)     != 0; }
    private boolean haveQuote()  { return (features & HAVE_QUOTE)  != 0; }
    private boolean haveLexC()   { return (features & HAVE_LEXC)   != 0; }

    public LambdaJ() {
        this(HAVE_ALL_LEXC, TRC_NONE);
    }

    public LambdaJ(int features, int trace, Tracer... tracer) {
        this.features = features;
        this.trace = trace;
        this.tracer = tracer != null && tracer.length > 0 ? tracer[0] : System.err::println;
    }



    private static boolean isWhiteSpace(int x) { return x == ' ' || x == '\t' || x == '\n' || x == '\r'; }
    private static boolean isSExSyntaxChar(int x) { return x == '(' || x == ')' || x == '\''; }

    private static boolean containsSExSyntaxOrBlank(String s) {
        for (int i = 0; i < s.length(); i++) {
            char c;
            if (isSExSyntaxChar(c = s.charAt(i))) return true;
            if (isWhiteSpace(c)) return true;
        }
        return false;
    }

    /** This class will write objects as S-expressions to the given {@link WriteConsumer} */
    public static class SExpressionWriter implements ObjectWriter {
        private WriteConsumer out;  // printObj() and printEol() will write to this

        public SExpressionWriter(WriteConsumer out) { this.out = out; }
        @Override public void printObj(Object ob) { out.print(printSEx(ob)); }
        @Override public void printString(String s) { out.print(s); }
        @Override public void printEol() { out.print(System.lineSeparator()); }
    }

    /// scanner, symboltable and S-expression parser
    /** This class will read and parse S-Expressions (while generating symbol table entries)
     *  from the given {@link ReadSupplier} */
    public class SExpressionParser implements Parser {
        private ReadSupplier in;    // readObj() will read from this
        private boolean init;

        private int lineNo = 1, charNo;
        private boolean escape; // is the lookahead escaped
        private boolean tokEscape;
        private int look;
        private int token[] = new int[TOKEN_MAX + 1]; // provide for trailing '\0'
        private Object tok;

        public SExpressionParser(ReadSupplier in) { this.in = in; }

        /// scanner
        private boolean isSpace(int x)  { return !escape && isWhiteSpace(x); }
        private boolean isDigit(int x)  { return !escape && (x >= '0' && x <= '9'); }
        private boolean isDQuote(int x) { return !escape && x == '"'; }

        private boolean isSyntax(int x) { return !escape && isSExSyntaxChar(x); }

        private int getchar() {
            try {
                tokEscape = escape;
                escape = false;
                int c = readchar();
                if (c == '\\') {
                    escape = true;
                    return readchar();
                }
                if (c == ';') {
                    while ((c = readchar()) != '\n' && c != EOF);
                }
                return c;
            } catch (Exception e) {
                throw new RuntimeException("I/O error reading");
            }
        }

        private int readchar() throws IOException {
            int c = in.read();
            if (c == '\n') {
                lineNo++;
                charNo = 0;
            } else charNo++;
            return c;
        }

        private void readToken() {
            int index = 0;
            while (isSpace(look)) { look = getchar(); }
            if (look != EOF) {
                if (isSyntax(look)) {
                    token[index++] = look;  look = getchar();
                } else if (haveString() && isDQuote(look)) {
                    do {
                        if (index < TOKEN_MAX) token[index++] = look;
                        look = getchar();
                    } while (look != EOF && !isDQuote(look));
                    if (look == EOF) throw new LambdaJError("line " + lineNo + ':' + charNo + ": ' string literal too long or missing closing \"");
                    else look = getchar(); // consume trailing "
                } else {
                    while (look != EOF && !isSpace(look) && !isSyntax(look)) {
                        if (index < TOKEN_MAX) token[index++] = look;
                        look = getchar();
                    }
                }
            }
            token[index] = '\0';
            if (haveDouble() && isNumber()) {
                try {
                    tok = Double.valueOf(tokenToString(token));
                }
                catch (NumberFormatException e) {
                    throw new LambdaJError("line " + lineNo + ':' + charNo + ": '" + tokenToString(token)
                    + "' is not a valid symbol or number");
                }
            } else if (haveString() && token[0] == '"') {
                tok = new LambdaJString(tokenToString(token).substring(1));
            } else if (token[0] == '\0'){
                tok = null;
            } else {
                tok = tokenToString(token);
            }
            if (trace >= TRC_LEX)
                tracer.println("*** scan  token  |" + String.valueOf(tok) + '|');
        }

        private boolean isNumber() {
            final int first = token[0];
            if (isDigit(first)) return true;
            return ((first == '-' || first == '+') && isDigit(token[1]));
        }

        private String tokenToString(int[] s) {
            final StringBuffer ret = new StringBuffer(32);
            int len = s.length, c;
            for (int i = 0; i < len && (c = s[i++]) != '\0'; )
                ret.append((char)c);
            return ret.toString();
        }



        /// symbol table implemented with a list just because. could easily replaced by a HashMap for better performance.
        private ConsCell symbols = null;

        // String#equalsIgnoreCase is slow. we could String#toUpperCase al symbols then we could use String#equals
        @Override
        public Object intern(String sym) {
            ConsCell pair = symbols;
            for ( ; pair != null; pair = (ConsCell)cdr(pair)) {
                if (sym.equalsIgnoreCase((String)car(pair))) {
                    return car(pair);
                }
            }
            symbols = cons(sym, symbols);
            return sym;
        }



        /// parser
        @Override
        public Object readObj() {
            if (!init) {
                look = getchar();
                init = true;
            }
            readToken();
            return readObject();
        }

        private Object quote = intern("quote");

        private Object readObject() {
            if (tok == null) {
                if (trace >= TRC_PARSE) tracer.println("*** parse list   ()");
                return null;
            }
            if (!tokEscape && "(".equals(tok)) {
                Object list = readList();
                if (!tokEscape && ".".equals(tok)) {
                    Object cdr = readList();
                    Object cons = cons(car(list), car(cdr));
                    if (trace >= TRC_PARSE) tracer.println("*** parse cons   " + printSEx(cons));
                    return cons;
                }
                if (trace >= TRC_PARSE) tracer.println("*** parse list   " + printSEx(list));
                return list;
            }
            if (!tokEscape && haveQuote() && "'".equals(tok)) {
                readToken();
                return cons(quote, cons(readObject(), null));
            }
            if (symbolp(tok)) {
                if (trace >= TRC_TOK) tracer.println("*** parse symbol " + (String)tok);
                return intern((String)tok);
            }
            if (trace >= TRC_TOK) tracer.println("*** parse value  " + tok.toString());
            return tok;
        }

        private Object readList() {
            readToken();
            if (tok == null) throw new LambdaJError("line " + lineNo + ':' + charNo + ": cannot read list. missing ')'?");
            if (!tokEscape) {
                if (")".equals(tok)) return null;
                if (".".equals(tok)) return null;
            }
            final Object tmp = readObject();
            if (symbolp(tmp)) return cons(tmp, readList());
            else return cons(tmp, readList());
        }
    }



    /// eval - interpreter
    private SymbolTable symtab;

    /** look up the symbols for special forms only once on first use.
     *  the suppliers below will do a lookup on first use and then replace themselves by another supplier
     *  that simply returns the cached value */
    private void setSymtab(SymbolTable symtab) {
        this.symtab = symtab;

        // (re-)read the new symtab
        sApply  = symtab.intern("apply");
        sCond   = symtab.intern("cond");
        sIf     = symtab.intern("if");
        sDefine = symtab.intern("define");
        sDefun  = symtab.intern("defun");
        sLabels = symtab.intern("labels");
        sLambda = symtab.intern("lambda");
        sQuote  = symtab.intern("quote");
        expTrue = makeExpTrue();
    }

    private Object sApply, sCond, sIf, sDefine, sDefun, sLabels, sLambda, sQuote;
    private Object expTrue;

    private Object makeExpTrue() {
        if (haveT()) return symtab.intern("t"); // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (haveQuote()) return cons(symtab.intern("quote"), cons(symtab.intern("t"), null));
        else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
    }



    private Object eval(Object exp, ConsCell topEnv, ConsCell env, int stack, int level) {
        boolean isTc = false;
        try {
            level--;

            tailcall:
            while (true) {
                level++;
                dbgEvalStart(isTc ? "eval TC" : "eval", exp, env, stack, level);

                if (symbolp(exp)) {                 // this line is convenient breakpoint
                    if (exp == null) return null;
                    final ConsCell envEntry = assoc(exp, env);
                    if (envEntry != null) return cdr(envEntry);
                    throw new LambdaJError("'" + exp + "' is undefined");
                }

                if (atom(exp)) {
                    return exp;
                }

                if (consp(exp)) {
                    final Object func;                     // will be used in case of a function call
                    final ConsCell argList;                // will be used in case of a function call

                    final Object operator = car(exp);      // first symbol of the S-expression
                    final ConsCell operandlist = (ConsCell) cdr(exp);   // list with remaining symbols

                    // special forms
                    if (haveQuote() && operator == sQuote) {
                        oneArg("quote", operandlist);
                        return car(operandlist);
                    }

                    if (haveXtra() && operator == sIf) {
                        nArgs("if", operandlist, 2, 3, exp);
                        if (eval(car(operandlist), topEnv, env, stack+1, level+1) != null) {
                            exp = cadr(operandlist); isTc = true; continue;
                        } else if (cddr(operandlist) != null) {
                            exp = caddr(operandlist); isTc = true; continue;
                        } else return null;
                    }

                    if (haveXtra() && operator == sDefine) {
                        twoArgs("define", operandlist, exp);
                        final Object symbol = car(operandlist); // todo ob statt symbol eine expression erlaubt sein sollte? expression koennte symbol errechnen
                                                                // ggf. symbol UND expression zulassen: if (symbolp(cdr(exp))...
                        if (!symbolp(symbol)) throw new LambdaJError("define: not a symbol: " + printSEx(symbol) + '.' + errorExp(exp));
                        final ConsCell envEntry = assoc(symbol, env);
                        if (envEntry != null) throw new LambdaJError("define: '" + symbol + "' was already defined, current value: " + printSEx(cdr(envEntry)) + errorExp(exp));

                        final Object value = eval(cadr(operandlist), topEnv, env, stack+1, level+1);
                        topEnv.cdr = cons(cons(symbol, value), cdr(topEnv));
                        return value;
                    }

                    if (haveXtra() && operator == sDefun) {
                        nArgs("defun", operandlist, 3, exp);
                        final Object symbol = car(operandlist);
                        if (!symbolp(symbol)) throw new LambdaJError("defun: not a symbol: " + printSEx(symbol) + '.' + errorExp(exp));
                        final Object params = cadr(operandlist);
                        if (!listp(params)) throw new LambdaJError("defun: expected a parameter list but got " + printSEx(params) + '.' + errorExp(exp));
                        final ConsCell envEntry = assoc(symbol, env);
                        if (envEntry != null) throw new LambdaJError("defun: '" + symbol + "' was already defined, current value: " + printSEx(cdr(envEntry)) + errorExp(exp));

                        final Object lambda = cons3(sLambda, cdr(operandlist), haveLexC() ? env : null);
                        topEnv.cdr = cons(cons(symbol, lambda), cdr(topEnv));
                        return lambda;
                    }

                    if (operator == sLambda) {
                        nArgs("lambda", operandlist, 2, exp);
                        if (haveLexC()) return cons3(sLambda, operandlist, env);
                        else return exp;
                    }

                    if (haveLabels() && operator == sLabels) { // labels bindings body -> object
                        nArgs("labels", operandlist, 2, exp);
                        ConsCell bindings, extenv = cons(cons(null, null), env);
                        // stick the functions into the extenv
                        for (bindings = (ConsCell) car(operandlist); bindings != null; bindings = (ConsCell) cdr(bindings)) { // todo circle check
                            final ConsCell currentFunc = (ConsCell)car(bindings);
                            final Object currentSymbol = symtab.intern((String)car(currentFunc));
                            final ConsCell lambda;
                            if (haveLexC()) lambda = cons3(sLambda, cdr(currentFunc), extenv);
                            else            lambda = cons (sLambda, cdr(currentFunc));
                            extenv.cdr = cons(cons(currentSymbol, lambda), cdr(extenv));
                        }
                        extenv.car = cadr(extenv);
                        extenv.cdr = cddr(extenv);

                        // run the function's expressions, the last one with TCO in case it's a tailcall
                        ConsCell body;
                        for (body = (ConsCell) cdr(operandlist); body != null && cdr(body) != null; body = (ConsCell) cdr(body))
                            eval(car(body), topEnv, extenv, stack+1, level+1);
                        if (body != null) {
                            exp = car(body); env = extenv; isTc = true; continue;
                        }
                    }

                    if (haveCond() && operator == sCond) {
                        for (ConsCell c = operandlist; c != null; c = (ConsCell) cdr(c)) {
                            if (eval(caar(c), topEnv, env, stack+1, level+1) != null) {
                                exp = cadar(c); isTc = true; continue tailcall;
                            }
                        }
                        return null;
                    }

                    // apply function to list
                    if (haveApply() && operator == sApply) {
                        twoArgs("apply", operandlist, exp);

                        func = eval(car(operandlist), topEnv, env, stack+1, level+1);
                        final Object _argList = eval(cadr(operandlist), topEnv, env, stack+1, level+1);
                        if (!listp(_argList)) throw new LambdaJError("apply: expected an argument list but got " + printSEx(_argList) + errorExp(exp));
                        argList = (ConsCell)_argList;
                        // fall through to "actually perform..."

                    // function call
                    } else {
                        func = eval(operator, topEnv, env, stack+1, level+1);
                        if (!listp(operandlist)) throw new LambdaJError("function aplication: expected an argument list but got " + printSEx(operandlist) + errorExp(exp));
                        argList = evlis(operandlist, topEnv, env, stack+1, level+1);
                        // fall through to "actually perform..."
                    }

                    // actually perform the function call that was set up by "apply" or "function call" above
                    if (consp(func)) {
                        final Object lambda = cdr(func);          // (params . bodylist)
                        final ConsCell closure = haveLexC() ? ((ConsCell)func).closure : env;  // lexical or dynamic env
                        nArgs("lambda application", lambda, 2, exp);
                        ConsCell extenv = zip(exp, closure, car(lambda), argList);

                        if (trace >= TRC_FUNC)  tracer.println(pfx(stack, level) + " #<lambda " + lambda + "> " + printSEx(extenv));
                        ConsCell body = (ConsCell) cdr(lambda);
                        for (; body != null && cdr(body) != null; body = (ConsCell) cdr(body))
                            eval(car(body), topEnv, extenv, stack+1, level+1);
                        if (body != null) {
                            exp = car(body); env = extenv; isTc = true; continue;
                        }
                    }
                    if (isPrim(func)) {
                        try { return applyPrimitive((Primitive) func, argList, stack, level); }
                        catch (LambdaJError e) { throw new LambdaJError(e.getMessage() + errorExp(exp)); }
                    }

                    throw new LambdaJError("function application: not a symbol or lambda: " + printSEx(func)
                                           + ". this was the result of evaluating the expression "
                                           + printSEx(car(operandlist)) + errorExp(exp));

                }

                // not a symbol/atom/cons - something is really wrong here
                // let's sprinkle some crack on him and get out of here, dave.
                throw new LambdaJError("cannot eval expression '" + printSEx(exp) + '\'');
            }

        } catch (Exception e) {
            throw e; // convenient breakpoint for errors
        } finally {
            dbgEvalDone(isTc ? "eval TC" : "eval", exp, env, stack, level);
        }
    }

    /** build an extended environment for a function invocation:<pre>
     *  loop over params and args
     *    construct a list (param arg)
     *    stick above list in front of the environment
     *  return extended environment</pre> */
    // todo for tail calls for dynamic environments re-use slots in the current methods environment with the same name
    private ConsCell zip(Object exp, ConsCell extenv, Object _params, ConsCell _args) {
        final Object paramList = _params == symtab.intern("nil") ? null : _params; // todo diesen Hack reparieren, hier sollte nie das symbol nil sondern ggf. null reinkommen, oder: nil ist wahrscheinlich voellig falsch implementiert
        if (!listp(paramList)) throw new LambdaJError("function application: expected a parameter list but got " + printSEx(_params) + errorExp(exp));

        ConsCell params = (ConsCell)paramList, args = _args;
        for ( ; params != null && args != null; params = (ConsCell) cdr(params), args = (ConsCell) cdr(args))
            extenv = cons(cons(car(params), car(args)), extenv);
        if (params != null)
            throw new LambdaJError("function application: not enough arguments. parameters w/o argument: " + printSEx(params) + errorExp(exp));
        if (args != null)
            throw new LambdaJError("function application: too many arguments. remaining arguments: " + printSEx(args) + errorExp(exp));
        return extenv;
    }

    private ConsCell evlis(ConsCell _list, ConsCell topEnv, ConsCell env, int stack, int level) {
        dbgEvalStart("evlis", _list, env, stack, level);
        ConsCell head = null, insertPos = null;
        Object list = _list;
        for (; list != null && _list != cdr(list); list = cdr(list)) {
            ConsCell currentArg = cons(eval(car(list), topEnv, env, stack+1, level+1), null);
            if (head == null) {
                head = currentArg;
                insertPos = head;
            }
            else {
                insertPos.cdr = currentArg;
                insertPos = currentArg;
            }
            if (!listp(cdr(list))) throw new LambdaJError("evlis: not a proper list: " + printSEx(_list));
        }
        dbgEvalDone("evlis", _list, head, stack, level);
        return head;
    }

    private static int nCells;  // todo sollte eigentlich nicht static sein, ists aber weils der konstruktor der stat Klasse ConsCell schreibt
    private int maxEnvLen;
    private int maxEvalStack;
    private int maxEvalLevel;

    /** spaces printed to the left indicate java stack usage, spaces+asterisks indicate Lisp call hierarchy depth.
     *  due to tail call optimization Java stack usage should be less than Lisp call hierarchy depth. */
    private void dbgEvalStart(String evFunc, Object exp, ConsCell env, int stack, int level) {
        if (trace >= TRC_STATS) {
            if (maxEvalStack < stack) maxEvalStack = stack;
            if (maxEvalLevel < level) maxEvalLevel = level;
            if (trace >= TRC_EVAL) {
                evFunc = fmtEvFunc(evFunc);

                final String pfx = pfx(stack, level);
                tracer.println(pfx + " " + evFunc + " (" + stack + '/' + level + "), exp:          " + printSEx(exp));
                if (trace >= TRC_ENV) {
                    tracer.println(pfx + " -> env size:" + length(env) + " env:     " + printSEx(env));
                }
            }
        }
    }

    private void dbgEvalDone(String evFunc, Object exp, Object env, int stack, int level) {
        if (trace >= TRC_STATS) {
            final int envLen = length(env);
            if (maxEnvLen < envLen) maxEnvLen = envLen;
            if (trace >= TRC_EVAL) {
                evFunc = fmtEvFunc(evFunc);
                final String pfx = pfx(stack, level);
                tracer.println(pfx + " " + evFunc + " (" + stack + '/' + level + ") done, exp was: " + printSEx(exp));
            }
        }
    }

    private static String fmtEvFunc(String func) {
        return (func + "          ").substring(0, 10);
    }

    private String pfx(int stack, int level) {
        final char[] cpfx = new char[stack*2];
        Arrays.fill(cpfx, ' ');

        char[] csfx = new char[3+(level - stack)*2];
        Arrays.fill(csfx, '*');
        return new String(cpfx) + new String(csfx);
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private static ConsCell cons(Object car, Object cdr) { return new ConsCell(car, cdr); }
    private static ConsCell cons3(Object car, Object cdr, ConsCell closure) { return new ConsCell(car, cdr, closure); }

    private static Object   car(ConsCell x)    { return x.car; }
    private static Object   car(Object x)      { return ((ConsCell)x).car; }
    private static Object   caar(ConsCell l)   { return car(car(l)); }
    private static Object   cadr(ConsCell l)   { return car(cdr(l)); }
    private static Object   cadar(ConsCell l)  { return car(cdr(car(l))); }
    private static Object   caddr(ConsCell l)  { return car(cddr(l)); }

    private static Object   cdr(ConsCell x)    { return x.cdr; }
    private static Object   cdr(Object x)      { return ((ConsCell)x).cdr; }
    private static Object   cddr(ConsCell l)   { return cdr(cdr(l)); }
    private static Object   cddr(Object l)     { return cdr(cdr(l)); }

    private static boolean  consp(Object x)    { return x instanceof ConsCell; }
    private static boolean  atom(Object x)     { return x == null || !(x instanceof ConsCell); } // !isCons(x)
    private static boolean  symbolp(Object x)  { return x == null || x instanceof String; } // null (alias nil) is a symbol too
    private static boolean  listp(Object x)    { return x == null || x instanceof ConsCell; } // null is a list too
    private static boolean  isPrim(Object x)   { return x instanceof Primitive; }

    private static boolean  numberp(Object x)  { return x instanceof Number; }
    private static boolean  stringp(Object x)  { return x instanceof LambdaJString; }

    private static int length(Object list) {
        if (list == null) return 0;
        int n = 0;
        for (Object l = list; l != null; l = cdr(l)) n++;
        return n;
    }

    private static Object nthcdr(int n, Object list) {
        if (list == null) return null;
        for (; list != null && n-- > 0; list = cdr(list)) ;
        return list;
    }

    /** note: searches using object identity, will work for interned symbols, won't work for e.g. numbers */
    private static ConsCell assoc(Object atom, Object maybeList) {
        if (atom == null) return null;
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("assoc: expected second argument to be a List but got " + printSEx(maybeList));
        for (ConsCell env = (ConsCell) maybeList ; env != null && maybeList != cdr(env); env = (ConsCell) cdr(env)) {
            if (atom == caar(env)) return (ConsCell) car(env);
        }
        return null;
    }

    private static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("listToArray: expected second argument to be a List but got " + printSEx(maybeList));
        final List<Object> ret = new ArrayList<>();
        for (ConsCell env = (ConsCell) maybeList; env != null && maybeList != cdr(env); env = (ConsCell) cdr(env))
            ret.add(car(env));
        return ret.toArray();
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (trace >= TRC_FUNC) {
            tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        }
        return primfn.apply(args);
    }

    /** transform {@code ob} into an S-expression, atoms are escaped */
    private static String printSEx(Object ob) {
        if (ob == null) return "nil";
        final StringBuffer sb = new StringBuffer(200);
        _printSEx(sb, ob, ob, true, true);
        return sb.toString();
    }

    private static String printObj(Object ob) {
        if (ob == null) return "nil";
        final StringBuffer sb = new StringBuffer(200);
        _printSEx(sb, ob, ob, true, false);
        return sb.toString();
    }

    private static void _printSEx(StringBuffer sb, Object list, Object current, boolean headOfList, boolean escapeAtoms) {
        while (true) {
            if (current == null) {
                sb.append("nil"); return;
            } else if (listp(current)) {
                if (headOfList) sb.append('(');
                if (car(current) == list) {
                    sb.append(headOfList ? "#<this cons>" : "#<this list>");
                } else {
                    _printSEx(sb, car(current), car(current), true, escapeAtoms);
                }
                if (cdr(current) != null) {
                    if (listp(cdr(current))) {
                        sb.append(' ');
                        if (list == cdr(current)) {
                            sb.append("#<circular list>)"); return;
                        } else {
                            current = cdr(current); headOfList = false; continue;
                        }
                    } else if (headOfList) {
                        sb.append(" . ");
                        _printSEx(sb, list, cdr(current), false, escapeAtoms);
                        sb.append(')');
                        return;
                    } else {
                        sb.append(' ');
                        _printSEx(sb, list, cdr(current), false, escapeAtoms); // must be an atom
                        sb.append(')');
                        return;
                    }
                } else {
                    sb.append(')');
                    return;
                }
            } else if (escapeAtoms && symbolp(current)) {
                if (containsSExSyntaxOrBlank(current.toString())) {
                    sb.append('|').append(current.toString()).append('|');
                    return;
                }
                sb.append(current.toString()); return;
            } else if (isPrim(current)) {
                sb.append("#<primitive>"); return;
            } else if (escapeAtoms && stringp(current)) {
                sb.append('"').append(escapeString(current.toString())).append('"'); return;
            } else if (atom(current)) {
                sb.append(current.toString()); return;
            } else {
                sb.append("<internal error>"); return;
            }
        }
    }

    private static final Pattern dQuotePattern = Pattern.compile("[\"]");
    private static final Pattern bSlashPattern = Pattern.compile("[\\\\]([^\"])");
    /** prepend " and \ by a \ */
    private static String escapeString(String s) {
        if (s == null) return null;
        s = dQuotePattern.matcher(s).replaceAll("\\\\\"");
        s = bSlashPattern.matcher(s).replaceAll("\\\\\\\\$1");
        return s;
    }



    /// error checking utils, used by interpreter and primitives
    private static void noArgs(String func, ConsCell a) {
        if (a != null) throw new LambdaJError(func + ": expected no arguments but got " + printSEx(a));
    }

    private static void oneArg(String func, Object a) {
        if (a == null) throw new LambdaJError(func + ": expected one argument but no argument was given");
        if (cdr(a) != null) throw new LambdaJError(func + ": expected one argument but got extra arg(s) " + printSEx(cdr(a)));
    }

    private static void oneOrMoreArgs(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected at least one argument but no argument was given");
    }

    private static void twoArgs(String func, Object a) {
        twoArgs(func, a, null);
    }

    private static void twoArgs(String func, Object a, Object exp) {
        if (a == null) throw new LambdaJError(func + ": expected two arguments but no argument was given" + errorExp(exp));
        if (cdr(a) == null) throw new LambdaJError(func + ": expected two arguments but only one argument was given" + errorExp(exp));
        if (cddr(a) != null) throw new LambdaJError(func + ": expected two arguments but got extra arg(s) " + printSEx(cddr(a)) + errorExp(exp));
    }

    /** at least {@code min} args */
    private static void nArgs(String func, Object a, int min, Object exp) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(func + ": expected " + min + " arguments or more but got only " + actualLength + errorExp(exp));
    }

    private static void nArgs(String func, Object a, int min, int max, Object exp) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(func + ": expected " + min + " to " + max + " arguments but got only " + actualLength + errorExp(exp));
        if (actualLength > max) throw new LambdaJError(func + ": expected " + min + " to " + max + " arguments but got extra arg(s) " + printSEx(nthcdr(max, a)) + errorExp(exp));
    }

    private static void onePair(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected one Pair argument but no argument was given");
        if (!listp(car(a))) throw new LambdaJError(func + ": expected one Pair argument but got " + printSEx(a));
        if (cdr(a) != null) throw new LambdaJError(func + ": expected one Pair argument but got extra arg(s) " + printSEx(cdr(a)));
    }

    /** arguments if any must be only numbers */
    private static void numberArgs(String func, ConsCell a) {
        if (a == null) return;
        for (; a != null; a = (ConsCell) cdr(a)) {
            if (!numberp(car(a)) || (cdr(a) != null && !consp(cdr(a))))
                throw new LambdaJError(func + ": expected only number arguments but got " + printSEx(a));
        }
    }

    private static void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numberArgs(func, a);
    }

    /** the given arg must be a LambdaJString */
    private void stringArg(String func, String arg, Object a) {
        if (!(car(a) instanceof LambdaJString))
            throw new LambdaJError(func + ": expected " + arg + " to be a String but got " + printSEx(car(a)));
    }

    private static String errorExp(Object exp) {
        if (exp == null) return "";
        return System.lineSeparator() + "error occurred in expression " + printSEx(exp);
    }



    /// runtime for Lisp programs
    private Object boolResult(boolean b) { return b ? expTrue : null; }

    /** generate a comparison operator */
    private Object makeCompareOp(ConsCell args, String opName, IntPredicate pred) {
        twoArgs(opName, args);
        numberArgs(opName, args);
        final double lhs = (Double)car(args);
        final double rhs = (Double)car(cdr(args));
        return boolResult(pred.test(Double.compare(lhs,  rhs)));
    }

    /** generate operator for zero or more args */
    private static Object makeAddOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        numberArgs(opName, args);
        for (; args != null; args = (ConsCell) cdr(args))
            startVal = op.applyAsDouble(startVal, (Double)car(args));
        return startVal;
    }

    /** generate operator for one or more args */
    private static Object makeSubOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        oneOrMoreNumbers("-", args);
        double result = (Double)car(args);
        if (cdr(args) == null) return op.applyAsDouble(startVal, result);
        for (args = (ConsCell) cdr(args); args != null; args = (ConsCell) cdr(args))
            result = op.applyAsDouble(result, (Double)car(args));
        return result;
    }

    private String stringFormat(ConsCell a) {
        nArgs("string-format", a, 1, null);
        stringArg("string-format", "first argument", a);
        String s = ((LambdaJString)car(a)).value;
        try {
            return String.format(s, listToArray(cdr(a)));
        } catch (IllegalFormatException e) {
            throw new LambdaJError("string-format: illegal format string and/ or arguments: " + e.getMessage()
            + "\nerror ocurred processing the argument(s) " + printSEx(a));
        }
    }

    private String stringFormatLocale(ConsCell a) {
        nArgs("string-format-locale", a, 2, null);
        String locString;
        if (car(a) != null) {
            stringArg("string-format-locale", "first argument", a);
            locString = ((LambdaJString)car(a)).value;
        } else locString = null;
        stringArg("string-format-locale", "second argument", cdr(a));
        String s = ((LambdaJString)car(cdr(a))).value;
        try {
            if (locString == null) return String.format(s, listToArray(cdr(cdr(a))));
            Locale loc = Locale.forLanguageTag(locString);
            return String.format(loc, s, listToArray(cdr(cdr(a))));
        } catch (IllegalFormatException e) {
            throw new LambdaJError("string-format-locale: illegal format string and/ or arguments: " + e.getMessage()
            + "\nerror ocurred processing the argument(s) " + printSEx(a));
        }
    }



    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@code symtab} on the fly */
    private ConsCell environment(ConsCell env, ObjectReader lispStdin, ObjectWriter lispStdout) {
        if (haveIO()) {
            final Primitive freadobj =  a -> { noArgs("read", a);    return lispStdin == null ? null : lispStdin.readObj(); };
            final Primitive fwriteobj = a -> {
                oneArg("write", a);
                if (lispStdout == null) throw new LambdaJError("write: lispStdout is nil");
                lispStdout.printObj(car(a)); return expTrue;
            };

            final Primitive fwriteln =  a -> {
                nArgs("writeln", a, 0, 1, null);
                if (lispStdout == null) throw new LambdaJError("writeln: lispStdout is nil");
                if (a == null) {
                    lispStdout.printEol();
                    return expTrue;
                }
                lispStdout.printObj(car(a));
                lispStdout.printEol();
                return expTrue;
            };

            env = cons(cons(symtab.intern("read"),    freadobj),
                  cons(cons(symtab.intern("write"),   fwriteobj),
                  cons(cons(symtab.intern("writeln"), fwriteln),
                  env)));

        }

        if (haveString()) {
            final Primitive fstringp =            a -> { oneArg("stringp", a); return boolResult(stringp(car(a))); };

            env = cons(cons(symtab.intern("stringp"), fstringp),
                  env);

            if (haveUtil()) {
                final Primitive fstringformat =       a -> new LambdaJString(stringFormat(a));
                final Primitive fstringformatLocale = a -> new LambdaJString(stringFormatLocale(a));

                env = cons(cons(symtab.intern("string-format"),        fstringformat),
                      cons(cons(symtab.intern("string-format-locale"), fstringformatLocale),
                      env));
            }

            if (haveIO()) {
                final Primitive fformat = a -> {
                    if (lispStdout == null) throw new LambdaJError("format: lispStdout is nil");
                    lispStdout.printString(stringFormat(a));
                    return expTrue;
                };

                final Primitive fformatLocale = a -> {
                    if (lispStdout == null) throw new LambdaJError("format: lispStdout is nil");
                    lispStdout.printString(stringFormatLocale(a));
                    return expTrue;
                };

                env = cons(cons(symtab.intern("format"),        fformat),
                      cons(cons(symtab.intern("format-locale"), fformatLocale),
                      env));
            }
        }

        if (haveT())
            env = cons(cons(symtab.intern("t"), symtab.intern("t")),
                  env);

        if (haveNil())
            env = cons(cons(symtab.intern("nil"), null),
                  env);

        if (haveUtil()) {
            final Primitive fnull =     a -> { oneArg("null?", a);   return boolResult(car(a) == null); };
            final Primitive fconsp =    a -> { oneArg("consp", a);   return boolResult(consp  (car(a))); };
            final Primitive fsymbolp =  a -> { oneArg("symbolp", a); return boolResult(symbolp(car(a))); };
            final Primitive flistp =    a -> { oneArg("listp", a);   return boolResult(listp  (car(a))); };
            final Primitive fassoc =    a -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); };

            env = cons(cons(symtab.intern("consp"),   fconsp),
                  cons(cons(symtab.intern("symbolp"), fsymbolp),
                  cons(cons(symtab.intern("listp"),   flistp),
                  cons(cons(symtab.intern("null?"),   fnull),
                  cons(cons(symtab.intern("assoc"),   fassoc),
                  env)))));


            env = cons(cons(symtab.intern("internal-time-units-per-second"), new Double(1e9)),
                  cons(cons(symtab.intern("get-internal-real-time"), (Primitive)a -> new Double(System.nanoTime())),
                  env));
        }

        if (haveAtom()) {
            final Primitive fatom =     a -> { oneArg("atom", a);    return boolResult(atom   (car(a))); };

            env = cons(cons(symtab.intern("atom"), fatom),
                       env);
        }

        if (haveDouble()) {
            final Primitive fnumberp =  args -> { oneArg("numberp", args); return boolResult(numberp(car(args))); };
            final Primitive fnumbereq = args -> makeCompareOp(args, "=",  compareResult -> compareResult == 0);
            final Primitive flt =       args -> makeCompareOp(args, "<",  compareResult -> compareResult <  0);
            final Primitive fle =       args -> makeCompareOp(args, "<=", compareResult -> compareResult <= 0);
            final Primitive fgt =       args -> makeCompareOp(args, ">",  compareResult -> compareResult >  0);
            final Primitive fge =       args -> makeCompareOp(args, ">=", compareResult -> compareResult >= 0);
            final Primitive fadd =      args -> makeAddOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs);
            final Primitive fmul =      args -> makeAddOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs);
            final Primitive fsub  =     args -> makeSubOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs);
            final Primitive fquot =     args -> makeSubOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs);

            final Primitive fmod = args -> {
                twoArgs("mod", args);
                numberArgs("mod", args);
                return (Double)car(args) % (Double)car(cdr(args));
            };

            env = cons(cons(symtab.intern("+"),       fadd),
                  cons(cons(symtab.intern("-"),       fsub),
                  cons(cons(symtab.intern("*"),       fmul),
                  cons(cons(symtab.intern("/"),       fquot),
                  cons(cons(symtab.intern("="),       fnumbereq),
                  cons(cons(symtab.intern(">"),       fgt),
                  cons(cons(symtab.intern(">="),      fge),
                  cons(cons(symtab.intern("<"),       flt),
                  cons(cons(symtab.intern("<="),      fle),
                  cons(cons(symtab.intern("mod"),     fmod),
                  cons(cons(symtab.intern("numberp"), fnumberp),
                  env)))))))))));
        }

        if (haveEq()) {
            final Primitive feq =       a -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); };

            env = cons(cons(symtab.intern("eq"), feq),
                       env);
        }

        if (haveCons()) {
            final Primitive fcons =     a -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); };
            final Primitive fcar =      a -> { onePair("car", a);    if (car(a) == null) return null; return caar(a); };
            final Primitive fcdr =      a -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); };

            env = cons(cons(symtab.intern("cdr"),     fcdr),
                  cons(cons(symtab.intern("car"),     fcar),
                  cons(cons(symtab.intern("cons"),    fcons),
                  env)));
        }

        return cons(cons(null, null), env); // top env begins with (nil . nil), this is where define will insert stuff.
    }



    /** <p>Build environment, read a single S-expression from {@code in}, invoke {@code eval()} and return result.
     *
     *  <p>After the expression was read from {@code in}, the primitive function {@code read} (if used)
     *  will read S-expressions from {@code in} as well,
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpression(ReadSupplier in, WriteConsumer out) {
        nCells = 0; maxEnvLen = 0;
        Parser parser = new SExpressionParser(in);
        setSymtab(parser);
        ObjectWriter outWriter = new SExpressionWriter(out);
        final ConsCell env = environment(null, parser, outWriter);
        final Object exp = parser.readObj();
        final Object result = eval(exp, env, env, 0, 0);
        traceStats();
        return result;
    }

    /** <p>Build environment, repeatedly read an S-expression from {@code program} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read S-expressions from {@code in}
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpressions(ReadSupplier program, ReadSupplier in, WriteConsumer out) {
        Parser parser = new SExpressionParser(program);
        ObjectReader inReader = new SExpressionParser(in);
        ObjectWriter outWriter = new SExpressionWriter(out);
        return interpretExpressions(parser, inReader, outWriter, (_symtab, _in, _out) -> null);
    }

    /** <p>Build environment, repeatedly read an expression from {@code parser} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read expressions from {@code inReader},
     *  and {@code write}/ {@code writeln} will write Objects to {@code out}. */
    public Object interpretExpressions(Parser parser, ObjectReader inReader, ObjectWriter outWriter, CustomBuiltinsSupplier customEnv) {
        nCells = 0; maxEnvLen = 0;
        setSymtab(parser);
        final ConsCell customEnvironment = customEnv == null ? null : customEnv.customEnvironment(parser, inReader, outWriter);
        final ConsCell env = environment(customEnvironment, inReader, outWriter);
        Object exp = parser.readObj();
        while (true) {
            final Object result = eval(exp, env, env, 0, 0);
            traceStats();
            exp = parser.readObj();
            if (exp == null) return result;
        }
    }

    private void traceStats() {
        if (trace >= TRC_EVAL) {
            tracer.println("*** max eval nesting: " + maxEvalLevel + " ***");
            tracer.println("*** max stack used:   " + maxEvalStack + " ***");
        }
        if (trace >= TRC_STATS) {
            tracer.println("*** total ConsCells:  " + nCells + " ***");
            tracer.println("*** max env length:   " + maxEnvLen + " ***");
        }
    }

    /** main() for interactive commandline use */
    public static void main(String args[]) {
        if (hasFlag("--version", args)) {
            showVersion();
            return;
        }

        if (hasFlag("--help", args) || hasFlag("--usage", args)) {
            showVersion();
            System.out.println();
            showUsage();
            return;
        }

        int trace = TRC_NONE;
        if (hasFlag("--trace=stats", args)) trace = TRC_STATS;
        if (hasFlag("--trace=eval", args))  trace = TRC_EVAL;
        if (hasFlag("--trace=env", args))   trace = TRC_ENV;
        if (hasFlag("--trace", args))       trace = TRC_LEX;

        int features = HAVE_ALL_LEXC;
        if (hasFlag("--dyn", args))         features = HAVE_ALL_DYN;
        else if (hasFlag("--lex", args))    features = HAVE_ALL_LEXC;

        if (hasFlag("--no-nil", args))      features &= ~HAVE_NIL;
        if (hasFlag("--no-t", args))        features &= ~HAVE_T;
        if (hasFlag("--no-extra", args))    features &= ~HAVE_XTRA;
        if (hasFlag("--no-double", args))   features &= ~HAVE_DOUBLE;
        if (hasFlag("--no-string", args))   features &= ~HAVE_STRING;
        if (hasFlag("--no-io", args))       features &= ~HAVE_IO;
        if (hasFlag("--no-util", args))     features &= ~HAVE_UTIL;

        if (hasFlag("--no-labels", args))   features &= ~HAVE_LABELS;
        if (hasFlag("--no-cons", args))     features &= ~HAVE_CONS;
        if (hasFlag("--no-cond", args))     features &= ~HAVE_COND;
        if (hasFlag("--no-apply", args))    features &= ~HAVE_APPLY;

        if (hasFlag("--no-atom", args))     features &= ~HAVE_ATOM;
        if (hasFlag("--no-eq", args))       features &= ~HAVE_EQ;
        if (hasFlag("--no-quote", args))    features &= ~HAVE_QUOTE;

        if (hasFlag("--min+", args))        features = HAVE_MINPLUS;
        if (hasFlag("--min", args))         features = HAVE_MIN;
        if (hasFlag("--lambda+", args))     features = HAVE_LAMBDAPLUS;
        if (hasFlag("--lambda", args))      features = HAVE_LAMBDA;

        final LambdaJ interpreter = new LambdaJ(features, trace);

        final boolean printResult = hasFlag("--result", args);

        if (argError(args)) {
            System.err.println("LambdaJ: exiting because of previous errors.");
            return;
        }

        final boolean istty = null != System.console();
        if (istty) {
            System.out.println("Enter a Lisp expression or :command (or enter :h for command help or :q to exit):");
            System.out.println();

            boolean isInit = false;
            SExpressionParser parser = null;
            ConsCell env = null;
            for (;;) {
                if (!isInit) {
                    nCells = 0; interpreter.maxEnvLen = 0;
                    parser = interpreter.new SExpressionParser(System.in::read);
                    interpreter.setSymtab(parser);
                    ObjectWriter outWriter = new SExpressionWriter(System.out::print);
                    env = interpreter.environment(null, parser, outWriter);
                    isInit = true;
                }

                System.out.print("LambdaJ> ");
                System.out.flush();

                try {
                    final Object exp = parser.readObj();

                    if (":q".equals(exp) || exp == null && parser.look == EOF) {
                        System.out.println("bye."); System.out.println();  return;
                    }

                    if (":h".equals(exp)) {
                        System.out.println("Available commands:");
                        System.out.println("  :h ...... this help screen");
                        System.out.println("  :env .... list current global environment");
                        System.out.println("  :init ... re-init global environment");
                        System.out.println("  :q ...... quit LambdaJ");
                        System.out.println();
                        continue;
                    }

                    if (":env".equals(exp)) {
                        System.out.println(env.toString()); System.out.println("env length: " + length(env));  System.out.println(); continue;
                    }

                    if (":init".equals(exp)) {
                        isInit = false;  continue;
                    }

                    final Object result = interpreter.eval(exp, env, env, 0, 0);
                    System.out.println();
                    interpreter.traceStats();
                    System.out.println("result: " + result);
                    System.out.println();
                } catch (LambdaJError e) {
                    System.out.println();
                    System.out.println(e.toString());
                    System.out.println();
                }
            }
        }

        try {
            final String result = printSEx(interpreter.interpretExpressions(System.in::read, System.in::read, System.out::print));
            if (printResult) {
                System.out.println(result);
            }
        } catch (LambdaJError e) {
            System.err.println();
            System.err.println(e.toString());
            System.exit(1);
        }
    }

    private static boolean hasFlag(String flag, String[] args) {
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (flag.equals(arg)) {
                args[i] = null; // consume the arg
                return true;
            }
        }
        return false;
    }

    private static boolean argError(String[] args) {
        boolean err = false;
        for (String arg: args) {
            if (arg != null && arg.startsWith("-")) {
                System.err.println("LambdaJ: unknown commandline argument " + arg);
                System.err.println("use '--help' to show available commandline arguments");
                err = true;
            }
        }
        return err;
    }

    private static void showVersion() {
        System.out.println("LambdaJ $Id: LambdaJ.java,v 1.97 2020/10/19 21:38:08 Robert Exp $");
    }

    // for updating the usage message edit the file usage.txt and copy/paste its contents here between double quotes
    private static void showUsage() {
        System.out.println("Usage:\n"
                + "\n"
                + "interactive:\n"
                + "java -jar lambdaj.jar [commandline arguments]*\n"
                + "\n"
                + "non-interactive:\n"
                + "java -jar lambdaj.jar [commandline arguments]* < lisp-source.lisp\n"
                + "\n"
                + "Commandline arguments are:\n"
                + "\n"
                + "Misc:\n"
                + "--version .....  show version and exit\n"
                + "--help ........  show this message and exit\n"
                + "--trace=stats .  print stack and memory stats at end\n"
                + "--trace=eval ..  print internal interpreter info during executing programs\n"
                + "--trace=eval ..  print more internal interpreter info during executing programs\n"
                + "--trace .......  print lots of internal interpreter info during\n"
                + "                 reading/ parsing/ executing programs\n"
                + "\n"
                + "Feature flags:\n"
                + "\n"
                + "--dyn .........  use dynamic environments\n"
                + "--lex .........  use lexical environments, this is the default\n"
                + "\n"
                + "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n"
                + "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n"
                + "--no-extra ....  no special form 'define' or 'if'\n"
                + "--no-double ...  no number support\n"
                + "--no-string ...  no string support\n"
                + "--no-io .......  no primitive functions read/ write/ writeln\n"
                + "--no-util .....  no primitive functions consp/ symbolp/ listp/ null?/ assoc/"
                + "                 string-format/ string-format-locale\n"
                + "\n"
                + "--min+ ........  turn off all above features, leaving a Lisp with 10 primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, cons, car, cdr, lambda, apply, cond, labels\n"
                + "\n"
                + "--no-apply ....  no special form 'apply'\n"
                + "--no-labels ...  no special form 'labels' (hint: use Y-combinator instead)\n"
                + "\n"
                + "--min .........  turn off all above features, leaving a Lisp with 8 special forms/ primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, cons, car, cdr, lambda, cond\n"
                + "\n"
                + "--no-cons .....  no primitive functions cons/ car/ cdr\n"
                + "--no-cond .....  no special form 'cond'\n"
                + "\n"
                + "--lambda+ .....  turn off pretty much everything except Lambda calculus,\n"
                + "                 leaving a Lisp with 4 primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, lambda\n"
                + "\n"
                + "--no-atom .....  no primitive function 'atom'\n"
                + "--no-eq .......  no primitive function 'eq'\n"
                + "--no-quote ....  no special form quote\n"
                + "\n"
                + "--lambda ......  turns off yet even more stuff, leaving I guess bare bones Lambda calculus:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   lambda");
    }
}
