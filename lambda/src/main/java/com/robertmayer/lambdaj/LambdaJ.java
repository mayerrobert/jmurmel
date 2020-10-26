/* LambdaJ is Copyright (C) 2020 Robert Mayer. All rights reserved.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

package com.robertmayer.lambdaj;

import java.io.IOException;
import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.zone.ZoneRules;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.IllegalFormatException;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.concurrent.TimeUnit;
import java.util.function.DoubleBinaryOperator;
import java.util.function.IntPredicate;
import java.util.function.Supplier;
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
        public LambdaJError(String msg, Object... params) { super(String.format(msg, params), null, false, false); }
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
        public ConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
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

    HAVE_LISPEOL = 1 << 16,


    HAVE_LAMBDA     = 0,
    HAVE_LAMBDAPLUS = HAVE_LAMBDA | HAVE_ATOM | HAVE_QUOTE | HAVE_EQ,
    HAVE_MIN        = HAVE_LAMBDAPLUS | HAVE_CONS | HAVE_COND,
    HAVE_MINPLUS    = HAVE_MIN | HAVE_APPLY | HAVE_LABELS,
    HAVE_ALL_DYN    = HAVE_MINPLUS | HAVE_NIL | HAVE_T | HAVE_XTRA | HAVE_DOUBLE | HAVE_STRING | HAVE_IO | HAVE_UTIL,

    HAVE_ALL_LEXC   = HAVE_ALL_DYN | HAVE_LEXC;
    ;
    private final int features;

    private boolean haveLabels()  { return (features & HAVE_LABELS)  != 0; }
    private boolean haveNil()     { return (features & HAVE_NIL)     != 0; }
    private boolean haveT()       { return (features & HAVE_T)       != 0; }
    private boolean haveXtra()    { return (features & HAVE_XTRA)    != 0; }
    private boolean haveDouble()  { return (features & HAVE_DOUBLE)  != 0; }
    private boolean haveString()  { return (features & HAVE_STRING)  != 0; }
    private boolean haveIO()      { return (features & HAVE_IO)      != 0; }
    private boolean haveUtil()    { return (features & HAVE_UTIL)    != 0; }
    private boolean haveApply()   { return (features & HAVE_APPLY)   != 0; }
    private boolean haveCons()    { return (features & HAVE_CONS)    != 0; }
    private boolean haveCond()    { return (features & HAVE_COND)    != 0; }
    private boolean haveAtom()    { return (features & HAVE_ATOM)    != 0; }
    private boolean haveEq()      { return (features & HAVE_EQ)      != 0; }
    private boolean haveQuote()   { return (features & HAVE_QUOTE)   != 0; }
    private boolean haveLexC()    { return (features & HAVE_LEXC)    != 0; }
    private boolean haveLispEOL() { return (features & HAVE_LISPEOL) != 0; }

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
    public class SExpressionWriter implements ObjectWriter {
        private final WriteConsumer out;  // printObj() and printEol() will write to this

        public SExpressionWriter(WriteConsumer out) { this.out = out; }
        @Override public void printObj(Object o) { if (o == null && !haveNil()) out.print("()"); printSEx(out, o); }
        @Override public void printString(String s) { out.print(s); }
        @Override public void printEol() { out.print(System.lineSeparator()); }
    }

    /// scanner, symboltable and S-expression parser
    /** This class will read and parse S-Expressions (while generating symbol table entries)
     *  from the given {@link ReadSupplier} */
    public class SExpressionParser implements Parser {
        private ReadSupplier in;    // readObj() will read from this
        private boolean init;

        private int lineNo = 1, charNo = 0;
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
                charNo = 1;
            } else if (c != '\r' && c != EOF) charNo++;
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
                    if (look == EOF) throw new LambdaJError("line %d:%d: string literal is missing closing \"", lineNo, charNo);
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
                    throw new LambdaJError("line %d:%d: '%s' is not a valid symbol or number", lineNo, charNo, tokenToString(token));
                }
            } else if (haveString() && token[0] == '"') {
                tok = new LambdaJString(tokenToString(token).substring(1));
            } else if (token[0] == '\0'){
                tok = null;
            } else {
                tok = tokenToString(token);
            }
            if (trace >= TRC_LEX)
                tracer.println("*** scan  token  |" + tok + '|');
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
            if (haveNil() && !tokEscape && tok instanceof String && "nil".equalsIgnoreCase((String) tok)) {
                return null;
            }
            if (!tokEscape && ")".equals(tok)) {
                throw new LambdaJError("line %d:%d: unexpected ')'", lineNo, charNo);
            }
            if (!tokEscape && "(".equals(tok)) {
                Object list = readList();
                if (!tokEscape && ".".equals(tok)) {
                    Object cdr = readList();
                    if (cdr(cdr) != null) throw new LambdaJError("line %d:%d: illegal end of dotted list: %s", lineNo, charNo, printSEx(cdr));
                    Object cons = combine(list, car(cdr));
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
            if (tok == null) throw new LambdaJError("line %d:%d: cannot read list. missing ')'?", lineNo, charNo);
            if (!tokEscape) {
                if (")".equals(tok)) return null;
                if (".".equals(tok)) return null;
            }
            final Object tmp = readObject();
            if (symbolp(tmp)) return cons(tmp, readList());
            else return cons(tmp, readList());
        }
    }

    /// symboltable Object
    private SymbolTable symtab;

    /** look up the symbols for special forms only once on first use.
     *  the suppliers below will do a lookup on first use and then replace themselves by another supplier
     *  that simply returns the cached value */
    private void setSymtab(SymbolTable symtab) {
        this.symtab = symtab;

        // (re-)read the new symtab
        sLambda = symtab.intern("lambda");
        if (haveQuote())  sQuote  = symtab.intern("quote");
        if (haveCond())   sCond   = symtab.intern("cond");
        if (haveLabels()) sLabels = symtab.intern("labels");

        if (haveXtra())   sEval   = symtab.intern("eval");
        if (haveXtra())   sIf     = symtab.intern("if");
        if (haveXtra())   sDefine = symtab.intern("define");
        if (haveXtra())   sDefun  = symtab.intern("defun");
        if (haveXtra())   sLetStar = symtab.intern("let*");
        if (haveXtra())   sLetrec = symtab.intern("letrec");

        if (haveApply())  sApply  = symtab.intern("apply");
        if (haveXtra())   sProgn  = symtab.intern("progn");

        expTrue = () -> { Object s = makeExpTrue(); expTrue = () -> s; return s; };
    }

    /** well known symbols for special forms */
    private Object sLambda, sQuote, sCond, sLabels, sEval, sIf, sDefine, sDefun, sLetStar, sLetrec, sApply, sProgn;
    private Supplier<Object> expTrue;

    private Object makeExpTrue() {
        if (haveT()) return symtab.intern("t"); // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (haveQuote()) return cons(symtab.intern("quote"), cons(symtab.intern("t"), null));
        else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
    }



    /// eval - the heart of most if not all Lisp interpreters
    private Object eval(Object exp, ConsCell topEnv, ConsCell env, int stack, int level) {
        boolean isTc = false;
        try {
            stack++;

            tailcall:
            while (true) {
                level++;
                dbgEvalStart(isTc ? "eval TC" : "eval", exp, env, stack, level);

                if (symbolp(exp)) {                 // this line is a convenient breakpoint
                    if (exp == null) return null;
                    final ConsCell envEntry = assoc(exp, env);
                    if (envEntry != null) return cdr(envEntry);
                    throw new LambdaJError("%s: '%s' is undefined", "eval", exp);
                }

                if (atom(exp)) {
                    return exp;
                }

                if (consp(exp)) {
                    final Object operator = car(exp);      // first element of the of the form should be a symbol or an expression that computes a symbol
                    if (!listp(cdr(exp))) throw new LambdaJError("%s: expected an operand list to follow operator but got %s%s", "eval", printSEx(exp), errorExp(exp));
                    final ConsCell arguments = (ConsCell) cdr(exp);   // list with remaining atoms/ expressions



                    /// special forms

                    // (quote exp) -> exp
                    if (haveQuote() && operator == sQuote) {
                        oneArg("quote", arguments);
                        return car(arguments);
                    }

                    // (lambda (params...) forms...) -> lambda or closure
                    if (operator == sLambda) {
                        nArgs("lambda", arguments, 2, exp);
                        if (haveLexC()) return makeClosure(arguments, env);
                        else return exp;
                    }



                    /// special forms that change the global environment

                    // (define symbol exp) -> symbol with a side of global environment extension
                    if (haveXtra() && operator == sDefine) {
                        twoArgs("define", arguments, exp);
                        final Object symbol = car(arguments); // todo ob statt symbol eine expression erlaubt sein sollte? expression koennte symbol errechnen
                                                              // ggf. symbol UND expression zulassen: if (symbolp(cdr(exp))...
                        if (!symbolp(symbol)) throw new LambdaJError("%s: not a symbol: %s%s", "define", printSEx(symbol), errorExp(exp));
                        final ConsCell envEntry = assoc(symbol, env);
                        if (envEntry != null) throw new LambdaJError("%s: '%s' was already defined, current value: %s%s", "define", symbol, printSEx(cdr(envEntry)), errorExp(exp));

                        final Object value = eval(cadr(arguments), topEnv, env, stack, level);
                        topEnv.cdr = cons(cons(symbol, value), cdr(topEnv));
                        return symbol;
                    }

                    // (defun symbol (params...) forms...) -> symbol with a side of global environment extension
                    if (haveXtra() && operator == sDefun) {
                        nArgs("defun", arguments, 3, exp);
                        final Object symbol = car(arguments);
                        if (!symbolp(symbol)) throw new LambdaJError("%s: not a symbol: %s%s", "defun", printSEx(symbol), errorExp(exp));
                        final Object params = cadr(arguments);
                        if (!listp(params)) throw new LambdaJError("%s: expected a parameter list but got %s%s", "defun", printSEx(params), errorExp(exp));
                        final ConsCell envEntry = assoc(symbol, env);
                        if (envEntry != null) throw new LambdaJError("%s: '%s' was already defined, current value: %s%s", "defun", symbol, printSEx(cdr(envEntry)), errorExp(exp));

                        final Object lambda = makeClosure(cdr(arguments), env);
                        topEnv.cdr = cons(cons(symbol, lambda), cdr(topEnv));
                        return symbol;
                    }



                    /// special forms that run expressions

                    // (eval form) -> object
                    if (operator == sEval) {
                        oneArg("eval", arguments);
                        exp = eval(car(arguments), topEnv, env, stack, level); isTc = true; continue tailcall;
                    }

                    // (if condform form optionalform) -> object
                    if (haveXtra() && operator == sIf) {
                        nArgs("if", arguments, 2, 3, exp);
                        if (eval(car(arguments), topEnv, env, stack, level) != null) {
                            exp = cadr(arguments); isTc = true; continue tailcall;
                        } else if (cddr(arguments) != null) {
                            exp = caddr(arguments); isTc = true; continue tailcall;
                        } else return null;
                    }

                    // "forms" will be set up depending on the special form and then used in "eval a list of forms" below
                    ConsCell forms = null;

                    // (progn forms...) -> object
                    if (haveXtra() && operator == sProgn) {
                        if (!consp(arguments)) throw new LambdaJError("%s: expected a list of forms but got %s%s", "progn", printSEx(arguments), errorExp(exp));
                        forms = arguments;
                        // fall through to "eval a list of forms"

                    // (cond (condform forms...)... ) -> object
                    } else if (haveCond() && operator == sCond) {
                        for (ConsCell c = arguments; c != null; c = (ConsCell) cdr(c)) {
                            if (!listp(car(c))) throw new LambdaJError("cond: malformed cond expression. was expecting a list (condexpr forms...) but got %s%s",
                                                                       printSEx(car(c)), errorExp(exp));
                            if (eval(caar(c), topEnv, env, stack, level) != null) {
                                forms = (ConsCell) cdar(c);
                                break;
                            }
                        }
                        if (forms == null) return null; // no condition was true
                        // fall through to "eval a list of forms"

                    // (labels ((symbol (params...) forms...)...) forms...) -> object
                    } else if (haveLabels() && operator == sLabels) {
                        nArgs("labels", arguments, 2, exp);
                        ConsCell bindings, extenv = cons(cons(null, null), env);
                        // stick the functions into the extenv
                        for (bindings = (ConsCell) car(arguments); bindings != null; bindings = (ConsCell) cdr(bindings)) { // todo circle check, dotted list
                            final ConsCell currentFunc = (ConsCell)car(bindings);
                            final Object currentSymbol = symtab.intern((String)car(currentFunc));
                            final ConsCell lambda = makeClosure(cdr(currentFunc), extenv);
                            extenv.cdr = cons(cons(currentSymbol, lambda), cdr(extenv));
                        }
                        extenv.car = cadr(extenv);  extenv.cdr = cddr(extenv);
                        forms = (ConsCell) cdr(arguments);  env = extenv;
                        // fall through to "eval a list of forms"

                    // (let* optsymbol? (bindings...) forms...) -> object
                    // (letrec optsymbol? (bindings...) forms...) -> object
                    } else if (haveXtra() && (operator == sLetrec) || operator == sLetStar) {
                        final boolean rec = operator == sLetrec;
                        final boolean named = symbolp(car(arguments));
                        final ConsCell let = named ? (ConsCell)cdr(arguments) : arguments;
                        final ConsCell bindings = (ConsCell)car(let);

                        final ConsCell bodyEnvEntry = cons(null, null);
                        ConsCell extenv = cons(bodyEnvEntry, env);
                        for (ConsCell binding = bindings; binding != null; binding = (ConsCell)cdr(binding)) {
                            final ConsCell newBinding = cons(caar(binding), null);
                            if (rec) extenv.cdr = cons(newBinding, cdr(extenv));
                            newBinding.cdr = eval(cadar(binding), topEnv, extenv, stack, level);
                            if (!rec) extenv.cdr = cons(newBinding, cdr(extenv));
                        }
                        if (named) {
                            bodyEnvEntry.car = car(arguments);
                            ConsCell bodyParams = null, insertPos = null;
                            for (ConsCell binding = bindings; binding != null; binding = (ConsCell) cdr(binding)) {
                                if (bodyParams == null) {
                                    bodyParams = cons(caar(binding), null);
                                    insertPos = bodyParams;
                                } else {
                                    insertPos.cdr = cons(caar(binding), null);
                                }
                            }
                            bodyEnvEntry.cdr = makeClosure(cons(bodyParams, cdr(let)), extenv);
                        }
                        forms = (ConsCell)cdr(let);  env = extenv;
                        // fall through to "eval a list of forms"
                    }



                    /// function application
                    else {
                        final Object func;
                        final ConsCell argList;

                        // apply function to list
                        // (apply form argform) -> object
                        if (haveApply() && operator == sApply) {
                            twoArgs("apply", arguments, exp);

                            func = eval(car(arguments), topEnv, env, stack, level);
                            final Object _argList = eval(cadr(arguments), topEnv, env, stack, level);
                            if (!listp(_argList)) throw new LambdaJError("%s: expected an argument list but got %s%s", "apply", printSEx(_argList), errorExp(exp));
                            argList = (ConsCell)_argList;
                            // fall through to "actually perform..."

                        // function call
                        // (expr args...) -> object
                        } else {
                            func = eval(operator, topEnv, env, stack, level);
                            if (!listp(arguments)) throw new LambdaJError("%s: expected an argument list but got %s%s", "function application", printSEx(arguments), errorExp(exp));
                            argList = evlis(arguments, topEnv, env, stack, level);
                            // fall through to "actually perform..."
                        }

                        // actually perform the function call that was set up by "apply" or "function call" above
                        if (primp(func)) {
                            try { return applyPrimitive((Primitive) func, argList, stack, level); }
                            catch (LambdaJError e) { throw new LambdaJError(e.getMessage() + errorExp(exp)); }

                        } else if (consp(func) && car(func) == sLambda) {
                            final Object lambda = cdr(func);          // (params . bodylist)
                            final ConsCell closure = haveLexC() ? ((ConsCell)func).closure : env;  // lexical or dynamic env
                            nArgs("lambda application", lambda, 2, exp);
                            env = zip(exp, closure, car(lambda), argList);

                            if (trace >= TRC_FUNC)  tracer.println(pfx(stack, level) + " #<lambda " + lambda + "> " + printSEx(env));
                            forms = (ConsCell) cdr(lambda);
                            // fall through to "eval a list of forms"

                        } else {
                            throw new LambdaJError("function application: not a primitive or lambda: %s%s", printSEx(func), errorExp(exp));
                        }
                    }

                    // eval a list of forms
                    // todo dotted list, circular list
                    for (; forms != null && cdr(forms) != null; forms = (ConsCell) cdr(forms))
                        eval(car(forms), topEnv, env, stack, level);
                    if (forms != null) {
                        exp = car(forms); isTc = true; continue tailcall;
                    }
                    return null; // lambda/ progn/ labels/... w/o body, shouldn't happen

                }

                // not a symbol/atom/cons - something is really wrong here
                // let's sprinkle some crack on him and get out of here, dave.
                throw new LambdaJError("eval: cannot eval expression '%s'", printSEx(exp));
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
    private ConsCell zip(Object exp, ConsCell extenv, Object paramList, ConsCell args) {
        if (paramList == null && args == null) return extenv; // shortcut for no params
        if (!listp(paramList)) throw new LambdaJError("%s: expected a parameter list but got %s%s",
                                                      "function application", printSEx(paramList), errorExp(exp));

        ConsCell params = (ConsCell)paramList;
        for ( ; params != null && args != null; params = (ConsCell) cdr(params), args = (ConsCell) cdr(args))
            extenv = cons(cons(car(params), car(args)), extenv);
        if (params != null) throw new LambdaJError("%s: not enough arguments. parameters w/o argument: %s%s", "function application", printSEx(params), errorExp(exp));
        if (args != null)   throw new LambdaJError("%s: too many arguments. remaining arguments: %s%s", "function application", printSEx(args), errorExp(exp));
        return extenv;
    }

    /** eval a list of forms and return a list of results */
    private ConsCell evlis(ConsCell _list, ConsCell topEnv, ConsCell env, int stack, int level) {
        dbgEvalStart("evlis", _list, env, stack, level);
        ConsCell head = null, insertPos = null;
        Object list = _list;
        for (; list != null && _list != cdr(list); list = cdr(list)) {
            ConsCell currentArg = cons(eval(car(list), topEnv, env, stack, level), null);
            if (head == null) {
                head = currentArg;
                insertPos = head;
            }
            else {
                insertPos.cdr = currentArg;
                insertPos = currentArg;
            }
            if (!listp(cdr(list))) throw new LambdaJError("%s: was expecting a proper list of expressions but got %s", "evlis", printSEx(_list));
        }
        dbgEvalDone("evlis", _list, head, stack, level);
        return head;
    }

    /** make a lexical closure (if enabled) or lambda */
    private ConsCell makeClosure(final Object paramsAndForms, ConsCell env) {
        return cons3(sLambda, paramsAndForms, haveLexC() ? env : null);
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (trace >= TRC_FUNC) tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        try { return primfn.apply(args); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError("#<primitive> throws exception: %s", e.getMessage()); }
    }



    /// stats during eval and at the end
    private int nCells;
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
    private  ConsCell cons(Object car, Object cdr) { nCells++; return new ConsCell(car, cdr); }
    private  ConsCell cons3(Object car, Object cdr, ConsCell closure) { nCells++; return new ConsCell(car, cdr, closure); }

    private static Object   car(ConsCell c)    { return c == null ? null : c.car; }
    private static Object   car(Object x)      { return x == null ? null : ((ConsCell)x).car; }
    private static Object   caar(ConsCell c)   { return c == null ? null : car(car(c)); }
    private static Object   cadr(ConsCell c)   { return c == null ? null : car(cdr(c)); }
    private static Object   cadar(ConsCell c)  { return c == null ? null : car(cdr(car(c))); }
    private static Object   caddr(ConsCell c)  { return c == null ? null : car(cddr(c)); }

    private static Object   cdr(ConsCell c)    { return c == null ? null : c.cdr; }
    private static Object   cdr(Object x)      { return x == null ? null : ((ConsCell)x).cdr; }
    private static Object   cdar(ConsCell c)   { return c == null ? null : cdr(c.car); }
    private static Object   cdar(Object x)     { return x == null ? null : cdr(car(x)); }
    private static Object   cddr(ConsCell c)   { return c == null ? null : cdr(cdr(c)); }
    private static Object   cddr(Object x)     { return x == null ? null : cdr(cdr(x)); }

    private static boolean  consp(Object x)    { return x instanceof ConsCell; }
    private static boolean  atom(Object x)     { return x == null || !(x instanceof ConsCell); } // ! consp(x)
    private static boolean  symbolp(Object x)  { return x == null || x instanceof String; }      // null (aka nil) is a symbol too
    private static boolean  listp(Object x)    { return x == null || x instanceof ConsCell; }    // null (aka nil) is a list too
    private static boolean  primp(Object x)    { return x instanceof Primitive; }
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
        if (!listp(maybeList)) throw new LambdaJError("%s: expected second argument to be a List but got %s", "assoc", printSEx(maybeList));
        for (ConsCell env = (ConsCell) maybeList ; env != null && maybeList != cdr(env); env = (ConsCell) cdr(env)) {
            if (atom == caar(env)) return (ConsCell) car(env);
        }
        return null;
    }

    /** Append rest at the end of first. If first is a list it will be modified. */
    private ConsCell combine(Object first, Object rest) {
        if (consp(first)) return appendToList((ConsCell)first, rest);
        else return cons(first, rest);
    }

    /** Append rest at the end of first, modifying first in the process.
     *  Returns a dotted list unless rest is a proper list. */
    private ConsCell appendToList(ConsCell first, Object rest) {
        for (ConsCell last = first; last != null; last = (ConsCell) cdr(last)) {
            if (cdr(last) == first) throw new LambdaJError("%s: first argument is a circular list", "appendToList");
            if (cdr(last) == null) {
                last.cdr = rest;
                return first;
            }
            if (!consp(cdr(last))) {
                last.cdr = cons(last.cdr, rest);
                return first;
            }
        }
        throw new LambdaJError("%s: internal error, can't append %s and %s", "appendToList", printSEx(first), printSEx(rest));
    }

    private static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("%s: expected second argument to be a List but got %s", "listToArray", printSEx(maybeList));
        final List<Object> ret = new ArrayList<>();
        for (ConsCell env = (ConsCell) maybeList; env != null && maybeList != cdr(env); env = (ConsCell) cdr(env))
            ret.add(car(env));
        return ret.toArray();
    }

    /** transform {@code ob} into an S-expression, atoms are not escaped */
    private static String printObj(Object ob) {
        if (ob == null) return "nil";
        final StringBuffer sb = new StringBuffer(200);
        _printSEx(sb::append, ob, ob, true, false);
        return sb.toString();
    }

    /** transform {@code ob} into an S-expression, atoms are escaped */
    private static String printSEx(Object obj) {
        if (obj == null) return "nil";
        final StringBuffer sb = new StringBuffer(200);
        _printSEx(sb::append, obj, obj, true, true);
        return sb.toString();
    }

    private static void printSEx(WriteConsumer w, Object obj) {
        _printSEx(w, obj, obj, true, true);
    }

    private static void _printSEx(WriteConsumer sb, Object list, Object obj, boolean headOfList, boolean escapeAtoms) {
        while (true) {
            if (obj == null) {
                sb.print("nil"); return;
            } else if (listp(obj)) {
                if (headOfList) sb.print("(");
                if (car(obj) == list) {
                    sb.print(headOfList ? "#<this cons>" : "#<this list>");
                } else {
                    _printSEx(sb, car(obj), car(obj), true, escapeAtoms);
                }
                if (cdr(obj) != null) {
                    if (listp(cdr(obj))) {
                        sb.print(" ");
                        if (list == cdr(obj)) {
                            sb.print("#<circular list>)"); return;
                        } else {
                            obj = cdr(obj); headOfList = false; continue;
                        }
                    } else if (headOfList) {
                        sb.print(" . ");
                        _printSEx(sb, list, cdr(obj), false, escapeAtoms);
                        sb.print(")");
                        return;
                    } else {
                        sb.print(" . ");
                        _printSEx(sb, list, cdr(obj), false, escapeAtoms); // must be an atom
                        sb.print(")");
                        return;
                    }
                } else {
                    sb.print(")");
                    return;
                }
            } else if (escapeAtoms && symbolp(obj)) {
                if (containsSExSyntaxOrBlank(obj.toString())) {
                    sb.print("|"); sb.print(obj.toString()); sb.print("|");
                    return;
                }
                sb.print(obj.toString()); return;
            } else if (primp(obj)) {
                sb.print("#<primitive>"); return;
            } else if (escapeAtoms && stringp(obj)) {
                sb.print("\""); sb.print(escapeString(obj.toString())); sb.print("\""); return;
            } else if (atom(obj)) {
                sb.print(obj.toString()); return;
            } else {
                sb.print("<internal error>"); return;
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
        if (a != null) throw new LambdaJError("%s: expected no arguments but got %s", func, printSEx(a));
    }

    /** ecactly one argument */
    private static void oneArg(String func, Object a) {
        if (a == null)      throw new LambdaJError("%s: expected one argument but no argument was given", func);
        if (cdr(a) != null) throw new LambdaJError("%s: expected one argument but got extra arg(s) %s", func, printSEx(cdr(a)));
    }

    private static void oneOrMoreArgs(String func, ConsCell a) {
        if (a == null) throw new LambdaJError("%s: expected at least one argument but no argument was given", func);
    }

    private static void twoArgs(String func, Object a) {
        twoArgs(func, a, null);
    }

    private static void twoArgs(String func, Object a, Object exp) {
        if (a == null)       throw new LambdaJError("%s: expected two arguments but no argument was given%s", func, errorExp(exp));
        if (cdr(a) == null)  throw new LambdaJError("%s: expected two arguments but only one argument was given%s", func, errorExp(exp));
        if (cddr(a) != null) throw new LambdaJError("%s: expected two arguments but got extra arg(s) %s%s", func, printSEx(cddr(a)), errorExp(exp));
    }

    /** at least {@code min} args */
    private static void nArgs(String func, Object a, int min, Object exp) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError("%s: expected %d arguments or more but got only %d%s", func, min, actualLength, errorExp(exp));
    }

    private static void nArgs(String func, Object a, int min, int max, Object exp) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError("%s: expected %d to %d arguments but got only %d%s", func, min, max, actualLength, errorExp(exp));
        if (actualLength > max) throw new LambdaJError("%s: expected %d to %d arguments but got extra arg(s) %s%s", func, min, max, printSEx(nthcdr(max, a)), errorExp(exp));
    }

    private static void onePair(String func, ConsCell a) {
        if (a == null)      throw new LambdaJError("%s: expected one Pair argument but no argument was given", func);
        if (!listp(car(a))) throw new LambdaJError("%s: expected one Pair argument but got %s", func, printSEx(a));
        if (cdr(a) != null) throw new LambdaJError("%s: expected one Pair argument but got extra arg(s) %s", func, printSEx(cdr(a)));
    }

    /** arguments if any must be only numbers */
    private static void numberArgs(String func, ConsCell a) {
        if (a == null) return;
        for (; a != null; a = (ConsCell) cdr(a)) {
            if (!numberp(car(a)) || (cdr(a) != null && !consp(cdr(a))))
                throw new LambdaJError("%s: expected only number arguments but got %s", func, printSEx(a));
        }
    }

    private static void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numberArgs(func, a);
    }

    /** the given arg must be a LambdaJString */
    private void stringArg(String func, String arg, Object a) {
        if (!(car(a) instanceof LambdaJString))
            throw new LambdaJError("%s: expected %s to be a String but got %s", func, arg, printSEx(car(a)));
    }

    private static String errorExp(Object exp) {
        if (exp == null) return "";
        return System.lineSeparator() + "error occurred in expression " + printSEx(exp);
    }



    /// runtime for Lisp programs
    private Object boolResult(boolean b) { return b ? expTrue.get() : null; }

    /** generate a comparison operator */
    private Object makeCompareOp(ConsCell args, String opName, IntPredicate pred) {
        twoArgs(opName, args);
        numberArgs(opName, args);
        final Number lhs = (Number)car(args);
        final Number rhs = (Number)cadr(args);
        if (lhs instanceof Long && rhs instanceof Long) return boolResult(pred.test(Long.compare(lhs.longValue(),  rhs.longValue())));
        else                                            return boolResult(pred.test(Double.compare(lhs.doubleValue(),  rhs.doubleValue())));
    }

    /** generate operator for zero or more args */
    private static Object makeAddOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        numberArgs(opName, args);
        for (; args != null; args = (ConsCell) cdr(args))
            startVal = op.applyAsDouble(startVal, ((Number)car(args)).doubleValue());
        return startVal;
    }

    /** generate operator for one or more args */
    private static Object makeSubOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        oneOrMoreNumbers(opName, args);
        double result = ((Number)car(args)).doubleValue();
        if (cdr(args) == null) return op.applyAsDouble(startVal, result);
        for (args = (ConsCell) cdr(args); args != null; args = (ConsCell) cdr(args))
            result = op.applyAsDouble(result, ((Number)car(args)).doubleValue());
        return result;
    }

    private String stringFormat(ConsCell a, String func) {
        nArgs(func, a, 1, null);
        stringArg(func, "first argument", a);
        String s = ((LambdaJString)car(a)).value;
        try {
            return String.format(s, listToArray(cdr(a)));
        } catch (IllegalFormatException e) {
            throw new LambdaJError("%s: illegal format string and/ or arguments: %s\nerror ocurred processing the argument(s) %s",
                                   func, e.getMessage(), printSEx(a));
        }
    }

    private String stringFormatLocale(ConsCell a, String func) {
        nArgs(func, a, 2, null);
        String locString;
        if (car(a) != null) {
            stringArg(func, "first argument", a);
            locString = ((LambdaJString)car(a)).value;
        } else locString = null;
        stringArg(func, "second argument", cdr(a));
        String s = ((LambdaJString)car(cdr(a))).value;
        try {
            if (locString == null) return String.format(s, listToArray(cdr(cdr(a))));
            Locale loc = Locale.forLanguageTag(locString);
            return String.format(loc, s, listToArray(cdr(cdr(a))));
        } catch (IllegalFormatException e) {
            throw new LambdaJError("%s: illegal format string and/ or arguments: %s\nerror ocurred processing the argument(s) %s",
                    func, e.getMessage(), printSEx(a));
        }
    }

    private static ThreadMXBean getThreadBean(final String func) {
        final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
        if (threadBean == null)
            throw new LambdaJError("%s: ThreadMXBean not supported in this Java Runtime", func);
        if (!threadBean.isCurrentThreadCpuTimeSupported())
            throw new LambdaJError("%s: ThreadMXBean.getCurrentThreadCpuTime() not supported in this Java Runtime", func);
        return threadBean;
    }



    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@code symtab} on the fly */
    private ConsCell environment(ConsCell env, ObjectReader lispStdin, ObjectWriter lispStdout) {
        if (haveIO()) {
            final Primitive freadobj =  a -> { noArgs("read", a);    return lispStdin == null ? null : lispStdin.readObj(); };
            final Primitive fwriteobj = a -> {
                oneArg("write", a);
                if (lispStdout == null) throw new LambdaJError("%s: lispStdout is nil", "write");
                lispStdout.printObj(car(a)); return expTrue.get();
            };

            final Primitive fwriteln =  a -> {
                nArgs("writeln", a, 0, 1, null);
                if (lispStdout == null) throw new LambdaJError("%s: lispStdout is nil", "write");
                if (a == null) {
                    lispStdout.printEol();
                    return expTrue.get();
                }
                if (haveLispEOL()) {
                    lispStdout.printEol();
                    lispStdout.printObj(car(a));
                    lispStdout.printString(" ");
                } else {
                    lispStdout.printObj(car(a));
                    lispStdout.printEol();
                }
                return expTrue.get();
            };

            env = cons(cons(symtab.intern("read"),    freadobj),
                  cons(cons(symtab.intern("write"),   fwriteobj),
                  cons(cons(symtab.intern("writeln"), fwriteln),
                  env)));

        }

        if (haveString()) {
            env = cons(cons(symtab.intern("stringp"), (Primitive) a -> { oneArg("stringp", a); return boolResult(stringp(car(a))); }),
                  env);

            if (haveUtil()) {
                env = cons(cons(symtab.intern("string-format"),        (Primitive) a -> new LambdaJString(stringFormat(a, "string-format"))),
                      cons(cons(symtab.intern("string-format-locale"), (Primitive) a -> new LambdaJString(stringFormatLocale(a, "string-format-locale"))),
                      env));
            }

            if (haveIO()) {
                final Primitive fformat = a -> {
                    if (lispStdout == null) throw new LambdaJError("%s: lispStdout is nil", "format");
                    lispStdout.printString(stringFormat(a, "format"));
                    return expTrue.get();
                };

                final Primitive fformatLocale = a -> {
                    if (lispStdout == null) throw new LambdaJError("%s: lispStdout is nil", "format");
                    lispStdout.printString(stringFormatLocale(a, "format-locale"));
                    return expTrue.get();
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
            env = cons(cons(symtab.intern("consp"),   (Primitive) a -> { oneArg("consp", a);   return boolResult(consp  (car(a))); }),
                  cons(cons(symtab.intern("symbolp"), (Primitive) a -> { oneArg("symbolp", a); return boolResult(symbolp(car(a))); }),
                  cons(cons(symtab.intern("listp"),   (Primitive) a -> { oneArg("listp", a);   return boolResult(listp  (car(a))); }),
                  cons(cons(symtab.intern("null?"),   (Primitive) a -> { oneArg("null?", a);   return boolResult(car(a) == null); }),
                  cons(cons(symtab.intern("assoc"),   (Primitive) a -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); }),
                  env)))));

            final Primitive fusertime = a -> { return new Double(getThreadBean("get-internal-run-time").getCurrentThreadUserTime()); };
            final Primitive fcputime = a -> { return new Double(getThreadBean("get-internal-cpu-time").getCurrentThreadCpuTime()); };
            final Primitive fsleep = a -> {
                oneArg("sleep", a);
                numberArgs("sleep", a);
                try {
                    long startNanos = System.nanoTime();
                    long nanos = ((Double)car(a)).longValue();
                    long millis = TimeUnit.NANOSECONDS.toMillis(nanos);
                    Thread.sleep(millis);
                    return new Double(System.nanoTime() - startNanos);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new LambdaJError("sleep: got interrupted");
                }
            };
            final Primitive fUniversalTime = a -> {
                ZoneId utc = ZoneId.of("UTC");
                ZonedDateTime ld1900 = ZonedDateTime.of(1900, 1, 1, 0, 0, 0, 0, utc);
                return new Double(ld1900.until(ZonedDateTime.now(utc), ChronoUnit.SECONDS));
            };
            final Primitive fDecodedTime = a -> {
                final Instant now = Clock.systemDefaultZone().instant();
                final ZonedDateTime n = now.atZone(ZoneId.systemDefault());
                final ZoneRules rules = n.getZone().getRules();
                boolean daylightSavings = rules.isDaylightSavings(now);
                double offset = rules.getOffset(now).get(ChronoField.OFFSET_SECONDS) / 3600.0;
                //get-decoded-time <no arguments> => second, minute, hour, date, month, year, day, daylight-p, zone
                return cons(n.getSecond(), cons(n.getMinute(), cons(n.getHour(),
                       cons(n.getDayOfMonth(), cons(n.getMonthValue(), cons(n.getYear(),
                       cons(boolResult(daylightSavings), cons(offset, null))))))));
            };
            env = cons(cons(symtab.intern("internal-time-units-per-second"), new Double(1e9)),
                  cons(cons(symtab.intern("get-internal-real-time"), (Primitive)a -> new Double(System.nanoTime())),
                  cons(cons(symtab.intern("get-internal-run-time"), fusertime), // user
                  cons(cons(symtab.intern("get-internal-cpu-time"), fcputime), // user + system
                  cons(cons(symtab.intern("sleep"), fsleep),
                  cons(cons(symtab.intern("get-universal-time"), fUniversalTime), // seconds since 1.1.1900
                  cons(cons(symtab.intern("get-decoded-time"), fDecodedTime),
                  env)))))));
        }

        if (haveAtom()) {
            env = cons(cons(symtab.intern("atom"), (Primitive) a -> { oneArg("atom", a); return boolResult(atom(car(a))); }),
                       env);
        }

        if (haveDouble()) {
            final Primitive fmod = args -> {
                twoArgs("mod", args);
                numberArgs("mod", args);
                return ((Number)car(args)).doubleValue() % ((Number)car(cdr(args))).doubleValue();
            };

            env = cons(cons(symtab.intern("="),       (Primitive) args -> makeCompareOp(args, "=",  compareResult -> compareResult == 0)),
                  cons(cons(symtab.intern(">"),       (Primitive) args -> makeCompareOp(args, ">",  compareResult -> compareResult >  0)),
                  cons(cons(symtab.intern(">="),      (Primitive) args -> makeCompareOp(args, ">=", compareResult -> compareResult >= 0)),
                  cons(cons(symtab.intern("<"),       (Primitive) args -> makeCompareOp(args, "<",  compareResult -> compareResult <  0)),
                  cons(cons(symtab.intern("<="),      (Primitive) args -> makeCompareOp(args, "<=", compareResult -> compareResult <= 0)),
                  env)))));
            env = cons(cons(symtab.intern("+"),       (Primitive) args -> makeAddOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs)),
                  cons(cons(symtab.intern("-"),       (Primitive) args -> makeSubOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs)),
                  cons(cons(symtab.intern("*"),       (Primitive) args -> makeAddOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs)),
                  cons(cons(symtab.intern("/"),       (Primitive) args -> makeSubOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs)),
                  cons(cons(symtab.intern("mod"),     fmod),
                  env)))));
            env = cons(cons(symtab.intern("numberp"), (Primitive) args -> { oneArg("numberp", args); return boolResult(numberp(car(args))); }),
                  cons(cons(symtab.intern("round"),   (Primitive) args -> { oneArg("round",   args); return (long)Math.round((Double)car(args)); }),
                  cons(cons(symtab.intern("floor"),   (Primitive) args -> { oneArg("floor",   args); return (long)Math.floor((Double)car(args)); }),
                  cons(cons(symtab.intern("ceiling"), (Primitive) args -> { oneArg("ceiling", args); return (long)Math.ceil ((Double)car(args)); }),
                  env))));
        }

        if (haveEq()) {
            env = cons(cons(symtab.intern("eq"), (Primitive) a -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); }),
                       env);
        }

        if (haveCons()) {
            env = cons(cons(symtab.intern("cdr"),     (Primitive) a -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); }),
                  cons(cons(symtab.intern("car"),     (Primitive) a -> { onePair("car", a);    if (car(a) == null) return null; return caar(a); }),
                  cons(cons(symtab.intern("cons"),    (Primitive) a -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); }),
                  env)));
        }

        env = cons(cons(symtab.intern("throw"), (Primitive) a -> { oneArg("throw", a); throw new RuntimeException(car(a).toString()); }),
                env);

        return cons(cons(null, null), env); // top env begins with (nil . nil), define/ defun will insert stuff immediately after (nil . nil).
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
        if (trace >= TRC_STATS) {
            tracer.println("*** max eval nesting: " + maxEvalLevel + " ***");
            tracer.println("*** max stack used:   " + maxEvalStack + " ***");

            tracer.println("*** total ConsCells:  " + nCells + " ***");
            tracer.println("*** max env length:   " + maxEnvLen + " ***");

            maxEvalLevel = maxEvalStack = nCells = maxEnvLen = 0;
        }
    }

    private static class BoolHolder { boolean value; BoolHolder(boolean value) { this.value = value; }}

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
        if (hasFlag("--dyn", args))         features =  HAVE_ALL_DYN;
        else if (hasFlag("--lex", args))    features =  HAVE_ALL_LEXC;

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

        if (hasFlag("--min+", args))        features =  HAVE_MINPLUS;
        if (hasFlag("--min", args))         features =  HAVE_MIN;
        if (hasFlag("--lambda+", args))     features =  HAVE_LAMBDAPLUS;
        if (hasFlag("--lambda", args))      features =  HAVE_LAMBDA;

        if (hasFlag("--eol=C", args))       features &= ~HAVE_LISPEOL;
        if (hasFlag("--eol=LISP", args))    features |= HAVE_LISPEOL;

        final LambdaJ interpreter = new LambdaJ(features, trace);

        final boolean printResult = hasFlag("--result", args);
        final boolean istty = null != System.console();
        final boolean repl        = hasFlag("--repl", args) || istty;

        final BoolHolder echo = new BoolHolder(hasFlag("--echo", args));

        if (argError(args)) {
            System.err.println("LambdaJ: exiting because of previous errors.");
            return;
        }

        if (repl) {
            if (!echo.value) {
                System.out.println("Enter a Lisp expression or :command (or enter :h for command help or :q to exit):");
                System.out.println();
            }

            boolean isInit = false;
            SExpressionParser parser = null;
            ObjectWriter outWriter = null;
            ConsCell env = null;
            for (;;) {
                if (!isInit) {
                    interpreter.nCells = 0; interpreter.maxEnvLen = 0;
                    parser = interpreter.new SExpressionParser(() -> {
                        int c = System.in.read();
                        if (echo.value && c != EOF)
                            if (istty && c == '\r') System.out.print(System.lineSeparator());
                            else System.out.print((char)c);
                        return c;
                    });
                    interpreter.setSymtab(parser);
                    outWriter = interpreter.new SExpressionWriter(System.out::print);
                    env = interpreter.environment(null, parser, outWriter);
                    isInit = true;
                }

                if (!echo.value) {
                    System.out.print("LambdaJ> ");
                    System.out.flush();
                }

                try {
                    parser.lineNo = 1;  parser.charNo = 1;
                    final Object exp = parser.readObj();

                    if (":q".equals(exp) || exp == null && parser.look == EOF) {
                        System.out.println("bye."); System.out.println();  return;
                    }

                    if (":h".equals(exp)) {
                        System.out.println("Available commands:");
                        System.out.println("  :h ........ this help screen");
                        System.out.println("  :echo ..... this help screen");
                        System.out.println("  :noecho ... this help screen");
                        System.out.println("  :env ...... list current global environment");
                        System.out.println("  :init ..... re-init global environment");
                        System.out.println("  :q ........ quit LambdaJ");
                        System.out.println();
                        continue;
                    }
                    if (":echo".equals(exp)) {
                        echo.value = true;
                        continue;
                    }
                    if (":noecho".equals(exp)) {
                        echo.value = false;
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
                    System.out.print("==> "); outWriter.printObj(result); System.out.println();
                } catch (LambdaJError e) {
                    if (istty) {
                        System.out.println();
                        System.out.println(e.toString());
                        System.out.println();
                    } else {
                        System.err.println();
                        System.err.println(e.toString());
                        System.exit(1);
                    }
                }
            }
        }

        try {
            final String result = printSEx(interpreter.interpretExpressions(System.in::read, System.in::read, System.out::print));
            if (printResult) {
                System.out.println();
                System.out.println("==> " + result);
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
        System.out.println("LambdaJ $Id: LambdaJ.java,v 1.128 2020/10/26 07:41:37 Robert Exp $");
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
                + "\n"
                + "--repl ........  enter REPL even if the input isn't a tty,\n"
                + "                 i.e. print prompt and results and support :commands.\n"
                + "\n"
                + "--eol=LISP ....  writeln prints <EOL><argument>< >\n"
                + "--eol=C .......  writeln prints <argument><EOL>\n"
                + "\n"
                + "--echo ........  echo all input while reading\n"
                + "--trace=stats .  print stack and memory stats at end\n"
                + "--trace=eval ..  print internal interpreter info during executing programs\n"
                + "--trace=eval ..  print more internal interpreter info executing programs\n"
                + "--trace .......  print lots of internal interpreter info during\n"
                + "                 reading/ parsing/ executing programs\n"
                + "\n"
                + "Feature flags:\n"
                + "\n"
                + "--dyn .........  use dynamic environments\n"
                + "--lex .........  use lexical closures (with dynamic global environment),\n"
                + "                 this is the default\n"
                + "\n"
                + "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n"
                + "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n"
                + "--no-extra ....  no special forms 'eval', 'if', 'define', 'defun',\n"
                + "                 'letrec', 'progn'\n"
                + "--no-double ...  no number support\n"
                + "--no-string ...  no string support\n"
                + "--no-io .......  no primitive functions read/ write/ writeln/\n"
                + "                 format/ format-locale\n"
                + "--no-util .....  no primitive functions consp/ symbolp/ listp/ null?/ assoc/\n"
                + "                 string-format/ string-format-locale\n"
                + "                 no time related primitives\n"
                + "\n"
                + "--min+ ........  turn off all above features, leaving a Lisp\n"
                + "                 with 10 primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, cons, car, cdr, lambda, apply, cond, labels\n"
                + "\n"
                + "--no-apply ....  no special form 'apply'\n"
                + "--no-labels ...  no special form 'labels' (hint: use Y-combinator instead)\n"
                + "\n"
                + "--min .........  turn off all above features, leaving a Lisp with\n"
                + "                 8 special forms/ primitives:\n"
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
                + "--lambda ......  turns off yet even more stuff, leaving I guess\n"
                + "                 bare bones Lambda calculus:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   lambda");
    }
}
