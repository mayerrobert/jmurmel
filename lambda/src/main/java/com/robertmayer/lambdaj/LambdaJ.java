/* LambdaJ is Copyright (C) 2020 Robert Mayer. All rights reserved.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

package com.robertmayer.lambdaj;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.zone.ZoneRules;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IllegalFormatException;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.concurrent.TimeUnit;
import java.util.function.DoubleBinaryOperator;
import java.util.function.IntPredicate;
import java.util.function.Supplier;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;

import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;

/** <p>Interpreter for the Lisp-dialect Murmel. Could probably read top-down like a book.
 *
 *  <p>Comments starting with '///' could be considered similar to headings or chapter titles.
 *  You may want to run 'grep " ///" LambdaJ.java' to get something like birds-eye-view
 *  or sort of a table-of-contents of the interpreter implementation. */
public class LambdaJ {

    /// Public interfaces and an exception class to use the interpreter from Java

    public static final String ENGINE_NAME = "JMurmel: Java based interpreter for Murmel";
    public static final String ENGINE_VERSION = "LambdaJ $Id: LambdaJ.java,v 1.183 2020/11/08 11:58:12 Robert Exp $";
    public static final String LANGUAGE_VERSION = "1.0-SNAPSHOT";

    @FunctionalInterface public interface ReadSupplier { int read() throws IOException; }
    @FunctionalInterface public interface WriteConsumer { void print(String s); }
    @FunctionalInterface public interface Tracer { void println(String msg); }

    @FunctionalInterface public interface ObjectReader { Object readObj(); }
    public interface SymbolTable { Object intern(LambdaJSymbol symbol); }
    public interface Parser extends ObjectReader, SymbolTable {
        default void setInput(ReadSupplier input) {
            throw new UnsupportedOperationException("This parser does not support changing input");
        }
    }

    public interface ObjectWriter { void printObj(Object o); default void printString(String s) { printObj(s); } void printEol(); }

    @FunctionalInterface public interface Primitive { Object apply(ConsCell x); }

    public interface CustomEnvironmentSupplier {
        ConsCell customEnvironment(SymbolTable symtab);
    }



    public static class LambdaJError extends RuntimeException {
        public static final long serialVersionUID = 1L;

        public LambdaJError(String msg)                                    { super(msg, null, false, false); }
        public LambdaJError(boolean format,  String msg, Object... params) { super((format ? String.format(msg, params) : msg) + getErrorExp(params), null, false, false); }
        public LambdaJError(Throwable cause, String msg, Object... params) { super(String.format(msg, params) + getErrorExp(params), cause); }

        @Override public String toString() { return "Error: " + getMessage(); }

        private static String getErrorExp(Object[] params) {
            if (params != null && params.length > 0 && params[params.length-1] instanceof ConsCell) return errorExp(params[params.length-1]);
            return "";
        }
    }



    /// Data types used by interpreter program as well as interpreted programs
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
                final Object ret = cursor;  // last element of dotted list
                cursor = null;
                return ret;
            }
        }

        private static final long serialVersionUID = 1L;
        public Object car, cdr;
        public ConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
        @Override public String toString() { return printObj(this); }
        @Override public Iterator<Object> iterator() { return new ConsCellIterator(this); }

        public ConsCell closure() { return null; }
    }

    public static class SExpConsCell extends ConsCell {
        private static final long serialVersionUID = 1L;
        private final int lineNo, charNo;
        public SExpConsCell(int line, int charNo, Object car, Object cdr)    { super(car, cdr); this.lineNo = line; this.charNo = charNo; }
    }

    public static class ClosureConsCell extends ConsCell {
        private static final long serialVersionUID = 1L;
        private ConsCell closure; // only used for Lambdas with lexical environments. doesn't waste space because Java object sizes are multiples of 8 and this uses an otherwise unused slot
        public ClosureConsCell(Object car, Object cdr, ConsCell closure)    { super(car, cdr); this.closure = closure; }

        @Override
        public ConsCell closure() { return closure; }
    }

    public static class LambdaJSymbol implements Serializable {
        private static final long serialVersionUID = 1L;
        private final String value;
        public LambdaJSymbol(String value) { this.value = value; }
        @Override public String toString() { return value.toString(); }

        @Override public int hashCode() { return value.hashCode(); }
        @Override public boolean equals(Object o) { return o instanceof LambdaJSymbol && value.equals(((LambdaJSymbol)o).value); }

        public boolean equalsIgnoreCase(LambdaJSymbol other) { return value.equalsIgnoreCase(other.value); }
    }





    /// Infrastructure
    public static final int EOF = -1;
    public static final int TOKEN_MAX = 2000; // max length of string literals
    public static final int SYMBOL_MAX = 30;  // max length of symbols

    public static final int TRC_NONE = 0, TRC_STATS = 1, TRC_ENVSTATS = 2, TRC_EVAL = 3, TRC_ENV = 4, TRC_FUNC = 5, TRC_PARSE = 6, TRC_TOK = 7, TRC_LEX = 8;
    private final int trace;

    private final Tracer tracer;

    public static final int
    HAVE_LABELS = 1,                   // use Y-combinator instead
    HAVE_NIL    = 1<<2, HAVE_T = 1<<3, // use () and (quote t) instead. printObj will print nil regardless
    HAVE_XTRA   = 1<<4,                // extra special forms such as if
    HAVE_DOUBLE = 1<<5,                // numbers, +-<>..., numberp, remaining datatypes are symbls and cons-cells (lists)
    HAVE_LONG   = 1<<6,                // turns on only Long support in the reader, you'll want DOUBLE as well
    HAVE_STRING = 1<<7,                // strings, string literals of string related functions
    HAVE_IO     = 1<<8,                // read/ write, result only
    HAVE_UTIL   = 1<<9,                // null?, consp, listp, symbolp, assoc
    HAVE_APPLY  = 1<<10,               // McCarthy didn't list apply
    HAVE_CONS   = 1<<11,
    HAVE_COND   = 1<<12,
    HAVE_ATOM   = 1<<13,
    HAVE_EQ     = 1<<14,
    HAVE_QUOTE  = 1<<15,

    HAVE_LEXC   = 1<<16,

    HAVE_LISPEOL = 1 << 17,


    HAVE_LAMBDA     = 0,
    HAVE_LAMBDAPLUS = HAVE_LAMBDA | HAVE_ATOM | HAVE_QUOTE | HAVE_EQ,
    HAVE_MIN        = HAVE_LAMBDAPLUS | HAVE_CONS | HAVE_COND,
    HAVE_MINPLUS    = HAVE_MIN | HAVE_APPLY | HAVE_LABELS,
    HAVE_ALL_DYN    = HAVE_MINPLUS | HAVE_NIL | HAVE_T | HAVE_XTRA | HAVE_DOUBLE | HAVE_LONG | HAVE_STRING | HAVE_IO | HAVE_UTIL,

    HAVE_ALL_LEXC   = HAVE_ALL_DYN | HAVE_LEXC;
    ;
    private final int features;

    private boolean haveLabels()  { return (features & HAVE_LABELS)  != 0; }
    private boolean haveNil()     { return (features & HAVE_NIL)     != 0; }
    private boolean haveT()       { return (features & HAVE_T)       != 0; }
    private boolean haveXtra()    { return (features & HAVE_XTRA)    != 0; }
    private boolean haveDouble()  { return (features & HAVE_DOUBLE)  != 0; }
    private boolean haveLong()    { return (features & HAVE_LONG)    != 0; }
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
    public static class SExpressionWriter implements ObjectWriter {
        private final WriteConsumer out;  // printObj() and printEol() will write to this

        public SExpressionWriter(WriteConsumer out) { this.out = out; }
        @Override public void printObj(Object o) { printSEx(out, o); }
        @Override public void printString(String s) { out.print(s); }
        @Override public void printEol() { out.print(System.lineSeparator()); }
    }

    /// Scanner, symboltable and S-expression parser
    /** This class will read and parse S-Expressions (while generating symbol table entries)
     *  from the given {@link ReadSupplier} */
    public class SExpressionParser implements Parser {
        private ReadSupplier in;    // readObj() will read from this
        private boolean init;

        private boolean pos = false;
        private int lineNo = 1, charNo = 0;
        private boolean escape; // is the lookahead escaped
        private boolean tokEscape;
        private int look;
        private int token[] = new int[TOKEN_MAX + 1]; // provide for trailing '\0'
        private Object tok;

        public SExpressionParser(ReadSupplier in) { this.in = in; }
        @Override public void setInput(ReadSupplier input) { in = input; init = false; }

        /// Scanner
        private boolean isSpace(int x)  { return !escape && isWhiteSpace(x); }
        private boolean isDigit(int x)  { return !escape && (x >= '0' && x <= '9'); }
        private boolean isDQuote(int x) { return !escape && x == '"'; }
        private boolean isBar(int x)    { return !escape && x == '|'; }

        private boolean isSyntax(int x) { return !escape && isSExSyntaxChar(x); }

        private int readchar() throws IOException {
            int c = in.read();
            if (c == '\n') {
                lineNo++;
                charNo = 0;
            } else if (c != '\r' && c != EOF) charNo++;
            return c;
        }

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
                    if (look == EOF) throw new LambdaJError(true, "line %d:%d: string literal is missing closing \"", lineNo, charNo);
                    else look = getchar(); // consume trailing "
                } else if (isBar(look)) {
                    look = getchar();
                    do {
                        if (index < SYMBOL_MAX) token[index++] = look;
                        look = getchar();
                    } while (look != EOF && !isBar(look));
                    if (look == EOF) throw new LambdaJError(true, "line %d:%d: |-quoted symbol is missing closing |", lineNo, charNo);
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
                    String s = tokenToString(token);
                    if (!haveLong() || s.indexOf('.') != -1 || s.indexOf('e') != -1 || s.indexOf('E') != -1) tok = Double.valueOf(s);
                    else tok = Long.valueOf(s);
                }
                catch (NumberFormatException e) {
                    throw new LambdaJError(true, "line %d:%d: '%s' is not a valid symbol or number", lineNo, charNo, tokenToString(token));
                }
            } else if (haveString() && token[0] == '"') {
                tok = tokenToString(token).substring(1);
            } else if (token[0] == '\0'){
                tok = null;
            } else {
                String s = tokenToString(token);
                if (s.length() > SYMBOL_MAX) s = s.substring(0, SYMBOL_MAX);
                tok = new LambdaJSymbol(s);
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
            final StringBuilder ret = new StringBuilder(32);
            int len = s.length, c;
            for (int i = 0; i < len && (c = s[i++]) != '\0'; )
                ret.append((char)c);
            return ret.toString();
        }



        /// A symbol table implemented with a list just because. could easily replaced by a HashMap for better performance.
        private ConsCell symbols = null;

        // String#equalsIgnoreCase is slow. we could String#toUpperCase all symbols then we could use String#equals
        @Override
        public Object intern(LambdaJSymbol sym) {
            if (symbols != null)
                for (Object symbol: symbols) {
                    if (((LambdaJSymbol) symbol).equalsIgnoreCase(sym)) {
                        return symbol;
                    }
                }
            symbols = cons(sym, symbols);
            return sym;
        }



        /// S-expression parser
        @Override
        public Object readObj() {
            if (!init) {
                lineNo = 1; charNo = 0;
                look = getchar();
                init = true;
            }
            readToken();
            return readObject();
        }

        public Object readObj(boolean pos) {
            this.pos = true;
            Object ret = readObj();
            this.pos = false;
            return ret;
        }

        private final Object quote = intern(new LambdaJSymbol("quote"));

        private Object readObject() {
            if (tok == null) {
                if (trace >= TRC_PARSE) tracer.println("*** parse list   ()");
                return null;
            }
            if (haveNil() && !tokEscape && tok instanceof LambdaJSymbol && isToken(tok, "nil")) {
                return null;
            }
            if (!tokEscape && isToken(tok, ")")) {
                throw new LambdaJError(true, "line %d:%d: unexpected ')'", lineNo, charNo);
            }
            if (!tokEscape && isToken(tok, "(")) {
                final Object list = readList();
                if (!tokEscape && isToken(tok, ".")) {
                    final Object cdr = readList();
                    if (cdr(cdr) != null) throw new LambdaJError(true, "line %d:%d: illegal end of dotted list: %s", lineNo, charNo, printSEx(cdr));
                    final Object cons = combine(list, car(cdr));
                    if (trace >= TRC_PARSE) tracer.println("*** parse cons   " + printSEx(cons));
                    return cons;
                }
                if (trace >= TRC_PARSE) tracer.println("*** parse list   " + printSEx(list));
                return list;
            }
            if (!tokEscape && isToken(tok, "'")) {
                readToken();
                return cons(quote, cons(readObject(), null));
            }
            if (symbolp(tok)) {
                if (trace >= TRC_TOK) tracer.println("*** parse symbol " + tok);
                return intern((LambdaJSymbol)tok);
            }
            if (trace >= TRC_TOK) tracer.println("*** parse value  " + tok.toString());
            return tok;
        }

        private Object readList() {
            readToken();
            if (tok == null) throw new LambdaJError(true, "line %d:%d: cannot read list. missing ')'?", lineNo, charNo);
            if (!tokEscape) {
                if (isToken(tok, ")")) return null;
                if (isToken(tok, ".")) return null;
            }
            final Object tmp = readObject();
            if (symbolp(tmp)) return cons(tmp, readList());
            else return cons(tmp, readList());
        }

        private boolean isToken(Object tok, String s) {
            return tok == null && s == null || tok != null && tok.toString().equalsIgnoreCase(s);
        }

        private ConsCell cons(Object car, Object cdr) {
            return pos ? new SExpConsCell(lineNo, charNo, car, cdr) : new ConsCell(car, cdr);
        }
    }



    /// Reserved words may not be used as a symbol
    private ConsCell reservedWords;

    private void reserve(Object word) { reservedWords = cons(word, reservedWords); }

    /** Throw error if sym is a reserved symbol */
    private void notReserved(final String op, final Object sym) {
        if (member(sym, reservedWords)) throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", op, sym.toString());
    }


    /// Symboltable
    private SymbolTable symtab;

    public static final Object VALUE_NOT_DEFINED = new Object();   // only relevant in letrec
    private static final Object NOT_A_SYMBOL = new Object();       // to avoid matches on pseudo env entries

    /** <p>Look up the symbols for special forms only once on first use.
     *  the suppliers below will do a lookup on first use and then replace themselves by another supplier
     *  that simply returns the cached value.
     *
     *  <p>Also start to build the table of reserved words. */
    private void setSymtab(SymbolTable symtab) {
        this.symtab = symtab;

        // (re-)read the new symtab
        sLambda =                      symtab.intern(new LambdaJSymbol("lambda"));   reserve(sLambda);
        sDynamic =                     symtab.intern(new LambdaJSymbol("dynamic"));  reserve(sDynamic);

        if (haveQuote())  { sQuote   = symtab.intern(new LambdaJSymbol("quote"));    reserve(sQuote); }
        if (haveCond())   { sCond    = symtab.intern(new LambdaJSymbol("cond"));     reserve(sCond); }
        if (haveLabels()) { sLabels  = symtab.intern(new LambdaJSymbol("labels"));   reserve(sLabels); }

        if (haveXtra())   { sEval    = symtab.intern(new LambdaJSymbol("eval"));     reserve(sEval); }
        if (haveXtra())   { sIf      = symtab.intern(new LambdaJSymbol("if"));       reserve(sIf); }
        if (haveXtra())   { sDefine  = symtab.intern(new LambdaJSymbol("define"));   reserve(sDefine); }
        if (haveXtra())   { sDefun   = symtab.intern(new LambdaJSymbol("defun"));    reserve(sDefun); }
        if (haveXtra())   { sLet     = symtab.intern(new LambdaJSymbol("let"));      reserve(sLet); }
        if (haveXtra())   { sLetStar = symtab.intern(new LambdaJSymbol("let*"));     reserve(sLetStar); }
        if (haveXtra())   { sLetrec  = symtab.intern(new LambdaJSymbol("letrec"));   reserve(sLetrec); }

        if (haveApply())  { sApply   = symtab.intern(new LambdaJSymbol("apply"));    reserve(sApply); }
        if (haveXtra())   { sProgn   = symtab.intern(new LambdaJSymbol("progn"));    reserve(sProgn); }

        expTrue = () -> { Object s = makeExpTrue(); expTrue = () -> s; return s; };
    }

    /** well known symbols for special forms */
    private Object sLambda, sDynamic, sQuote, sCond, sLabels, sEval, sIf, sDefine, sDefun, sLet, sLetStar, sLetrec, sApply, sProgn;
    private Supplier<Object> expTrue;

    private Object makeExpTrue() {
        if (haveT()) return symtab.intern(new LambdaJSymbol("t")); // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (haveQuote()) return cons(symtab.intern(new LambdaJSymbol("quote")), cons(symtab.intern(new LambdaJSymbol("t")), null));
        else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
    }



    /// eval - the heart of most if not all Lisp interpreters
    private ConsCell topEnv;

    private Object eval(Object form, ConsCell env, int stack, int level) {
        boolean isTc = false;
        try {
            stack++;

            tailcall:
            while (true) {
                level++;
                dbgEvalStart(isTc ? "eval TC" : "eval", form, env, stack, level);

                /// eval - lookup symbols in the current environment
                if (symbolp(form)) {                 // this line is a convenient breakpoint
                    if (form == null) return null;
                    final ConsCell envEntry = assoc(form, env);
                    if (envEntry != null) {
                        final Object value = cdr(envEntry);
                        if (value == VALUE_NOT_DEFINED) throw new LambdaJError(true, "%s: '%s' is bound but has no assigned value", "eval", form);
                        return value;
                    }
                    throw new LambdaJError(true, "%s: '%s' is not bound", "eval", form);
                }

                /// eval - atoms that are not symbols eval to themselves
                if (atom(form)) {
                    return form;   // this catches nil as well
                }

                /// eval - the form is enclosed in parentheses, either a special form or a function application
                if (consp(form)) {
                    final Object operator = car(form);      // first element of the of the form should be a symbol or an expression that computes a symbol
                    if (!listp(cdr(form))) throw new LambdaJError(true, "%s: expected an operand list to follow operator but got %s", "eval", printSEx(form));
                    final ConsCell arguments = (ConsCell) cdr(form);   // list with remaining atoms/ expressions



                    /// eval - special forms

                    /// eval - (quote exp) -> exp
                    if (haveQuote() && operator == sQuote) {
                        oneArg("quote", arguments);
                        return car(arguments);
                    }

                    /// eval - (lambda dynamic? (params...) forms...) -> lambda or closure
                    if (operator == sLambda) {
                        return makeClosureFromForm(form, env);
                    }



                    /// eval - special forms that change the global environment

                    /// eval - (define symbol exp) -> symbol with a side of global environment extension
                    if (haveXtra() && operator == sDefine) {
                        twoArgs("define", arguments);
                        final Object symbol = car(arguments); // todo ob statt symbol eine expression erlaubt sein sollte? expression koennte symbol errechnen
                                                              // ggf. symbol UND expression zulassen: if (symbolp(cdr(exp))...
                        if (!symbolp(symbol)) throw new LambdaJError(true, "%s: not a symbol: %s", "define", printSEx(symbol));
                        notReserved("define", symbol);
                        final ConsCell envEntry = assoc(symbol, env);
                        if (envEntry != null) throw new LambdaJError(true, "%s: '%s' was already defined, current value: %s", "define", symbol, printSEx(cdr(envEntry)));

                        final Object value = eval(cadr(arguments), env, stack, level);
                        insertFront(topEnv, symbol, value);
                        return symbol;
                    }

                    /// eval - (defun symbol (params...) forms...) -> symbol with a side of global environment extension
                    // shortcut for (define symbol (lambda (params...) forms...))
                    if (haveXtra() && operator == sDefun) {
                        nArgs("defun", arguments, 3);
                        form = list(sDefine, car(arguments), cons(sLambda, cons(cadr(arguments), cddr(arguments))));
                        continue tailcall;
                    }



                    /// eval - special forms that run expressions

                    /// eval - (eval form) -> object
                    if (operator == sEval) {
                        oneArg("eval", arguments);
                        form = eval(car(arguments), env, stack, level); isTc = true; continue tailcall;
                    }

                    /// eval - (if condform form optionalform) -> object
                    if (haveXtra() && operator == sIf) {
                        nArgs("if", arguments, 2, 3);
                        if (eval(car(arguments), env, stack, level) != null) {
                            form = cadr(arguments); isTc = true; continue tailcall;
                        } else if (cddr(arguments) != null) {
                            form = caddr(arguments); isTc = true; continue tailcall;
                        } else return null;
                    }

                    // "forms" will be set up depending on the special form and then used in "eval a list of forms" below
                    ConsCell forms = null;

                    /// eval - (progn forms...) -> object
                    if (haveXtra() && operator == sProgn) {
                        if (!consp(arguments)) throw new LambdaJError(true, "%s: malformed cond. expected a list of forms but got %s", "progn", printSEx(arguments));
                        forms = arguments;
                        // fall through to "eval a list of forms"

                    /// eval - (cond (condform forms...)... ) -> object
                    } else if (haveCond() && operator == sCond) {
                        if (arguments != null)
                            for (Object c: arguments) {
                                if (!listp(c)) throw new LambdaJError(true, "%s: malformed cond. expected a list (condexpr forms...) but got %s", "cond", printSEx(c));
                                if (eval(car(c), env, stack, level) != null) {
                                    forms = (ConsCell) cdr(c);
                                    break;
                                }
                            }

                        if (forms == null) return null; // no condition was true
                        // fall through to "eval a list of forms"

                    /// eval - (labels ((symbol (params...) forms...)...) forms...) -> object
                    } else if (haveLabels() && operator == sLabels) {
                        nArgs("labels", arguments, 2);
                        ConsCell extEnv = cons(cons(NOT_A_SYMBOL, VALUE_NOT_DEFINED), env);
                        // stick the functions into the env
                        if (car(arguments) != null)
                            for (Object binding: (ConsCell) car(arguments)) {
                                final ConsCell currentFunc = (ConsCell)binding;
                                final Object currentSymbol = car(currentFunc);
                                notReserved("labels", currentSymbol);
                                final ConsCell lambda = makeClosure(cdr(currentFunc), extEnv);
                                insertFront(extEnv, currentSymbol, lambda);
                            }
                        forms = (ConsCell) cdr(arguments);
                        env = extEnv;
                        // fall through to "eval a list of forms"

                    /// eval - (let optsymbol? (bindings...) bodyforms...) -> object
                    /// eval - (let* optsymbol? (bindings...) bodyforms...) -> object
                    /// eval - (letrec optsymbol? (bindings...) bodyforms...) -> object
                    } else if (haveXtra() && (operator == sLet) || operator == sLetStar || operator == sLetrec) {
                        final boolean letStar  = operator == sLetStar;
                        final boolean letRec   = operator == sLetrec;
                        final boolean namedLet = /*!star && !rec &&*/ symbolp(car(arguments));

                        final String op = (namedLet ? "named " : "") + operator.toString();
                        final ConsCell bindingsAndBodyForms = namedLet ? (ConsCell)cdr(arguments) : arguments;  // ((bindings...) bodyforms...)

                        final ConsCell bindings = (ConsCell)car(bindingsAndBodyForms);
                        if (!consp(bindings)) throw new LambdaJError(true, "%s: malformed %s: expected a list of bindings but got %s", op, op, printSEx(car(bindingsAndBodyForms)));

                        ConsCell extenv = cons(cons(NOT_A_SYMBOL, VALUE_NOT_DEFINED), env);
                        for (Object binding: bindings) {
                            if (!consp(binding))        throw new LambdaJError(true, "%s: malformed %s: expected bindings to contain lists but got %s", op, op, printSEx(binding));
                            final Object sym = car(binding);
                            if (!symbolp(sym)) throw new LambdaJError(true, "%s: malformed %s: expected binding to contain a symbol and a form but got %s", op, op, printSEx(binding));
                            notReserved(op, sym);
                            if (!listp(cdr(binding)))   throw new LambdaJError(true, "%s: malformed %s: expected binding to contain a symbol and a form but got %s", op, op, printSEx(binding));

                            ConsCell newBinding = null;
                            if (letRec) newBinding = insertFront(extenv, sym, VALUE_NOT_DEFINED);
                            Object val = eval(cadr(binding), letStar || letRec ? extenv : env, stack, level);      // todo sollte das cdr(binding) heissen, damit (symbol forms...) gehen wuerde? in clisp ist nur eine form erlaubt, mehr gibt *** - LET: illegal variable specification (X (WRITE "in binding") 1)
                            if (letRec)      newBinding.cdr = val;
                            else extenv = extendEnv(extenv, sym, val);
                        }
                        forms = (ConsCell)cdr(bindingsAndBodyForms);
                        if (namedLet) {
                            final ConsCell bodyParams = extractParamList(op, bindings);
                            final Object bodyForms = makeClosure(cons(bodyParams, forms), extenv);   // (optsymbol . (lambda (params bodyforms)))
                            insertFront(extenv, car(arguments), bodyForms);
                        }
                        env = extenv;
                        // fall through to "eval a list of forms"
                    }



                    /// eval - function application
                    else {
                        final Object func;
                        final ConsCell argList;

                        /// eval - apply function to list
                        /// eval - (apply form argform) -> object
                        if (haveApply() && operator == sApply) {
                            twoArgs("apply", arguments);

                            func = eval(car(arguments), env, stack, level);
                            final Object _argList = eval(cadr(arguments), env, stack, level);
                            if (!listp(_argList)) throw new LambdaJError(true, "%s: expected an argument list but got %s", "apply", printSEx(_argList));
                            argList = (ConsCell)_argList;
                            // fall through to "actually perform..."

                        /// eval - function call
                        /// eval - (operatorform argforms...) -> object
                        } else {
                            func = eval(operator, env, stack, level);
                            if (!listp(arguments)) throw new LambdaJError(true, "%s: expected an argument list but got %s", "function application", printSEx(arguments));
                            argList = evlis(arguments, env, stack, level);
                            // fall through to "actually perform..."
                        }

                        /// eval - actually perform the function call that was set up by "apply" or "function call" above
                        if (primp(func)) {
                            try { return applyPrimitive((Primitive) func, argList, stack, level); }
                            catch (LambdaJError e) { throw new LambdaJError(e.getMessage()); }

                        } else if (consp(func) && car(func) == sLambda) {
                            final Object lambda = cdr(func);          // (params . (forms...))
                            nArgs("lambda application", lambda, 2);
                            final ConsCell closure = ((ConsCell)func).closure();
                            env = zip(closure != null ? closure : env, car(lambda), argList);

                            if (trace >= TRC_FUNC)  tracer.println(pfx(stack, level) + " #<lambda " + lambda + "> " + printSEx(env));
                            forms = (ConsCell) cdr(lambda);
                            // fall through to "eval a list of forms"

                        } else {
                            throw new LambdaJError(true, "function application: not a primitive or lambda: %s", printSEx(func));
                        }
                    }

                    /// eval - eval a list of forms
                    // todo dotted list wird cce geben
                    for (; forms != null && cdr(forms) != null; forms = (ConsCell) cdr(forms))
                        eval(car(forms), env, stack, level);
                    if (forms != null) {
                        form = car(forms); isTc = true; continue tailcall;
                    }
                    return null; // lambda/ progn/ labels/... w/o body, shouldn't happen

                }

                /// eval - not a symbol/atom/cons - something is really wrong here.
                /// eval - let's sprinkle some crack on him and get out of here, Dave.
                throw new LambdaJError("eval: cannot eval expression");
            }

        } catch (LambdaJError e) {
            throw new LambdaJError(false, e.getMessage(), form);
        } catch (Exception e) {
            throw new LambdaJError(e, "eval: internal error - caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
        } finally {
            dbgEvalDone(isTc ? "eval TC" : "eval", form, env, stack, level);
        }
    }

    /** Insert a new symbolentry at the front of env, env is modified in place, address of the list will not change.
     *  Returns the newly created (and inserted) symbolentry (symbol . value) */
    private ConsCell insertFront(ConsCell env, Object symbol, Object value) {
        final ConsCell symbolEntry = cons(symbol, value);
        final Object oldCar = car(env);
        final Object oldCdr = cdr(env);
        env.car = symbolEntry;
        env.cdr = cons(oldCar, oldCdr);
        return symbolEntry;
    }

    /** Extend env by attaching a new symbolentry at the front of env, env is unchanged.
     *  Returns the extended list with newly created symbolentry (symbol . value) */
    private ConsCell extendEnv(ConsCell env, Object symbol, Object value) {
        final ConsCell symbolEntry = cons(symbol, value);
        return cons(symbolEntry, env);
    }

    /** from a list of (symbol form) conses return the symbols as new a list */
    private ConsCell extractParamList(String op, final ConsCell bindings) {
        ConsCell bodyParams = null, insertPos = null;
        if (bindings != null)
            for (Object binding: bindings) {
                final Object symbol = car(binding);
                notReserved(op, symbol);
                if (bodyParams == null) {
                    bodyParams = cons(symbol, null);
                    insertPos = bodyParams;
                } else {
                    insertPos.cdr = cons(symbol, null);
                    insertPos = (ConsCell) insertPos.cdr;
                }
            }
        return bodyParams;
    }

    /** build an extended environment for a function invocation:<pre>
     *  loop over params and args
     *    construct a list (param arg)
     *    stick above list in front of the environment
     *  return extended environment</pre> */
    private ConsCell zip(ConsCell env, Object paramList, ConsCell args) {
        if (paramList == null && args == null) return env; // shortcut for no params/ no args

        for (Object params = paramList; params != null; ) {
            // regular param/arg: add to env
            if (consp(params)) env = cons(cons(car(params), car(args)), env);

            // if paramList is a dotted list then the last param will be bound to the list of remaining args
            else {
                env = cons(cons(params, args), env);
                args = null; break;
            }

            params = cdr(params);
            if (params == paramList) throw new LambdaJError(true, "%s: malformed lambda: bindings are a circular list", "function application");

            args = (ConsCell) cdr(args);
            if (args == null) {
                if (consp(params)) throw new LambdaJError(true, "%s: not enough arguments. parameters w/o argument: %s", "function application", printSEx(params));
                else {
                    // paramList is a dotted list, no argument for vararg parm: assign nil
                    env = cons(cons(params, null), env);
                    break;
                }
            }
        }
        if (args != null)   throw new LambdaJError(true, "%s: too many arguments. remaining arguments: %s", "function application", printSEx(args));
        return env;
    }

    /** eval a list of forms and return a list of results */
    private ConsCell evlis(ConsCell _forms, ConsCell env, int stack, int level) {
        dbgEvalStart("evlis", _forms, env, stack, level);
        ConsCell head = null, insertPos = null;
        ConsCell forms = _forms;
        if (forms != null)
            for (Object form: forms) {
                ConsCell currentArg = cons(eval(form, env, stack, level), null);
                if (head == null) {
                    head = currentArg;
                    insertPos = head;
                }
                else {
                    insertPos.cdr = currentArg;
                    insertPos = currentArg;
                }
            }
        dbgEvalDone("evlis", _forms, head, stack, level);
        return head;
    }

    /** make a lexical closure (if enabled) or lambda from a lambda-form,
     *  considering whether or not "dynamic" was specified after "lambda" */
    private Object makeClosureFromForm(final Object form, ConsCell env) {
        final ConsCell paramsAndForms = (ConsCell) cdr(form);

        if (car(paramsAndForms) == sDynamic) {
            final Object _paramsAndForms = cdr(paramsAndForms);
            nArgs("lambda dynamic", _paramsAndForms, 2);
            symbolArgs("lambda dynamic", car(_paramsAndForms));
            return cons(sLambda, _paramsAndForms);
        }
        nArgs("lambda", paramsAndForms, 2);
        symbolArgs("lambda", car(paramsAndForms));

        if (haveLexC()) return makeClosure(paramsAndForms, env);
        return form;
    }

    /** make a lexical closure (if enabled) or lambda */
    private ConsCell makeClosure(final Object paramsAndForms, ConsCell env) {
        return cons3(sLambda, paramsAndForms, haveLexC() ? env : null);
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (trace >= TRC_FUNC) tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        try { return primfn.apply(args); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<primitive> throws exception: %s", e.getMessage()); }
    }



    /// Stats during eval and at the end
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
        if (trace >= TRC_ENVSTATS) {
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



    /// Functions used by interpreter program, a subset is used by interpreted programs as well
    private        ConsCell cons(Object car, Object cdr)                    { nCells++; return new ConsCell(car, cdr); }
    private        ConsCell cons3(Object car, Object cdr, ConsCell closure) { nCells++; return new ClosureConsCell(car, cdr, closure); }

    private static Object   car(ConsCell c)    { return c == null ? null : c.car; }
    private static Object   car(Object x)      { return x == null ? null : ((ConsCell)x).car; }
    private static Object   caar(ConsCell c)   { return c == null ? null : car(car(c)); }
    private static Object   cadr(ConsCell c)   { return c == null ? null : car(cdr(c)); }
    private static Object   cadr(Object c)     { return c == null ? null : car(cdr(c)); }
    private static Object   cadar(ConsCell c)  { return c == null ? null : car(cdr(car(c))); }
    private static Object   caddr(ConsCell c)  { return c == null ? null : car(cddr(c)); }

    private static Object   cdr(ConsCell c)    { return c == null ? null : c.cdr; }
    private static Object   cdr(Object x)      { return x == null ? null : ((ConsCell)x).cdr; }
    private static Object   cdar(ConsCell c)   { return c == null ? null : cdr(c.car); }
    private static Object   cdar(Object x)     { return x == null ? null : cdr(car(x)); }
    private static Object   cddr(ConsCell c)   { return c == null ? null : cdr(cdr(c)); }
    private static Object   cddr(Object x)     { return x == null ? null : cdr(cdr(x)); }

    private static boolean  consp(Object x)    { return x instanceof ConsCell; }
    private static boolean  atom(Object x)     { return !(x instanceof ConsCell); }                // ! consp(x)
    private static boolean  symbolp(Object x)  { return x == null || x instanceof LambdaJSymbol; } // null (aka nil) is a symbol too
    private static boolean  listp(Object x)    { return x == null || x instanceof ConsCell; }      // null (aka nil) is a list too
    private static boolean  primp(Object x)    { return x instanceof Primitive; }
    private static boolean  numberp(Object x)  { return x instanceof Number; }
    private static boolean  stringp(Object x)  { return x instanceof String; }

    private static int length(Object list) {
        if (list == null) return 0;
        int n = 0;
        for (Object l: (ConsCell)list) n++;
        return n;
    }

    /** todo this should handle circular and dotted lists but doesn't, todo avoid cce on dotted lists, throw eror instead:
     * (nthcdr 3 '(0 . 1))) -> Error: Attempted to take CDR of 1. */
    private static Object nthcdr(int n, Object list) {
        if (list == null) return null;
        for (; list != null && n-- > 0; list = cdr(list)) ;
        return list;
    }

    /** note: searches using object identity, will work for interned symbols, won't work for e.g. numbers */
    private static ConsCell assoc(Object atom, Object maybeList) {
        if (atom == null || maybeList == null) return null;
        if (!consp(maybeList)) throw new LambdaJError(true, "%s: expected second argument to be a List but got %s", "assoc", printSEx(maybeList));
        for (Object env: (ConsCell) maybeList) {
            if (atom == car(env)) return (ConsCell) env;
        }
        return null;
    }

    /** this method returns true while Lisp member returns the sublist starting at obj */
    private static boolean member(Object obj, ConsCell list) {
        if (obj == null) return false;
        if (list == null) return false;
        for (Object e: list) if (e == obj) return true;
        return false;
    }

    private ConsCell list(Object... a) {
        if (a == null || a.length == 0) return null;
        ConsCell ret = null, insertPos = null;
        for (Object o: a) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                insertPos.cdr = cons(o, null);
                insertPos = (ConsCell) insertPos.cdr;
            }
        }
        return ret;
    }

    /** Append rest at the end of first. If first is a list it will be modified. */
    private ConsCell combine(Object first, Object rest) {
        if (consp(first)) return appendToList((ConsCell)first, rest);
        else return cons(first, rest);
    }

    /** Append rest at the end of first, modifying first in the process.
     *  Returns a dotted list unless rest is a proper list. */
    // todo ist das nconc (destructive concatenate)
    private ConsCell appendToList(ConsCell first, Object rest) {
        for (ConsCell last = first; last != null; last = (ConsCell) cdr(last)) {
            if (cdr(last) == first) throw new LambdaJError(true, "%s: first argument is a circular list", "appendToList");
            if (cdr(last) == null) {
                last.cdr = rest;
                return first;
            }
            if (!consp(cdr(last))) {
                last.cdr = cons(last.cdr, rest);
                return first;
            }
        }
        throw new LambdaJError(true, "%s: internal error, can't append %s and %s", "appendToList", printSEx(first), printSEx(rest));
    }

    private static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError(true, "%s: expected second argument to be a List but got %s", "listToArray", printSEx(maybeList));
        final List<Object> ret = new ArrayList<>();
        ((ConsCell) maybeList).forEach(ret::add);
        return ret.toArray();
    }

    /** transform {@code ob} into an S-expression, atoms are not escaped */
    private static String printObj(Object ob) {
        if (ob == null) return "nil";
        final StringBuilder sb = new StringBuilder(200);
        _printSEx(sb::append, ob, ob, true, false);
        return sb.toString();
    }

    /** transform {@code ob} into an S-expression, atoms are escaped */
    private static String printSEx(Object obj) {
        if (obj == null) return "nil";
        final StringBuilder sb = new StringBuilder(200);
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
                    sb.print("|"); sb.print(escapeSymbol((LambdaJSymbol) obj)); sb.print("|");
                    return;
                }
                sb.print(escapeSymbol((LambdaJSymbol) obj)); return;
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

    private static String escapeSymbol(LambdaJSymbol s) {
        if (s.value == null) return null;
        if (s.value.length() == 0) return "";

        final StringBuilder ret = new StringBuilder();
        for (char c: s.value.toCharArray()) {
            switch (c) {
            case '|':  ret.append('\\').append('|'); break;
            default: ret.append(c);
            }
        }
        return ret.toString();
    }

    /** prepend " and \ by a \ */
    private static String escapeString(String s) {
        if (s == null) return null;
        if (s.length() == 0) return "";

        final StringBuilder ret = new StringBuilder();
        for (char c: s.toCharArray()) {
            switch (c) {
            case '\"':  ret.append('\\').append('\"'); break;
            case '\\': ret.append('\\').append('\\'); break;
            default: ret.append(c);
            }
        }
        return ret.toString();
    }



    /// Error checking functions, used by interpreter and primitives
    /** ecactly one argument */
    private static void oneArg(String func, Object a) {
        if (a == null)      throw new LambdaJError(true, "%s: expected one argument but no argument was given", func);
        if (cdr(a) != null) throw new LambdaJError(true, "%s: expected one argument but got extra arg(s) %s", func, printSEx(cdr(a)));
    }

    /** ecactly two arguments */
    private static void twoArgs(String func, Object a) {
        if (a == null)       throw new LambdaJError(true, "%s: expected two arguments but no argument was given", func);
        if (cdr(a) == null)  throw new LambdaJError(true, "%s: expected two arguments but only one argument was given", func);
        if (cddr(a) != null) throw new LambdaJError(true, "%s: expected two arguments but got extra arg(s) %s", func, printSEx(cddr(a)));
    }

    /** at least {@code min} args */
    private static void nArgs(String func, Object a, int min) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(true, "%s: expected %d arguments or more but got only %d", func, min, actualLength);
    }

    /** between {@code min} and {@code max} args */
    private static void nArgs(String func, Object a, int min, int max) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(true, "%s: expected %d to %d arguments but got only %d", func, min, max, actualLength);
        if (actualLength > max) throw new LambdaJError(true, "%s: expected %d to %d arguments but got extra arg(s) %s", func, min, max, printSEx(nthcdr(max, a)));
    }

    /** 'a' must be a symbol or a proper or dotted list of only symbols (empty list is fine, too).
     *  Also 'a' must not contain reserved symbols. */
    private void symbolArgs(String func, Object a) {
        if (a == null) return;
        if (symbolp(a)) return;
        if (atom(a)) throw new LambdaJError(true, "%s: malformed %s: expected bindings to be a symbol or list of symbols but got %s", func, func, a);
        final ConsCell start = (ConsCell) a;
        for (; a != null; a = cdr(a)) {
            if (listp(a) && cdr(a) == start) throw new LambdaJError(true, "%s: malformed %s: circular list of bindings is not allowed", func, func);
            if (!symbolp(car(a)) || (atom(cdr(a)) && !symbolp(cdr(a))))
                throw new LambdaJError(true, "%s: expected a symbol or a list of symbols but got %s", func, printSEx(a));
            notReserved(func, car(a));
            if (atom(cdr(a))) {
                notReserved(func, cdr(a));
                return; // that was the end of a dotted list, everything a-ok, move along
            }
        }
    }

    private static String errorExp(Object exp) {
        if (exp == null) return "";
        final String l = exp instanceof SExpConsCell ? ("before line " + ((SExpConsCell)exp).lineNo + ':' + ((SExpConsCell)exp).charNo + ": ") : "";
        return System.lineSeparator() + "error occurred in expression " + l + printSEx(exp);
    }

    ///
    /// That's (almost) all, folks.
    ///
    /// At this point we have reached the end of the Murmel interpreter core, i.e. we have everything needed
    /// to read S-Expressions and eval() them in an environment.
    ///
    /// The rest of this file contains Murmel primitives and driver functions such as interpretExpression/s and main
    /// for interactive use.
    ///



    /// Additional error checking functions used by primitives only.

    /** a must be the empty list */
    private static void noArgs(String func, ConsCell a) {
        if (a != null) throw new LambdaJError(true, "%s: expected no arguments but got %s", func, printSEx(a));
    }

    private static void oneOrMoreArgs(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(true, "%s: expected at least one argument but no argument was given", func);
    }

    private static void onePair(String func, ConsCell a) {
        if (a == null)      throw new LambdaJError(true, "%s: expected one Pair argument but no argument was given", func);
        if (!listp(car(a))) throw new LambdaJError(true, "%s: expected one Pair argument but got %s", func, printSEx(a));
        if (cdr(a) != null) throw new LambdaJError(true, "%s: expected one Pair argument but got extra arg(s) %s", func, printSEx(cdr(a)));
    }

    /** a must be a proper list of only numbers (empty list is fine, too) */
    private static void numberArgs(String func, ConsCell a) {
        if (a == null) return;
        ConsCell start = a;
        for (; a != null; a = (ConsCell) cdr(a)) {
            if (!numberp(car(a)) || (cdr(a) != null && (cdr(a) == start || !consp(cdr(a)))))
                throw new LambdaJError(true, "%s: expected a proper list of numbers but got %s", func, printSEx(a));
        }
    }

    /** between {@code min} and {@code max} number args */
    private static void numberArgs(String func, ConsCell a, int min, int max) {
        nArgs(func, a, min, max);
        numberArgs(func, a);
    }

    /** at least one number arg */
    private static void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numberArgs(func, a);
    }

    /** the given arg must be a LambdaJString */
    private static void stringArg(String func, String arg, Object a) {
        if (!stringp(car(a)))
            throw new LambdaJError(true, "%s: expected %s to be a String but got %s", func, arg, printSEx(car(a)));
    }

    /** a must be a proper list of only strings (empty list is fine, too) */
    private static void stringArgs(String func, ConsCell a) {
        if (a == null) return;
        ConsCell start = a;
        for (; a != null; a = (ConsCell) cdr(a)) {
            if (!stringp(car(a)) || (cdr(a) != null && (cdr(a) == start || !consp(cdr(a)))))
                throw new LambdaJError(true, "%s: expected a proper list of strings but got %s", func, printSEx(a));
        }
    }



    /// Runtime for Lisp programs, i.e. an environment with primitives and predefined global symbols

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

    private String format(ConsCell a, String func) {
        nArgs(func, a, 2);
        boolean toString = car(a) == null;
        a = (ConsCell) cdr(a);
        stringArg(func, "second argument", a);
        final String s = (String) car(a);
        final Object[] args = listToArray(cdr(a));
        try {
            if (toString) return String.format(s, args);
            if (!haveIO()) throw new LambdaJError(true, "%s: I/O is disabled", func);
            if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", func);
            lispPrinter.printString(String.format(s, args));
            return null;
        } catch (IllegalFormatException e) {
            throw new LambdaJError(true,
                                   "%s: illegal format string and/ or arguments: %s\nerror ocurred processing the argument(s) %s", func, e.getMessage(), printSEx(a));
        }
    }

    private String formatLocale(ConsCell a, String func) {
        nArgs(func, a, 3);
        boolean toString = car(a) == null;
        a = (ConsCell) cdr(a);

        String locString;
        if (car(a) != null) {
            stringArg(func, "first argument", a);
            locString = (String) car(a);
        } else locString = null;

        stringArg(func, "thrid argument", cdr(a));
        String s = (String) cadr(a);
        final Object[] args = listToArray(cddr(a));
        try {
            if (locString == null) {
                if (toString) return String.format(s, args);
                if (!haveIO()) throw new LambdaJError(true, "%s: I/O is disabled", func);
                if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", func);
                lispPrinter.printString(String.format(s, args));
                return null;
            }
            Locale loc = Locale.forLanguageTag(locString);
            if (toString) return String.format(loc, s, args);
            if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", func);
            lispPrinter.printString(String.format(loc, s, args));
            return null;
        } catch (IllegalFormatException e) {
            throw new LambdaJError(true,
                    "%s: illegal format string and/ or arguments: %s\nerror ocurred processing the argument(s) %s", func, e.getMessage(), printSEx(a));
        }
    }

    private static ThreadMXBean getThreadBean(final String func) {
        final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
        if (threadBean == null)
            throw new LambdaJError(true, "%s: ThreadMXBean not supported in this Java Runtime", func);
        if (!threadBean.isCurrentThreadCpuTimeSupported())
            throw new LambdaJError(true, "%s: ThreadMXBean.getCurrentThreadCpuTime() not supported in this Java Runtime", func);
        return threadBean;
    }



    private static class JavaConstructor implements Primitive {
        private final Constructor<?> constructor;

        private JavaConstructor(Constructor<?> constructor) { this.constructor = constructor; }

        @Override
        public Object apply(ConsCell x) {
            final Object[] args = listToArray(x);
            try { return constructor.newInstance(args); }
            catch (Exception e) { throw new LambdaJError(true, "new %s: %s: %s", constructor.getName(), e.getClass().getName(), e.getMessage()); }
        }
    }

    private static class JavaMethod implements Primitive {
        private final Method method;

        private JavaMethod(Method method) { this.method = method; }

        @Override
        public Object apply(ConsCell x) {
            final Object obj = car(x);
            if (obj != null && !method.getDeclaringClass().isInstance(obj))
                throw new LambdaJError(true, ":: : %s is not an instance of class %s", obj, method.getDeclaringClass().getName());
            final Object[] args = listToArray(cdr(x));
            try { return method.invoke(obj, args); }
            catch (Exception e) { throw new LambdaJError(true, "%s.%s: exception: %s", method.getClass(), method.getName(), e.getMessage()); }
        }
    }

    private static Primitive findJavaMethod(ConsCell x) {
        stringArgs(":: ", x);
        final String className = (String) car(x);
        final String methodName = (String) cadr(x);
        final ArrayList<Class<?>> paramTypes = new ArrayList<>();
        if (cddr(x) != null)
        for (Object arg: (ConsCell)cddr(x)) {
            String paramType = (String)arg;
            try { paramTypes.add(Class.forName(paramType)); }
            catch (ClassNotFoundException e) { throw new LambdaJError(true, ":: : exception finding parameter class %s: %s : %s", paramType, e.getClass().getName(), e.getMessage()); }
        }
        final Class<?>[] params = paramTypes.isEmpty() ? null : paramTypes.toArray(new Class[0]);
        try {
            final Class<?> clazz = Class.forName(className);
            return "new".equals(methodName)
                    ? new JavaConstructor(clazz.getDeclaredConstructor(params))
                            : new JavaMethod(clazz.getMethod(methodName, params));
        }
        catch (Exception e) { throw new LambdaJError(true, ":: : exception finding method: %s: %s", e.getClass().getName(), e.getMessage()); }
    }



    public ObjectReader lispReader;
    public ObjectWriter lispPrinter;

    public void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) {
        this.lispReader = lispStdin;
        this.lispPrinter = lispStdout;
    }

    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@code symtab} on the fly */
    private ConsCell environment(ConsCell env) {
        if (haveIO()) {
            final Primitive freadobj =  a -> {
                noArgs("read", a);
                if (lispReader == null) throw new LambdaJError(true, "%s: lispStdin is nil", "read");
                return lispReader.readObj();
            };
            final Primitive fwriteobj = a -> {
                oneArg("write", a);
                if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "write");
                lispPrinter.printObj(car(a)); return expTrue.get();
            };

            final Primitive fwriteln =  a -> {
                nArgs("writeln", a, 0, 1);
                if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "writeln");
                if (a == null) {
                    lispPrinter.printEol();
                    return expTrue.get();
                }
                if (haveLispEOL()) {
                    lispPrinter.printEol();
                    lispPrinter.printObj(car(a));
                    lispPrinter.printString(" ");
                } else {
                    lispPrinter.printObj(car(a));
                    lispPrinter.printEol();
                }
                return expTrue.get();
            };

            env = cons(cons(symtab.intern(new LambdaJSymbol("read")),    freadobj),
                  cons(cons(symtab.intern(new LambdaJSymbol("write")),   fwriteobj),
                  cons(cons(symtab.intern(new LambdaJSymbol("writeln")), fwriteln),
                  env)));

        }

        if (haveString()) {
            env = cons(cons(symtab.intern(new LambdaJSymbol("stringp")), (Primitive) a -> { oneArg("stringp", a); return boolResult(stringp(car(a))); }),
                  env);

            if (haveUtil()) {
                env = cons(cons(symtab.intern(new LambdaJSymbol("format")),        (Primitive) a -> format(a, "format")),
                      cons(cons(symtab.intern(new LambdaJSymbol("format-locale")), (Primitive) a -> formatLocale(a, "format-locale")),
                      env));
            }
        }

        if (haveXtra()) {
            env = cons(cons(sDynamic, sDynamic),
                    env);
        }

        if (haveT()) {
            Object sT = symtab.intern(new LambdaJSymbol("t"));
            env = cons(cons(sT, sT),
                  env);
            reserve(sT);
        }

        if (haveNil()) {
            final Object sNil = symtab.intern(new LambdaJSymbol("nil"));
            env = cons(cons(sNil, null),
                  env);
            reserve(sNil);
        }

        if (haveUtil()) {
            env = cons(cons(symtab.intern(new LambdaJSymbol("consp")),   (Primitive) a -> { oneArg("consp",   a);  return boolResult(consp  (car(a))); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("symbolp")), (Primitive) a -> { oneArg("symbolp", a);  return boolResult(symbolp(car(a))); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("listp")),   (Primitive) a -> { oneArg("listp",   a);  return boolResult(listp  (car(a))); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("not")),     (Primitive) a -> { oneArg("not",     a);  return boolResult(car(a) == null); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("assoc")),   (Primitive) a -> { twoArgs("assoc",  a);  return assoc(car(a), car(cdr(a))); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("list")),    (Primitive) a -> a),
                  env))))));

            final Primitive fusertime = a -> { return new Double(getThreadBean("get-internal-run-time").getCurrentThreadUserTime()); };
            final Primitive fcputime  = a -> { return new Double(getThreadBean("get-internal-cpu-time").getCurrentThreadCpuTime()); };
            final Primitive fsleep    = a -> {
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
            env = cons(cons(symtab.intern(new LambdaJSymbol("internal-time-units-per-second")), new Double(1e9)),
                  cons(cons(symtab.intern(new LambdaJSymbol("get-internal-real-time")), (Primitive)a -> new Double(System.nanoTime())),
                  cons(cons(symtab.intern(new LambdaJSymbol("get-internal-run-time")), fusertime), // user
                  cons(cons(symtab.intern(new LambdaJSymbol("get-internal-cpu-time")), fcputime), // user + system
                  cons(cons(symtab.intern(new LambdaJSymbol("sleep")), fsleep),
                  cons(cons(symtab.intern(new LambdaJSymbol("get-universal-time")), fUniversalTime), // seconds since 1.1.1900
                  cons(cons(symtab.intern(new LambdaJSymbol("get-decoded-time")), fDecodedTime),
                  env)))))));

            env = cons(cons(symtab.intern(new LambdaJSymbol("fatal")), (Primitive) a -> { oneArg("fatal", a); throw new RuntimeException(car(a).toString()); }),
                  env);

            env = cons(cons(symtab.intern(new LambdaJSymbol("::")), (Primitive) a -> findJavaMethod(a)),
                  env);

        }

        if (haveAtom()) {
            env = cons(cons(symtab.intern(new LambdaJSymbol("atom")), (Primitive) a -> { oneArg("atom", a); return boolResult(atom(car(a))); }),
                       env);
        }

        if (haveDouble()) {
            final Primitive fmod = args -> {
                twoArgs("mod", args);
                numberArgs("mod", args);
                return ((Number)car(args)).doubleValue() % ((Number)car(cdr(args))).doubleValue();
            };

            env = cons(cons(symtab.intern(new LambdaJSymbol("numberp")), (Primitive) args -> { oneArg("numberp", args); return boolResult(numberp(car(args))); }),
                  env);

            env = cons(cons(symtab.intern(new LambdaJSymbol("=")),       (Primitive) args -> makeCompareOp(args, "=",  compareResult -> compareResult == 0)),
                  cons(cons(symtab.intern(new LambdaJSymbol(">")),       (Primitive) args -> makeCompareOp(args, ">",  compareResult -> compareResult >  0)),
                  cons(cons(symtab.intern(new LambdaJSymbol(">=")),      (Primitive) args -> makeCompareOp(args, ">=", compareResult -> compareResult >= 0)),
                  cons(cons(symtab.intern(new LambdaJSymbol("<")),       (Primitive) args -> makeCompareOp(args, "<",  compareResult -> compareResult <  0)),
                  cons(cons(symtab.intern(new LambdaJSymbol("<=")),      (Primitive) args -> makeCompareOp(args, "<=", compareResult -> compareResult <= 0)),
                  env)))));
            env = cons(cons(symtab.intern(new LambdaJSymbol("+")),       (Primitive) args -> makeAddOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs)),
                  cons(cons(symtab.intern(new LambdaJSymbol("-")),       (Primitive) args -> makeSubOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs)),
                  cons(cons(symtab.intern(new LambdaJSymbol("*")),       (Primitive) args -> makeAddOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs)),
                  cons(cons(symtab.intern(new LambdaJSymbol("/")),       (Primitive) args -> makeSubOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs)),
                  env))));

            env = cons(cons(symtab.intern(new LambdaJSymbol("mod")),     fmod),
                  cons(cons(symtab.intern(new LambdaJSymbol("sqrt")),    (Primitive) args -> { numberArgs("sqrt",  args, 1, 1); return Math.sqrt (((Number)car(args)).doubleValue()); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("log")),     (Primitive) args -> { numberArgs("log",   args, 1, 1); return Math.log  (((Number)car(args)).doubleValue()); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("log10")),   (Primitive) args -> { numberArgs("log10", args, 1, 1); return Math.log10(((Number)car(args)).doubleValue()); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("exp")),     (Primitive) args -> { numberArgs("exp",   args, 1, 1); return Math.exp  (((Number)car(args)).doubleValue()); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("expt")),    (Primitive) args -> { numberArgs("expt",  args, 2, 2); return Math.pow  (((Number)car(args)).doubleValue(), ((Number)cadr(args)).doubleValue()); }),
                  env))))));

            env = cons(cons(symtab.intern(new LambdaJSymbol("round")),   (Primitive) args -> { numberArgs("round",   args, 1, 1); return (long)Math.round(((Number)car(args)).doubleValue()); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("floor")),   (Primitive) args -> { numberArgs("floor",   args, 1, 1); return (long)Math.floor(((Number)car(args)).doubleValue()); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("ceiling")), (Primitive) args -> { numberArgs("ceiling", args, 1, 1); return (long)Math.ceil (((Number)car(args)).doubleValue()); }),
                  env)));

            env = cons(cons(symtab.intern(new LambdaJSymbol("pi")),      Math.PI),
                    env);
        }

        if (haveEq()) {
            env = cons(cons(symtab.intern(new LambdaJSymbol("eq")), (Primitive) a -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); }),
                       env);
        }

        if (haveCons()) {
            env = cons(cons(symtab.intern(new LambdaJSymbol("cdr")),     (Primitive) a -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("car")),     (Primitive) a -> { onePair("car", a);    if (car(a) == null) return null; return caar(a); }),
                  cons(cons(symtab.intern(new LambdaJSymbol("cons")),    (Primitive) a -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); }),
                  env)));
        }

        return env;
    }



    /// JMurmel native FFI: Java calls Murmel with getValue() and getFunction()

    /** Return the value of {@code globalSymbol} in the interpreter's current global environment */
    public Object getValue(String globalSymbol) {
        if (topEnv == null) throw new LambdaJError("getValue: not initialized (must interpret *something* first)");
        final ConsCell envEntry = assoc(symtab.intern(new LambdaJSymbol(globalSymbol)), topEnv);
        if (envEntry != null) return cdr(envEntry);
        throw new LambdaJError(true, "%s: '%s' is undefined", "getValue", globalSymbol);
    }

    /** interface for compiled lambdas as well as primitives, used for FFI as well as compiled Murmel */
    public interface MurmelFunction { Object apply(Object... args) throws LambdaJError; }

    private class CallPrimitive implements MurmelFunction {
        final Primitive p;
        CallPrimitive(Primitive p) { this.p = p; }
        @Override public Object apply(Object... args) { return p.apply(list(args)); }
    }

    private class CallLambda implements MurmelFunction {
        final ConsCell lambda;
        final ConsCell env;
        CallLambda(ConsCell lambda) { this.lambda = lambda; this.env = topEnv; }
        @Override
        public Object apply(Object... args) {
            if (env != topEnv) throw new LambdaJError("MurmelFunction.apply: stale function object, global environment has changed");
            return eval(cons(lambda, list(args)), env, 0, 0);
        }
    }

    /** Function objects of Lambdas will be usable until the interpreter's environment is rebuilt
     *  by a call to interpretExpression/s, eg.<pre>
     *  MurmelFunction f = getFunction("my-function");
     *  interpreter.interpretExpressions("...");
     *  f.apply(1, 2, 3);  // this will throw a "stale function..." Exception
     *  </pre>
     */
    public MurmelFunction getFunction(String func) {
        final Object maybeFunction = getValue(func);
        if (maybeFunction instanceof Primitive) {
            return new CallPrimitive((Primitive)maybeFunction);
        }
        if (maybeFunction instanceof ConsCell && car((ConsCell)maybeFunction) == sLambda) {
            return new CallLambda((ConsCell)maybeFunction);
        }
        throw new LambdaJError(true, "getFunction: not a primitive or lambda: %s", func);
    }



    /// JMurmel JSR-223 FFI support - Java calls Murmel with JSR223 eval

    /** <p>evalScript is for JSR-223 support.
     *  <p>First call creates a new parser (parsers contain the symbol table) and inits the global environment
     *  <p>Subsequent calls will re-use the parser (including symbol table) and global environment. */
    public Object evalScript(ReadSupplier program, ReadSupplier in, WriteConsumer out) {
        if (symtab == null) {
            Parser scriptParser = new SExpressionParser(in);
            setSymtab(scriptParser);
            final ConsCell env = environment(null);
            topEnv = env;
        }
        Parser scriptParser = (Parser)symtab;
        scriptParser.setInput(program);
        setReaderPrinter(scriptParser, new SExpressionWriter(out));
        Object result = null;
        while (true) {
            final Object exp = (scriptParser instanceof SExpressionParser) ? ((SExpressionParser)scriptParser).readObj(true) : scriptParser.readObj();
            if (exp != null) result = eval(exp, topEnv, 0, 0);
            else return result;
        }
    }



    /// Public driver methods and functions to use the interpreter from Java (embedded)
    /// or from the command prompt (interactive)

    /** <p>Build environment, read a single S-expression from {@code in}, invoke {@code eval()} and return result.
     *
     *  <p>After the expression was read from {@code in}, the primitive function {@code read} (if used)
     *  will read S-expressions from {@code in} as well,
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpression(ReadSupplier in, WriteConsumer out) {
        nCells = 0; maxEnvLen = 0;
        SExpressionParser parser = new SExpressionParser(in);
        setSymtab(parser);
        ObjectWriter outWriter = new SExpressionWriter(out);
        setReaderPrinter(parser, outWriter);
        final ConsCell env = environment(null);
        topEnv = env;
        final Object exp = parser.readObj(true);
        long tStart = System.nanoTime();
        final Object result = eval(exp, env, 0, 0);
        traceStats(System.nanoTime() - tStart);
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
        return interpretExpressions(parser, inReader, outWriter, (_symtab) -> null);
    }

    /** <p>Build environment, repeatedly read an expression from {@code parser} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read expressions from {@code inReader},
     *  and {@code write}/ {@code writeln} will write Objects to {@code out}. */
    public Object interpretExpressions(Parser parser, ObjectReader inReader, ObjectWriter outWriter, CustomEnvironmentSupplier customEnv) {
        nCells = 0; maxEnvLen = 0;
        setSymtab(parser);
        setReaderPrinter(parser, outWriter);
        final ConsCell customEnvironment = customEnv == null ? null : customEnv.customEnvironment(parser);
        final ConsCell env = environment(customEnvironment);
        topEnv = env;
        Object exp = (parser instanceof SExpressionParser) ? ((SExpressionParser)parser).readObj(true) : parser.readObj();
        while (true) {
            long tStart = System.nanoTime();
            final Object result = eval(exp, env, 0, 0);
            traceStats(System.nanoTime() - tStart);
            exp = (parser instanceof SExpressionParser) ? ((SExpressionParser)parser).readObj(true) : parser.readObj();
            if (exp == null) return result;
        }
    }

    /** print and reset interpreter stats */
    private void traceStats(long nanos) {
        if (trace >= TRC_STATS) {
            tracer.println("*** max eval nesting:  " + maxEvalLevel + " ***");
            tracer.println("*** max stack used:    " + maxEvalStack + " ***");

            tracer.println("*** total ConsCells:   " + nCells + " ***");
            if (trace >= TRC_ENVSTATS) tracer.println("*** max env length:    " + maxEnvLen + " ***");

            long millis = (long)(nanos * 0.000001D);
            String ms = Long.toString(millis) + '.' + Long.toString((long)(nanos * 0.001D + 0.5D) - (long) (millis * 1000D));
            tracer.println("*** elapsed wall time: " + ms + "ms ***");
            tracer.println("");

            maxEvalLevel = maxEvalStack = nCells = maxEnvLen = 0;
        }
    }



    /** static main() function for commandline use of the Murmel interpreter */
    public static void main(String args[]) {
        misc(args);
        int trace = trace(args);
        int features = features(args);
        final LambdaJ interpreter = new LambdaJ(features, trace);

        final boolean istty = null != System.console();
        final boolean repl        = hasFlag("--repl", args) || istty;
        final boolean echo        = hasFlag("--echo", args);    // used only in repl
        final boolean printResult = hasFlag("--result", args);  // used only in filemode

        if (argError(args)) {
            System.err.println("LambdaJ: exiting because of previous errors.");
            System.exit(1);
        }

        if (repl) repl(interpreter, istty, echo); // repl() doesn't return

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

    private static class BoolHolder { boolean value; BoolHolder(boolean value) { this.value = value; }}

    /** Enter REPL, doesn't return */
    private static void repl(final LambdaJ interpreter, final boolean istty, final boolean echo) {
        final BoolHolder echoHolder = new BoolHolder(echo);

        if (!echoHolder.value) {
            System.out.println("Enter a Murmel form or :command (or enter :h for command help or :q to exit):");
            System.out.println();
        }

        final List<Object> history = new ArrayList<>();
        boolean isInit = false;
        SExpressionParser parser = null;
        ObjectWriter outWriter = null;
        ConsCell env = null;
        for (;;) {
            if (!isInit) {
                interpreter.nCells = 0; interpreter.maxEnvLen = 0;
                parser = interpreter.new SExpressionParser(() -> {
                    int c = System.in.read();
                    if (echoHolder.value && c != EOF)
                        if (istty && c == '\r') System.out.print(System.lineSeparator());
                        else System.out.print((char)c);
                    return c;
                });
                interpreter.setSymtab(parser);
                outWriter = new SExpressionWriter(System.out::print);
                interpreter.lispReader = parser; interpreter.lispPrinter = outWriter;
                env = interpreter.environment(null);
                interpreter.topEnv = env;
                isInit = true;
            }

            if (!echoHolder.value) {
                System.out.print("JMurmel> ");
                System.out.flush();
            }

            try {
                if (istty) { parser.lineNo = 1;  parser.charNo = 0; }
                final Object exp = parser.readObj(true);

                if (exp == null && parser.look == EOF
                    || exp != null && ":q"  .equals(exp.toString())) { System.out.println("bye."); System.out.println();  System.exit(0); }
                if (exp != null) {
                    if (":h"     .equals(exp.toString())) { showHelp();  continue; }
                    if (":echo"  .equals(exp.toString())) { echoHolder.value = true; continue; }
                    if (":noecho".equals(exp.toString())) { echoHolder.value = false; continue; }
                    if (":env"   .equals(exp.toString())) { System.out.println(env.toString()); System.out.println("env length: " + length(env));  System.out.println(); continue; }
                    if (":init"  .equals(exp.toString())) { isInit = false; history.clear();  continue; }
                    if (":l"     .equals(exp.toString())) { listHistory(history); continue; }
                    if (":w"     .equals(exp.toString())) { writeHistory(history, parser.readObj(false)); continue; }
                    if (":java"  .equals(exp.toString())) { compileToJava(history, parser.readObj(false), parser.readObj(false)); continue; }
                    history.add(exp);
                }

                long tStart = System.nanoTime();
                final Object result = interpreter.eval(exp, env, 0, 0);
                long tEnd = System.nanoTime();
                System.out.println();
                interpreter.traceStats(tEnd - tStart);
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

    private static void writeHistory(List<Object> history, Object filename) {
        try {
            Path p = Paths.get(filename.toString());
            Files.createFile(p);
            Files.write(p, history.stream()
                                        .map(sexp -> printSEx(sexp))
                                        .collect(Collectors.toList()));
            System.out.println("wrote history to file '" + p.toString() + '\'');
        }
        catch (Exception e) {
            System.out.println("history NOT written - error: " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
    }

    private static void listHistory(List<Object> history) {
        for (Object sexp: history) {
            System.out.println(printSEx(sexp));
        }
    }

    private static void compileToJava(List<Object> history, Object className, Object filename) {
        MurmelJavaCompiler c = new MurmelJavaCompiler(Paths.get("target")); // todo tempdir, ggf compiler nur 1x instanzieren, damits nicht so viele murmelclassloader gibt
        if (null == filename) {
            System.out.println(c.formsToJavaProgram(className.toString(), history));
            return;
        }

        final Path p = Paths.get(filename.toString());
        final CharsetEncoder encoder = StandardCharsets.UTF_8.newEncoder();
        try (final OutputStream os = Files.newOutputStream(Files.createFile(p));
             final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(os, encoder))) {
            writer.append(c.formsToJavaProgram(className.toString(), history));
            System.out.println("compiled history to Java file '" + p.toString() + '\'');
        }
        catch (Exception e) {
            System.out.println("history NOT compiled to Java - error: " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
    }

    private static void misc(String[] args) {
        if (hasFlag("--version", args)) {
            showVersion();
            System.exit(0);
        }

        if (hasFlag("--help", args) || hasFlag("--usage", args)) {
            showVersion();
            System.out.println();
            showUsage();
            System.exit(0);
        }
    }
    private static int trace(String[] args) {
        int trace = TRC_NONE;
        if (hasFlag("--trace=stats", args))    trace = TRC_STATS;
        if (hasFlag("--trace=envstats", args)) trace = TRC_ENVSTATS;
        if (hasFlag("--trace=eval", args))     trace = TRC_EVAL;
        if (hasFlag("--trace=env", args))      trace = TRC_ENV;
        if (hasFlag("--trace", args))          trace = TRC_LEX;
        return trace;
    }

    private static int features(String[] args) {
        int features = HAVE_ALL_LEXC;
        if (hasFlag("--XX-dyn", args))      features =  HAVE_ALL_DYN;

        if (hasFlag("--no-nil", args))      features &= ~HAVE_NIL;
        if (hasFlag("--no-t", args))        features &= ~HAVE_T;
        if (hasFlag("--no-extra", args))    features &= ~HAVE_XTRA;
        if (hasFlag("--no-number", args))   features &= ~(HAVE_DOUBLE | HAVE_LONG);
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
        return features;
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
        System.out.println(ENGINE_VERSION);
    }

    private static void showHelp() {
        System.out.println("Available commands:");
        System.out.println("  :h ................. this help screen");
        System.out.println("  :echo .............. print forms to screen before eval'ing");
        System.out.println("  :noecho ............ don't print forms");
        System.out.println("  :env ............... list current global environment");
        System.out.println("  :init .............. re-init global environment, clear history");
        System.out.println("  :l ................. print history to a the screen");
        System.out.println("  :w <filename> ...... write history to a new file with the given filename");
        System.out.println("  :java nil .......... compile history to Java and print to the screen");
        System.out.println("  :java <filename> ... compile history to Java and write to a new file with the given filename");
        System.out.println("  :q ................. quit LambdaJ");
        System.out.println();
    }

    // for updating the usage message edit the file usage.txt and copy/paste its contents here between double quotes
    private static void showUsage() {
        System.out.println("Usage:\n"
                + "\n"
                + "interactive:\n"
                + "java -jar jmurmel.jar [commandline arguments]*\n"
                + "\n"
                + "non-interactive:\n"
                + "java -jar jmurmel.jar [commandline arguments]* < lisp-source.lisp\n"
                + "\n"
                + "Commandline arguments are:\n"
                + "\n"
                + "Misc:\n"
                + "--version ........  show version and exit\n"
                + "--help ...........  show this message and exit\n"
                + "\n"
                + "--repl ...........  enter REPL even if the input isn't a tty,\n"
                + "                    i.e. print prompt and results and support :commands.\n"
                + "\n"
                + "--eol=LISP .......  writeln prints <EOL><argument>< >\n"
                + "--eol=C ..........  writeln prints <argument><EOL>\n"
                + "\n"
                + "--echo ...........  echo all input while reading\n"
                + "--trace=stats ....  print stack and memory stats at end\n"
                + "--trace=envstats .  print stack and memory stats at end\n"
                + "--trace=eval .....  print internal interpreter info during executing programs\n"
                + "--trace=env ......  print more internal interpreter info executing programs\n"
                + "--trace ..........  print lots of internal interpreter info during\n"
                + "                    reading/ parsing/ executing programs\n"
                + "\n"
                + "Feature flags:\n"
                + "\n"
                + "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n"
                + "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n"
                + "--no-extra ....  no special forms 'eval', 'if', 'define', 'defun',\n"
                + "                 'letrec', 'progn'\n"
                + "--no-number ...  no number support\n"
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
                + "                   lambda\n"
                + "\n"
                + "\n"
                + "--XX-dyn ......  Use dynamic environments instead of Murmel's\n"
                + "                 lexical closures with dynamic global environment.\n"
                + "                 WARNING: This flag is for experimentation purposes only\n"
                + "                          and may be removed in future versions.\n"
                + "                          Use at your own discretion.\n"
                + "                          Using --XX-dyn JMurmel will no longer implement Murmel\n"
                + "                          and your programs may silently compute different\n"
                + "                          results!");
    }



    /** Base class for compiled Murmel programs, contains Murmel runtime. */
    public abstract static class MurmelJavaRuntime {

        protected final LambdaJ intp = new LambdaJ();

        protected final Object nil = null;
        protected final Object t = true;
        protected final MurmelFunction write = args -> { intp.lispPrinter.printObj(args[0]); return intp.expTrue.get(); };

        protected MurmelJavaRuntime() { intp.interpretExpression(() -> -1, System.out::print); }

        protected Object apply(Object fn, Object... args) {
            MurmelFunction f = (MurmelFunction)fn;
            return f.apply(args);
        }

        public abstract Object body();
        public abstract Object getValue(String globalSymbol);

        public MurmelFunction getFunction(String func) {
            final Object maybeFunction = getValue(func);
            if (maybeFunction instanceof MurmelFunction) {
                return (MurmelFunction)maybeFunction;
            }
            throw new LambdaJError(true, "getFunction: not a primitive or lambda: %s", func);
        }
    }

    public static class MurmelJavaCompiler {
        private MurmelClassLoader murmelClassLoader;

        public MurmelJavaCompiler(Path outPath) {
            AccessController.doPrivileged((PrivilegedAction<?>) () -> {
                murmelClassLoader = new MurmelClassLoader(outPath);
                return null;
            });
        }

        /** Compile a Murmel compilation unit to a Java class for a standalone application with a "public static void main()" */
        @SuppressWarnings("unchecked")
        public Class <MurmelJavaRuntime> formsToApplicationClass(String unitName, Iterable<Object> forms, String jarFile) throws Exception {
            Class<MurmelJavaRuntime> program = (Class<MurmelJavaRuntime>) javaToClass(unitName, formsToJavaProgram(unitName, forms));
            if (jarFile == null) return program;

            final Manifest mf = new Manifest();
            mf.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
            mf.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_TITLE, LambdaJ.ENGINE_NAME);
            mf.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_VERSION, LambdaJ.ENGINE_VERSION);
            mf.getMainAttributes().put(Attributes.Name.MAIN_CLASS, unitName);
            mf.getMainAttributes().put(Attributes.Name.CLASS_PATH, "jmurmel-1.0-SNAPSHOT.jar"); // todo nicht das jar hier hardcoded, entweder alle jmurmel klassen ins jar stecken oder i-wie konfigurierbar
            final JarOutputStream jar = new JarOutputStream(new FileOutputStream(jarFile), mf);

            // todo klassen mit pkg
            final JarEntry entry = new JarEntry(unitName + ".class");
            entry.setTime(System.currentTimeMillis());
            jar.putNextEntry(entry);
            jar.write(murmelClassLoader.getBytes(unitName));
            jar.closeEntry();

            jar.close();
            return program;
        }

        /** Compile a Murmel compilation unit to Java source for a standalone application with a "public static void main()" */
        public String formsToJavaProgram(String unitName, Iterable<Object> forms) {
            final StringBuilder ret = new StringBuilder();
            final String clsName;
            final int dotpos = unitName.lastIndexOf('.');
            if (dotpos == -1) {
                clsName = unitName;
            } else {
                ret.append("package " + unitName.substring(0, dotpos)).append(";\n\n");
                clsName = unitName.substring(dotpos+1);
            }
            ret.append("import com.robertmayer.lambdaj.LambdaJ;\n\n"
                    + "public class " + clsName + " extends LambdaJ.MurmelJavaRuntime {\n"
                    + "    public static void main(String[] args) {\n"
                    + "        final " + clsName + " program = new " + clsName + "();\n"
                    + "        try {\n"
                    + "            Object result = program.body();\n"
                    + "            if (result != null) { System.out.println(); System.out.print(\"==> \"); program.write.apply(result); System.out.println(); }\n"
                    + "            return;\n"
                    + "        } catch (LambdaJ.LambdaJError e) {\n"
                    + "            System.err.println(e.getMessage());\n"
                    + "            System.exit(1);\n"
                    + "        }\n"
                    + "    }\n\n");

            for (Object form: forms)
                if (consp(form) && isSymbol(car(form), "define")) defineGlobal(ret, (ConsCell) cdr(form));

            // generate getValue() and getFunction()
            ret.append("\n    public Object getValue(String symbol) {\n"
                     + "        switch (symbol) {\n");
            for (Object form: forms)
                if (consp(form) && isSymbol(car(form), "define"))
                    ret.append("        case \"").append(cadr(form)).append("\": return ").append(cadr(form)).append(";\n");

            ret.append("        default: throw new LambdaJ.LambdaJError(true, \"%s: '%s' is undefined\", \"getValue\", symbol);\n"
                     + "        }\n"
                     + "    }\n\n");

            ret.append("\n    public Object body() {\n        Object result = null;\n");
            formsToJava(ret, forms);
            ret.append("        return result;\n    }\n");

            ret.append("}\n");
            return ret.toString();
        }

        private boolean isSymbol(Object form, String sym) {
            return form.toString().equals(sym);
        }

        private void defineGlobal(StringBuilder sb, ConsCell form) {
            sb.append("    private final Object ").append(car(form)).append(" = ");
            formToJava(sb, cadr(form));
            sb.append(';').append(System.lineSeparator());
        }

        private void formsToJava(StringBuilder ret, Iterable<Object> forms) {
            for (Object form: forms) {
                if (!(consp(form) && isSymbol(car(form), "define"))) {
                    ret.append("        result = ");
                    formToJava(ret, form);
                    ret.append(';').append(System.lineSeparator());
                }
            }
        }

        private void formToJava(StringBuilder sb, Object form) {
            if (atom(form)) {
                sb.append(printSEx(form));  return;
            }
            if (consp(form)) {
                final Object op = car(form);
                Object args = cdr(form);

                if (isSymbol(op, "define")) return;

                if (isSymbol(op, "lambda")) {
                    sb.append("(LambdaJ.MurmelFunction)(args -> {\n        Object result;\n");
                    params(sb, car(args));
                    formsToJava(sb, (ConsCell)cdr(args));
                    sb.append("        return result; })");
                    return;
                }

                // function call
                sb.append("apply(");
                formToJava(sb, op);
                if (args != null)
                    for (Object arg: (ConsCell)args) {
                        sb.append(", ");
                        formToJava(sb, arg);
                    }
                sb.append(')');
                return;
            }
            throw new LambdaJError("compile-to-java: form not implemented: " + printSEx(form));
        }

        private void params(StringBuilder sb, Object paramList) {
            sb.append("        final Object ").append(car(paramList).toString()).append(" = args[0];");
            int n = 1;
            for (Object param: (ConsCell)cdr(paramList)) {
                sb.append("\n        final Object ").append(param.toString()).append(" = args[").append(n++).append("];");
            }
            sb.append("\n");
        }



        /** Compile Java sourcecode of class {@code className} to Java bytecode */
        public Class<?> javaToClass(String className, String javaSource) throws Exception {
            final JavaCompiler comp = ToolProvider.getSystemJavaCompiler();
            final StandardJavaFileManager fm = comp.getStandardFileManager(null, null, null);
            try {
                fm.setLocation(StandardLocation.CLASS_OUTPUT, Collections.singletonList(murmelClassLoader.getOutPathFile()));
                final CompilationTask c = comp.getTask(null, fm, null, null, null, Collections.singletonList(new JavaSourceFromString(className, javaSource)));
                if (c.call()) {
                    return Class.forName(className, true, murmelClassLoader);
                }
                return null;
            }
            finally {
                fm.close();
            }
        }
    }
}

class JavaSourceFromString extends SimpleJavaFileObject {
    /**
     * The source code of this "file".
     */
    private final String code;

    /**
     * Constructs a new JavaSourceFromString.
     * @param name the name of the compilation unit represented by this file object
     * @param code the source code for the compilation unit represented by this file object
     */
    JavaSourceFromString(String name, String code) {
        super(java.net.URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE);
        this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return code;
    }
}

class MurmelClassLoader extends ClassLoader {
    private final Path outPath;

    MurmelClassLoader(Path outPath) { this.outPath = outPath; }

    @Override
    public Class<?> findClass(String name) throws ClassNotFoundException {
        try {
            byte[] ba = getBytes(name);
            if (ba == null) return super.findClass(name);
            return defineClass(name, ba, 0, ba.length);
        }
        catch (IOException e) {
            throw new ClassNotFoundException(e.getMessage());
        }
    }

    File getOutPathFile() { return outPath.toFile(); }
    byte[] getBytes(String name) throws IOException {
        Path p = outPath.resolve(Paths.get(name + ".class"));
        if (!Files.isReadable(p)) return null;
        p.toFile().deleteOnExit();
        return Files.readAllBytes(p);
    }
}
