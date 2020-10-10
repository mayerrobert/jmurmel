/* Copyright (C) 2020 by Robert Mayer */
package com.robertmayer.lambdaj;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.IllegalFormatException;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.DoubleBinaryOperator;
import java.util.function.IntPredicate;
import java.util.function.Supplier;

public class LambdaJ {

    @FunctionalInterface public interface ReadSupplier { int read() throws IOException; }
    @FunctionalInterface public interface WriteConsumer { void print(String s); }
    @FunctionalInterface public interface Tracer { void println(String msg); }

    @FunctionalInterface public interface ObjectReader { Object readObj(); }
    public interface SymbolTable { String intern(String symbol); }
    public interface Parser extends ObjectReader, SymbolTable {}

    public interface ObjectWriter { void printObj(Object o); void printEol(); }

    @FunctionalInterface public interface Primitive { Object apply(ConsCell x); }

    public interface CustomBuiltinsSupplier {
        ConsCell customEnvironment(SymbolTable symtab, ObjectReader lispStdin, ObjectWriter lispStdout);
    }



    public static class LambdaJError extends RuntimeException {
        public static final long serialVersionUID = 1;

        public LambdaJError(String msg) { super(msg, null, false, false); }

        @Override
        public String toString() { return "Error: " + getMessage(); }
    }



    /// data type used by interpreter program as well as interpreted programs
    public static class ConsCell implements Iterable<Object> {
        private static class ConsCellIterator implements Iterator<Object> {
            private final ConsCell coll;
            private Object cursor;

            private ConsCellIterator(ConsCell coll) { this.coll = coll; this.cursor = coll; }

            @Override
            public boolean hasNext() { return cursor != null; }

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

        public Object car, cdr;
        public ConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }

        @Override
        public String toString() { return printSEx(this); }

        @Override
        public Iterator<Object> iterator() { return new ConsCellIterator(this); }
    }

    public static class LambdaJString {
        private final String value;
        public LambdaJString(String value) { this.value = value; }
        @Override
        public String toString() { return value.toString(); }
    }



    /// infrastructure
    public static final int EOF = -1;
    public static final int TOKEN_MAX = 2000; // max length of symbols and literals

    public static final int TRC_NONE = 0, TRC_EVAL = 1, TRC_PRIM = 2, TRC_PARSE = 3, TRC_TOK = 4, TRC_LEX = 5;
    public int trace = TRC_NONE;

    private Tracer tracer = System.err::println;

    // see https://news.ycombinator.com/item?id=8714988 for how to implement cons, car, cdr, true, false, if in Lambda
    // as well as how to implement numbers using lists
    private boolean
    HAVE_LABELS = true,                   // use Y-combinator instead
    HAVE_NIL = true, HAVE_T = true,       // use () and (quote t) instead. printObj will print nil regardless
    HAVE_XTRA = true,                     // no extra special forms such as if
    HAVE_DOUBLE = true,                   // no +-<>..., numberp, remaining datatypes are symbls and cons-cells (lists)
                                          // see https://stackoverflow.com/questions/3467317/can-you-implement-any-pure-lisp-function-using-the-ten-primitives-ie-no-type-p/3468060#3468060
                                          // for how to implement numbers in lambda
    HAVE_STRING = true,
    HAVE_IO = true,                       // no read/ write, result only
    HAVE_UTIL = true,                     // no null?, consp, listp, symbolp, assoc
    HAVE_APPLY = true,                    // McCarthy didn't list apply
    HAVE_CONS = true,
    HAVE_COND = true,
    HAVE_ATOM = true,
    HAVE_EQ = true,
    HAVE_QUOTE = true
    ;

    /** nothing except cons, car, cdr, cond, apply */
    public void haveMinPlus() {
        HAVE_NIL = false;
        HAVE_T = false;
        HAVE_XTRA = false;
        HAVE_DOUBLE = false;
        HAVE_STRING = false;
        HAVE_IO = false;
        HAVE_UTIL = false;
    }

    public void haveMin() {
        haveMinPlus();
        HAVE_APPLY = false;
        HAVE_LABELS = false;
    }

    /** almost bare lambda calculus */
    public void haveLambdaPlus() {
        haveMin();
        HAVE_CONS = false;
        HAVE_COND = false;
    }

    /** bare lambda calculus */
    public void haveLambda() {
        haveLambdaPlus();
        HAVE_ATOM = false;
        HAVE_EQ = false;
        HAVE_QUOTE = false;
    }



    private static boolean isSExSyntaxChar(int x) { return x == '(' || x == ')' || x == '\''; }

    private static boolean containsSExSyntax(String s) {
        for (int i = 0; i < s.length(); i++)
        if (isSExSyntaxChar(s.charAt(i))) return true;
        return false;
    }

    public static class SExpressionWriter implements ObjectWriter {
        private WriteConsumer out;  // printObj() and printEol() will write to this

        public SExpressionWriter(WriteConsumer out) { this.out = out; }
        @Override public void printObj(Object ob) { out.print(printSEx(ob)); }
        @Override public void printEol() { out.print(System.lineSeparator()); }
    }

    // todo zum nur-lesen sollte zumindest symtab/intern, ggf. auch single quote handling mit einem flag im construktor abgedreht werden, oder die gelesenen objekte muessen halt strings in double quotes enthalten
    /** This class can read, parse (while generating symbol table entries) and write S-Expressions */
    public class SExpressionParser implements Parser {
        /// scanner
        private ReadSupplier in;    // readObj() will read from this
        private boolean init;

        private int lineNo = 1, charNo;
        private boolean escape; // is the lookahead escaped
        private boolean tokEscape;
        private int look;
        private int token[] = new int[TOKEN_MAX + 1]; // provide for trailing '\0'
        private Object tok;

        public SExpressionParser(ReadSupplier in) { this.in = in; }

        private boolean isSpace(int x)  { return !escape && (x == ' ' || x == '\t' || x == '\n' || x == '\r'); }
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
                } else if (HAVE_STRING && isDQuote(look)) {
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
            if (HAVE_DOUBLE && isNumber()) {
                try {
                    tok = Double.valueOf(tokenToString(token));
                }
                catch (NumberFormatException e) {
                    throw new LambdaJError("line " + lineNo + ':' + charNo + ": '" + tokenToString(token)
                    + "' is not a valid symbol or number");
                }
            } else if (HAVE_STRING && token[0] == '"') {
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
            StringBuffer ret = new StringBuffer(TOKEN_MAX);
            for (int c: s) {
                if (c == '\0') break;
                ret.append((char)c);
            }
            return ret.toString();
        }



        /// symbol table implemented with a list. could easily replaced by a HashMap for better performance
        private ConsCell symbols = null;

        @Override
        public String intern(String sym) {
            ConsCell pair = symbols;
            for ( ; pair != null; pair = (ConsCell)cdr(pair)) {
                if (sym.equalsIgnoreCase((String)car(pair))) {
                    return (String) car(pair);
                }
            }
            symbols = cons(sym, symbols);
            return (String) car(symbols);
        }



        /// parser
        @Override
        public Object readObj() {
            if (!init) {
                look = getchar();
                init = true;
            }
            readToken();
            return _readObj();
        }

        private Object quote = intern("quote");

        private Object _readObj() {
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
            if (!tokEscape && HAVE_QUOTE && "'".equals(tok)) {
                readToken();
                return cons(quote, cons(_readObj(), null));
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
            Object tmp = _readObj();
            if (symbolp(tmp)) return cons(tmp, readList());
            else return cons(tmp, readList());
        }
    }




    /// eval - interpreter
    private SymbolTable symtab;

    private void setSymtab(SymbolTable symtab) {
        this.symtab = symtab;

        // (re-)set the suppliers so that they will (re-)read the new symtab
        sApply  = () -> { Supplier<String> sym = () -> symtab.intern("apply");  return (sApply  = sym).get(); };
        sCond   = () -> { Supplier<String> sym = () -> symtab.intern("cond");   return (sCond   = sym).get(); };
        sIf     = () -> { Supplier<String> sym = () -> symtab.intern("if");     return (sIf     = sym).get(); };
        sLabels = () -> { Supplier<String> sym = () -> symtab.intern("labels"); return (sLabels = sym).get(); };
        sLambda = () -> { Supplier<String> sym = () -> symtab.intern("lambda"); return (sLambda = sym).get(); };
        sQuote  = () -> { Supplier<String> sym = () -> symtab.intern("quote");  return (sQuote  = sym).get(); };
    }
    // look up the symbols for special forms only once on first use.
    // the suppliers below will do a lookup on first use and then replace themselves by another supplier
    // that simply returns the cached value
    private Supplier<String> sApply;
    private Supplier<String> sCond;
    private Supplier<String> sIf;
    private Supplier<String> sLabels;
    private Supplier<String> sLambda;
    private Supplier<String> sQuote;

    private Object eval(Object exp, ConsCell env, int stack, int level) {
        dbgEvalStart(exp, env, stack, level);
        try {
            level--;
            while (true) {
                level++;
                if (symbolp(exp)) {
                    if (exp == null) return null;
                    ConsCell envEntry = assoc(exp, env);
                    if (envEntry != null) return car(cdr(envEntry));
                    throw new LambdaJError("'" + exp + "' is undefined");

                } else if (atom(exp)) {
                    return exp;

                // special forms
                } else if (symbolp(car (exp))) {
                    if (HAVE_QUOTE && car(exp) == sQuote.get()) {
                        oneArg("quote", cdr(exp));
                        return car(cdr(exp));

                    } else if (HAVE_XTRA && car(exp) == sIf.get()) {
                        nArgs("if", cdr(exp), 2, 3, exp);
                        if (eval(car(cdr(exp)), env, stack + 1, level + 1) != null) {
                            exp = car(cdr(cdr(exp))); continue;
                        } else if (cdr(cdr(cdr(exp))) != null) {
                            exp = car(cdr(cdr(cdr(exp)))); continue;
                        } else
                            return null;

                    } else if (car(exp) == sLambda.get()) {
                        nArgs("lambda", cdr(exp), 2, exp);
                        return exp;

                    } else if (HAVE_LABELS && car(exp) == sLabels.get()) { // labels bindings body -> object
                        nArgs("labels", cdr(exp), 2, exp);
                        ConsCell bindings = (ConsCell) car(cdr(exp));
                        ConsCell body =     (ConsCell) cdr(cdr(exp));
                        return evlabels(bindings, body, env, stack, level);

                    } else if (HAVE_COND && car(exp) == sCond.get()) {
                        return evcon((ConsCell) cdr(exp), env, stack, level);

                    } else if (HAVE_APPLY && car(exp) == sApply.get()) { // apply function to list
                        twoArgs("apply", cdr(exp), exp);
                        final Object func = eval(car(cdr(exp)), env, stack + 1, level + 1);
                        final ConsCell args = (ConsCell)car(evlis((ConsCell) cdr(cdr(exp)), env, stack, level));
                        if (consp(func)) {
                            exp = cons(func, args); continue;
                        } else if (isPrim(func)) return applyPrimitive((Primitive) func, args, stack);
                        else throw new LambdaJError("apply: not a function: " + printSEx(func)
                        + ". this was the result of evaluating the expression "
                        + printSEx(car(cdr(exp))) + errorExp(exp));

                    } else { /* function call */
                        Object func = eval(car(exp), env, stack + 1, level + 1);
                        if (consp(func)) { /* user defined lambda, arg list eval happens in binding  below */
                            exp = cons(func, cdr(exp)); continue;
                        } else if (isPrim(func)) {
                            return applyPrimitive((Primitive) func, evlis((ConsCell) cdr(exp), env, stack, level + 1), stack);
                        }
                        else throw new LambdaJError("not a function: " + printSEx(func) + errorExp(exp));
                    }

                } else if (consp(car(exp)) && car(car(exp)) == sLambda.get()) {
                    /* should be a lambda, bind args as "names" into env and eval body-list */
                    final Object lambda = cdr(car(exp));
                    nArgs("lambda", lambda, 2, exp);

                    ConsCell extenv = env, params = (ConsCell) car(lambda), args = (ConsCell) cdr(exp);
                    for ( ; params != null && args != null; params = (ConsCell) cdr(params), args = (ConsCell) cdr(args))
                        extenv = cons(cons(car(params),  cons(eval(car(args), env, stack + 1, level + 1), null)), extenv);
                    if (params != null)
                        throw new LambdaJError("lambda: not enough arguments. parameters w/o argument: " + printSEx(params)
                        + errorExp(exp));
                    if (args != null)
                        throw new LambdaJError("lambda: too many arguments. remaining arguments: " + printSEx(args)
                        + errorExp(exp));

                    ConsCell body = (ConsCell) cdr(lambda);
                    for (; body != null && cdr(body) != null; body = (ConsCell) cdr(body))
                        eval(car(body), extenv, stack + 1, level + 1);
                    if (body != null) {
                        exp = car(body); env = extenv; continue;
                    } // else fall through to "cannot eval". should really not happen anyway

                } else if (atom(car(exp))) {
                    throw new LambdaJError("not a function: " + printSEx(car(exp)) + errorExp(exp));

                }

                throw new LambdaJError("cannot eval expression '" + printSEx(exp) + '\'');
            }

        } catch (Exception e) {
            throw e; // convenient breakpoint for errors
        } finally {
            dbgEvalDone(stack, level);
        }
    }

    /*
   (evcon (c e)
     (cond ((eval (caar c) e)
             (eval (cadar c) e))
           (t
             (evcon (cdr c) e))))
    */
    private Object evcon(ConsCell c, ConsCell e, int stack, int level) {
        for ( ; c != null; c = (ConsCell) cdr(c)) {
            Object condResult = eval(car(car(c)), e, stack + 1, level + 1);
            if (condResult != null) return eval(car(cdr(car(c))), e, stack + 1, level + 1);
        }
        return null;
    }

    private ConsCell evlis(ConsCell list, ConsCell env, int stack, int level) {
        ConsCell head = null, insertPos = null;
        for ( ; list != null; list = (ConsCell) cdr(list)) {
            ConsCell currentArg = cons(eval(car(list), env, stack + 1, level + 1), null);
            if (head == null) {
                head = currentArg;
                insertPos = head;
            }
            else {
                insertPos.cdr = currentArg;
                insertPos = currentArg;
            }
        }
        return head;
    }

    private Object evlabels(ConsCell bindings, ConsCell body, ConsCell env, int stack, int level) {
        ConsCell extenv = env;
        for (; bindings != null; bindings = (ConsCell)cdr(bindings)) {
            final ConsCell currentFunc = (ConsCell)car(bindings);
            final String currentName = (String)car(currentFunc);
            final ConsCell currentBody = (ConsCell)cdr(currentFunc);
            final ConsCell lambda = cons(cons(sLambda.get(), currentBody), null);
            extenv = cons(cons(symtab.intern(currentName), lambda), extenv);
        }

        Object result = null;
        for (; body != null; body = (ConsCell) cdr(body))
            result = eval(car(body), extenv, stack + 1, level + 1);
        return result;
    }

    private int maxEvalStack;
    private int maxEvalLevel;

    private void dbgEvalStart(Object exp, ConsCell env, int stack, int level) {
        if (trace >= TRC_EVAL) {
            if (maxEvalStack < stack) maxEvalStack = stack;
            if (maxEvalLevel < level) maxEvalLevel = level;
            char[] cpfx = new char[stack*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            tracer.println(pfx + "*** eval (" + stack + '/' + level + ") ********");
            tracer.println(pfx + "env: " + printSEx(env));
            tracer.println(pfx + "exp: " + printSEx(exp));
        }
    }

    private void dbgEvalDone(int stack, int level) {
        if (trace >= TRC_EVAL) {
            char[] cpfx = new char[stack*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            tracer.println(pfx + "*** eval (" + stack + '/' + level + ") done ***");
        }
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private static ConsCell cons(Object car, Object cdr) { return new ConsCell(car, cdr); }
    private static Object   car(Object x)                { return ((ConsCell)x).car; }
    private static Object   cdr(Object x)                { return ((ConsCell)x).cdr; }

    private static boolean  consp(Object x)             { return x instanceof ConsCell; }
    private static boolean  atom(Object x)              { return x == null || !(x instanceof ConsCell); } // !isCons(x)
    private static boolean  symbolp(Object x)           { return x == null || x instanceof String; } // null (alias nil) is a symbol too
    private static boolean  listp(Object x)             { return x == null || x instanceof ConsCell; } // null is a list too
    private static boolean  isPrim(Object x)            { return x instanceof Primitive; }

    private static boolean  numberp(Object x)           { return x instanceof Number; }
    private static boolean  stringp(Object x)           { return x instanceof LambdaJString; }

    private static int length(Object list) {
        if (list == null) return 0;
        int n = 0;
        for (ConsCell l = (ConsCell) list; l != null; l = (ConsCell) cdr(l)) n++;
        return n;
    }

    private static Object nthcdr(int n, Object list) {
        if (list == null) return null;
        ConsCell l = (ConsCell) list;
        for (; l != null && n-- > 0; l = (ConsCell) cdr(l)) ;
        return l;
    }

    /** note: searches using object identity, will work for interned symbols, won't work for e.g. numbers */
    private static ConsCell assoc(Object atom, Object maybeList) {
        if (atom == null) return null;
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("assoc: expected second argument to be a List but got " + printSEx(maybeList));
        ConsCell env = (ConsCell) maybeList;
        for ( ; env != null; env = (ConsCell)cdr(env)) {
            if (atom == car(car(env))) return (ConsCell) car(env);
            if (maybeList == cdr(env)) return null; // circular list, we didn't find the symbol
        }
        return null;
    }

    private static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("listToArray: expected second argument to be a List but got " + printSEx(maybeList));
        ConsCell env = (ConsCell) maybeList;
        List<Object> ret = new ArrayList<>();
        for ( ; env != null && maybeList != cdr(env); env = (ConsCell)cdr(env))
            ret.add(car(env));
        return ret.toArray();
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack) {
        if (trace >= TRC_PRIM) {
            char[] cpfx = new char[stack*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            tracer.println(pfx + "(<primitive> " + printSEx(args) + ')');
        }
        return primfn.apply(args);
    }

    /** transform {@code ob} into an S-expression */
    private static String printSEx(Object ob) {
        if (ob == null) return "nil";
        final StringBuffer sb = new StringBuffer(200);
        _printSEx(sb, ob, ob, true);
        return sb.toString();
    }

    private static void _printSEx(StringBuffer sb, Object list, Object current, boolean head_of_list) {
        while (true) {
            if (current == null) {
                sb.append("nil"); return;
            } else if (listp(current)) {
                if (head_of_list) sb.append('(');
                if (car(current) == list) {
                    sb.append(head_of_list ? "#<this cons>" : "#<this list>");
                } else {
                    _printSEx(sb, car(current), car(current), true);
                }
                if (cdr(current) != null) {
                    if (listp(cdr(current))) {
                        sb.append(' ');
                        if (list == cdr(current)) {
                            sb.append("#<circular list>)"); return;
                        } else {
                            current = cdr(current); head_of_list = false; continue;
                        }
                    } else if (head_of_list) {
                        sb.append(" . ");
                        _printSEx(sb, list, cdr(current), false);
                        sb.append(')');
                        return;
                    } else {
                        sb.append(' ');
                        _printSEx(sb, list, cdr(current), false); // must be an atom
                        sb.append(')');
                        return;
                    }
                } else {
                    sb.append(')');
                    return;
                }
            } else if (symbolp(current)) {
                if (containsSExSyntax(current.toString())) {
                    sb.append('|').append(current.toString()).append('|');
                    return;
                }
                sb.append(current.toString()); return;
            } else if (isPrim(current)) {
                sb.append("#<primitive>"); return;
            } else if (stringp(current)) {
                sb.append('"').append(current.toString()).append('"'); return;
            } else if (atom(current)) {
                sb.append(current.toString()); return;
            } else {
                sb.append("<internal error>"); return;
            }
        }
    }



    /// runtime for Lisp programs
    private Object _expTrue;
    private Object expTrue() {
        if (_expTrue == null) {
            if (HAVE_T) _expTrue = symtab.intern("t"); // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
            else if (HAVE_QUOTE) _expTrue = cons(symtab.intern("quote"), cons(symtab.intern("t"), null));
            else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
        }
        return _expTrue;
    }

    private Object boolResult(boolean b) { return b ? expTrue() : null; }

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
        if (cdr(cdr(a)) != null) throw new LambdaJError(func + ": expected two arguments but got extra arg(s) " + printSEx(cdr(cdr(a))) + errorExp(exp));
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
    private static void numbers(String func, ConsCell a) {
        if (a == null) return;
        for (; a != null; a = (ConsCell) cdr(a))
            if (!numberp(car(a))) throw new LambdaJError(func + ": expected only number arguments but got " + printSEx(a));
    }

    private static void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numbers(func, a);
    }

    private static String errorExp(Object exp) {
        if (exp == null) return "";
        return "\nerror occurred in expression " + printSEx(exp);
    }

    /** generate a comparison operator */
    private Object makeCompareOp(ConsCell args, String opName, IntPredicate pred) {
        twoArgs(opName, args);
        numbers(opName, args);
        final double lhs = (Double)car(args);
        final double rhs = (Double)car(cdr(args));
        return boolResult(pred.test(Double.compare(lhs,  rhs)));
    }

    /** generate operator for zero or more args */
    private static Object makeAddOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        numbers(opName, args);
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





    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@code symtab} on the fly */
    private ConsCell environment(SymbolTable symtab, ConsCell prev, ObjectReader lispStdin, ObjectWriter lispStdout) {
        ConsCell env = prev;
        if (HAVE_EQ) {
            final Primitive feq =       (ConsCell a) -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); };
            env = cons(cons(symtab.intern("eq"), cons(feq, null)),
                       env);
        }

        if (HAVE_ATOM) {
            final Primitive fatom =     (ConsCell a) -> { oneArg("atom", a);    return boolResult(atom   (car(a))); };
            env = cons(cons(symtab.intern("atom"),    cons(fatom, null)),
                       env);
        }

        if (HAVE_T)
            env = cons(cons(symtab.intern("t"),
                  cons(symtab.intern("t"), null)),
                  env);

        if (HAVE_NIL)
            env = cons(cons(symtab.intern("nil"),
                  cons(null, null)),
                  env);

        if (HAVE_IO) {
            final Primitive freadobj =  (ConsCell a) -> { noArgs("read", a);    return lispStdin.readObj(); };
            final Primitive fwriteobj = (ConsCell a) -> { oneArg("write", a);   lispStdout.printObj(car(a)); return expTrue(); };

            final Primitive fwriteln = (ConsCell a) -> {
                nArgs("writeln", a, 0, 1, null);
                if (a == null) {
                    lispStdout.printEol();
                    return expTrue();
                }
                lispStdout.printObj(car(a));
                lispStdout.printEol();
                return expTrue();
            };

            env = cons(cons(symtab.intern("read"),    cons(freadobj, null)),
                  cons(cons(symtab.intern("write"),   cons(fwriteobj, null)),
                  cons(cons(symtab.intern("writeln"), cons(fwriteln, null)),
                  env)));
        }

        if (HAVE_UTIL) {
            final Primitive fnull =     (ConsCell a) -> { oneArg("null?", a);   return boolResult(car(a) == null); };

            final Primitive fconsp =    (ConsCell a) -> { oneArg("consp", a);   return boolResult(consp  (car(a))); };
            final Primitive fsymbolp =  (ConsCell a) -> { oneArg("symbolp", a); return boolResult(symbolp(car(a))); };
            final Primitive flistp =    (ConsCell a) -> { oneArg("listp", a);   return boolResult(listp  (car(a))); };

            final Primitive fassoc =    (ConsCell a) -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); };
            env = cons(cons(symtab.intern("consp"),   cons(fconsp, null)),
                  cons(cons(symtab.intern("symbolp"), cons(fsymbolp, null)),
                  cons(cons(symtab.intern("listp"),   cons(flistp, null)),
                  cons(cons(symtab.intern("null?"),   cons(fnull, null)),

                  cons(cons(symtab.intern("assoc"),   cons(fassoc, null)),
                  env)))));
        }

        if (HAVE_CONS) {
            final Primitive fcons =     (ConsCell a) -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); };
            final Primitive fcar =      (ConsCell a) -> { onePair("car", a);    if (car(a) == null) return null; return car(car(a)); };
            final Primitive fcdr =      (ConsCell a) -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); };

            env = cons(cons(symtab.intern("car"),     cons(fcar, null)),
                  cons(cons(symtab.intern("cdr"),     cons(fcdr, null)),
                  cons(cons(symtab.intern("cons"),    cons(fcons, null)),
                  env)));
        }

        if (HAVE_STRING) {
            final Primitive fstringp =  (ConsCell a) -> { oneArg("stringp", a); return boolResult(stringp(car(a))); };
            final Primitive fformat =   a -> {
                nArgs("string-format", a, 2, null);
                if (!(car(a) instanceof LambdaJString))
                    throw new LambdaJError("string-format: expected first argument to be a String but got " + printSEx(cdr(a)));
                String s = ((LambdaJString)car(a)).value;
                try {
                    return String.format(s, listToArray(cdr(a)));
                }
                catch (IllegalFormatException e) {
                    throw new LambdaJError("string-format: illegal format string and/ or arguments: " + e.getMessage()
                    + "\nerror ocurred processing the argument(s) " + printSEx(a));
                }
            };

            env = cons(cons(symtab.intern("stringp"), cons(fstringp, null)),
                  cons(cons(symtab.intern("string-format"), cons(fformat, null)),
                  env));
        }

        if (HAVE_DOUBLE) {
            final Primitive fnumberp =  (ConsCell a) -> { oneArg("numberp", a); return boolResult(numberp(car(a))); };

            final Primitive fnumbereq = args -> makeCompareOp(args, "=",  compareResult -> compareResult == 0);
            final Primitive flt =       args -> makeCompareOp(args, "<",  compareResult -> compareResult <  0);
            final Primitive fle =       args -> makeCompareOp(args, "<=", compareResult -> compareResult <= 0);
            final Primitive fgt =       args -> makeCompareOp(args, ">",  compareResult -> compareResult >  0);
            final Primitive fge =       args -> makeCompareOp(args, ">=", compareResult -> compareResult >= 0);

            final Primitive fadd =  args -> makeAddOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs);
            final Primitive fmul =  args -> makeAddOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs);

            final Primitive fsub  = args -> makeSubOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs);
            final Primitive fquot = args -> makeSubOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs);

            final Primitive fmod = (ConsCell args) -> {
                twoArgs("mod", args);
                numbers("mod", args);
                return (Double)car(args) % (Double)car(cdr(args));
            };

            env = cons(cons(symtab.intern("numberp"), cons(fnumberp, null)),

                  cons(cons(symtab.intern("="),       cons(fnumbereq, null)),
                  cons(cons(symtab.intern(">"),       cons(fgt, null)),
                  cons(cons(symtab.intern(">="),      cons(fge, null)),
                  cons(cons(symtab.intern("<"),       cons(flt, null)),
                  cons(cons(symtab.intern("<="),      cons(fle, null)),

                  cons(cons(symtab.intern("+"),       cons(fadd, null)),
                  cons(cons(symtab.intern("-"),       cons(fsub, null)),
                  cons(cons(symtab.intern("*"),       cons(fmul, null)),
                  cons(cons(symtab.intern("/"),       cons(fquot, null)),
                  cons(cons(symtab.intern("mod"),     cons(fmod, null)),
                  env)))))))))));
        }

        return env;
    }



    /** <p>Build environment, read a single S-expression from {@code in}, invoke {@code eval()} and return result.
     *
     *  <p>After the expression was read from {@code in}, the primitive function {@code read} (if used)
     *  will read S-expressions from {@code in} as well,
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpression(ReadSupplier in, WriteConsumer out) {
        Parser parser = new SExpressionParser(in);
        setSymtab(parser);
        ObjectWriter outWriter = new SExpressionWriter(out);
        final ConsCell env = environment(symtab, null, parser, outWriter);
        final Object exp = parser.readObj();
        final Object result = eval(exp, env, 0, 0);
        if (trace >= TRC_EVAL) {
            tracer.println("*** max eval depth: " + maxEvalStack + " ***");
        }
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
        setSymtab(parser);
        final ConsCell env = environment(symtab, customEnv.customEnvironment(parser, inReader, outWriter), inReader, outWriter);
        Object exp = parser.readObj();
        while (true) {
            final Object result = eval(exp, env, 0, 0);
            if (trace >= TRC_EVAL) {
                tracer.println("*** max eval depth: " + maxEvalStack + " ***");
            }
            exp = parser.readObj();
            if (exp == null) return result;
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

        final LambdaJ interpreter = new LambdaJ();

        if (hasFlag("--trace", args))     interpreter.trace = TRC_LEX;

        if (hasFlag("--no-nil", args))    interpreter.HAVE_NIL = false;
        if (hasFlag("--no-t", args))      interpreter.HAVE_T = false;
        if (hasFlag("--no-extra", args))  interpreter.HAVE_XTRA = false;
        if (hasFlag("--no-double", args)) interpreter.HAVE_DOUBLE = false;
        if (hasFlag("--no-string", args)) interpreter.HAVE_STRING = false;
        if (hasFlag("--no-io", args))     interpreter.HAVE_IO = false;
        if (hasFlag("--no-util", args))   interpreter.HAVE_UTIL = false;

        if (hasFlag("--no-labels", args)) interpreter.HAVE_LABELS = false;
        if (hasFlag("--no-cons", args))   interpreter.HAVE_CONS = false;
        if (hasFlag("--no-cond", args))   interpreter.HAVE_COND = false;
        if (hasFlag("--no-apply", args))  interpreter.HAVE_APPLY = false;

        if (hasFlag("--no-atom", args))   interpreter.HAVE_ATOM = false;
        if (hasFlag("--no-eq", args))     interpreter.HAVE_EQ = false;
        if (hasFlag("--no-quote", args))  interpreter.HAVE_QUOTE = false;

        if (hasFlag("--min+", args))      interpreter.haveMinPlus();
        if (hasFlag("--min", args))       interpreter.haveMin();
        if (hasFlag("--lambda+", args))   interpreter.haveLambdaPlus();
        if (hasFlag("--lambda", args))    interpreter.haveLambda();

        final boolean printResult = hasFlag("--result", args);

        if (argError(args)) {
            System.err.println("LambdaJ: exiting because of previous errors.");
            return;
        }

        final boolean istty = null != System.console();
        if (istty) {
            System.out.println("Enter a Lisp expression:");
            System.out.print("LambdaJ> ");
            System.out.flush();
        }

        try {
            final String result = printSEx(interpreter.interpretExpression(System.in::read, System.out::print));
            if (istty) {
                System.out.println();
                System.out.println("result: " + result);
            } else {
                if (printResult) {
                    System.out.println(result);
                }
            }
        } catch (LambdaJError e) {
            System.out.println();
            System.out.println(e.toString());
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
        System.out.println("LambdaJ $Id: LambdaJ.java,v 1.59 2020/10/10 17:36:28 Robert Exp $");
    }

    // for updating the usage message edit the file usage.txt and copy/paste its contents here between double quotes
    private static void showUsage() {
        System.out.println("Usage:\n" +
                "\n" +
                "interactive:\n" +
                "java -jar lambda.jar [commandline-flags]*\n" +
                "\n" +
                "non-interactive:\n" +
                "java -jar lambda.jar [commandline-flags]* < lisp-source.lisp\n" +
                "\n" +
                "Commandline-flags are:\n" +
                "\n" +
                "Misc:\n" +
                "--version .....  show version and exit\n" +
                "--help ........  show this message and exit\n" +
                "--trace .......  print internal interpreter info during\n" +
                "                 reading/ parsing/ executing programs\n" +
                "\n" +
                "Feature flags:\n" +
                "\n" +
                "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n" +
                "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n" +
                "--no-extra ....  no special form 'if'\n" +
                "--no-double ...  no number support\n" +
                "--no-string ...  no string support\n" +
                "--no-io .......  no primitive functions read/ write/ writeln\n" +
                "--no-util .....  no primitive functions consp/ symbolp/ listp/ null?/ assoc\n" +
                "\n" +
                "--min+ ........  turn off all above features, leaving a Lisp with 10 primitives:\n" +
                "                   S-expressions\n" +
                "                   symbols and cons-cells (i.e. lists)\n" +
                "                   function application\n" +
                "                   the special form quote\n" +
                "                   atom, eq, cons, car, cdr, lambda, apply, cond, labels\n" +
                "\n" +
                "--no-apply ....  no special form 'apply'\n" +
                "--no-labels ...  no special form 'labels' (hint: use Y-combinator instead)\n" +
                "\n" +
                "--min .........  turn off all above features, leaving a Lisp with 8 primitives:\n" +
                "                   S-expressions\n" +
                "                   symbols and cons-cells (i.e. lists)\n" +
                "                   function application\n" +
                "                   the special form quote\n" +
                "                   atom, eq, cons, car, cdr, lambda, cond\n" +
                "\n" +
                "--no-cons .....  no primitive functions cons/ car/ cdr\n" +
                "--no-cond .....  no special form 'cond'\n" +
                "\n" +
                "--lambda+ .....  turn off pretty much everything except Lambda calculus,\n" +
                "                 leaving a Lisp with 4 primitives:\n" +
                "                   S-expressions\n" +
                "                   symbols and cons-cells (i.e. lists)\n" +
                "                   function application\n" +
                "                   the special form quote\n" +
                "                   atom, eq, lambda\n" +
                "\n" +
                "--no-atom .....  no primitive function 'atom'\n" +
                "--no-eq .......  no primitive function 'eq'\n" +
                "--no-quote ....  no special form quote\n" +
                "\n" +
                "--lambda ......  turns off yet even more stuff, leaving I guess bare bones Lambda calculus:\n" +
                "                   S-expressions\n" +
                "                   symbols and cons-cells (i.e. lists)\n" +
                "                   function application\n" +
                "                   lambda");
    }
}
