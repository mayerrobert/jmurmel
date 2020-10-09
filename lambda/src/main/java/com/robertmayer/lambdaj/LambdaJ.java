/* Copyright (C) 2020 by Robert Mayer */
package com.robertmayer.lambdaj;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.IllegalFormatException;
import java.util.List;
import java.util.function.DoubleBinaryOperator;
import java.util.function.IntPredicate;

public class LambdaJ {

    /// infrastructure
    public static final int EOF = -1;
    public static final int TOKEN_MAX = 2000; // max length of symbols and literals

    public static final int TRC_NONE = 0, TRC_EVAL = 1, TRC_PRIM = 2, TRC_PARSE = 3, TRC_TOK = 4, TRC_LEX = 5;
    public int trace = TRC_NONE;

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

    private PrintStream out;

    public static class LambdaJError extends RuntimeException {
        public static final long serialVersionUID = 1;
        LambdaJError(String msg) {
            super(msg, null, false, false);
        }

        @Override
        public String toString() {
            return "Error: " + getMessage();
        }
    }

    @FunctionalInterface
    public interface Builtin {
        Object apply(ConsCell x);
    }



    public class LambdaJString {
        private final String value;
        public LambdaJString(String value) { this.value = value; }
        @Override
        public String toString() { return value.toString(); }
    }

    public interface Parser {
        String intern(String symbol);
        Object readObj();
    }

    public class LispParser implements Parser {
        /// scanner
        private InputStream in;
        private boolean init;

        private int lineNo = 1, charNo;
        private boolean escape;
        private int look;
        private int token[] = new int[TOKEN_MAX + 1]; // provide for trailing '\0'
        private Object tok;

        public LispParser(InputStream in) { this.in = in; }

        private boolean isSpace(int x)  { return !escape && (x == ' ' || x == '\t' || x == '\n' || x == '\r'); }
        private boolean isSyntax(int x) { return !escape && (x == '(' || x == ')' || x == '\''); }
        private boolean isDQuote(int x) { return !escape && x == '"'; }
        private boolean isDigit(int x)  { return !escape && (x >= '0' && x <= '9'); }

        private int getchar() {
            try {
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
                System.err.println("*** token  |" + String.valueOf(tok) + '|');
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

        public Object _readObj() {
            if (tok == null) {
                if (trace >= TRC_PARSE) System.err.println("*** list   ()");
                return null;
            }
            if ("(".equals(tok)) {
                Object list = readList();
                if (trace >= TRC_PARSE) System.err.println("*** list   " + printObj(list, true));
                return list;
            }
            if (symbolp(tok)) {
                if (trace >= TRC_TOK) System.err.println("*** symbol " + (String)tok);
                return intern((String)tok);
            }
            if (HAVE_QUOTE && "'".equals(tok)) {
                readToken();
                return cons(quote, cons(_readObj(), null));
            }
            if (trace >= TRC_TOK) System.err.println("*** value  " + tok.toString());
            return tok;
        }

        private Object readList() {
            readToken();
            if (tok == null) throw new LambdaJError("line " + lineNo + ':' + charNo + ": cannot read list. missing ')'?");
            if (")".equals(tok)) return null;
            Object tmp = _readObj();
            if (symbolp(tmp)) return cons(tmp, readList());
            else return cons(tmp, readList());
        }
    }

    private Parser program;
    private Parser inputData;



    /// eval - interpreter
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
                    if (HAVE_QUOTE && car(exp) == program.intern("quote")) {
                        oneArg("quote", cdr(exp));
                        return car(cdr(exp));

                    } else if (HAVE_XTRA && car(exp) == program.intern("if")) {
                        nArgs("if", cdr(exp), 2, 3, exp);
                        if (eval(car(cdr(exp)), env, stack + 1, level + 1) != null) {
                            exp = car(cdr(cdr(exp))); continue;
                        } else if (cdr(cdr(cdr(exp))) != null) {
                            exp = car(cdr(cdr(cdr(exp)))); continue;
                        } else
                            return null;

                    } else if (car(exp) == program.intern("lambda")) {
                        nArgs("lambda", cdr(exp), 2, exp);
                        return exp;

                    } else if (HAVE_LABELS && car(exp) == program.intern("labels")) { // labels bindings body -> object
                        nArgs("labels", cdr(exp), 2, exp);
                        ConsCell bindings = (ConsCell) car(cdr(exp));
                        ConsCell body =     (ConsCell) cdr(cdr(exp));
                        return evlabels(bindings, body, env, stack, level);

                    } else if (HAVE_COND && car(exp) == program.intern("cond")) {
                        return evcon((ConsCell) cdr(exp), env, stack, level);

                    } else if (HAVE_APPLY && car(exp) == program.intern("apply")) { // apply function to list
                        twoArgs("apply", cdr(exp), exp);
                        final Object func = eval(car(cdr(exp)), env, stack + 1, level + 1);
                        final ConsCell args = (ConsCell)car(evlis((ConsCell) cdr(cdr(exp)), env, stack, level));
                        if (consp(func)) {
                            exp = cons(func, args); continue;
                        } else if (isPrim(func)) return applyPrimitive((Builtin) func, args, stack);
                        else throw new LambdaJError("apply: not a function: " + printObj(func, true)
                        + ". this was the result of evaluating the expression "
                        + printObj(car(cdr(exp)), true) + errorExp(exp));

                    } else { /* function call */
                        Object func = eval(car(exp), env, stack + 1, level + 1);
                        if (consp(func)) { /* user defined lambda, arg list eval happens in binding  below */
                            exp = cons(func, cdr(exp)); continue;
                        } else if (isPrim(func)) {
                            return applyPrimitive((Builtin) func, evlis((ConsCell) cdr(exp), env, stack, level + 1), stack);
                        }
                        else throw new LambdaJError("not a function: " + printObj(func, true) + errorExp(exp));
                    }

                } else if (consp(car(exp)) && car(car(exp)) == program.intern("lambda")) {
                    /* should be a lambda, bind args as "names" into env and eval body-list */
                    final Object lambda = cdr(car(exp));
                    nArgs("lambda", lambda, 2, exp);

                    ConsCell extenv = env, params = (ConsCell) car(lambda), args = (ConsCell) cdr(exp);
                    for ( ; params != null && args != null; params = (ConsCell) cdr(params), args = (ConsCell) cdr(args))
                        extenv = cons(cons(car(params),  cons(eval(car(args), env, stack + 1, level + 1), null)), extenv);
                    if (params != null)
                        throw new LambdaJError("lambda: not enough arguments. parameters w/o argument: " + printObj(params, true)
                        + errorExp(exp));
                    if (args != null)
                        throw new LambdaJError("lambda: too many arguments. remaining arguments: " + printObj(args, true)
                        + errorExp(exp));

                    ConsCell body = (ConsCell) cdr(lambda);
                    for (; body != null && cdr(body) != null; body = (ConsCell) cdr(body))
                        eval(car(body), extenv, stack + 1, level + 1);
                    if (body != null) {
                        exp = car(body); env = extenv; continue;
                    } // else fall through to "cannot eval". should really not happen anyway

                } else if (atom(car(exp))) {
                    throw new LambdaJError("not a function: " + printObj(car(exp), true) + errorExp(exp));

                }

                throw new LambdaJError("cannot eval expression '" + printObj(exp, true) + '\'');
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
            final ConsCell lambda = cons(cons(program.intern("lambda"), currentBody), null);
            extenv = cons(cons(program.intern(currentName), lambda), extenv);
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
            System.err.println(pfx + "*** eval (" + stack + '/' + level + ") ********");
            System.err.print(pfx + "env: "); System.err.println(printObj(env, true));
            System.err.print(pfx + "exp: "); System.err.println(printObj(exp, true));
        }
    }
    private void dbgEvalDone(int stack, int level) {
        if (trace >= TRC_EVAL) {
            char[] cpfx = new char[stack*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            System.err.println(pfx + "*** eval (" + stack + '/' + level + ") done ***");
        }
    }



    /// data type used by interpreter program as well as interpreted programs
    public static class ConsCell {
        public Object car, cdr;
        public ConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
        @Override
        public String toString() { return printObj(this, true); }
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private static ConsCell cons(Object car, Object cdr) { return new ConsCell(car, cdr); }
    private static Object   car(Object x)                { return ((ConsCell)x).car; }
    private static Object   cdr(Object x)                { return ((ConsCell)x).cdr; }

    private static boolean  consp(Object x)             { return x instanceof ConsCell; }
    private static boolean  atom(Object x)              { return x == null || !(x instanceof ConsCell); } // !isCons(x)
    private static boolean  symbolp(Object x)           { return x == null || x instanceof String; } // null (alias nil) is a symbol too
    private static boolean  listp(Object x)             { return x == null || x instanceof ConsCell; } // null is a list too
    private static boolean  isPrim(Object x)            { return x instanceof Builtin; }

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
        if (!listp(maybeList)) throw new LambdaJError("assoc: expected second argument to be a List but got " + printObj(maybeList, true));
        ConsCell env = (ConsCell) maybeList;
        for ( ; env != null; env = (ConsCell)cdr(env)) {
            if (atom == car(car(env))) return (ConsCell) car(env);
            if (maybeList == cdr(env)) return null; // circular list, wne didn't find the symbol
        }
        return null;
    }

    private static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("listToArray: expected second argument to be a List but got " + printObj(maybeList, true));
        ConsCell env = (ConsCell) maybeList;
        List<Object> ret = new ArrayList<>();
        for ( ; env != null; env = (ConsCell)cdr(env)) {
            ret.add(car(env));
            if (maybeList == cdr(env)) return null;
        }
        return ret.toArray();
    }

    private Object applyPrimitive(Builtin primfn, ConsCell args, int stack) {
        if (trace >= TRC_PRIM) {
            char[] cpfx = new char[stack*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            System.err.println(pfx + "(<primitive> " + printObj(args, true) + ')');
        }
        return primfn.apply(args);
    }

    private static String printObj(Object ob, boolean head_of_list) {
        if (ob == null) return "nil";
        final StringBuffer sb = new StringBuffer(200);
        _printObj(sb, ob, ob, head_of_list);
        return sb.toString();
    }

    private static void _printObj(StringBuffer sb, Object list, Object current, boolean head_of_list) {
        while (true) {
            if (current == null) {
                sb.append("nil"); return;
            } else if (listp(current)) {
                if (head_of_list) sb.append('(');
                if (car(current) == list) {
                    sb.append(head_of_list ? "#<this cons>" : "#<this list>");
                } else {
                    _printObj(sb, car(current), car(current), true);
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
                        _printObj(sb, list, cdr(current), false);
                        sb.append(')');
                        return;
                    } else {
                        sb.append(' ');
                        _printObj(sb, list, cdr(current), false); // must be an atom
                        sb.append(')');
                        return;
                    }
                } else {
                    sb.append(')');
                    return;
                }
            } else if (symbolp(current)) {
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
            if (HAVE_T) _expTrue = program.intern("t");
            else if (HAVE_QUOTE) _expTrue = cons(program.intern("quote"), cons(program.intern("t"), null));
            else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
        }
        return _expTrue;
    }

    private Object boolResult(boolean b) { return b ? expTrue() : null; }

    private static void noArgs(String func, ConsCell a) {
        if (a != null) throw new LambdaJError(func + ": expected no arguments but got " + printObj(a, true));
    }

    private static void oneArg(String func, Object a) {
        if (a == null) throw new LambdaJError(func + ": expected one argument but no argument was given");
        if (cdr(a) != null) throw new LambdaJError(func + ": expected one argument but got extra arg(s) " + printObj(cdr(a), true));
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
        if (cdr(cdr(a)) != null) throw new LambdaJError(func + ": expected two arguments but got extra arg(s) " + printObj(cdr(cdr(a)), true) + errorExp(exp));
    }

    /** at least {@code min} args */
    private static void nArgs(String func, Object a, int min, Object exp) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(func + ": expected " + min + " arguments or more but got only " + actualLength + errorExp(exp));
    }

    private static void nArgs(String func, Object a, int min, int max, Object exp) {
        int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(func + ": expected " + min + " to " + max + " arguments but got only " + actualLength + errorExp(exp));
        if (actualLength > max) throw new LambdaJError(func + ": expected " + min + " to " + max + " arguments but got extra arg(s) " + printObj(nthcdr(max, a), true) + errorExp(exp));
    }

    private static void onePair(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected one Pair argument but no argument was given");
        if (!listp(car(a))) throw new LambdaJError(func + ": expected one Pair argument but got " + printObj(a, true));
        if (cdr(a) != null) throw new LambdaJError(func + ": expected one Pair argument but got extra arg(s) " + printObj(cdr(a), true));
    }

    /** arguments if any must be only numbers */
    private static void numbers(String func, ConsCell a) {
        if (a == null) return;
        for (; a != null; a = (ConsCell) cdr(a))
            if (!numberp(car(a))) throw new LambdaJError(func + ": expected only number arguments but got " + printObj(a, true));
    }

    private static void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numbers(func, a);
    }

    private static String errorExp(Object exp) {
        if (exp == null) return "";
        return "\nerror occurred in expression " + printObj(exp, true);
    }

    /** generate a comparison operator */
    private Object compareOp(ConsCell args, String opName, IntPredicate pred) {
        twoArgs(opName, args);
        numbers(opName, args);
        final double lhs = (Double)car(args);
        final double rhs = (Double)car(cdr(args));
        return boolResult(pred.test(Double.compare(lhs,  rhs)));
    }

    /** generate operator for zero or more args */
    private static Object addOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        numbers(opName, args);
        for (; args != null; args = (ConsCell) cdr(args))
            startVal = op.applyAsDouble(startVal, (Double)car(args));
        return startVal;
    }

    /** generate operator for one or more args */
    private static Object subOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        oneOrMoreNumbers("-", args);
        double result = (Double)car(args);
        if (cdr(args) == null) return op.applyAsDouble(startVal, result);
        for (args = (ConsCell) cdr(args); args != null; args = (ConsCell) cdr(args))
            result = op.applyAsDouble(result, (Double)car(args));
        return result;
    }



    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link Parser} {@code program} on the fly */
    private ConsCell environment(Parser program, ConsCell prev) {

        ConsCell env = prev;
        if (HAVE_EQ) {
            final Builtin feq =       (ConsCell a) -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); };
            env = cons(cons(program.intern("eq"), cons(feq, null)),
                       env);
        }

        if (HAVE_ATOM) {
            final Builtin fatom =     (ConsCell a) -> { oneArg("atom", a);    return boolResult(atom   (car(a))); };
            env = cons(cons(program.intern("atom"),    cons(fatom, null)),
                       env);
        }

        if (HAVE_T)
            env = cons(cons(program.intern("t"),
                  cons(program.intern("t"), null)),
                  env);

        if (HAVE_NIL)
            env = cons(cons(program.intern("nil"),
                  cons(null, null)),
                  env);

        if (HAVE_IO) {
            final Builtin freadobj =  (ConsCell a) -> { noArgs("read", a);    return inputData.readObj(); };
            final Builtin fwriteobj = (ConsCell a) -> { oneArg("write", a);   out.print(printObj(car(a), true)); return expTrue(); };

            final Builtin fwriteln = (ConsCell a) -> {
                nArgs("writeln", a, 0, 1, null);
                if (a == null) {
                    out.println();
                    return expTrue();
                }
                out.println(printObj(car(a), true));
                return expTrue();
            };

            env = cons(cons(program.intern("read"),    cons(freadobj, null)),
                  cons(cons(program.intern("write"),   cons(fwriteobj, null)),
                  cons(cons(program.intern("writeln"), cons(fwriteln, null)),
                  env)));
        }

        if (HAVE_UTIL) {
            final Builtin fnull =     (ConsCell a) -> { oneArg("null?", a);   return boolResult(car(a) == null); };

            final Builtin fconsp =    (ConsCell a) -> { oneArg("consp", a);   return boolResult(consp  (car(a))); };
            final Builtin fsymbolp =  (ConsCell a) -> { oneArg("symbolp", a); return boolResult(symbolp(car(a))); };
            final Builtin flistp =    (ConsCell a) -> { oneArg("listp", a);   return boolResult(listp  (car(a))); };

            final Builtin fassoc =    (ConsCell a) -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); };
            env = cons(cons(program.intern("consp"),   cons(fconsp, null)),
                  cons(cons(program.intern("symbolp"), cons(fsymbolp, null)),
                  cons(cons(program.intern("listp"),   cons(flistp, null)),
                  cons(cons(program.intern("null?"),   cons(fnull, null)),

                  cons(cons(program.intern("assoc"),   cons(fassoc, null)),
                  env)))));
        }

        if (HAVE_CONS) {
            final Builtin fcons =     (ConsCell a) -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); };
            final Builtin fcar =      (ConsCell a) -> { onePair("car", a);    if (car(a) == null) return null; return car(car(a)); };
            final Builtin fcdr =      (ConsCell a) -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); };

            env = cons(cons(program.intern("car"),     cons(fcar, null)),
                  cons(cons(program.intern("cdr"),     cons(fcdr, null)),
                  cons(cons(program.intern("cons"),    cons(fcons, null)),
                  env)));
        }

        if (HAVE_STRING) {
            final Builtin fstringp =  (ConsCell a) -> { oneArg("stringp", a); return boolResult(stringp(car(a))); };
            final Builtin fformat =   a -> {
                nArgs("string-format", a, 2, null);
                if (!(car(a) instanceof LambdaJString))
                    throw new LambdaJError("string-format: expected first argument to be a String but got " + printObj(cdr(a), true));
                String s = ((LambdaJString)car(a)).value;
                try {
                    return String.format(s, listToArray(cdr(a)));
                }
                catch (IllegalFormatException e) {
                    throw new LambdaJError("string-format: illegal format string and/ or arguments: " + e.getMessage()
                    + "\nerror ocurred processing the argument(s) " + printObj(a, true));
                }
            };

            env = cons(cons(program.intern("stringp"), cons(fstringp, null)),
                  cons(cons(program.intern("string-format"), cons(fformat, null)),
                  env));
        }

        if (HAVE_DOUBLE) {
            final Builtin fnumberp =  (ConsCell a) -> { oneArg("numberp", a); return boolResult(numberp(car(a))); };

            final Builtin fnumbereq = args -> compareOp(args, "=",  compareResult -> compareResult == 0);
            final Builtin flt =       args -> compareOp(args, "<",  compareResult -> compareResult <  0);
            final Builtin fle =       args -> compareOp(args, "<=", compareResult -> compareResult <= 0);
            final Builtin fgt =       args -> compareOp(args, ">",  compareResult -> compareResult >  0);
            final Builtin fge =       args -> compareOp(args, ">=", compareResult -> compareResult >= 0);

            final Builtin fadd =  args -> addOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs);
            final Builtin fmul =  args -> addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs);

            final Builtin fsub  = args -> subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs);
            final Builtin fquot = args -> subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs);

            final Builtin fmod = (ConsCell args) -> {
                twoArgs("mod", args);
                numbers("mod", args);
                return (Double)car(args) % (Double)car(cdr(args));
            };

            env = cons(cons(program.intern("numberp"), cons(fnumberp, null)),

                  cons(cons(program.intern("="),       cons(fnumbereq, null)),
                  cons(cons(program.intern(">"),       cons(fgt, null)),
                  cons(cons(program.intern(">="),      cons(fge, null)),
                  cons(cons(program.intern("<"),       cons(flt, null)),
                  cons(cons(program.intern("<="),      cons(fle, null)),

                  cons(cons(program.intern("+"),       cons(fadd, null)),
                  cons(cons(program.intern("-"),       cons(fsub, null)),
                  cons(cons(program.intern("*"),       cons(fmul, null)),
                  cons(cons(program.intern("/"),       cons(fquot, null)),
                  cons(cons(program.intern("mod"),     cons(fmod, null)),
                  env)))))))))));
        }

        return env;
    }



    /// build environment, read an S-expression and invoke eval()
    public Object interpretExpression(InputStream in, PrintStream out) {
        program = new LispParser(in);
        inputData = program;
        this.out = out;
        final ConsCell env = environment(program, null);
        final Object exp = program.readObj();
        final Object result = eval(exp, env, 0, 0);
        if (trace >= TRC_EVAL) {
            System.err.println("*** max eval depth: " + maxEvalStack + " ***");
        }
        return result;
    }

    /// build environment, read S-expression and invoke eval() until EOF
    public Object interpretExpressions(InputStream in, PrintStream out) {
        program = new LispParser(in);
        inputData = program;
        this.out = out;
        final ConsCell env = environment(program, null);
        Object exp = program.readObj();
        while (true) {
            final Object result = eval(exp, env, 0, 0);
            if (trace >= TRC_EVAL) {
                System.err.println("*** max eval depth: " + maxEvalStack + " ***");
            }
            exp = program.readObj();
            if (exp == null) return result;
        }
    }

    public static void main(String args[]) {
        final LambdaJ interpreter = new LambdaJ();

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
            final String result = printObj(interpreter.interpretExpression(System.in, System.out), true);
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

    private static void showVersion() {
        System.out.println("LambdaJ $Id: LambdaJ.java,v 1.48 2020/10/09 10:23:55 Robert Exp $");
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
}
