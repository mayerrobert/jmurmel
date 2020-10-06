package com.robertmayer.lambdaj;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;

public class LambdaJ {

    /// infrastructure
    public static final int EOF = -1;
    public static final int SYMBOL_MAX = 32;

    public static final int TRC_NONE = 0, TRC_EVAL = 1, TRC_PRIM = 2, TRC_PARSE = 3, TRC_TOK = 4, TRC_LEX = 5;
    public int trace = TRC_NONE;

    private InputStream in;
    private PrintStream out;

    public static class Error extends RuntimeException {
        public static final long serialVersionUID = 1;
        Error(String msg) {
            super(msg, null, false, false);
        }

        @Override
        public String toString() {
            return "Error: " + getMessage();
        }
    }

    @FunctionalInterface
    public static interface Builtin {
        Object apply(Pair x);
    }



    /// scanner
    private int lineNo = 1, charNo;
    private boolean escape;
    private int look;
    private int token[] = new int[SYMBOL_MAX];
    private Object tok;

    private boolean isSpace(int x)  { return !escape && (x == ' ' || x == '\t' || x == '\n' || x == '\r'); }
    private boolean isParens(int x) { return !escape && (x == '(' || x == ')'); }
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
            if (isParens(look)) {
                token[index++] = look;  look = getchar();
            } else {
                while (index < SYMBOL_MAX - 1 && look != EOF && !isSpace(look) && !isParens(look)) {
                    if (index < SYMBOL_MAX - 1) token[index++] = look;
                    look = getchar();
                }
            }
        }
        token[index] = '\0';
        if (trace >= TRC_LEX)
            System.err.println("*** token  |" + tokenToString(token) + '|');
        if (isDigit(token[0])) {
            try {
                tok = Double.valueOf(tokenToString(token));
            }
            catch (NumberFormatException e) {
                throw new Error("line " + lineNo + ':' + charNo + ": '" + tokenToString(token)
                + "' is not a valid symbol or number");
            }
        } else if (token[0] == '\0'){
            tok = null;
        } else {
            tok = tokenToString(token);
        }
    }

    private String tokenToString(int[] s) {
        StringBuffer ret = new StringBuffer(32);
        for (int c: s) {
            if (c == '\0') break;
            ret.append((char)c);
        }
        return ret.toString();
    }



    /// symbol table
    private Pair symbols = null;

    private String intern(String sym) {
        Pair pair = symbols;
        for ( ; pair != null; pair = (Pair)cdr(pair)) {
            if (sym.equalsIgnoreCase((String)car(pair))) {
                return (String) car(pair);
            }
        }
        symbols = cons(sym, symbols);
        return (String) car(symbols);
    }



    /// parser
    private Object readObj() {
        if (tok == null) {
            if (trace >= TRC_PARSE) System.err.println("*** list   ()");
            return null;
        }
        if ("(".equals(tok)) {
            Object list = readList();
            if (trace >= TRC_PARSE) System.err.println("*** list   " + printObj(list, true));
            return list;
        }
        if (tok instanceof Number) {
            if (trace >= TRC_TOK) System.err.println("*** number " + tok.toString());
            return tok;
        }
        if (trace >= TRC_TOK) System.err.println("*** symbol " + (String)tok);
        return intern((String)tok);
    }

    private Object readList() {
        readToken();
        if (tok == null) throw new Error("line " + lineNo + ':' + charNo + ": cannot read list. missing ')'?");
        if (")".equals(tok)) return null;
        Object tmp = readObj();
        if (isAtom(tmp)) return cons(tmp, readList());
        else return cons(tmp, readList());
    }



    /// eval - interpreter
    private Object eval(Object exp, Pair env, int level) {
        dbgEvalStart(exp, env, level);
        try {
            if (isAtom(exp)) {
                if (exp == null) return null;
                Pair envEntry = assoc(exp, env);
                if (envEntry != null) return car(cdr(envEntry));
                throw new Error("'" + exp + "' is undefined");

            } else if (isNumber(exp)) {
                return exp;

            } else if (isAtom(car (exp))) { /* special forms */
                if (car(exp) == intern("quote")) {
                    return car(cdr(exp));

                } else if (car(exp) == intern("if")) {
                    if (eval(car(cdr(exp)), env, level + 1) != null)
                        return eval(car(cdr(cdr(exp))), env, level + 1);
                    else
                        return eval(car(cdr(cdr(cdr(exp)))), env, level + 1);

                } else if (car(exp) == intern("lambda")) {
                    return exp;

                } else if (car(exp) == intern("labels")) { // labels bindings body -> object
                    Pair bindings = (Pair) car(cdr(exp));
                    Pair body =     (Pair) cdr(cdr(exp));
                    return evlabels(bindings, body, env, level);

                } else if (car(exp) == intern("cond")) {
                    return evcon((Pair) cdr(exp), env, level);

                } else if (car(exp) == intern("apply")) { // apply function to list
                    /* assumes one argument and that it is a list */
                    oneArg("apply", (Pair) cdr(cdr(exp)));
                    Pair args = evlis((Pair) cdr(cdr(exp)), env, level);
                    args = (Pair)car(args);
                    final Object func = eval(car(cdr(exp)), env, level + 1);
                    if (isPair(func)) return eval(cons(func, args), env, level + 1);
                    else if (isPrim(func)) return applyPrimitive((Builtin) func, args, level);
                    else throw new Error("apply: not a function: " + printObj(func, true));

                } else { /* function call */
                    Object func = eval(car(exp), env, level + 1);
                    if (isPair(func)) { /* user defined lambda, arg list eval happens in binding  below */
                        return eval(cons(func, cdr(exp)), env, level + 1);
                    } else if (isPrim(func)) { /* built-in primitive */ // todo umbauen auf isPrim und ein "else error"
                        return applyPrimitive((Builtin) func, evlis((Pair) cdr(exp), env, level), level);
                    }
                    else throw new Error("not a function: " + printObj(func, true));
                }

            } else if (car(car(exp)) == intern("lambda")) {
                /* should be a lambda, bind args as "names" into env and eval body-list */
                final Pair lambda = (Pair) cdr(car(exp));
                Pair extenv = env, params = (Pair) car(lambda), args = (Pair) cdr(exp);
                for ( ; params != null; params = (Pair) cdr(params), args = (Pair) cdr(args))
                    extenv = cons(cons(car(params),  cons(eval(car(args), env, level + 1), null)), extenv);
                Pair body = (Pair) cdr(lambda);
                Object result = null;
                for (; body != null; body = (Pair) cdr(body))
                    result = eval(car(body), extenv, level);
                return result;

            }

            throw new Error("cannot eval expression '" + printObj(exp, true) + '\'');

        } catch (Exception e) {
            throw e; // convenient breakpoint for errors
        } finally {
            dbgEvalDone(level);
        }
    }

    /*
   (evcon (c e)
     (cond ((eval (caar c) e)
             (eval (cadar c) e))
           (t
             (evcon (cdr c) e))))
    */
    private Object evcon(Pair c, Pair e, int level) {
        for ( ; c != null; c = (Pair) cdr(c)) {
            Object condResult = eval(car(car(c)), e, level + 1);
            if (condResult != null) return eval(car(cdr(car(c))), e, level + 1);
        }
        return null;
    }

    private Pair evlis(Pair list, Pair env, int level) {
        Pair head = null, insertPos = null;
        for ( ; list != null; list = (Pair) cdr(list)) {
            Pair currentArg = cons(eval(car(list), env, level + 1), null);
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

    private Object evlabels(Pair bindings, Pair body, Pair env, int level) {
        Pair extenv = env;
        for (; bindings != null; bindings = (Pair)cdr(bindings)) {
            final Pair currentFunc = (Pair)car(bindings);
            final String currentName = (String)car(currentFunc);
            final Pair currentBody = (Pair)cdr(currentFunc);
            final Pair lambda = cons(cons(intern("lambda"), currentBody), null);
            extenv = cons(cons(intern(currentName), lambda), extenv);
        }

        Object result = null;
        for (; body != null; body = (Pair) cdr(body))
            result = eval(car(body), extenv, level);
        return result;
    }

    private int maxEvalDepth;

    private void dbgEvalStart(Object exp, Pair env, int level) {
        if (trace >= TRC_EVAL) {
            if (maxEvalDepth < level) maxEvalDepth = level;
            char[] cpfx = new char[level*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            System.err.println(pfx + "*** eval (" + level + ") ********");
            System.err.print(pfx + "env: "); System.err.println(printObj(env, true));
            System.err.print(pfx + "exp: "); System.err.println(printObj(exp, true));
        }
    }
    private void dbgEvalDone(int level) {
        if (trace >= TRC_EVAL) {
            char[] cpfx = new char[level*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            System.err.println(pfx + "*** eval (" + level + ") done ***");
        }
    }



    /// data type used by interpreter program as well as interpreted programs
    private static class Pair {
        Object car, cdr;
        Pair(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private boolean isAtom(Object x)           { return x == null || x instanceof String; }
    private boolean isPrim(Object x)           { return x instanceof Builtin; }
    private boolean isPair(Object x)           { return x instanceof Pair; }
    private boolean isNumber(Object x)         { return x instanceof Number; }
    private Object car(Object x)               { return ((Pair)x).car; }
    private Object cdr(Object x)               { return ((Pair)x).cdr; }
    private Pair cons(Object car, Object cdr)  { return new Pair(car, cdr); }

    private Pair assoc(Object atom, Object maybePair) {
        if (atom == null) return null;
        if (maybePair == null) return null;
        if (!isPair(maybePair)) throw new Error("assoc: expected second argument to be a Pair but got " + printObj(maybePair, true));
        Pair env = (Pair) maybePair;
        for ( ; env != null; env = (Pair)cdr(env))
            if (atom == car(car(env))) return (Pair) car(env);
        return null;
    }

    private Object applyPrimitive(Builtin primfn, Pair args, int level) {
        if (trace >= TRC_PRIM) {
            char[] cpfx = new char[level*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            System.err.println(pfx + "(<primitive> " + printObj(args, true) + ')');
        }
        return primfn.apply(args);
    }

    private String printObj(Object ob, boolean head_of_list) {
        if (ob == null) {
            return "nil";
        } else if (isPair(ob)) {
            if (car(ob) == intern("quote") && car(cdr(ob)) == intern("t")) return "t";
            final StringBuffer sb = new StringBuffer(200);
            if (head_of_list) sb.append('(');
            sb.append(printObj(car(ob), true));
            if (cdr(ob) != null) {
                if (isPair(cdr(ob))) sb.append(' ').append(printObj(cdr(ob), false));
                else sb.append(" . ").append(printObj(cdr(ob), false)).append(')');
            } else sb.append(')');
            return sb.toString();
        } else if (isAtom(ob)) {
            return ob.toString();
        } else if (isPrim(ob)) {
            return "#<primitive>";
        } else if (isNumber(ob)) {
            return ob.toString();
        } else {
            return "<internal error>";
        }
    }



    /// runtime for Lisp programs
    private final Pair expTrue = cons(intern("quote"), cons(intern("t"), null));

    private void noArgs(String func, Pair a) {
        if (a != null) throw new Error(func + ": expected no arguments but got " + printObj(a, true));
    }

    private void oneArg(String func, Pair a) {
        if (a == null) throw new Error(func + ": expected one argument but no argument was given");
    }

    /** two args or more */
    private void twoArgs(String func, Pair a) {
        if (a == null) throw new Error(func + ": expected two arguments but no argument was given");
        if (cdr(a) == null) throw new Error(func + ": expected two arguments but only one argument was given");
    }

    private void onePair(String func, Pair a) {
        if (a == null) throw new Error(func + ": expected one Pair argument but no argument was given");
        if (!isPair(car(a))) throw new Error(func + ": expected one Pair argument but got " + printObj(a, true));
    }

    private void optNumbers(String func, Pair a) {
        if (a == null) return;
        for (; a != null; a = (Pair) cdr(a))
            if (!isNumber(car(a))) throw new Error(func + ": expected only number arguments but got " + printObj(a, true));
    }

    private Builtin fcar =      (Pair a) -> { onePair("car", a);    if (car(a) == null) return null; return car(car(a)); };
    private Builtin fcdr =      (Pair a) -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); };
    private Builtin fcons =     (Pair a) -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); };
    private Builtin fassoc =    (Pair a) -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); };
    private Builtin feq =       (Pair a) -> { twoArgs("eq", a);     return car(a) == car(cdr(a)) ? expTrue : null; };
    private Builtin fpair =     (Pair a) -> { oneArg("pair?", a);   return isPair(car(a))        ? expTrue : null; };
    private Builtin fatom =     (Pair a) -> { oneArg("symbol?", a); return isAtom(car(a))        ? expTrue : null; };
    private Builtin fnull =     (Pair a) -> { oneArg("null?", a);   return car(a) == null        ? expTrue : null; };
    private Builtin freadobj =  (Pair a) -> { noArgs("read", a);    look = getchar(); readToken(); return readObj(); };
    private Builtin fwriteobj = (Pair a) -> { oneArg("write", a);   out.print(printObj(car(a), true)); return expTrue; };

    private Builtin fwriteln = (Pair a) -> {
        if (a == null) {
            out.println();
            return expTrue;
        }
        out.println(printObj(car(a), true));
        return expTrue;
    };

    private Builtin fnumbereq = (Pair args) -> {
        twoArgs("=", args);
        optNumbers("=", args);
        return ((Double)car(args)).equals(car(cdr(args))) ? expTrue : null;
    };

    private Builtin flt = (Pair args) -> {
        twoArgs("<", args);
        optNumbers("<", args);
        return ((Double)car(args)) < (double)car(cdr(args)) ? expTrue : null;
    };

    private Builtin fle = (Pair args) -> {
        twoArgs("<=", args);
        optNumbers("<=", args);
        return ((Double)car(args)) <= (double)car(cdr(args)) ? expTrue : null;
    };

    private Builtin fgt = (Pair args) -> {
        twoArgs(">", args);
        optNumbers(">", args);
        return ((Double)car(args)) > (double)car(cdr(args)) ? expTrue : null;
    };

    private Builtin fge = (Pair args) -> {
        twoArgs(">=", args);
        optNumbers(">=", args);
        return ((Double)car(args)) >= (double)car(cdr(args)) ? expTrue : null;
    };

    private Builtin fadd = (Pair args) -> {
        optNumbers("+", args);
        Double result = 0.0;
        for (; args != null; args = (Pair) cdr(args))
            result += (Double)car(args);
        return result;
    };

    private Builtin fsub = (Pair args) -> {
        oneArg("-", args);
        optNumbers("-", args);
        Double result = (Double)car(args);
        if (cdr(args) == null) return -result;
        for (args = (Pair) cdr(args); args != null; args = (Pair) cdr(args))
            result -= (Double)car(args);
        return result;
    };

    private Builtin fmul = (Pair args) -> {
        optNumbers("*", args);
        Double result = 1.0;
        for (; args != null; args = (Pair) cdr(args))
            result *= (Double)car(args);
        return result;
    };

    private Builtin fquot = (Pair args) -> {
        oneArg("-", args);
        optNumbers("/", args);
        Double result = (Double)car(args);
        if (cdr(args) == null) return 1 / result;
        for (args = (Pair) cdr(args); args != null; args = (Pair) cdr(args))
            result /= (Double)car(args);
        return result;
    };

    private Builtin fmod = (Pair args) -> {
        twoArgs("-", args);
        optNumbers("/", args);
        return (Double)car(args) % (Double)car(cdr(args));
    };

    private Pair environment() {
        return cons(cons(intern("car"),     cons(fcar, null)),
               cons(cons(intern("cdr"),     cons(fcdr, null)),
               cons(cons(intern("cons"),    cons(fcons, null)),
               cons(cons(intern("assoc"),   cons(fassoc, null)),
               cons(cons(intern("eq"),      cons(feq, null)),
               cons(cons(intern("pair?"),   cons(fpair, null)),
               cons(cons(intern("symbol?"), cons(fatom, null)),
               cons(cons(intern("null?"),   cons(fnull, null)),
               cons(cons(intern("read"),    cons(freadobj, null)),
               cons(cons(intern("write"),   cons(fwriteobj, null)),
               cons(cons(intern("writeln"), cons(fwriteln, null)),

               cons(cons(intern("="),       cons(fnumbereq, null)),
               cons(cons(intern(">"),       cons(fgt, null)),
               cons(cons(intern(">="),      cons(fge, null)),
               cons(cons(intern("<"),       cons(flt, null)),
               cons(cons(intern("<="),      cons(fle, null)),

               cons(cons(intern("+"),       cons(fadd, null)),
               cons(cons(intern("-"),       cons(fsub, null)),
               cons(cons(intern("*"),       cons(fmul, null)),
               cons(cons(intern("/"),       cons(fquot, null)),
               cons(cons(intern("mod"),     cons(fmod, null)),

               cons(cons(intern("nil"),     cons((String)null, null)),
               null))))))))))))))))))))));
    }



    /// build environment, read an S-expression and invoke eval()
    public String interpretExpression(InputStream in, PrintStream out) {
        this.in = in;
        this.out = out;
        final Pair env = environment();
        look = getchar();
        readToken();
        final Object exp = readObj();
        if (exp == null) return null;
        final String result = printObj(eval(exp, env, 0), true);
        if (trace >= TRC_EVAL) {
            System.err.println("*** max eval depth: " + maxEvalDepth + " ***");
        }
        return result;
    }

    /// build environment, read S-expression and invoke eval() until EOF
    public String interpretExpressions(InputStream in, PrintStream out) {
        this.in = in;
        this.out = out;
        final Pair env = environment();
        look = getchar();
        readToken();
        Object exp = readObj();
        if (exp == null) return null;
        while (true) {
            final String result = printObj(eval(exp, env, 0), true);
            if (trace >= TRC_EVAL) {
                System.err.println("*** max eval depth: " + maxEvalDepth + " ***");
            }
            look = getchar();
            readToken();
            exp = readObj();
            if (exp == null) return result;
        }
    }

    public static void main(String argv[]) {
        final LambdaJ interpreter = new LambdaJ();
        interpreter.trace = TRC_LEX;
        final boolean istty = null != System.console();
        if (istty) {
            System.out.println("Enter a Lisp expression:");
            System.out.print("LambdaJ> ");
            System.out.flush();
        }
        try {
            final String result = interpreter.interpretExpression(System.in, System.out);
            if (istty) {
                System.out.println();
                System.out.println("result: " + result);
            }
        } catch (Error e) {
            System.out.println();
            System.out.println(e.toString());
        }
    }
}
