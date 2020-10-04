package com.robertmayer.lambdaj;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;

public class LambdaJ {

    /// infrastructure
    public static final int EOF = -1;
    public static final int SYMBOL_MAX = 32;

    public static final int TRC_NONE = 0, TRC_EVAL = 1, TRC_PRIM = 2, TRC_PARSE = 3, TRC_LEX = 4;
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
        Pair apply(Pair x);
    }



    /// scanner
    private boolean escape;
    private int look;
    private int token[] = new int[SYMBOL_MAX];

    private boolean isSpace(int x)  { return !escape && (x == ' ' || x == '\t' || x == '\n' || x == '\r'); }
    private boolean isParens(int x) { return !escape && (x == '(' || x == ')'); }

    private int getchar() {
        try {
            escape = false;
            int c = in.read();
            if (c == '\\') {
                escape = true;
                return in.read();
            }
            if (c == ';') {
                while ((c = in.read()) != '\n' && c != EOF);
            }
            return c;
        } catch (Exception e) {
            throw new RuntimeException("I/O error reading");
        }
    }

    private void readToken() {
        int index = 0;
        while (isSpace(look)) { look = getchar(); }
        if (isParens(look)) {
            token[index++] = look;  look = getchar();
        } else {
            while (index < SYMBOL_MAX - 1 && look != EOF && !isSpace(look) && !isParens(look)) {
                if (index < SYMBOL_MAX - 1) token[index++] = look;
                look = getchar();
            }
        }
        if (index == 0) throw new Error("cannot read list. missing ')'?");
        token[index] = '\0';
        if (trace >= TRC_LEX)
            System.err.println("*** token |" + tokenToString(token) + '|');
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

    private String intern(int[] sym) {
        return intern(tokenToString(sym));
    }

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
        if (token[0] == '(') {
            Object list = readList();
            if (trace >= TRC_PARSE)
                System.err.println("*** list  " + printObj(list, true));
            return list;
        }
        return intern(token);
    }

    private Object readList() {
        readToken();
        if (token[0] == ')') return null;
        Object tmp = readObj();
        if (isAtom(tmp)) return cons((String)tmp, (Pair) readList());
        else return cons((Pair)tmp, (Pair) readList());
    }



    /// eval - interpreter
    private Object eval(Object exp, Pair env, int level) {
        dbgEvalStart(exp, env, level);
        try {
            if (isAtom(exp)) {
                if (exp == null) return null;
                Pair envEntry = assoc(exp, env);
                if (envEntry != null) return car((Pair)cdr(envEntry));
                throw new Error("'" + exp + "' is undefined");

            } else if (isAtom(car ((Pair) exp))) { /* special forms */
                if (car((Pair) exp) == intern("quote")) {
                    return car((Pair)cdr((Pair) exp));

                } else if (car((Pair) exp) == intern("if")) {
                    if (eval(car((Pair)cdr((Pair) exp)), env, level + 1) != null)
                        return eval(car((Pair)cdr((Pair)cdr((Pair) exp))), env, level + 1);
                    else
                        return eval(car((Pair)cdr((Pair)cdr((Pair)cdr((Pair) exp)))), env, level + 1);

                } else if (car((Pair) exp) == intern("lambda")) {
                    return exp;

                } else if (car((Pair) exp) == intern("labels")) { // labels bindings body -> object
                    Pair bindings = (Pair) car((Pair) cdr((Pair) exp));
                    Pair body =     (Pair) cdr((Pair) cdr((Pair) exp));
                    return evlabels(bindings, body, env, level);

                } else if (car((Pair) exp) == intern("cond")) {
                    return evcon((Pair) cdr((Pair) exp), env, level);

                } else if (car((Pair) exp) == intern("apply")) { // apply function to list
                    Pair args = evlis((Pair) cdr((Pair) cdr((Pair) exp)), env, level);
                    args = (Pair)car(args); /* assumes one argument and that it is a list */
                    return applyPrimitive((Builtin) eval(car((Pair)cdr((Pair) exp)), env, level + 1), args, level);

                } else { /* function call */
                    Object primop = eval(car((Pair) exp), env, level + 1);
                    if (isPair(primop)) { /* user defined lambda, arg list eval happens in binding  below */
                        return eval(cons(primop, cdr((Pair) exp)), env, level + 1);
                    } else if (primop != null) { /* built-in primitive */ // todo umbauen auf isPrim und ein "else error"
                        return applyPrimitive((Builtin) primop, evlis((Pair) cdr((Pair) exp), env, level), level);
                    }
                }

            } else if (car((Pair) car((Pair) exp)) == intern("lambda")) {
                /* should be a lambda, bind args as "names" into env and eval body-list */
                final Pair lambda = (Pair) cdr((Pair) car((Pair) exp));
                Pair extenv = env, params = (Pair) car(lambda), args = (Pair) cdr((Pair) exp);
                for ( ; params != null; params = (Pair) cdr(params), args = (Pair) cdr(args))
                    extenv = cons(cons((String) car(params),  cons(eval(car(args), env, level + 1), null)), extenv);
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
            Object condResult = eval(car((Pair) car(c)), e, level + 1);
            if (condResult != null) return eval(car((Pair) cdr((Pair) car(c))), e, level + 1);
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

        Pair(String str, Object cdr)    { this.car = str; this.cdr = cdr; }
        Pair(Builtin prim, Object cdr)  { this.car = prim; }
        Pair(Pair car, Object cdr)      { this.car = car; this.cdr = cdr; }
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private boolean isAtom(Object x)           { return x == null || x instanceof String; }
    private boolean isPrim(Object x)           { return x instanceof Builtin; }
    private boolean isPair(Object x)           { return x instanceof Pair; }
    private Object car(Pair x)                 { return x.car; }
    private Object cdr(Pair x)                 { return x.cdr; }
    private Pair cons(String car, Object cdr)  { return new Pair(car, cdr); }
    private Pair cons(Pair car, Object cdr)    { return new Pair(car, cdr); }
    private Pair cons(Builtin car, Object cdr) { return new Pair(car, cdr); }

    private Pair cons(Object car, Object cdr) {
        if (isAtom(car)) return new Pair((String)car, cdr);
        return new Pair((Pair)car, cdr);
    }

    private Pair assoc(Object atom, Object maybePair) {
        if (atom == null) return null;
        if (maybePair == null) return null;
        if (!isPair(maybePair)) throw new Error("assoc: expected second argument to be a Pair but got " + printObj(maybePair, true));
        Pair env = (Pair) maybePair;
        for ( ; env != null; env = (Pair)cdr(env))
            if (atom == car((Pair) car(env)))
                return (Pair) car(env);
        return null;
    }

    private Pair applyPrimitive(Builtin primfn, Pair args, int level) {
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
            StringBuffer sb = new StringBuffer(200);
            if (head_of_list) sb.append('(');
            sb.append(printObj(car((Pair) ob), true));
            if (cdr((Pair) ob) != null) {
                if (isPair(cdr((Pair) ob))) sb.append(' ').append(printObj(cdr((Pair) ob), false));
                else sb.append(" . ").append(printObj(cdr((Pair) ob), false)).append(')');
            } else sb.append(')');
            return sb.toString();
        } else if (isAtom(ob)) {
            return ob.toString();
        } else if (isPrim(ob)) {
            return "#<primitive>";
        } else {
            return "<internal error>";
        }
    }



    /// runtime for Lisp programs
    private Pair expTrue() { return cons(intern("quote"), cons(intern("t"), null)); }

    private void noArgs(String func, Pair a) {
        if (a != null) throw new Error(func + ": expected no arguments but got " + printObj(a, true));
    }

    private void oneArg(String func, Pair a) {
        if (a == null) throw new Error(func + ": expected one argument but no arument was given");
    }

    private void twoArgs(String func, Pair a) {
        if (a == null) throw new Error(func + ": expected two arguments but no arument was given");
        if (cdr(a) == null) throw new Error(func + ": expected two arguments but only one arument was given");
    }

    private void onePair(String func, Pair a) {
        if (a == null) throw new Error(func + ": expected one nonnull Pair argument but no argument was given");
        if (!isPair(car(a))) throw new Error(func + ": expected one nonnull Pair argument but got " + printObj(a, true));
    }

    private Builtin fcar =      (Pair a) -> { oneArg("car", a);     if (car(a) == null) return null; return (Pair) car((Pair) car(a)); };
    private Builtin fcdr =      (Pair a) -> { oneArg("cdr", a);     if (car(a) == null) return null; return (Pair) cdr((Pair) car(a)); };
    private Builtin fcons =     (Pair a) -> { twoArgs("cons", a);   if (car(a) == null && car((Pair) cdr(a)) == null) return null; return cons(car(a), car((Pair) cdr(a))); };
    private Builtin fassoc =    (Pair a) -> { twoArgs("assoc", a);  return assoc(car(a), car((Pair) cdr(a))); };
    private Builtin feq =       (Pair a) -> { twoArgs("eq", a);     return car(a) == car((Pair) cdr(a)) ? expTrue() : null; };
    private Builtin fpair =     (Pair a) -> { oneArg("pair?", a);   return isPair(car(a))               ? expTrue() : null; };
    private Builtin fatom =     (Pair a) -> { oneArg("symbol?", a); return isAtom(car(a))               ? expTrue() : null; };
    private Builtin fnull =     (Pair a) -> { oneArg("null?", a);   return car(a) == null               ? expTrue() : null; };
    private Builtin freadobj =  (Pair a) -> { noArgs("read", a);    look = getchar(); readToken(); return (Pair) readObj(); };
    private Builtin fwriteobj = (Pair a) -> { oneArg("write", a);   out.print(printObj(car(a), true)); return expTrue(); };

    private Builtin fwriteln = (Pair a) -> {
        if (a == null) {
            out.println();
            return expTrue();
        }
        out.println(printObj(car(a), true));
        return expTrue();
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
               cons(cons(intern("nil"),     cons((String)null, null)),
               null))))))))))));
    }



    /// build environment, read an S-expression and invoke eval()
    public String interpretExpression(InputStream in, PrintStream out) {
        this.in = in;
        this.out = out;
        Pair env = environment();
        look = getchar();
        readToken();
        final String result = printObj(eval(readObj(), env, 0), true);
        if (trace >= TRC_EVAL) {
            System.err.println("*** max eval depth: " + maxEvalDepth + " ***");
        }
        return result;
    }

    public static void main(String argv[]) {
        LambdaJ interpreter = new LambdaJ();
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
