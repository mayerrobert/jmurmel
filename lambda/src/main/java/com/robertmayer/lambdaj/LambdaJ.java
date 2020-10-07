package com.robertmayer.lambdaj;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.function.DoubleBinaryOperator;
import java.util.function.IntPredicate;

public class LambdaJ {

    /// infrastructure
    public static final int EOF = -1;
    public static final int SYMBOL_MAX = 32;

    public static final int TRC_NONE = 0, TRC_EVAL = 1, TRC_PRIM = 2, TRC_PARSE = 3, TRC_TOK = 4, TRC_LEX = 5;
    public int trace = TRC_NONE;

    private InputStream in;
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
    public static interface Builtin {
        Object apply(ConsCell x);
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
        if (isNumber()) {
            try {
                tok = Double.valueOf(tokenToString(token));
            }
            catch (NumberFormatException e) {
                throw new LambdaJError("line " + lineNo + ':' + charNo + ": '" + tokenToString(token)
                + "' is not a valid symbol or number");
            }
        } else if (token[0] == '\0'){
            tok = null;
        } else {
            tok = tokenToString(token);
        }
    }

    private boolean isNumber() {
        final int first = token[0];
        if (isDigit(first)) return true;
        return ((first == '-' || first == '+') && isDigit(token[1]));
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
    private ConsCell symbols = null;

    private String intern(String sym) {
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
        if (tok == null) throw new LambdaJError("line " + lineNo + ':' + charNo + ": cannot read list. missing ')'?");
        if (")".equals(tok)) return null;
        Object tmp = readObj();
        if (symbolp(tmp)) return cons(tmp, readList());
        else return cons(tmp, readList());
    }



    /// eval - interpreter
    private Object eval(Object exp, ConsCell env, int level) {
        dbgEvalStart(exp, env, level);
        try {
            if (symbolp(exp)) {
                if (exp == null) return null;
                ConsCell envEntry = assoc(exp, env);
                if (envEntry != null) return car(cdr(envEntry));
                throw new LambdaJError("'" + exp + "' is undefined");

            } else if (numberp(exp)) {
                return exp;

            } else if (symbolp(car (exp))) { /* special forms */
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
                    ConsCell bindings = (ConsCell) car(cdr(exp));
                    ConsCell body =     (ConsCell) cdr(cdr(exp));
                    return evlabels(bindings, body, env, level);

                } else if (car(exp) == intern("cond")) {
                    return evcon((ConsCell) cdr(exp), env, level);

                } else if (car(exp) == intern("apply")) { // apply function to list
                    /* assumes one argument and that it is a list */
                    oneArg("apply", (ConsCell) cdr(cdr(exp)));
                    ConsCell args = evlis((ConsCell) cdr(cdr(exp)), env, level);
                    args = (ConsCell)car(args);
                    final Object func = eval(car(cdr(exp)), env, level + 1);
                    if (listp(func)) return eval(cons(func, args), env, level + 1);
                    else if (isPrim(func)) return applyPrimitive((Builtin) func, args, level);
                    else throw new LambdaJError("apply: not a function: " + printObj(func, true));

                } else { /* function call */
                    Object func = eval(car(exp), env, level + 1);
                    if (listp(func)) { /* user defined lambda, arg list eval happens in binding  below */
                        return eval(cons(func, cdr(exp)), env, level + 1);
                    } else if (isPrim(func)) { /* built-in primitive */ // todo umbauen auf isPrim und ein "else error"
                        return applyPrimitive((Builtin) func, evlis((ConsCell) cdr(exp), env, level), level);
                    }
                    else throw new LambdaJError("not a function: " + printObj(func, true));
                }

            } else if (car(car(exp)) == intern("lambda")) {
                /* should be a lambda, bind args as "names" into env and eval body-list */
                final ConsCell lambda = (ConsCell) cdr(car(exp));
                ConsCell extenv = env, params = (ConsCell) car(lambda), args = (ConsCell) cdr(exp);
                for ( ; params != null; params = (ConsCell) cdr(params), args = (ConsCell) cdr(args))
                    extenv = cons(cons(car(params),  cons(eval(car(args), env, level + 1), null)), extenv);
                ConsCell body = (ConsCell) cdr(lambda);
                Object result = null;
                for (; body != null; body = (ConsCell) cdr(body))
                    result = eval(car(body), extenv, level);
                return result;

            }

            throw new LambdaJError("cannot eval expression '" + printObj(exp, true) + '\'');

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
    private Object evcon(ConsCell c, ConsCell e, int level) {
        for ( ; c != null; c = (ConsCell) cdr(c)) {
            Object condResult = eval(car(car(c)), e, level + 1);
            if (condResult != null) return eval(car(cdr(car(c))), e, level + 1);
        }
        return null;
    }

    private ConsCell evlis(ConsCell list, ConsCell env, int level) {
        ConsCell head = null, insertPos = null;
        for ( ; list != null; list = (ConsCell) cdr(list)) {
            ConsCell currentArg = cons(eval(car(list), env, level + 1), null);
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

    private Object evlabels(ConsCell bindings, ConsCell body, ConsCell env, int level) {
        ConsCell extenv = env;
        for (; bindings != null; bindings = (ConsCell)cdr(bindings)) {
            final ConsCell currentFunc = (ConsCell)car(bindings);
            final String currentName = (String)car(currentFunc);
            final ConsCell currentBody = (ConsCell)cdr(currentFunc);
            final ConsCell lambda = cons(cons(intern("lambda"), currentBody), null);
            extenv = cons(cons(intern(currentName), lambda), extenv);
        }

        Object result = null;
        for (; body != null; body = (ConsCell) cdr(body))
            result = eval(car(body), extenv, level);
        return result;
    }

    private int maxEvalDepth;

    private void dbgEvalStart(Object exp, ConsCell env, int level) {
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
    private static class ConsCell {
        Object car, cdr;
        ConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private ConsCell cons(Object car, Object cdr) { return new ConsCell(car, cdr); }
    private Object   car(Object x)                { return ((ConsCell)x).car; }
    private Object   cdr(Object x)                { return ((ConsCell)x).cdr; }

    private boolean  consp(Object x)             { return x != null && x instanceof ConsCell; }
    private boolean  atom(Object x)              { return x == null || !(x instanceof ConsCell); } // !isCons(x)
    private boolean  symbolp(Object x)           { return x == null || x instanceof String; } // null (alias nil) is a symbol too
    private boolean  listp(Object x)             { return x == null || x instanceof ConsCell; } // null is a list too
    private boolean  isPrim(Object x)            { return x instanceof Builtin; }
    private boolean  numberp(Object x)           { return x instanceof Number; }

    private ConsCell assoc(Object atom, Object maybeList) {
        if (atom == null) return null;
        if (maybeList == null) return null;
        if (!listp(maybeList)) throw new LambdaJError("assoc: expected second argument to be a List but got " + printObj(maybeList, true));
        ConsCell env = (ConsCell) maybeList;
        for ( ; env != null; env = (ConsCell)cdr(env))
            if (atom == car(car(env))) return (ConsCell) car(env);
        return null;
    }

    private Object applyPrimitive(Builtin primfn, ConsCell args, int level) {
        if (trace >= TRC_PRIM) {
            char[] cpfx = new char[level*2]; Arrays.fill(cpfx, ' '); String pfx = new String(cpfx);
            System.err.println(pfx + "(<primitive> " + printObj(args, true) + ')');
        }
        return primfn.apply(args);
    }

    private String printObj(Object ob, boolean head_of_list) {
        if (ob == null) {
            return "nil";
        } else if (listp(ob)) {
            if (car(ob) == intern("quote") && car(cdr(ob)) == intern("t")) return "t";
            final StringBuffer sb = new StringBuffer(200);
            if (head_of_list) sb.append('(');
            sb.append(printObj(car(ob), true));
            if (cdr(ob) != null) {
                if (listp(cdr(ob)))
                    sb.append(' ').append(printObj(cdr(ob), false));
                else if (head_of_list)
                    sb.append(" . ").append(printObj(cdr(ob), false)).append(')');
                else
                    sb.append(' ').append(printObj(cdr(ob), false)).append(')');
            } else sb.append(')');
            return sb.toString();
        } else if (symbolp(ob)) {
            return ob.toString();
        } else if (isPrim(ob)) {
            return "#<primitive>";
        } else if (numberp(ob)) {
            return ob.toString();
        } else {
            return "<internal error>";
        }
    }



    /// runtime for Lisp programs
    private final ConsCell expTrue = cons(intern("quote"), cons(intern("t"), null));
    private Object boolResult(boolean b) { return b ? expTrue : null; }

    private void noArgs(String func, ConsCell a) {
        if (a != null) throw new LambdaJError(func + ": expected no arguments but got " + printObj(a, true));
    }

    private void oneArg(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected one argument but no argument was given");
        if (cdr(a) != null) throw new LambdaJError(func + ": expected one argument but got extra arg(s) " + printObj(cdr(a), true));
    }

    private void oneOrMoreArgs(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected at least one argument but no argument was given");
    }

    private void twoArgs(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected two arguments but no argument was given");
        if (cdr(a) == null) throw new LambdaJError(func + ": expected two arguments but only one argument was given");
        if (cdr(cdr(a)) != null) throw new LambdaJError(func + ": expected two arguments but got extra arg(s) " + printObj(cdr(cdr(a)), true));
    }

    private void onePair(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(func + ": expected one Pair argument but no argument was given");
        if (!listp(car(a))) throw new LambdaJError(func + ": expected one Pair argument but got " + printObj(a, true));
        if (cdr(a) != null) throw new LambdaJError(func + ": expected one Pair argument but got extra arg(s) " + printObj(cdr(a), true));
    }

    /** arguments if any must be only numbers */
    private void numbers(String func, ConsCell a) {
        if (a == null) return;
        for (; a != null; a = (ConsCell) cdr(a))
            if (!numberp(car(a))) throw new LambdaJError(func + ": expected only number arguments but got " + printObj(a, true));
    }

    private void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numbers(func, a);
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
    private Object addOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        numbers(opName, args);
        for (; args != null; args = (ConsCell) cdr(args))
            startVal = op.applyAsDouble(startVal, (Double)car(args));
        return startVal;
    }

    /** generate operator for one or more args */
    private Object subOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        oneOrMoreNumbers("-", args);
        double result = (Double)car(args);
        if (cdr(args) == null) return op.applyAsDouble(startVal, result);
        for (args = (ConsCell) cdr(args); args != null; args = (ConsCell) cdr(args))
            result = op.applyAsDouble(result, (Double)car(args));
        return result;
    }

    private Builtin fnull =     (ConsCell a) -> { oneArg("null?", a);   return boolResult(car(a) == null); };

    private Builtin fcons =     (ConsCell a) -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); };
    private Builtin fcar =      (ConsCell a) -> { onePair("car", a);    if (car(a) == null) return null; return car(car(a)); };
    private Builtin fcdr =      (ConsCell a) -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); };

    private Builtin feq =       (ConsCell a) -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); };

    private Builtin fconsp =    (ConsCell a) -> { oneArg("consp", a);   return boolResult(consp  (car(a))); };
    private Builtin fatom =     (ConsCell a) -> { oneArg("atom", a);    return boolResult(atom   (car(a))); };
    private Builtin fsymbolp =  (ConsCell a) -> { oneArg("symbolp", a); return boolResult(symbolp(car(a))); };
    private Builtin flistp =    (ConsCell a) -> { oneArg("listp", a);   return boolResult(listp  (car(a))); };
    private Builtin fnumberp =  (ConsCell a) -> { oneArg("numberp", a); return boolResult(numberp(car(a))); };

    private Builtin fassoc =    (ConsCell a) -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); };
    private Builtin freadobj =  (ConsCell a) -> { noArgs("read", a);    look = getchar(); readToken(); return readObj(); };
    private Builtin fwriteobj = (ConsCell a) -> { oneArg("write", a);   out.print(printObj(car(a), true)); return expTrue; };

    private Builtin fwriteln = (ConsCell a) -> {
        if (a == null) {
            out.println();
            return expTrue;
        }
        out.println(printObj(car(a), true));
        return expTrue;
    };

    private Builtin fnumbereq = args -> compareOp(args, "=",  compareResult -> compareResult == 0);
    private Builtin flt =       args -> compareOp(args, "<",  compareResult -> compareResult <  0);
    private Builtin fle =       args -> compareOp(args, "<=", compareResult -> compareResult <= 0);
    private Builtin fgt =       args -> compareOp(args, ">",  compareResult -> compareResult >  0);
    private Builtin fge =       args -> compareOp(args, ">=", compareResult -> compareResult >= 0);

    private Builtin fadd =  args -> addOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs);
    private Builtin fmul =  args -> addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs);

    private Builtin fsub  = args -> subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs);
    private Builtin fquot = args -> subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs);

    private Builtin fmod = (ConsCell args) -> {
        twoArgs("mod", args);
        numbers("mod", args);
        return (Double)car(args) % (Double)car(cdr(args));
    };

    private ConsCell environment() {
        return cons(cons(intern("car"),     cons(fcar, null)),
               cons(cons(intern("cdr"),     cons(fcdr, null)),
               cons(cons(intern("cons"),    cons(fcons, null)),

               cons(cons(intern("eq"),      cons(feq, null)),
               cons(cons(intern("consp"),   cons(fconsp, null)),
               cons(cons(intern("atom"),    cons(fatom, null)),
               cons(cons(intern("symbolp"), cons(fsymbolp, null)),
               cons(cons(intern("listp"),   cons(flistp, null)),
               cons(cons(intern("numberp"), cons(fnumberp, null)),
               cons(cons(intern("null?"),   cons(fnull, null)),

               cons(cons(intern("assoc"),   cons(fassoc, null)),
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
               cons(cons(intern("t"),       cons(intern("t"), null)),
               null))))))))))))))))))))))))));
    }



    /// build environment, read an S-expression and invoke eval()
    public String interpretExpression(InputStream in, PrintStream out) {
        this.in = in;
        this.out = out;
        final ConsCell env = environment();
        look = getchar();
        readToken();
        final Object exp = readObj();
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
        final ConsCell env = environment();
        look = getchar();
        readToken();
        Object exp = readObj();
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

    public static void main(String args[]) {
        final LambdaJ interpreter = new LambdaJ();
        if (hasFlag("--trace", args)) interpreter.trace = TRC_LEX;

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
            } else if (hasFlag("--result", args)) {
                System.out.println(result);
            }
        } catch (LambdaJError e) {
            System.out.println();
            System.out.println(e.toString());
        }
    }

    private static boolean hasFlag(String flag, String[] args) {
        for (String arg: args)
            if (flag.equals(arg)) return true;
        return false;
    }
}
