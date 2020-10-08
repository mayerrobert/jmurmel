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

    private boolean
    HAVE_NIL = true, HAVE_T = true,       // use () and (quote t) instead
    HAVE_APPLY = true,                    // apply brauchts fuer Lisp, aber nicht fuer Lambda Kalkuel (?)
    HAVE_LABELS = true,                   // use Y-combinator instead
    HAVE_XTRA = true,                     // no if, in zukunft no loop. entweder if oder cond ist notwendig
    HAVE_DOUBLE = true,                   // no +-<>..., numberp, remaining datatypes are symbls and cons-cells (lists)
    HAVE_IO = true,                       // no read/ write, result only
    HAVE_UTIL = true                      // no null?, consp, listp, symbolp, assoc
    ;

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
        private int token[] = new int[SYMBOL_MAX];
        private Object tok;

        public LispParser(InputStream in) { this.in = in; }

        private boolean isSpace(int x)  { return !escape && (x == ' ' || x == '\t' || x == '\n' || x == '\r'); }
        private boolean isSyntax(int x) { return !escape && (x == '(' || x == ')' || x == '\''); }
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
                } else {
                    while (index < SYMBOL_MAX - 1 && look != EOF && !isSpace(look) && !isSyntax(look)) {
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
            if (tok instanceof Number) {
                if (trace >= TRC_TOK) System.err.println("*** number " + tok.toString());
                return tok;
            }
            if ("'".equals(tok)) {
                readToken();
                return cons(quote, cons(_readObj(), null));
            }
            if (trace >= TRC_TOK) System.err.println("*** symbol " + (String)tok);
            return intern((String)tok);
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
    private Object eval(Object exp, ConsCell env, int level) {
        dbgEvalStart(exp, env, level);
        try {
            if (symbolp(exp)) {
                if (exp == null) return null;
                ConsCell envEntry = assoc(exp, env);
                if (envEntry != null) return car(cdr(envEntry));
                throw new LambdaJError("'" + exp + "' is undefined");

            } else if (atom(exp)) {
                return exp;

            } else if (symbolp(car (exp))) { /* special forms */
                if (car(exp) == program.intern("quote")) {
                    oneArg("quote", cdr(exp));
                    return car(cdr(exp));

                } else if (car(exp) == program.intern("if")) {
                    nArgs("if", cdr(exp), 2, 3, exp);
                    if (eval(car(cdr(exp)), env, level + 1) != null)
                        return eval(car(cdr(cdr(exp))), env, level + 1);
                    else if (cdr(cdr(cdr(exp))) != null)
                        return eval(car(cdr(cdr(cdr(exp)))), env, level + 1);
                    else
                        return null;

                } else if (car(exp) == program.intern("lambda")) {
                    nArgs("lambda", cdr(exp), 2, exp);
                    return exp;

                } else if (car(exp) == program.intern("labels")) { // labels bindings body -> object
                    nArgs("labels", cdr(exp), 2, exp);
                    ConsCell bindings = (ConsCell) car(cdr(exp));
                    ConsCell body =     (ConsCell) cdr(cdr(exp));
                    return evlabels(bindings, body, env, level);

                } else if (car(exp) == program.intern("cond")) {
                    return evcon((ConsCell) cdr(exp), env, level);

                } else if (car(exp) == program.intern("apply")) { // apply function to list
                    twoArgs("apply", cdr(exp), exp);
                    final Object func = eval(car(cdr(exp)), env, level + 1);
                    final ConsCell args = (ConsCell)car(evlis((ConsCell) cdr(cdr(exp)), env, level));
                    if (consp(func)) return eval(cons(func, args), env, level + 1);
                    else if (isPrim(func)) return applyPrimitive((Builtin) func, args, level);
                    else throw new LambdaJError("apply: not a function: " + printObj(func, true)
                                                + ". this was the result of evaluating the expression "
                                                + printObj(car(cdr(exp)), true) + errorExp(exp));

                } else { /* function call */
                    Object func = eval(car(exp), env, level + 1);
                    if (consp(func)) { /* user defined lambda, arg list eval happens in binding  below */
                        return eval(cons(func, cdr(exp)), env, level + 1);
                    } else if (isPrim(func)) {
                        return applyPrimitive((Builtin) func, evlis((ConsCell) cdr(exp), env, level), level);
                    }
                    else throw new LambdaJError("not a function: " + printObj(func, true) + errorExp(exp));
                }

            } else if (consp(car(exp)) && car(car(exp)) == program.intern("lambda")) {
                /* should be a lambda, bind args as "names" into env and eval body-list */
                final Object lambda = cdr(car(exp));
                nArgs("lambda", lambda, 2, exp);

                ConsCell extenv = env, params = (ConsCell) car(lambda), args = (ConsCell) cdr(exp);
                for ( ; params != null && args != null; params = (ConsCell) cdr(params), args = (ConsCell) cdr(args))
                    extenv = cons(cons(car(params),  cons(eval(car(args), env, level + 1), null)), extenv);
                if (params != null)
                    throw new LambdaJError("lambda: not enough arguments. parameters w/o argument: " + printObj(params, true)
                                           + errorExp(exp));
                if (args != null)
                    throw new LambdaJError("lambda: too many arguments. remaining arguments: " + printObj(args, true)
                                           + errorExp(exp));

                ConsCell body = (ConsCell) cdr(lambda);
                Object result = null;
                for (; body != null; body = (ConsCell) cdr(body))
                    result = eval(car(body), extenv, level);
                return result;

            } else if (atom(car(exp))) {
                throw new LambdaJError("not a function: " + printObj(car(exp), true) + errorExp(exp));

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
            final ConsCell lambda = cons(cons(program.intern("lambda"), currentBody), null);
            extenv = cons(cons(program.intern(currentName), lambda), extenv);
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
        @Override
        public String toString() { return printObj(this, true); }
    }



    /// functions used by interpreter program, a subset is used by interpreted programs as well
    private static ConsCell cons(Object car, Object cdr) { return new ConsCell(car, cdr); }
    private static Object   car(Object x)                { return ((ConsCell)x).car; }
    private static Object   cdr(Object x)                { return ((ConsCell)x).cdr; }

    private static boolean  consp(Object x)             { return x != null && x instanceof ConsCell; }
    private static boolean  atom(Object x)              { return x == null || !(x instanceof ConsCell); } // !isCons(x)
    private static boolean  symbolp(Object x)           { return x == null || x instanceof String; } // null (alias nil) is a symbol too
    private static boolean  listp(Object x)             { return x == null || x instanceof ConsCell; } // null is a list too
    private static boolean  isPrim(Object x)            { return x instanceof Builtin; }
    private static boolean  numberp(Object x)           { return x instanceof Number; }

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

    private static ConsCell assoc(Object atom, Object maybeList) {
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

    private static String printObj(Object ob, boolean head_of_list) {
        if (ob == null) {
            return "nil";
        } else if (listp(ob)) {
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
        } else if (atom(ob)) {
            return ob.toString();
        } else {
            return "<program.internal error>";
        }
    }



    /// runtime for Lisp programs
    //private final ConsCell expTrue() = cons(program.intern("quote"), cons(program.intern("t"), null)); // (quote t) could b used if there was no builtin t in the environment
    private Object _expTrue;
    private Object expTrue() {
        if (_expTrue == null) _expTrue = program.intern("t");
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

    private final Builtin fnull =     (ConsCell a) -> { oneArg("null?", a);   return boolResult(car(a) == null); };

    private final Builtin fcons =     (ConsCell a) -> { twoArgs("cons", a);   if (car(a) == null && car(cdr(a)) == null) return null; return cons(car(a), car(cdr(a))); };
    private final Builtin fcar =      (ConsCell a) -> { onePair("car", a);    if (car(a) == null) return null; return car(car(a)); };
    private final Builtin fcdr =      (ConsCell a) -> { onePair("cdr", a);    if (car(a) == null) return null; return cdr(car(a)); };

    private final Builtin feq =       (ConsCell a) -> { twoArgs("eq", a);     return boolResult(car(a) == car(cdr(a))); };

    private final Builtin fconsp =    (ConsCell a) -> { oneArg("consp", a);   return boolResult(consp  (car(a))); };
    private final Builtin fatom =     (ConsCell a) -> { oneArg("atom", a);    return boolResult(atom   (car(a))); };
    private final Builtin fsymbolp =  (ConsCell a) -> { oneArg("symbolp", a); return boolResult(symbolp(car(a))); };
    private final Builtin flistp =    (ConsCell a) -> { oneArg("listp", a);   return boolResult(listp  (car(a))); };
    private final Builtin fnumberp =  (ConsCell a) -> { oneArg("numberp", a); return boolResult(numberp(car(a))); };

    private final Builtin fassoc =    (ConsCell a) -> { twoArgs("assoc", a);  return assoc(car(a), car(cdr(a))); };
    private final Builtin freadobj =  (ConsCell a) -> { noArgs("read", a);    return inputData.readObj(); };
    private final Builtin fwriteobj = (ConsCell a) -> { oneArg("write", a);   out.print(printObj(car(a), true)); return expTrue(); };

    private final Builtin fwriteln = (ConsCell a) -> {
        if (a == null) {
            out.println();
            return expTrue();
        }
        out.println(printObj(car(a), true));
        return expTrue();
    };

    private final Builtin fnumbereq = args -> compareOp(args, "=",  compareResult -> compareResult == 0);
    private final Builtin flt =       args -> compareOp(args, "<",  compareResult -> compareResult <  0);
    private final Builtin fle =       args -> compareOp(args, "<=", compareResult -> compareResult <= 0);
    private final Builtin fgt =       args -> compareOp(args, ">",  compareResult -> compareResult >  0);
    private final Builtin fge =       args -> compareOp(args, ">=", compareResult -> compareResult >= 0);

    private final Builtin fadd =  args -> addOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs);
    private final Builtin fmul =  args -> addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs);

    private final Builtin fsub  = args -> subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs);
    private final Builtin fquot = args -> subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs);

    private final Builtin fmod = (ConsCell args) -> {
        twoArgs("mod", args);
        numbers("mod", args);
        return (Double)car(args) % (Double)car(cdr(args));
    };

    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link Parser} {@code program} on the fly */
    private ConsCell environment(Parser program, ConsCell prev) {
        return cons(cons(program.intern("car"),     cons(fcar, null)),
               cons(cons(program.intern("cdr"),     cons(fcdr, null)),
               cons(cons(program.intern("cons"),    cons(fcons, null)),

               cons(cons(program.intern("eq"),      cons(feq, null)),
               cons(cons(program.intern("consp"),   cons(fconsp, null)),
               cons(cons(program.intern("atom"),    cons(fatom, null)),
               cons(cons(program.intern("symbolp"), cons(fsymbolp, null)),
               cons(cons(program.intern("listp"),   cons(flistp, null)),
               cons(cons(program.intern("numberp"), cons(fnumberp, null)),
               cons(cons(program.intern("null?"),   cons(fnull, null)),

               cons(cons(program.intern("assoc"),   cons(fassoc, null)),
               cons(cons(program.intern("read"),    cons(freadobj, null)),
               cons(cons(program.intern("write"),   cons(fwriteobj, null)),
               cons(cons(program.intern("writeln"), cons(fwriteln, null)),

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

               cons(cons(program.intern("t"),       cons(program.intern("t"), null)),
               cons(cons(program.intern("nil"),     cons(null, null)),
               prev))))))))))))))))))))))))));
    }



    /// build environment, read an S-expression and invoke eval()
    public Object interpretExpression(InputStream in, PrintStream out) {
        program = new LispParser(in);
        inputData = program;
        this.out = out;
        final ConsCell env = environment(program, null);
        final Object exp = program.readObj();
        final Object result = eval(exp, env, 0);
        if (trace >= TRC_EVAL) {
            System.err.println("*** max eval depth: " + maxEvalDepth + " ***");
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
            final Object result = eval(exp, env, 0);
            if (trace >= TRC_EVAL) {
                System.err.println("*** max eval depth: " + maxEvalDepth + " ***");
            }
            exp = program.readObj();
            if (exp == null) return result;
        }
    }

    public static void main(String args[]) {
        final LambdaJ interpreter = new LambdaJ();

        if (hasFlag("--trace", args))     interpreter.trace = TRC_LEX;

        if (hasFlag("--no-nil", args))    interpreter.HAVE_NIL = false;
        if (hasFlag("--no-true", args))   interpreter.HAVE_T = false;
        if (hasFlag("--no-apply", args))  interpreter.HAVE_APPLY = false;
        if (hasFlag("--no-labels", args)) interpreter.HAVE_LABELS = false;
        if (hasFlag("--no-extra", args))  interpreter.HAVE_XTRA = false;
        if (hasFlag("--no-double", args)) interpreter.HAVE_DOUBLE = false;
        if (hasFlag("--no-io", args))     interpreter.HAVE_IO = false;
        if (hasFlag("--no-util", args))   interpreter.HAVE_UTIL = false;

        if (hasFlag("--min", args)) {
            // nothing except apply
            interpreter.HAVE_NIL = false;
            interpreter.HAVE_T = false;
            interpreter.HAVE_LABELS = false;
            interpreter.HAVE_XTRA = false;
            interpreter.HAVE_DOUBLE = false;
            interpreter.HAVE_IO = false;
            interpreter.HAVE_UTIL = false;
        }

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
            } else if (hasFlag("--result", args)) {
                System.out.println(result);
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
                err = true;
            }
        }
        return err;
    }
}
