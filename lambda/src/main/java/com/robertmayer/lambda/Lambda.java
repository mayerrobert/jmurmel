package com.robertmayer.lambda;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.function.UnaryOperator;

public class Lambda {

    static final int EOF = -1;
    static final int SYMBOL_MAX = 32;

    private InputStream in;
    private PrintStream out;

    private enum ObjType {
        Atom,
        Prim,
        List,
        ;
    }

    private static class Pair {
        ObjType objType;
        Object car;
        Pair cdr;

        Pair(String str, Pair cdr)               { this.car = str; this.cdr = cdr; objType = ObjType.Atom; }
        Pair(UnaryOperator<Pair> prim, Pair cdr) { this.car = prim; objType = ObjType.Prim; }
        Pair(Pair car, Pair cdr)                 { this.car = car; this.cdr = cdr; objType = ObjType.List; }
    }

    private Pair symbols = null;

    private int look; /* look ahead character */

    private int token[] = new int[SYMBOL_MAX]; /* token */
    private boolean is_space(int x)  { return x == ' ' || x == '\t' || x == '\n' || x == '\r'; }
    private boolean is_parens(int x) { return x == '(' || x == ')'; }

    private int getchar() { try {return in.read();} catch (Exception e) { throw new RuntimeException("I/O error reading"); } }
    private void gettoken() {
        int index = 0;
        while(is_space(look)) { look = getchar(); }
        if (is_parens(look)) {
            token[index++] = look;  look = getchar();
        } else {
            while(index < SYMBOL_MAX - 1 && look != EOF && !is_space(look) && !is_parens(look)) {
                token[index++] = look;  look = getchar();
            }
        }
        token[index] = '\0';
    }

    private boolean is_pair(Object x) { return x instanceof Pair; }
    private boolean is_atom(Object x) { return x instanceof String; }
    private Object car(Pair x) { return x.car; }
    private Pair cdr(Pair x) { return x.cdr; }
    private Pair e_true() { return cons( intern("quote"), cons( intern("t"), null)); }
    private Pair e_false() { return null; }

    private Pair cons(String _car, Pair _cdr) {
        return new Pair(_car, _cdr);
    }

    private Pair cons(Pair _car, Pair _cdr) {
        return new Pair(_car, _cdr);
    }

    private Pair cons(UnaryOperator<Pair> _car, Pair _cdr) {
        return new Pair(_car, _cdr);
    }

    private Pair cons(Object _car, Pair _cdr) {
        if (is_atom(_car)) return new Pair((String)_car, _cdr);
        return new Pair((Pair)_car, _cdr);
    }

    private String toChars(int[] s) {
        StringBuffer ret = new StringBuffer(32);
        for (int c: s) {
            if (c == '\0') break;
            ret.append((char)c);
        }
        return ret.toString();
    }

    private String intern(int[] sym) {
        return intern(toChars(sym));
    }

    private String intern(String sym) {
        Pair _pair = symbols;
        for ( ; _pair != null ; _pair = cdr(_pair)) {
            if (sym.equalsIgnoreCase((String) car(_pair))) {
                return (String) car(_pair);
            }
        }
        symbols = cons(sym, symbols);
        return (String) car(symbols);
    }

    private Object getobj() {
        if (token[0] == '(') return getlist();
        return intern(token);
    }

    private Object getlist() {
        gettoken();
        if (token[0] == ')') return null;
        Object tmp = getobj();
        if (is_atom(tmp)) return cons((String)tmp, (Pair) getlist());
        else return cons((Pair)tmp, (Pair) getlist());
    }

    private void print_obj(Object ob, boolean head_of_list) {
        if (!is_pair(ob) ) {
            out.print(ob != null ? ob.toString() : "null" );
        } else {
            if (head_of_list) out.print('(');
            print_obj(car((Pair) ob), true);
            if (cdr((Pair) ob) != null) {
                out.print(' ');
                print_obj(cdr((Pair) ob), false);
            } else out.print(')');
        }
    }

    private UnaryOperator<Pair> fcons =     (Pair a) -> {  return cons(car(a), (Pair)car(cdr(a)));  };
    private UnaryOperator<Pair> fcar =      (Pair a) -> {  return (Pair) car((Pair) car(a));  };
    private UnaryOperator<Pair> fcdr =      (Pair a) -> {  return cdr((Pair) car(a));  };
    private UnaryOperator<Pair> feq =       (Pair a) -> {  return car(a) == car(cdr(a)) ? e_true() : e_false();  };
    private UnaryOperator<Pair> fpair =     (Pair a) -> {  return is_pair(car(a))       ? e_true() : e_false();  };
    private UnaryOperator<Pair> fatom =     (Pair a) -> {  return is_atom(car(a))       ? e_true() : e_false();  };
    private UnaryOperator<Pair> fnull =     (Pair a) -> {  return car(a) == null        ? e_true() : e_false(); };
    private UnaryOperator<Pair> freadobj =  (Pair a) -> {  look = getchar(); gettoken(); return (Pair) getobj();  };
    private UnaryOperator<Pair> fwriteobj = (Pair a) -> {  print_obj(car(a), true); out.println(""); return e_true();  };

    private Pair evlist(Pair list, Pair env) {
        /* http://cslibrary.stanford.edu/105/LinkedListProblems.pdf */
        Pair head = null, insertPos = null;
        for ( ; list != null ; list = cdr(list) ) {
            Pair currentArg = cons(eval(car(list), env), null);
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

    private Pair apply_primitive(UnaryOperator<Pair> primfn, Pair args) {
        return primfn.apply(args);
    }

    private Object eval(Object exp, Pair env) {
        //out.print("exp: "); print_obj(exp, true); out.println();
        //out.print("env: "); print_obj(env, true); out.println();
        //out.flush();

        if (is_atom(exp) ) {
            for ( ; env != null; env = cdr(env) )
                if (exp == car((Pair) car(env)))
                    return car(cdr((Pair) car(env)));
            return null;
        } else if (is_atom( car ((Pair) exp))) { /* special forms */
            if (car((Pair) exp) == intern("quote")) {
                return car(cdr((Pair) exp));
            } else if (car((Pair) exp) == intern("if")) {
                if (eval (car(cdr((Pair) exp)), env) != null)
                    return eval (car(cdr(cdr((Pair) exp))), env);
                else
                    return eval (car(cdr(cdr(cdr((Pair) exp)))), env);
            } else if (car((Pair) exp) == intern("lambda")) {
                return exp; /* todo: create a closure and capture free vars */
            } else if (car((Pair) exp) == intern("apply")) { /* apply function to list */
                Pair args = evlist (cdr(cdr((Pair) exp)), env);
                args = (Pair)car(args); /* assumes one argument and that it is a list */
                return apply_primitive( (UnaryOperator<Pair>) eval(car(cdr((Pair) exp)), env), args);
            } else { /* function call */
                Object primop = eval (car((Pair) exp), env);
                if (is_pair(primop)) { /* user defined lambda, arg list eval happens in binding  below */
                    return eval( cons(primop, cdr((Pair) exp)), env );
                } else if (primop != null) { /* built-in primitive */
                    return apply_primitive((UnaryOperator<Pair>) primop, evlist(cdr((Pair) exp), env));
                }
            }
        } else if (car((Pair) car((Pair) exp)) == intern("lambda")) { /* should be a lambda, bind names into env and eval body */
            Pair extenv = env, names = (Pair) car(cdr((Pair) car((Pair) exp))), vars = cdr((Pair) exp);
            for (  ; names != null; names = cdr(names), vars = cdr(vars) )
                extenv = cons (cons((String) car(names),  cons(eval (car(vars), env), null)), extenv);
            return eval (car(cdr(cdr((Pair) car((Pair) exp)))), extenv);
        }
        out.println("cannot evaluate expression");
        return null;
    }

    private void run(InputStream in, PrintStream out) {
        this.in = in;
        this.out = out;
        Pair env = cons (cons(intern("car"),     cons(fcar, null)),
                   cons (cons(intern("cdr"),     cons(fcdr, null)),
                   cons (cons(intern("cons"),    cons(fcons, null)),
                   cons (cons(intern("eq?"),     cons(feq, null)),
                   cons (cons(intern("pair?"),   cons(fpair, null)),
                   cons (cons(intern("symbol?"), cons(fatom, null)),
                   cons (cons(intern("null?"),   cons(fnull, null)),
                   cons (cons(intern("read"),    cons(freadobj, null)),
                   cons (cons(intern("write"),   cons(fwriteobj, null)),
                   cons (cons(intern("null"),    cons((String)null,null)), null))))))))));
        look = getchar();
        gettoken();
        print_obj( eval(getobj(), env), true );
    }

    public static void main(String argv[]) {
        Lambda interpreter = new Lambda();
        interpreter.run(System.in, System.out);
        System.out.println();
    }
}
