package com.robertmayer.lambda;

import java.util.function.UnaryOperator;

public class Lambda {

    static final int EOF = -1;

    enum ObjType {
        Atom,
        List,
        Primitive
        ;
    }

    private static class Value {
        String atom;
        List list;
        UnaryOperator<List> prim;

        Value(String s) { atom = s; }
        Value(List l) { list = l; }
        Value(UnaryOperator<List> p) { prim = p; }
    }

    private static class List {
        ObjType objType;

        Value car;
        List next;

        List(String car, List cdr)              { objType = ObjType.Atom;      this.car = new Value(car); next = cdr; }
        List(List car, List cdr)                { objType = ObjType.List;      this.car = new Value(car); next = cdr; }
        List(UnaryOperator<List> car, List cdr) { objType = ObjType.Primitive; this.car = new Value(car); next = cdr; }
    }



    static List symbols = null;

    static int look; /* look ahead character */

    static final int SYMBOL_MAX = 32;
    static int token[] = new int[SYMBOL_MAX]; /* token */
    static boolean is_space(int x)  { return x == ' ' || x == '\t' || x == '\n' || x == '\r'; }
    static boolean is_parens(int x) { return x == '(' || x == ')'; }

    static int getchar() { try {return System.in.read();} catch (Exception e) { throw new RuntimeException("I/O error reading"); } }
    static void gettoken() {
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

    static boolean is_pair(List x) { return x.objType == ObjType.List; }
    static boolean is_atom(List x) { return x.objType == ObjType.Atom; }
    static List car(List x) { return x.car; }
    static List cdr(List x) { return x.next; }
    static List e_true() { return cons( intern("quote"), cons( intern("t"), null)); }
    static List e_false() { return null; }

    static List cons(String _car, List _cdr) {
        return new List(_car, _cdr);
    }

    static List cons(List _car, List _cdr) {
        return new List(_car, _cdr);
    }

    static List cons(UnaryOperator<List> _car, List _cdr) {
        return new List(_car, _cdr);
    }

    static String toChars(int[] s) {
        StringBuffer ret = new StringBuffer(32);
        for (int c: s) {
            ret.append((char)c);
        }
        return ret.toString();
    }

    static List intern(int[] sym) {
        return intern(toChars(sym));
    }

    static List intern(String sym) {
      List _pair = symbols;
      for ( ; _pair != null ; _pair = cdr(_pair)) {
          if (sym.equalsIgnoreCase(car(_pair).car.atom)) {
              return car(_pair).car.atom;
          }
      }
      symbols = cons(sym, symbols);
      return car(symbols).car.atom;
    }

    static List getobj() {
      if (token[0] == '(') return getlist();
      return intern(token);
    }

    static List getlist() {
      gettoken();
      if (token[0] == ')') return null;
      List tmp = getobj();
      return cons(tmp, getlist());
    }

    static void print_obj(List ob, boolean head_of_list) {
      if (!is_pair(ob) ) {
        System.out.print(ob != null ? ob.car.atom : "null" );
      } else {
        if (head_of_list) System.out.print('(');
        print_obj(ob.car.list, true);
        if (cdr(ob) != null) {
          System.out.print(' ');
          print_obj(cdr(ob), false);
        } else System.out.print(')');
      }
    }

    static UnaryOperator<List> fcons =     (List a) -> {  return cons(car(a), car(cdr(a)));  };
    static UnaryOperator<List> fcar =      (List a) -> {  return car(car(a));  };
    static UnaryOperator<List> fcdr =      (List a) -> {  return cdr(car(a));  };
    static UnaryOperator<List> feq =       (List a) -> {  return car(a) == car(cdr(a)) ? e_true() : e_false();  };
    static UnaryOperator<List> fpair =     (List a) -> {  return is_pair(car(a))       ? e_true() : e_false();  };
    static UnaryOperator<List> fatom =     (List a) -> {  return is_atom(car(a))       ? e_true() : e_false();  };
    static UnaryOperator<List> fnull =     (List a) -> {  return car(a) == null        ? e_true() : e_false(); };
    static UnaryOperator<List> freadobj =  (List a) -> {  look = getchar(); gettoken(); return getobj();  };
    static UnaryOperator<List> fwriteobj = (List a) -> {  print_obj(car(a), true); System.out.println(""); return e_true();  };

    static List evlist(List list, List env) {
      /* http://cslibrary.stanford.edu/105/LinkedListProblems.pdf */
      List head = null, insertPos = null;
      for ( ; list != null ; list = cdr(list) ) {
        List currentArg = cons(eval(car(list), env), null);
        if (head == null) {
            head = currentArg;
            insertPos = head;
        }
        else {
            insertPos.next = currentArg;
            insertPos = currentArg;
        }
      }
      return head;
    }

    static List apply_primitive(UnaryOperator<List> primfn, List args) {
      return primfn.apply(args);
    }

    static List eval(List exp, List env) {
        if (is_atom(exp) ) {
          for ( ; env != null; env = cdr(env) )
            if (exp == car(car(env)))  return car(cdr(car(env)));
          return null;
        } else if (is_atom( car (exp))) { /* special forms */
          if (car(exp) == intern("quote")) {
            return car(cdr(exp));
          } else if (car(exp) == intern("if")) {
            if (eval (car(cdr(exp)), env) != null)
              return eval (car(cdr(cdr(exp))), env);
            else
              return eval (car(cdr(cdr(cdr(exp)))), env);
          } else if (car(exp) == intern("lambda")) {
            return exp; /* todo: create a closure and capture free vars */
          } else if (car(exp) == intern("apply")) { /* apply function to list */
            List args = evlist (cdr(cdr(exp)), env);
            args = car(args); /* assumes one argument and that it is a list */
            return apply_primitive( eval(car(cdr(exp)), env).car.prim, args);
          } else { /* function call */
            List primop = eval (car(exp), env);
            if (is_pair(primop)) { /* user defined lambda, arg list eval happens in binding  below */
              return eval( cons(primop, cdr(exp)), env );
            } else if (primop != null) { /* built-in primitive */
              return apply_primitive(primop.car.prim, evlist(cdr(exp), env));
            }
          }
        } else if (car(car(exp)) == intern("lambda")) { /* should be a lambda, bind names into env and eval body */
          List extenv = env, names = car(cdr(car(exp))), vars = cdr(exp);
          for (  ; names != null; names = cdr(names), vars = cdr(vars) )
            extenv = cons (cons(car(names),  cons(eval (car(vars), env), null)), extenv);
          return eval (car(cdr(cdr(car(exp)))), extenv);
        }
        System.out.println("cannot evaluate expression");
        return null;
      }

    static void main(String argv[]) {
        List env = cons (cons(intern("car"),     cons(fcar, null)),
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
        System.out.println();
    }
}
