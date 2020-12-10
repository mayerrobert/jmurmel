package com.robertmayer.lambdaj;

import com.robertmayer.lambdaj.LambdaJ.ConsCell;

public class TestUtils {

    static String sexp(Object exp) {
        StringBuilder sExp = new StringBuilder();
        LambdaJ.makeWriter(sExp::append).printObj(exp);
        return sExp.toString();

    }

    static ConsCell cdr(Object l) {
        return (ConsCell)((ConsCell)l).cdr();
    }
}
