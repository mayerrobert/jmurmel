package io.github.jmurmel.recurs;

import java.math.BigInteger;

/**
 * Factorial implemented in CPS-style with manual trampolining
 *
 * see https://stackoverflow.com/questions/189725/what-is-a-trampoline-function
 */
class Trampoline<T> {
    public T get() { return null; }
    public Trampoline<T>  run() { return null; }

    T execute() {
        Trampoline<T> trampoline = this;

        while (trampoline.get() == null) {
            trampoline = trampoline.run();
        }

        return trampoline.get();
    }
}

public class Factorial {
    /*
      (defun factorial (n product)
        (if (<= n 1)
              product
          (factorial (- n 1) (* product n))))

      (factorial 5 1) ; ==> 120
     */
    public static Trampoline<BigInteger> factorial(final int n, final BigInteger product) {
        if(n <= 1) {
            return new Trampoline<BigInteger>() {
                @Override
                public BigInteger get() {
                    return product;
                }
            };
        }
        else {
            return new Trampoline<BigInteger>() {
                @Override
                public Trampoline<BigInteger> run() {
                    return factorial(n - 1, product.multiply(BigInteger.valueOf(n)));
                }
            };
        }
    }

    public static void main(String[] args) {
        final BigInteger result = factorial(100, BigInteger.ONE).execute();
        System.out.println(result);
    }
}