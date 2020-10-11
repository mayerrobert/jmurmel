package com.robertmayer.lambdaj;

import java.io.StringReader;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Hilfe zum Aufruf:
 * java -jar target/benchmarks.jar -h
 *
 * Aufruf z.B. mit
 * java -jar target/benchmarks.jar
 *
 * oder:
 * java -jar target/benchmarks.jar LambdaJBenchmark -prof stack:lines=3
 *
 * oder:
 * java -jar target/benchmarks.jar LambdaJBenchmark -prof gc
 *
 * oder:
 * java  -jar target/benchmarks.jar -p prog=4 -prof stack:lines=3 ... prog=4 laesst nur das factorial programm laufen
 *
 * oder:
 * java -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining -XX:+PrintCompilation -jar target\benchmarks.jar
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = LambdaJBenchmark.WARMUP, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = LambdaJBenchmark.ITERATIONS, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(LambdaJBenchmark.FORK)
@State(value = Scope.Thread)
public class LambdaJBenchmark {
    public static final int FORK = 3;       // default: 3
    public static final int WARMUP = 5;     // default: 5
    public static final int ITERATIONS = 10; // default: 10

    public static final String EMPTY_PROGRAM = "; empty program";
    public static final String SIMPLE_CONS = "(cons 'a 'b); simple cons";
    public static final String LAMBDA_AND_ADD_DOUBLE = "((lambda (x y) (+ x y)) 2 3); lambda and add double";
    public static final String TAILREC = "(labels ((print-last (list)\r\n" +
    "              (if (null? (cdr list))\r\n" +
    "                  (car list)\r\n" +
    "                  (print-last (cdr list)))))\r\n" +
    "    (print-last (quote (0 1 2 3 4 5 6 7 8 9))))";
    public static final String FACT = "(labels ((factTR (n a)\r\n" +
            "                 (cond ((= n 0) a)\r\n" +
            "                       (t (factTR (- n 1) (* n a))))))\r\n" +
            " (string-format \"Factorial of 50 is %g\" (factTR 50 1)))";

    public static final String[] PROGRAMS = {
            EMPTY_PROGRAM,
            SIMPLE_CONS,
            LAMBDA_AND_ADD_DOUBLE,
            TAILREC,
            FACT,
    };

    public static final String[] RESULTS = {
            "nil",
            "(a . b)",
            "5.0",
            "9.0",
            "Factorial of 50 is 3,04141e+64", // the decimal char is locale depedent
    };

    @Param({
        "0", //EMPTY_PROGRAM,
        "1", //SIMPLE_CONS,
        "2", //LAMBDA_AND_ADD_DOUBLE,
        "3", //TAILREC,
        "4", //FACT
    })
    private int prog;

    private LambdaJ interpreter;

    @Setup(Level.Invocation)
    public void setup() {
        interpreter = new LambdaJ();
    }

    @Benchmark
    public Object eval() {
        return interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; });
    }

    @Benchmark
    public void evalFiveTimes(Blackhole bh) {
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; }));
    }
}
