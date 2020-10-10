package com.robertmayer.lambdaj;

import java.io.StringReader;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.*;

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
 * java -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining -XX:+PrintCompilation -jar target\benchmarks.jar
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
//@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@State(value = Scope.Thread)
public class LambdaJBenchmark {

    @Param({ "", "(cons 'a 'b)", "((lambda (x y) (+ x y)) 2 3)" })
    private String prog;

    private LambdaJ interpreter;

    @Setup
    public void setup() {
        interpreter = new LambdaJ();
    }

    @Benchmark
    public Object eval() {
        return interpreter.interpretExpression(new StringReader(prog)::read, (s) -> { return; });
    }
}
