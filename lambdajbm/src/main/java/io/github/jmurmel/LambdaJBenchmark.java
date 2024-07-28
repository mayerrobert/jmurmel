package io.github.jmurmel;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
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
 * java -jar target/benchmarks.jar LambdaJBenchmark.evalSame -prof stack:lines=3
 *
 * oder:
 * java -jar target/benchmarks.jar LambdaJBenchmark.evalSame -prof gc
 *
 * oder:
 * java  -jar target/benchmarks.jar -p prog=4 -prof stack:lines=3 LambdaJBenchmark.evalSame ... prog=4 laesst nur das factorial programm laufen
 *
 * oder:
 * java  -jar target/benchmarks.jar -p prog=4 LambdaJBenchmark.fiveTimesEval
 *
 * oder:
 * java -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining -XX:+PrintCompilation -jar target\benchmarks.jar
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = LambdaJBenchmark.WARMUP, time = 1)
@Measurement(iterations = LambdaJBenchmark.ITERATIONS, time = 1)
@Fork(LambdaJBenchmark.FORK)
@State(value = Scope.Thread)
public class LambdaJBenchmark {
    public static final int FORK = 5;       // default: 3
    public static final int WARMUP = 20;     // default: 5
    public static final int ITERATIONS = 5; // default: 10

    public static final String EMPTY_PROGRAM = "; empty program";
    public static final String SIMPLE_CONS = "(cons 'a 'b); simple cons";
    public static final String LAMBDA_AND_ADD_DOUBLE = "((lambda (x y) (+ x y)) 2 3); lambda and add double";
    public static final String TAILREC =
            "(labels ((print-last (list)\r\n" +
            "                     (if (null (cdr list))\r\n" +
            "                         (car list)\r\n" +
            "                         (print-last (cdr list)))))\r\n" +
            "        (print-last (quote (0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))))";
    public static final String FACT =
            "(labels ((factTR (n a)\r\n" +
            "                 (cond ((= n 0) a)\r\n" +
            "                       (t (factTR (- n 1) (* n a))))))\r\n" +
            "        (jformat-locale nil \"en-US\" \"Factorial of 50 is %g\" (factTR 50.0 1.0)))";

    public static final String TAK =
            "(labels ((tak´ (x y z)\n"
          + "           (cond ((null (< y x)) z)\n"
          + "                  (t (tak´ (tak´ (1- x) y z)\n"
          + "                           (tak´ (1- y) z x)\n"
          + "                           (tak´ (1- z) x y))))))\n" 
          + "  (tak´ 18 12 6))";

    public static final String[] PROGRAMS = {
            EMPTY_PROGRAM,
            SIMPLE_CONS,
            LAMBDA_AND_ADD_DOUBLE,
            TAILREC,
            FACT,
            TAK,
    };

    public static LambdaJ.MurmelProgram[] COMPILED_PROGRAMS;

    public static final String[] RESULTS = {
            "nil",
            "(a . b)",
            "5.0",
            "9.0",
            "Factorial of 50 is 3.04141e+64",
            "7",
    };

    @Param({
        "0", //EMPTY_PROGRAM,
        "1", //SIMPLE_CONS,
        "2", //LAMBDA_AND_ADD_DOUBLE,
        "3", //TAILREC,
        "4", //FACT
        "5", //TAK
    })
    private int prog;

    private LambdaJ interpreter;

    @Setup(Level.Trial)
    public static void compileAll() throws Exception {
        COMPILED_PROGRAMS = new LambdaJ.MurmelProgram[] {
                compile(EMPTY_PROGRAM),
                compile(SIMPLE_CONS),
                compile(LAMBDA_AND_ADD_DOUBLE),
                compile(TAILREC),
                compile(FACT),
                compile(TAK),
        };
    }

    @Setup(Level.Invocation)
    public void setup() {
        interpreter = new LambdaJ();
    }

    @Benchmark
    public Object evalSame() {
        return interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> {});
    }

    @Benchmark
    public Object evalNew() {
        return new LambdaJ().interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> {});
    }

    @Benchmark
    public Object create() {
        return new LambdaJ();
    }

    @Benchmark
    public void fiveTimesEval(Blackhole bh) {
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { }));
        bh.consume(interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { }));
    }

    @Benchmark
    public void compiled(Blackhole bh) {
        final LambdaJ.MurmelProgram murmelProgram = COMPILED_PROGRAMS[prog];
        bh.consume(murmelProgram.body());
        bh.consume(murmelProgram.body());
        bh.consume(murmelProgram.body());
    }

    private static LambdaJ.MurmelProgram compile(String source) throws Exception {
        final LambdaJ.MurmelJavaCompiler c = new LambdaJ.MurmelJavaCompiler(null, null, getTmpDir());
        final Reader reader = new StringReader(source);
        final LambdaJ.ObjectReader parser = LambdaJ.makeReader(reader::read, c.getSymbolTable(), null);

        final Class<LambdaJ.MurmelProgram> murmelClass = c.formsToJavaClass("Test", parser, null);

        return murmelClass.getDeclaredConstructor().newInstance();
    }

    static Path getTmpDir() throws IOException {
        final Path tmpDir = Files.createTempDirectory("jmurmelbm");
        tmpDir.toFile().deleteOnExit();
        return tmpDir;
    }
}
