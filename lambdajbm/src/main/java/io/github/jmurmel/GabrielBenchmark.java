package io.github.jmurmel;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;


/**
 * Hilfe zum Aufruf:
 * java -jar target/benchmarks.jar -h
 *
 * Aufruf z.B. mit
 * java -jar target/benchmarks.jar GabrielBenchmark
 *
 * oder:
 * java -jar target/benchmarks.jar GabrielBenchmark -prof stack:lines=3
 *
 * oder:
 * java -jar target/benchmarks.jar GabrielBenchmark -prof gc
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = GabrielBenchmark.WARMUP, time = 1)
@Measurement(iterations = GabrielBenchmark.ITERATIONS, time = 1)
@Fork(GabrielBenchmark.FORK)
@State(value = Scope.Thread)
public class GabrielBenchmark {
    public static final int FORK = 3;        // default: 3
    public static final int WARMUP = 10;     // default: 5
    public static final int ITERATIONS = 5;  // default: 10

    private static final String[] ALL_FILES = {
    "all.lisp",
    "000_lib",
    "00_tak_apo",
    "3.01_tak.lisp",
    "3.01_takeuchi",
    "3.02_stak",
    "3.04_takl",
    "3.10_deriv",
    };

    private static Path prog;
    private static String source, allSourceFiles;

    private static LambdaJ.MurmelProgram compiled;

    private static LambdaJ interpreter;


    @Setup(Level.Trial)
    public static void setup() throws Exception {
        prog = Paths.get("../samples.murmel/gabriel/all.lisp");
        source = JavaUtil.readString(prog, StandardCharsets.UTF_8);

        final StringBuilder sb = new StringBuilder();
        for (String fileName: ALL_FILES) {
            final Path p = Paths.get("../samples.murmel/gabriel/" + fileName);
            sb.append(JavaUtil.readString(prog, StandardCharsets.UTF_8));
        }
        allSourceFiles = sb.toString();

        final LambdaJ.MurmelJavaCompiler c = new LambdaJ.MurmelJavaCompiler(null, null, LambdaJBenchmark.getTmpDir());
        final Reader reader = new StringReader(source);
        final LambdaJ.ObjectReader parser = new LambdaJ.SExpressionReader(reader::read, c.getSymbolTable(), null, prog);
        final Class<LambdaJ.MurmelProgram> murmelClass = c.formsToJavaClass("Test", parser, null);
        compiled =  murmelClass.getDeclaredConstructor().newInstance();

        interpreter = new LambdaJ();
    }

    @Benchmark
    public void read(Blackhole bh) {
        final Reader reader = new StringReader(allSourceFiles);
        final LambdaJ.ObjectReader parser = interpreter.makeReader(reader::read, prog);
        final Object eof = new Object();
        Object form;
        while (eof != (form = parser.readObj(true, eof))) bh.consume(form);
    }

    @Benchmark
    public Object readAndInterpret() {
        final Reader reader = new StringReader(source);
        final LambdaJ.ObjectReader parser = interpreter.makeReader(reader::read, prog);
        return interpreter.interpretExpressions(parser, null, null, null);
    }

    @Benchmark
    public Object run() {
        return compiled.body();
    }

    public static void main(String[] args) throws Exception {
        setup();
        new GabrielBenchmark().readAndInterpret();
    }
}
