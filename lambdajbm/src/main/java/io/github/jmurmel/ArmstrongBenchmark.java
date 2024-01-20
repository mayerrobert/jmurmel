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
 * java -jar target/benchmarks.jar ArmstrongBenchmark
 *
 * oder:
 * java -jar target/benchmarks.jar ArmstrongBenchmark -prof stack:lines=3
 *
 * oder:
 * java -jar target/benchmarks.jar ArmstrongBenchmark -prof gc
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = ArmstrongBenchmark.WARMUP, time = 1)
@Measurement(iterations = ArmstrongBenchmark.ITERATIONS, time = 1)
@Fork(ArmstrongBenchmark.FORK)
@State(value = Scope.Thread)
public class ArmstrongBenchmark {
    public static final int FORK = 3;        // default: 3
    public static final int WARMUP = 10;     // default: 5
    public static final int ITERATIONS = 5;  // default: 10

    private static final LambdaJ.ObjectWriter DEV_NULL = LambdaJ.makeWriter(LambdaJ.NULL_WRITECHARS);
    private static LambdaJ.MurmelProgram compiled;


    @Setup(Level.Trial)
    public static void setup() throws Exception {
        final Path prog = Paths.get("../samples.mlib/pom.xml");
        final String source = "(require \"mlib\")\n"
                              + "(declaim (optimize (debug 0)))\n"
                              + "\n"
                              + "(defmacro pr items\n"
                              + "  \"Helper that will print each argument and then a newline.\"\n"
                              + "  `(progn ,@(mapcar (lambda (it)\n"
                              + "                      (list 'write it nil))\n"
                              + "                    items)\n"
                              + "          (writeln)))\n" 
                              + "(defun arms2 ()\n"
                              + "  \"Print all Armstrong numbers from 0 to 999.\"\n"
                              + "  (let ((count 0))\n"
                              + "    (dotimes (abc 1000)\n"
                              + "      (let ((a3b3c3 (+ (expt (floor abc 100) 3)\n"
                              + "                       (expt (mod (floor abc 10) 10) 3)\n"
                              + "                       (expt (mod abc 10) 3))))\n"
                              + "        (when (= abc a3b3c3)\n"
                              + "          (incf count)\n"
                              + "          (pr \"Armstrong number \" count \": \" abc))))))\n"
                              + "\n"
                              + "(arms2)\n";

        final LambdaJ.MurmelJavaCompiler c = new LambdaJ.MurmelJavaCompiler(null, null, LambdaJBenchmark.getTmpDir());
        final Reader reader = new StringReader(source);
        final LambdaJ.ObjectReader parser = new LambdaJ.SExpressionReader(reader::read, c.getSymbolTable(), null, prog);
        final Class<LambdaJ.MurmelProgram> murmelClass = c.formsToJavaClass("Test", parser, null);
        compiled =  murmelClass.getDeclaredConstructor().newInstance();
    }

    @Benchmark
    public Object run(Blackhole bh) {
        compiled.setReaderPrinter(null, LambdaJ.makeWriter(bh::consume));
        return compiled.body();
    }

    public Object run() {
        compiled.setReaderPrinter(null, LambdaJ.makeWriter(System.out::append));
        return compiled.body();
    }

    public static void main(String[] args) throws Exception {
        setup();
        new ArmstrongBenchmark().run();
    }
}
