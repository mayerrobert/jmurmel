package io.github.jmurmel;

import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(3)
@State(value = Scope.Thread)
public class StringBuilderBenchmark {
    private StringBuilder sb;

    @Setup(Level.Invocation)
    public void setup() {
        sb = new StringBuilder();
    }

    @Benchmark
    public StringBuilder appendString() {
        return sb.append("12");
    }

    /**
     * on Java8 appending two chars is faster than appending a two-character string.
     * In Java11+ Hotspot will use an intrinsic for StringBuilder.append(String)
     * so handoptimizing by appending two characters is actually slower.
     */
    @Benchmark
    public StringBuilder appendTwoChars() {
        return sb.append('1').append('2');
    }
}
