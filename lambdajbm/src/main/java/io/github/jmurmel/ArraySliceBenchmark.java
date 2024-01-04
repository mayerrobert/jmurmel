package io.github.jmurmel;

import org.openjdk.jmh.annotations.*;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.TimeUnit;

/**
 * Benchmark that verifies that an "optimized" ArraySliceIterator is actually slower.
 *
 * Hilfe zum Aufruf:
 * java -jar target/benchmarks.jar -h
 *
 * Aufruf z.B. mit
 * java -jar target/benchmarks.jar ArraySliceBenchmark
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(3)
@State(value = Scope.Thread)
public class ArraySliceBenchmark {
    private final Object[] numbers = new Object[] { 1, 2, 3, 4, 5 };

    @Benchmark
    public int bmFor() {
        int sum = 0;
        for (final Iterator<Object> it = new ArraySliceIterator(numbers, 2); it.hasNext(); ) {
            final Object o = it.next();
            sum += (Integer)o;
        }
        return sum;
    }

    @Benchmark
    public int bmForOpt() {
        int sum = 0;
        for (final Iterator<Object> it = new OptArraySliceIterator(numbers, 2); it.hasNext(); ) {
            final Object o = it.next();
            sum += (Integer)o;
        }
        return sum;
    }



    private static final class ArraySliceIterator implements Iterator<Object> {
        private final Object[] arry;
        private int cursor;

        ArraySliceIterator(Object[] arry, int offset) {
            this.arry = arry;
            this.cursor = offset;
        }

        @Override
        public boolean hasNext() { return cursor < arry.length; }

        @Override
        public Object next() {
            if (cursor >= arry.length) throw new NoSuchElementException();
            return arry[cursor++];
        }
    }


    private static final class OptArraySliceIterator implements Iterator<Object> {
        private final Object[] arry;
        private final int len;
        private int cursor;

        OptArraySliceIterator(Object[] arry, int offset) {
            this.arry = arry;
            this.len = arry.length;
            this.cursor = offset;
        }

        @Override
        public boolean hasNext() { return cursor != -1; }

        @Override
        public Object next() {
            if (cursor == -1) throw new NoSuchElementException();
            final Object ret = arry[cursor++];
            if (cursor == len) cursor = -1;
            return ret;
        }
    }
}
