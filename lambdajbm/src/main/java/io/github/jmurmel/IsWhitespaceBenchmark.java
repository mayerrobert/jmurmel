package io.github.jmurmel;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Random;
import java.util.concurrent.TimeUnit;

/**
 * Hilfe zum Aufruf:
 * java -jar target/benchmarks.jar -h
 *
 * Aufruf z.B. mit
 * java -jar target/benchmarks.jar IsWhitespaceBenchmark
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(3)
@State(value = Scope.Thread)
public class IsWhitespaceBenchmark {
    private static final int N;
    private static final int[] randomCharacters;
    private static final int[] mlibCharacters;

    static {
        try {
            final Path p = Paths.get("../samples.mlib/mlib.lisp");
            final String mlib = JavaUtil.readString(p, StandardCharsets.UTF_8);
            mlibCharacters = new int[mlib.length()];
            int wsMlib = 0;
            for (int i = 0; i < mlib.length(); i++) {
                mlibCharacters[i] = mlib.charAt(i);
                if (Character.isWhitespace(mlibCharacters[i])) wsMlib++;
            }
            System.out.printf("==========> %d/%d (%.1f%%) characters in mlib.lisp are whitespace%n", wsMlib, mlibCharacters.length, 100.0 * wsMlib / mlibCharacters.length);
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        N = mlibCharacters.length;

        randomCharacters = new int[N];
        final Random rnd = new Random(123L);
        int ws = 0;
        for (int i = 0; i < N; i++) {
            randomCharacters[i] = rnd.nextInt(128);
            if (Character.isWhitespace(randomCharacters[i])) ws++;
        }
        System.out.printf("==========> %d/%d (%.1f%%) random characters are whitespace%n", ws, N, 100.0 * ws / N);
    }

    @Benchmark
    public void isWhitespaceLinearCCC(Blackhole bh) {
        for (int i = 0; i < N; i++) {
            bh.consume(isWhitespaceCCC(i));
        }
    }

    @Benchmark
    public void isWhitespaceRndCCC(Blackhole bh) {
        final int[] c = randomCharacters;
        for (int i = 0; i < N; i++) {
            bh.consume(isWhitespaceCCC(c[i]));
        }
    }

    @Benchmark
    public void isWhitespaceMlibCCC(Blackhole bh) {
        final int[] c = mlibCharacters;
        for (int i = 0; i < c.length; i++) {
            bh.consume(isWhitespaceCCC(c[i]));
        }
    }


    @Benchmark
    public void isWhitespaceLinearJava(Blackhole bh) {
        for (int i = 0; i < N; i++) {
            bh.consume(Character.isWhitespace(i));
        }
    }

    @Benchmark
    public void isWhitespaceRndJava(Blackhole bh) {
        final int[] c = randomCharacters;
        for (int i = 0; i < N; i++) {
            bh.consume(Character.isWhitespace(c[i]));
        }
    }

    @Benchmark
    public void isWhitespaceMlibJava(Blackhole bh) {
        final int[] c = mlibCharacters;
        for (int i = 0; i < c.length; i++) {
            bh.consume(Character.isWhitespace(c[i]));
        }
    }


    // doesn't work for non-ascii whitespace
    @Benchmark
    public void isWhitespaceLinearOldJava(Blackhole bh) {
        for (int i = 0; i < N; i++) {
            bh.consume(Character.isSpace((char)i));
        }
    }

    @Benchmark
    public void isWhitespaceRndOldJava(Blackhole bh) {
        final int[] c = randomCharacters;
        for (int i = 0; i < N; i++) {
            bh.consume(Character.isSpace((char)c[i])); // the (char) cast needs an extra I2C bytecode
        }
    }

    @Benchmark
    public void isWhitespaceMlibOldJava(Blackhole bh) {
        final int[] c = mlibCharacters;
        for (int i = 0; i < c.length; i++) {
            bh.consume(Character.isSpace((char)c[i])); // the (char) cast needs an extra I2C bytecode
        }
    }


    // doesn't work for non-ascii whitespace
    @Benchmark
    public void isWhitespaceLinearOldMurmel(Blackhole bh) {
        for (int i = 0; i < N; i++) {
            bh.consume(isWhitespaceOldMurmel(i));
        }
    }

    @Benchmark
    public void isWhitespaceRndOldMurmel(Blackhole bh) {
        final int[] c = randomCharacters;
        for (int i = 0; i < N; i++) {
            bh.consume(isWhitespaceOldMurmel(c[i]));
        }
    }

    @Benchmark
    public void isWhitespaceMlibOldMurmel(Blackhole bh) {
        final int[] c = mlibCharacters;
        for (int i = 0; i < c.length; i++) {
            bh.consume(isWhitespaceOldMurmel(c[i]));
        }
    }


    static boolean isWhitespaceOldMurmel(int x) {
        return x == ' ' || x == '\t' || x == '\n' || x == '\r';
    }

    static boolean isWhitespaceCCC(int ch) {
        if (ch < 128) {
            // from: Coffee Compiler Club, 2023_12_29
            // optimize for the ASCII range
            //
            // this handles the following cases:
            //   U+0009   9  HT   Horizontal Tab
            //   U+000A  10  LF   Line Feed
            //   U+000B  11  VT   Vertical Tab
            //   U+000C  12  FF   Form Feed
            //   U+000D  13  CR   Carriage Return
            //   U+001A  26  SUB  End-of-File, or "control-Z"
            //   U+001C  28  FS   File Separator
            //   U+001D  29  GS   Group Separator
            //   U+001E  30  RS   Record Separator
            //   U+001F  31  US   Unit Separator
            //   U+0020  32  SP   Space
            //                                               2               1      0
            //                                               0FEDCBA9876543210FEDCBA9
            return ch <= 32 && ch >= 9 && ((1 << (ch-9)) & 0b111110100000000000011111) != 0;
        }

        // this handles the following cases:
        //   U+0085    133  NEL     Next Line
        //   U+00A0    160  &nbsp;  Non-breaking space
        //   U+1680   5760          Ogham Space Mark
        //   U+2000   8192          En Quad
        //   U+2001   8193          Em Quad
        //   U+2002   8194          En Space
        //   U+2003   8195          Em Space
        //   U+2004   8196          Three-Per-Em Space
        //   U+2005   8197          Four-Per-Em Space
        //   U+2006   8198          Six-Per-Em Space
        //   U+2007   8199          Figure Space
        //   U+2008   8200          Punctuation Space
        //   U+2009   8201          Thin Space
        //   U+200A   8202          Hair Space
        //   U+2028   8232   LS     Line Separator
        //   U+2029   8233   PS     Paragraph Separator
        //   U+202F   8239          Narrow No-Break Space
        switch (ch) {
        case 0x0085:
        case 0x00A0:
        case 0x1680:
        case 0x2000:
        case 0x2001:
        case 0x2002:
        case 0x2003:
        case 0x2004:
        case 0x2005:
        case 0x2006:
        case 0x2007:
        case 0x2008:
        case 0x2009:
        case 0x200A:
        case 0x2028:
        case 0x2029:
        case 0x202F:
        case 0x205F:
        case 0x3000:
            return true;

        default:
            return false;
        }
    }
}
