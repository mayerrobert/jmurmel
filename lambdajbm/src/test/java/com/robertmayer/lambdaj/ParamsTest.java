package com.robertmayer.lambdaj;

import java.io.StringReader;

import org.junit.Test;

import static org.junit.Assert.*;
import static com.robertmayer.lambdaj.LambdaJBenchmark.*;

/** Check if the Benchmark programs actually work.
 *  Also "Coverage As-> JUnit Test" will show which parts of LambdaJ are actually touched by the benchmark. */
public class ParamsTest {

    @Test
    public void runPrograms() {
        for (int prog = 0; prog < PROGRAMS.length; prog++) {
            LambdaJ interpreter = new LambdaJ();
            run(prog, interpreter);
        }
    }

    // check if multiple invocations on the same interpreter object deliver correct results
    @Test
    public void runProgramsFiveTimes() {
        for (int prog = 0; prog < PROGRAMS.length; prog++) {
            LambdaJ interpreter = new LambdaJ();
            for (int n = 0; n < 5; n++)
                run(prog, interpreter);
        }
    }

    private void run(int prog, LambdaJ interpreter) {
        Object result = interpreter.interpretExpression(new StringReader(PROGRAMS[prog])::read, (s) -> { return; });
        if (result == null) result = "nil";
        assertEquals("prog " + prog, RESULTS[prog], result.toString());
    }
}
