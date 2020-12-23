package com.robertmayer.lambdaj.embed;

import java.io.StringReader;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;

import static org.junit.Assert.assertEquals;

public class EmbedTest {

    @Test
    public void testInterpretedMurmelProgram() {
        LambdaJ interpreter = new LambdaJ();

        StringReader source = new StringReader("(defun myadd (lhs rhs) (+ lhs rhs))\n"
                                             + "(myadd 1 2)");
        LambdaJ.MurmelProgram prog = interpreter.formsToInterpretedProgram(source::read, () -> -1, s -> { return; });

        StringReader source2 = new StringReader("(myadd 3 4)");
        LambdaJ.MurmelProgram prog2 = interpreter.formsToInterpretedProgram(source2::read, () -> -1, s -> { return; });

        LambdaJ.MurmelFunction myAdd = prog.getFunction("myadd");
        LambdaJ.MurmelFunction myAdd2 = prog2.getFunction("myadd");

        // perform tests
        Object result = prog.body();
        assertEquals(3.0, result);

        Object result2 = prog2.body();
        assertEquals(7.0, result2);

        Object result3 = myAdd.apply(10L, 20L);
        assertEquals(30.0, result3);

        Object result4 = myAdd2.apply(30L, 40L);
        assertEquals(70.0, result4);

        // run prog again
        Object result5 = prog.body();
        assertEquals(3.0, result5);
    }
}
