package io.github.jmurmel.embed;

import org.junit.Test;

import io.github.jmurmel.LambdaJ;

import static org.junit.Assert.assertEquals;

public class EmbedTest {

    @Test
    public void testInterpretedMurmelProgram() {
        LambdaJ interpreter = new LambdaJ();

        String source = "(defun myadd (lhs rhs) (+ lhs rhs))\n"
                                             + "(myadd 1 2)";
        LambdaJ.MurmelProgram prog = interpreter.formsToInterpretedProgram(source, () -> -1, s -> { return; });


        // perform tests
        Object result = prog.body();
        assertEquals(3.0, result);

        LambdaJ.MurmelFunction myAdd = prog.getFunction("myadd");
        Object result3 = myAdd.apply(10L, 20L);
        assertEquals(30.0, result3);


        // run prog again
        Object result5 = prog.body();
        assertEquals(3.0, result5);
    }
}
