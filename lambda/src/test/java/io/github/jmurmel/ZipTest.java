package io.github.jmurmel;

import org.junit.Assert;
import org.testng.annotations.Test;

import static io.github.jmurmel.LambdaJ.ConsCell.cons;
import static io.github.jmurmel.LambdaJ.ConsCell.list;
import static io.github.jmurmel.LambdaJ.ConsCell.listStar;

@SuppressWarnings("ConstantConditions")
public class ZipTest {
    private final LambdaJ intp = new LambdaJ();
    private static final LambdaJ.LambdaJSymbol a = new LambdaJ.LambdaJSymbol("a");
    private static final LambdaJ.LambdaJSymbol b = new LambdaJ.LambdaJSymbol("b");
    private static final LambdaJ.LambdaJSymbol c = new LambdaJ.LambdaJSymbol("c");

    @Test
    public void test_NilParams_NilArgs_NilEnv() {
        final LambdaJ.ConsCell result = intp.zip("test", null, null, null, true);
        Assert.assertNull(result);
    }

    @Test
    public void test_NilParams_NilArgs_Env() {
        final LambdaJ.ConsCell env = list(cons(a, 1), cons(b, 2));
        final LambdaJ.ConsCell result = intp.zip("test", null, null, env, true);
        Assert.assertEquals(env, result);
        Assert.assertSame(env, result);
    }

    @Test
    public void test_Params_Args_Env() {
        final LambdaJ.ConsCell env = list(cons(a, 1), cons(b, 2));
        final LambdaJ.ConsCell result = intp.zip("test", list(c), list(11), env, true);
        Assert.assertEquals(listStar(cons(c, 11), env), result);
        Assert.assertSame(env, result.cdr());
    }

    @Test
    public void test_Params_Args_NilEnv() {
        final LambdaJ.ConsCell result = intp.zip("test", list(c), list(11), null, true);
        Assert.assertEquals(list(cons(c, 11)), result);
    }

    @Test
    public void test_ParamsVA_Args_Env() {
        final LambdaJ.ConsCell env = list(cons(a, 1), cons(b, 2));
        final LambdaJ.ConsCell result = intp.zip("test", c, list(11, 12, 13), env, true);
        Assert.assertEquals(listStar(cons(c, list(11, 12, 13)), env), result);
        Assert.assertSame(env, result.cdr());
    }

    @Test
    public void test_Params_Args_MatchingEnv() {
        final LambdaJ.ConsCell env = list(cons(a, 1), cons(b, 2), cons(c, 3));
        final LambdaJ.ConsCell result = intp.zip("test", list(a, b), list(11, 12), env, true);
        Assert.assertEquals(list(cons(a, 11), cons(b, 12), cons(c, 3)), result);
        Assert.assertSame(LambdaJ.cddr(env), LambdaJ.cddr(result));
        Assert.assertNotSame(env, result);
    }

    @Test(expectedExceptions = LambdaJ.ProgramError.class, expectedExceptionsMessageRegExp = "test: too many.*")
    public void test_NilParams_Args_NilEnv() {
        final LambdaJ.ConsCell result = intp.zip("test", null, list(2), null, true);
        Assert.assertNull(result);
    }

    @Test
    public void test_NilParams_Args_Env_DontMatch() {
        final LambdaJ.ConsCell env = list(cons(a, 1), cons(b, 2));
        final LambdaJ.ConsCell result = intp.zip("test", null, list(11, 12, 13), env, false);
        Assert.assertEquals(env, result);
        Assert.assertSame(env, result);
    }

    @Test(expectedExceptions = LambdaJ.ProgramError.class, expectedExceptionsMessageRegExp = "test: not enough.*")
    public void test_Params_NilArgs_NilEnv() {
        final LambdaJ.ConsCell result = intp.zip("test", list(a), null, null, true);
        Assert.assertNull(result);
    }

    @Test
    public void test_Params_NilArgs_Env_DontMatch() {
        final LambdaJ.ConsCell env = list(cons(a, 1), cons(b, 2));
        final LambdaJ.ConsCell result = intp.zip("test", list(c), null, env, false);
        Assert.assertEquals(listStar(cons(c, null), env), result);
    }
}
