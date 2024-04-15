package io.github.jmurmel;


import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static io.github.jmurmel.LambdaJ.ConsCell.list;
import static io.github.jmurmel.LambdaJ.ConsCell.listStar;
import static io.github.jmurmel.LambdaJ.printSEx;

class Rt extends LambdaJ.MurmelJavaProgram {
    @Override
    protected Object runbody() {
        return null;
    }
}

// pitest with:
// mvn test -Ppitest -pl lambda -DwithHistory=true -DtargetClasses=io.github.jmurmel.LambdaJ$Subr,io.github.jmurmel.LambdaJ$MurmelJavaProgram -DtargetTests=io.github.jmurmel.AppendPrimitiveTest
public class AppendPrimitiveTest {
    private static final LambdaJ intp = new LambdaJ();
    private static final LambdaJ.MurmelJavaProgram rt = new Rt();

    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
        { new Object[] {}, "nil" },
        { new Object[] { null, null, null }, "nil" },
        { new Object[] { 1 }, "1" },
        { new Object[] { list(1, 2, 3), 11 }, "(1 2 3 . 11)" },
        { new Object[] { null, list(1, 2, 3), null, null, 11 }, "(1 2 3 . 11)" },
        };
    }

    @Test(dataProvider = "test")
    public void testIntp(Object[] items, Object expectedResult) {
        //System.out.println(printSEx(items));

        final String result = printSEx(LambdaJ.Subr.append(intp, LambdaJ.arraySlice(items))).toString();
        Assert.assertEquals(result, expectedResult);
    }

    @Test(dataProvider = "test")
    public void testRt(Object[] items, Object expectedResult) {
        //System.out.println(printSEx(items));

        final String result = printSEx(rt._append(items)).toString();
        Assert.assertEquals(result, expectedResult);
    }

    @DataProvider(name = "error")
    public Object[][] createErrorData() {
        return new Object[][] {
        { new Object[] { 1, 2 } },
        { new Object[] { null, list(1, 2), null, 22, list(111, 222, 333) } },
        { new Object[] { null, listStar(1, 2), null, 22, list(111, 222, 333) } },
        };
    }

    @Test(dataProvider = "error", expectedExceptions = LambdaJ.SimpleTypeError.class)
    public void testIntpError(Object[] items) {
        LambdaJ.Subr.append(intp, LambdaJ.arraySlice(items));
    }

    @Test(dataProvider = "error", expectedExceptions = LambdaJ.SimpleTypeError.class)
    public void testRtError(Object[] items) {
        rt._append(items);
    }
}
