package io.github.jmurmel;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import static io.github.jmurmel.LambdaJ.Subr.*;

public class DecimalTest {
    static String dtoa(Number arg, char style, boolean forceSign, int w, int d) {
        switch (style) {
        case 'e':
        case 'E':
            return "todo";

        case 'f':
        case 'F':
            if (w == -1 && d == -1) {
                if (forceSign && cl_signum(arg).intValue() > 0) return '+' + Double.toString(arg.doubleValue());
                else return Double.toString(arg.doubleValue());
            }
            return dtoaF(arg, forceSign, w, d);

        case 'g':
        case 'G':
            return "todo";

        default: return "todo";
        }
    }

    static String dtoaF(Number arg, boolean forceSign, int w, int d) {

        final DecimalFormat f = new DecimalFormat("", DecimalFormatSymbols.getInstance(Locale.US));
        f.setDecimalSeparatorAlwaysShown(true);
        f.setGroupingUsed(false);
        f.setMinimumFractionDigits(1);

        if (forceSign) f.setPositivePrefix("+");

        if (d >= 0) f.setMaximumFractionDigits(d);

        final String ret = f.format(arg);
        if (w > 0 && ret.length() < w) {
            final StringBuilder sb = new StringBuilder(w);
            for (int i = 0; i < w - ret.length(); i++) {
                sb.append(' ');
            }
            sb.append(ret);
            return sb.toString();
        }
        return ret;
    }

    @DataProvider(name = "test")
    public Object[][] createdata() {
        return new Object[][] {
            { 123,     'f', false, -1, -1, "123.0",   },
            { 123.0,   'f', false, -1, -1, "123.0",   },
            
            { 123,     'f', true,  -1, -1, "+123.0",  },
            { 123.0,   'f', true,  -1, -1, "+123.0",  },
            
            { 123.456, 'f', false, -1,  2, "123.46",  },
            { 123.456, 'f', true,  -1,  2, "+123.46", },
        };
    }

    @Test(dataProvider = "test")
    public void testDtoa(Number arg, char style, boolean forceSign, int w, int d, String expected) {
        Assert.assertEquals(dtoa(arg, style, forceSign, w, d), expected);
    }
}
