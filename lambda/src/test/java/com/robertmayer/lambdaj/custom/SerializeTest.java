package com.robertmayer.lambdaj.custom;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StringReader;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;
import com.robertmayer.lambdaj.LambdaJ.*;

public class SerializeTest {

    public static class SerializationParser implements Parser {
        Object obj;

        public SerializationParser(byte[] serialized) throws IOException, ClassNotFoundException {
            obj = new ObjectInputStream(new ByteArrayInputStream(serialized)).readObject();
            internSymbols(obj);
        }

        private void internSymbols(Object o) {
            if (o instanceof ConsCell) {
                final ConsCell c = (ConsCell) o;
                if (c.car instanceof ConsCell) internSymbols(c.car);
                else if (c.car instanceof String) c.car = ((String)c.car).intern();

                if (c.cdr instanceof ConsCell) internSymbols(c.cdr);
                else if (c.car instanceof String) c.car = ((String)c.car).intern();
            }
        }

        @Override
        public Object readObj() {
            Object ret = obj;
            obj = null; // only return it once or interpreter will execute the same program forever
            return ret;
        }

        @Override
        public Object intern(String symbol) {
            return symbol.intern();
        }
    }

    @Test
    public void testSerialize() throws Exception {
        ConsCell c = new ConsCell("symbol", new LambdaJString("stringvalue"));

        ByteArrayOutputStream s = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(s);
        os.writeObject(c);

        ObjectInputStream is = new ObjectInputStream(new ByteArrayInputStream(s.toByteArray()));

        ConsCell readBack = (ConsCell) is.readObject();
        assertEquals("symbol", readBack.car);
        assertEquals("stringvalue", readBack.cdr.toString());
    }

    @Test
    public void testSerializationParser() throws Exception {
        LambdaJ interp = new LambdaJ();
        byte[] program = sExpToByteArray(interp, "(+ 1 2)");

        SerializationParser myParser = new SerializationParser(program);
        Object result = interp.interpretExpressions(myParser, (ObjectReader)null, (ObjectWriter)null, (CustomBuiltinsSupplier)null);
        assertEquals(3.0,  result);
    }

    private byte[] sExpToByteArray(LambdaJ interp, String sExp) throws IOException {
        ObjectReader parser = interp.new SExpressionParser(new StringReader(sExp)::read);
        ByteArrayOutputStream s = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(s);
        os.writeObject(parser.readObj());
        return s.toByteArray();
    }
}
