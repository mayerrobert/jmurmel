package com.robertmayer.lambdaj.custom;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StringReader;
import java.util.HashMap;

import org.junit.Test;

import com.robertmayer.lambdaj.LambdaJ;
import com.robertmayer.lambdaj.LambdaJ.*;

public class SerializeTest {

    public static class SerializationParser implements Parser {
        private Object obj;
        private HashMap<LambdaJSymbol, LambdaJSymbol> symbols = new HashMap<>();

        public SerializationParser(byte[] serialized) throws IOException, ClassNotFoundException {
            obj = new ObjectInputStream(new ByteArrayInputStream(serialized)).readObject();
            internSymbols(obj);
        }

        private void internSymbols(Object o) {
            if (o instanceof ConsCell) {
                final ConsCell c = (ConsCell) o;
                if (c.car instanceof ConsCell) internSymbols(c.car);
                else if (c.car instanceof LambdaJSymbol) c.car = intern((LambdaJSymbol)c.car);

                if (c.cdr instanceof ConsCell) internSymbols(c.cdr);
                else if (c.cdr instanceof LambdaJSymbol) c.cdr = intern((LambdaJSymbol)c.cdr);
            }
        }

        @Override
        public Object readObj() {
            Object ret = obj;
            obj = null; // only return it once or interpreter will execute the same program forever
            return ret;
        }

        @Override
        public LambdaJSymbol intern(LambdaJSymbol symbol) {
            LambdaJSymbol prev = symbols.putIfAbsent(symbol, symbol);
            if (prev != null) return prev;
            return symbol;
        }
    }

    @Test
    public void testSerialize() throws Exception {
        ConsCell c = new ConsCell("symbol", "stringvalue");

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
        Object result = interp.interpretExpressions(myParser, (ObjectReader)null, (ObjectWriter)null, (CustomEnvironmentSupplier)null);
        assertEquals(3.0,  result);
    }

    private byte[] sExpToByteArray(LambdaJ interp, String sExp) throws IOException {
        ObjectReader parser = new SExpressionParser(new StringReader(sExp)::read);
        ByteArrayOutputStream s = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(s);
        os.writeObject(parser.readObj());
        return s.toByteArray();
    }
}
