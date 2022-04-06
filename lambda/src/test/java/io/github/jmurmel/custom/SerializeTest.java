package io.github.jmurmel.custom;

import static org.junit.Assert.assertEquals;

import java.io.*;
import java.util.HashMap;

import io.github.jmurmel.LambdaJ;
import org.junit.Test;

public class SerializeTest {

    public static class SerializationParser implements LambdaJ.ObjectReader, LambdaJ.SymbolTable {
        private Object obj;
        private HashMap<LambdaJ.LambdaJSymbol, LambdaJ.LambdaJSymbol> symbols = new HashMap<>();

        public SerializationParser(byte[] serialized) throws IOException, ClassNotFoundException {
            obj = new ObjectInputStream(new ByteArrayInputStream(serialized)).readObject();
            internSymbols(obj);
        }

        private void internSymbols(Object o) {
            if (o instanceof LambdaJ.ConsCell) {
                final LambdaJ.ConsCell c = (LambdaJ.ConsCell) o;
                if (c.car() instanceof LambdaJ.ConsCell) internSymbols(c.car());
                else if (c.car() instanceof LambdaJ.LambdaJSymbol) c.rplaca(intern((LambdaJ.LambdaJSymbol)c.car()));

                if (c.cdr() instanceof LambdaJ.ConsCell) internSymbols(c.cdr());
                else if (c.cdr() instanceof LambdaJ.LambdaJSymbol) c.rplacd(intern((LambdaJ.LambdaJSymbol)c.cdr()));
            }
        }

        @Override
        public Object readObj() {
            Object ret = obj;
            obj = null; // only return it once or interpreter will execute the same program forever
            return ret;
        }

        @Override
        public LambdaJ.LambdaJSymbol intern(LambdaJ.LambdaJSymbol symbol) {
            LambdaJ.LambdaJSymbol prev = symbols.putIfAbsent(symbol, symbol);
            if (prev != null) return prev;
            return symbol;
        }
    }

    @Test
    public void testSerialize() throws Exception {
        LambdaJ.ConsCell c = LambdaJ.ConsCell.cons("symbol", "stringvalue");

        ByteArrayOutputStream s = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(s);
        os.writeObject(c);

        ObjectInputStream is = new ObjectInputStream(new ByteArrayInputStream(s.toByteArray()));

        LambdaJ.ConsCell readBack = (LambdaJ.ConsCell) is.readObject();
        assertEquals("symbol", readBack.car());
        assertEquals("stringvalue", readBack.cdr().toString());
    }

    @Test
    public void testSerializationParser() throws Exception {
        byte[] program = sExpToByteArray("(+ 1 2)");
        SerializationParser myParser = new SerializationParser(program);

        LambdaJ interp = new LambdaJ(myParser);

        Object result = interp.interpretExpressions(myParser, null, null, null);
        assertEquals(3.0,  result);
    }

    private static byte[] sExpToByteArray(String sExp) throws IOException {
        LambdaJ.ObjectReader parser = LambdaJ.makeReader(new StringReader(sExp)::read);
        ByteArrayOutputStream s = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(s);
        os.writeObject(parser.readObj());
        return s.toByteArray();
    }
}
