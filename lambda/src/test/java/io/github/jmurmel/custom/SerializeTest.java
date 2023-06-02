package io.github.jmurmel.custom;

import static org.junit.Assert.assertEquals;

import java.io.*;
import java.util.HashMap;
import java.util.Iterator;

import io.github.jmurmel.LambdaJ;
import jakarta.validation.constraints.NotNull;
import org.junit.Test;

public class SerializeTest {

    public static class SerializationSymbolTable implements LambdaJ.SymbolTable {
        private final HashMap<String, LambdaJ.LambdaJSymbol> symbols = new HashMap<>();

        @Override
        public LambdaJ.LambdaJSymbol intern(@NotNull LambdaJ.LambdaJSymbol symbol) {
            final LambdaJ.LambdaJSymbol prev = symbols.putIfAbsent(symbol.toString(), symbol);
            if (prev != null) return prev;
            return symbol;
        }

        @Override
        public Iterator<LambdaJ.LambdaJSymbol> iterator() {
            return symbols.values().iterator();
        }
    }

    public static class SerializationParser implements LambdaJ.ObjectReader {
        private LambdaJ.SymbolTable st;
        private byte[] serialized;

        public SerializationParser(LambdaJ.SymbolTable st, byte[] serialized) {
            this.st = st;
            this.serialized = serialized;
        }

        private void internSymbols(Object o) {
            if (o instanceof LambdaJ.ConsCell) {
                final LambdaJ.ConsCell c = (LambdaJ.ConsCell) o;
                if (c.car() instanceof LambdaJ.ConsCell) internSymbols(c.car());
                else if (c.car() instanceof LambdaJ.LambdaJSymbol) c.rplaca(st.intern((LambdaJ.LambdaJSymbol)c.car()));

                if (c.cdr() instanceof LambdaJ.ConsCell) internSymbols(c.cdr());
                else if (c.cdr() instanceof LambdaJ.LambdaJSymbol) c.rplacd(st.intern((LambdaJ.LambdaJSymbol)c.cdr()));
            }
        }

        @Override
        public Object readObj(Object eof) {
            if (serialized == null) return eof;
            try {
                final Object obj;
                obj = new ObjectInputStream(new ByteArrayInputStream(serialized)).readObject();
                internSymbols(obj);
                serialized = null; // only return it once or interpreter will execute the same program forever
                return obj;
            }
            catch (IOException | ClassNotFoundException e) {
                throw new LambdaJ.LambdaJError(true, "caught exception %s", e.getMessage(), e);
            }
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
        final SerializationSymbolTable st = new SerializationSymbolTable();
        final byte[] program = sExpToByteArray(st, "(+ 1 2)");
        final SerializationParser myParser = new SerializationParser(st, program);

        final LambdaJ interp = new LambdaJ(st);

        final Object result = interp.interpretExpressions(myParser, null, null, null);
        assertEquals(3.0,  result);
    }

    private static byte[] sExpToByteArray(LambdaJ.SymbolTable st, String sExp) throws IOException {
        final LambdaJ.ObjectReader parser = LambdaJ.makeReader(new StringReader(sExp)::read, st, null);
        final ByteArrayOutputStream s = new ByteArrayOutputStream();
        final ObjectOutputStream os = new ObjectOutputStream(s);
        os.writeObject(parser.readObj(null));
        return s.toByteArray();
    }
}
