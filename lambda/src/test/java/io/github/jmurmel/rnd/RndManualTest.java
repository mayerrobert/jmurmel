package io.github.jmurmel.rnd;

import org.junit.Test;

import java.io.*;
import java.util.Base64;
import java.util.Random;

import static org.junit.Assert.assertNotNull;

public class RndManualTest {
    
    @Test
    public void readWriteRnd() throws Exception {
        final Random rnd = new Random();

        final ByteArrayOutputStream bo = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(bo);
        oos.writeObject(rnd);
        oos.close();
        final byte[] rndAsBytes = bo.toByteArray();
        final String rndAsString = Base64.getMimeEncoder().encodeToString(rndAsBytes);

        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(Base64.getMimeDecoder().decode(rndAsString)));
        final Object rnd2 = ois.readObject();
        assertNotNull(rnd2);
    }
}
