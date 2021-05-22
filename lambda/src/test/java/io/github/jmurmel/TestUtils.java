package io.github.jmurmel;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import io.github.jmurmel.LambdaJ.ConsCell;

public class TestUtils {

    static String sexp(Object exp) {
        StringBuilder sExp = new StringBuilder();
        LambdaJ.makeWriter(sExp::append).printObj(exp);
        return sExp.toString();

    }

    static ConsCell cdr(Object l) {
        return (ConsCell)((ConsCell)l).cdr();
    }

    static Path getTmpDir() throws IOException {
        Path tmpDir = Files.createTempDirectory("jmurmeltest");
        tmpDir.toFile().deleteOnExit();
        return tmpDir;
    }
}
