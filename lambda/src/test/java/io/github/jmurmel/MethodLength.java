package io.github.jmurmel;

// print method lengths
// see https://stackoverflow.com/questions/48338337/how-to-check-bytecode-length-of-java-method/48343150#48343150

import javassist.bytecode.ClassFile;
import javassist.bytecode.CodeAttribute;
import javassist.bytecode.MethodInfo;

import java.io.DataInputStream;
import java.io.InputStream;

public class MethodLength {
    
    public static void main(String[] args) throws Exception {
        pr(LambdaJ.class);
        pr(LambdaJ.SExpressionReader.class);
        pr(LambdaJ.Subr.class);
        pr(LambdaJ.MurmelJavaProgram.class);
    }
    
    private static void pr(Class<?> clazz) throws Exception {
        System.out.println(clazz.getName() + ": **********");
        final String fileName = clazz.getName().replaceFirst("io.github.jmurmel.", "") + ".class";

        try (InputStream is = clazz.getResourceAsStream(fileName)) {
            final ClassFile cf = new ClassFile(new DataInputStream(is));
            for(MethodInfo mi: cf.getMethods()) {
                final CodeAttribute ca = mi.getCodeAttribute();
                if (ca == null) continue; // abstract or native
                final int bLen = ca.getCode().length;
                if ((bLen >= 35 && bLen <= 39) || bLen >= 325)
                    System.out.println(mi.getName() + " " + mi.getDescriptor() + ": " + bLen + " bytes");
            }
        }
        System.out.println();
    }
}
