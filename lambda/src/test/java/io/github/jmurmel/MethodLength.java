package io.github.jmurmel;

// print method lengths and some other class information
// see https://stackoverflow.com/questions/48338337/how-to-check-bytecode-length-of-java-method/48343150#48343150

import javassist.bytecode.AccessFlag;
import javassist.bytecode.ClassFile;
import javassist.bytecode.CodeAttribute;
import javassist.bytecode.MethodInfo;

import java.io.DataInputStream;
import java.io.InputStream;
import java.util.Objects;
import java.util.stream.Collectors;

public final class MethodLength {

    private MethodLength() {}

    public static void main(String[] args) throws Exception {
        pr(LambdaJ.class);
        pr(LambdaJ.SExpressionReader.class);
        pr(LambdaJ.Chk.class);
        pr(LambdaJ.Subr.class);
        pr(LambdaJ.MurmelJavaProgram.class);

        pr(LambdaJ.Subr.class, "slength");
        pr(LambdaJ.Chk.class, "requireCharsequence");
        pr(LambdaJ.class, "requireString");
        pr(LambdaJ.class, "requireList");
    }

    private static void pr(Class<?> clazz) throws Exception {
        System.out.println(clazz.getName() + ": **********");
        final String fileName = clazz.getName().substring("io.github.jmurmel.".length()) + ".class";

        try (InputStream is = Objects.requireNonNull(clazz.getResourceAsStream(fileName), "cannot open class file")) {
            final ClassFile cf = new ClassFile(new DataInputStream(is));

            System.out.println(cf.getMethods().size() + " methods total");
            System.out.print(cf.getMethods().stream()
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.SYNTHETIC) != 0)
                                 .count()
                               + " synthetic static or instance methods: ");
            System.out.println(cf.getMethods().stream()
                               .filter(f -> (f.getAccessFlags() & AccessFlag.SYNTHETIC) != 0)
                               .map(MethodLength::fmtSyntheticMethos)
                               .collect(Collectors.joining("\n  ")));
            System.out.println(cf.getMethods().stream()
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.SYNTHETIC) == 0)
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.STATIC) != 0)
                                 .count()
                               + " non-synthetic static methods");
            System.out.println(cf.getMethods().stream()
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.SYNTHETIC) == 0)
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.STATIC) == 0)
                                 .count()
                               + " non-synthetic instance methods");

            System.out.println(cf.getFields().size() + " fields total");
            System.out.println(cf.getFields().stream()
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.SYNTHETIC) == 0)
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.STATIC) != 0)
                                 .count()
                               + " non-synthetic static fields");
            System.out.println(cf.getFields().stream()
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.SYNTHETIC) == 0)
                                 .filter(f -> (f.getAccessFlags() & AccessFlag.STATIC) == 0)
                                 .count()
                               + " non-synthetic instance fields");

            System.out.println(cf.getConstPool().getSize() + " constant pool entries");

            System.out.println();

            for (MethodInfo mi: cf.getMethods()) {
                final CodeAttribute ca = mi.getCodeAttribute();
                if (ca == null) continue; // abstract or native
                final int bLen = ca.getCode().length;
                if ((bLen > 35 && bLen <= 38) || bLen > 325) System.out.println(mi.getName() + mi.getDescriptor() + ": " + bLen + " bytes, max stack = " + ca.getMaxStack());
            }
        }
        System.out.println();
    }

    private static void pr(Class<?> clazz, String methodName) throws Exception {
        final String fileName = clazz.getName().substring("io.github.jmurmel.".length()) + ".class";

        try (InputStream is = Objects.requireNonNull(clazz.getResourceAsStream(fileName), "cannot open class file")) {
            final ClassFile cf = new ClassFile(new DataInputStream(is));
            for (MethodInfo mi: cf.getMethods()) {
                if (methodName.equals(mi.getName())) {
                    final CodeAttribute ca = mi.getCodeAttribute();
                    System.out.println((clazz.getName() + '.' + methodName) + mi.getDescriptor() + ": " + ca.getCode().length + " bytes, max stack = " + ca.getMaxStack());
                }
            }
        }
    }

    private static String fmtSyntheticMethos(MethodInfo mi) {
        String ret = mi.getName() + mi.getDescriptor();
        final CodeAttribute ca = mi.getCodeAttribute();
        if (ca == null) return ret;

        final int codeLength = ca.getCodeLength();
        if (codeLength > 35) ret += " ( *** ";
        else ret += " (";

        ret += codeLength + " bytes";
        if ((mi.getAccessFlags() & AccessFlag.STATIC) == 0) ret += ", non-static)";
        else  ret += ", static)";
        
        return ret;
    }
}
