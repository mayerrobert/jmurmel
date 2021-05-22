package io.github.jmurmel.charset;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;

public class Echo {

    public static void main(String[] args) throws IOException {
        System.out.print("file.encoding:       ");  System.out.println(System.getProperty("file.encoding"));
        System.out.print("sun.jnu.encoding:    ");  System.out.println(System.getProperty("sun.jnu.encoding"));
        System.out.print("sun.stdout.encoding: ");  System.out.println(System.getProperty("sun.stdout.encoding"));
        System.out.print("sun.stderr.encoding: ");  System.out.println(System.getProperty("sun.stderr.encoding"));

        // doesn't exist, or rather: Java doesn't set/ use this property
        System.out.print("sun.stdin.encoding:  ");  System.out.println(System.getProperty("sun.stdin.encoding"));

        System.out.print("This is a string with an embedded '\n' and trailing newline character\n");
        String s = "";
        ArrayList<Byte> byteList = new ArrayList<>();
        for (;;) {
            int c = System.in.read();
            if (c == -1) break;
            System.out.println(String.format("char %-3d %s", c, Character.isAlphabetic(c) ? String.valueOf((char)c) : "(not alpabetic)"));
            s += (char)c;
            byteList.add((byte) (c & 0xff));
        }

        System.out.print("read string: '"); System.out.print(s); System.out.println('\'');

        byte[] bytes = new byte[byteList.size()];
        int n = 0;
        for (Byte b: byteList) {
            bytes[n++] = b.byteValue();
        }
        System.out.print("UTF8:            '"); System.out.print(new String(bytes, Charset.forName("UTF-8"))); System.out.println('\'');
        System.out.print("Windows-1252:    '"); System.out.print(new String(bytes, Charset.forName("Windows-1252"))); System.out.println('\'');
        System.out.print("OEM-850 (cp850): '"); System.out.print(new String(bytes, Charset.forName("cp850"))); System.out.println('\'');
    }
}
