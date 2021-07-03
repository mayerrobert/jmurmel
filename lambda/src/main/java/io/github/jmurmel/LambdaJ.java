/* LambdaJ is Copyright (C) 2020-2021 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

package io.github.jmurmel;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.zone.ZoneRules;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IllegalFormatException;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.*;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.tools.ToolProvider;
import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;

import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.SimpleJavaFileObject;

/// # JMurmel - Murmel interpreter/ compiler

/** <p>Implementation of JMurmel, an interpreter for the Lisp-dialect Murmel.
 *  Can be used as a standalone commandline application as well as embedded in a Java program.
 *
 *  <p><b>Embedded use in interpreter mode:</b>
 *  <p>Important methods for embedded use of the interpeter are:
 *  <ul><li>{@link LambdaJ#LambdaJ()} - constructor
 *      <li>{@link LambdaJ#interpretExpressions(ReadSupplier, ReadSupplier, WriteConsumer)}
 *          - interpret a Murmel program in S-expression surface representation
 *      <li>{@link #getFunction(String)} - after interpreting a Murmel program getFunction() can be used to get a handle
 *          on a Murmel function defined previously. See also {@link MurmelJavaProgram#getFunction(String)} which does
 *          the same for compiled Murmel programs.
 *  </ul>
 *
 *  <p><b>Embedded use in compiler mode:</b>
 *  <p>For compiling Murmel programs to Java or Java-classes see {@link MurmelJavaCompiler}.
 *
 *  <p><b>Connecting I/O of an embedded Murmel program</b>
 *  <p>Interpreted as well as compiled programs read using {@link ObjectReader}s
 *  and print using {@link ObjectWriter}s.
 *
 *  <p>Defaults for reading/ printing:
 *  <ul><li>{@code lispReader} is an {@link LambdaJ.SExpressionParser} that reads using a {@link ReadSupplier} (which defaults to {@link System#in})
 *      <li>{@code lispPrinter} is an {@link LambdaJ.SExpressionWriter} that prints using a {@link WriteConsumer} (which defaults to {@link System#out})
 *  </ul>
 *
 *  If you want to read/ write S-expressions from streams other than {@link System#in}/ {@link System#out} then do something like<pre>
 *  intp.setReaderPrinter(intp.new SExpressionParser(() -&gt; myReader::myFunctionThatReturnsCharsAsInt), intp.getLispPrinter());</pre>
 *
 *  If you want to read/ write a surface representation other than S-expressions then do something like<pre>
 *  ObjectReader myReader = new MyReader(...);
 *  intp.setReaderPrinter(myReader, intp.getLispPrinter());</pre>
 *
 *  <p><b>How to learn the inner workings of the interpreter and compiler:</b>
 *  <p>The source code for the class {@code LambdaJ} could probably be read from top to bottom like a book.
 *
 *  <p>Comments starting with '///' could be considered similar to headings or chapter titles.
 *  You may want to run 'grep " ///" LambdaJ.java' to get something like birds-eye-view
 *  or sort of a table-of-contents of the interpreter implementation. Or run<pre>
 *  sed -nf src\main\shell\litprog.sed src\main\java\io\github\jmurmel\LambdaJ.java &gt; jmurmel-doc.md</pre>
 */
public class LambdaJ {

    /// ## Public interfaces and an exception class to use the interpreter from Java

    public static final String ENGINE_NAME = "JMurmel: Java based implementation of Murmel";
    public static final String LANGUAGE_VERSION = "1.0-SNAPSHOT";
    public static final String ENGINE_VERSION;
    static {
        String versionInfo;
        final ClassLoader cl = LambdaJ.class.getClassLoader();
        final URL url = cl.getResource("META-INF/MANIFEST.MF");
        if (url == null) versionInfo = "unknown";
        else {
            try (final InputStream is = url.openStream()) {
                final Manifest manifest = new Manifest(is);
                versionInfo = manifest.getMainAttributes().getValue("Implementation-Version");
            } catch (IOException e) {
                versionInfo = "error";
            }
        }
        ENGINE_VERSION = versionInfo;
    }

    private static final Path murmelDir;
    static {
        Path path;
        try {
            Path p = Paths.get(LambdaJ.class.getProtectionDomain().getCodeSource().getLocation().toURI());
            if (Files.isDirectory(p)) {
                path = p;
            }
            else {
                path = p.getParent();
                if (path == null) {
                    System.out.println("cannot get Murmel dir: " + p + " is not a directory but does not have a parent to use");
                }
                else if (!Files.isDirectory(path)) {
                    System.out.println("cannot get Murmel dir: neither " + p + " nor " + path + " are directories");
                }
            }
            //System.out.println("murmeldir: " + path);
        }
        catch (URISyntaxException e) {
            System.out.println("cannot get Murmel dir: " + e.getMessage());
            path = Paths.get(".");
        }
        murmelDir = path;
    }

    private final Path libDir;

    private static final String[] CTRL = {
            "Nul", "Soh", "Stx", "Etx", "Eot", "Enq", "Ack", "Bel", "Backspace", "Tab", "Newline",
            "Vt", "Page", "Return", "So", "Si", "Dle", "Dc1", "Dc2", "Dc3", "Dc4",
            "Nak", "Syn", "Etb", "Can", "Em", "Sub", "Esc", "Fs", "Gs", "Rs",
            "Us"
    };

    private static final String[] FEATURES = {
            "murmel", "jvm", "ieee-floating-point"
    };
    
    @FunctionalInterface public interface ReadSupplier { int read() throws IOException; }
    @FunctionalInterface public interface WriteConsumer { void print(String s); }
    @FunctionalInterface public interface TraceConsumer { void println(String msg); }

    @FunctionalInterface public interface ObjectReader { Object readObj(); }
    public interface SymbolTable { LambdaJSymbol intern(LambdaJSymbol symbol); }
    public interface Parser extends ObjectReader, SymbolTable {
        default void setInput(ReadSupplier input) {
            throw new UnsupportedOperationException("this parser does not support changing input");
        }
    }

    public interface ObjectWriter { void printObj(Object o); default void printString(String s) { printObj(s); } void printEol(); }

    @FunctionalInterface public interface Primitive { Object apply(ConsCell x); }

    public interface CustomEnvironmentSupplier {
        ConsCell customEnvironment(SymbolTable symtab);
    }



    public static class LambdaJError extends RuntimeException {
        public static final long serialVersionUID = 1L;

        public LambdaJError(String msg)                                    { super(msg, null, false, false); }
        public LambdaJError(boolean format,  String msg, Object... params) { super((format ? String.format(msg, params) : msg) + getErrorExp(params), null, false, false); }
        public LambdaJError(Throwable cause, String msg, Object... params) { super(String.format(msg, params) + getErrorExp(params), cause); }

        @Override public String toString() { return "Error: " + getMessage(); }

        private static String getErrorExp(Object[] params) {
            if (params != null && params.length > 0 && params[params.length-1] instanceof ConsCell) return errorExp(params[params.length-1]);
            return "";
        }

        private static String errorExp(Object exp) {
            if (exp == null) return "";
            final String l = lineInfo(exp);
            return System.lineSeparator() + "error occurred in S-expression " + l + printSEx(exp);
        }
    }



    /// ## Data types used by interpreter program as well as interpreted programs

    /** Main building block for Lisp-lists */
    public abstract static class ConsCell implements Iterable<Object>, Serializable {
        private static final long serialVersionUID = 1L;

        public static ConsCell cons(Object car, Object cdr) { return new ListConsCell(car, cdr); }

        public abstract Object car();
        public Object rplaca(Object car) { throw new UnsupportedOperationException(); }

        public abstract Object cdr();
        public Object rplacd(Object cdr) { throw new UnsupportedOperationException(); }

        ConsCell closure() { return null; }
    }

    public static final class ListBuilder {
        private Object first;
        private Object last;

        public ListBuilder append(Object elem) {
            final ConsCell newCell = ConsCell.cons(elem, null);
            if (first == null) {
                last = first = newCell;
            }
            else if (last instanceof ConsCell) {
                ((ConsCell) last).rplacd(newCell);
                last = newCell;
            }
            else throw new LambdaJ.LambdaJError("can't append list element to dotted list");
            return this;
        }

        public ListBuilder appendLast(Object lastElem) {
            if (first == null) {
                last = first = lastElem;
            }
            else if (last instanceof ConsCell) {
                ((ConsCell) last).rplacd(lastElem);
                last = lastElem;
            }
            else throw new LambdaJ.LambdaJError("can't append last list element to dotted list");
            return this;
        }

        public Object first() { return first; }
    }

    private static class ListConsCell extends ConsCell {
        private static class ListConsCellIterator implements Iterator<Object> {
            private final ListConsCell coll;
            private Object cursor;

            private ListConsCellIterator(ListConsCell coll) { this.coll = coll; cursor = coll; }
            @Override public boolean hasNext() { return cursor != null; }

            @Override
            public Object next() {
                final Object _cursor;
                if ((_cursor = cursor) == null) throw new NoSuchElementException();
                if (_cursor instanceof ListConsCell) {
                    final ListConsCell list = (ListConsCell)_cursor;
                    if (list.cdr() == coll) cursor = null; // circle detected, stop here
                    else cursor = list.cdr();
                    return list.car();
                }
                cursor = null;
                return _cursor;  // last element of dotted list
            }
        }

        private static final long serialVersionUID = 1L;

        private Object car, cdr;

        private ListConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
        @Override public String toString() { return printObj(this); }
        @Override public Iterator<Object> iterator() { return new ListConsCellIterator(this); }

        @Override public Object car() { return car; }
        @Override public Object rplaca(Object car) { final Object ret = this.car; this.car = car; return ret; }

        @Override public Object cdr() { return cdr; }
        @Override public Object rplacd(Object cdr) { final Object ret = this.cdr; this.cdr = cdr; return ret; }
    }

    private static final class SExpConsCell extends ListConsCell {
        private static final long serialVersionUID = 1L;
        private final int startLineNo, startCharNo;
        private int lineNo, charNo;
        private SExpConsCell(int startLine, int startChar, int line, int charNo, Object car, Object cdr)    {
            super(car, cdr);
            this.startLineNo = startLine; this.startCharNo = startChar; this.lineNo = line; this.charNo = charNo;
        }
    }

    private static final class ClosureConsCell extends ListConsCell {
        private static final long serialVersionUID = 1L;
        private final ConsCell closure; // only used for Lambdas with lexical environments. doesn't waste space because Java object sizes are multiples of 8 and this uses an otherwise unused slot
        private ClosureConsCell(Object car, Object cdr, ConsCell closure)    { super(car, cdr); this.closure = closure; }

        @Override ConsCell closure() { return closure; }
    }

    /** A murmel symbol name */
    public static class LambdaJSymbol implements Serializable {
        private static final long serialVersionUID = 1L;
        private final String name;
        public LambdaJSymbol(String symbolName) { name = Objects.requireNonNull(symbolName, "can't use null symbolname"); }
        @Override public String toString() { return name; }

        @Override public int hashCode() { return name.hashCode(); }
        @Override public boolean equals(Object o) { return o == this || o instanceof LambdaJSymbol && name.equals(((LambdaJSymbol)o).name); }
    }



    /// ## Infrastructure
    public static final int EOF = -1;
    /** Max length of string literals */
    public static final int TOKEN_MAX = 2000;
    /** Max length of symbols*/
    public static final int SYMBOL_MAX = 30;

    public enum TraceLevel {
        TRC_NONE, TRC_STATS, TRC_ENVSTATS, TRC_EVAL, TRC_ENV, TRC_FUNC, TRC_PARSE, TRC_TOK, TRC_LEX;
        public boolean ge(TraceLevel l) { return ordinal() >= l.ordinal(); }
    }
    private final TraceLevel trace;

    private final TraceConsumer tracer;

    public enum Features {
        HAVE_QUOTE,          // quote will allow to distinguish code and data. without quote use cons.
        HAVE_ATOM,
        HAVE_EQ,

        HAVE_CONS,           // cons, car, cdr
        HAVE_COND,

        HAVE_APPLY,          // McCarthy didn't list apply, he probably implied eval, tough
        HAVE_LABELS,         // without labels: use Z-combinator (imperative version of the Y-combinator)

        HAVE_NIL, HAVE_T,    // use () and (quote t) instead. printObj will print nil regardless

        HAVE_XTRA,           // extra special forms such as if

        HAVE_NUMBERS,        // numbers, +-<>..., numberp, without it the remaining datatypes are symbols and cons-cells (lists)

        HAVE_DOUBLE,         // turns on Double support in the reader, you'll want NUMBERS as well
        HAVE_LONG,           // turns on Long support in the reader, you'll want NUMBERS as well
        HAVE_STRING,         // turns on String support in the reader and string literals and string related functions in the interpreter

        HAVE_IO,             // read/ write, without it only the result will be printed
        HAVE_UTIL,           // consp, symbolp, listp, null, assoc

        HAVE_LEXC,           // use lexical environments with dynamic global environment

        /** untyped lambda calculus with dynamic environments, S-expressions, that's all */
        HAVE_LAMBDA     { @Override public int bits() { return 0; } },
        HAVE_LAMBDAPLUS { @Override public int bits() { return HAVE_LAMBDA.bits() | HAVE_QUOTE.bits() | HAVE_ATOM.bits() | HAVE_EQ.bits(); } },
        HAVE_MIN        { @Override public int bits() { return HAVE_LAMBDAPLUS.bits() | HAVE_CONS.bits() | HAVE_COND.bits(); } },
        HAVE_MINPLUS    { @Override public int bits() { return HAVE_MIN.bits() | HAVE_APPLY.bits() | HAVE_LABELS.bits(); } },
        HAVE_ALL_DYN    { @Override public int bits() { return HAVE_MINPLUS.bits() | HAVE_NIL.bits() | HAVE_T.bits() | HAVE_XTRA.bits()
                                                               | HAVE_NUMBERS.bits()| HAVE_DOUBLE.bits() | HAVE_LONG.bits()
                                                               | HAVE_STRING.bits() | HAVE_IO.bits() | HAVE_UTIL.bits(); } },
        HAVE_ALL_LEXC   { @Override public int bits() { return HAVE_ALL_DYN.bits() | HAVE_LEXC.bits(); } }
        ;

        public int bits() { return 1 << ordinal(); }
    }

    private final int features;

    private boolean haveLabels()  { return (features & Features.HAVE_LABELS.bits())  != 0; }
    private boolean haveNil()     { return (features & Features.HAVE_NIL.bits())     != 0; }
    private boolean haveT()       { return (features & Features.HAVE_T.bits())       != 0; }
    private boolean haveXtra()    { return (features & Features.HAVE_XTRA.bits())    != 0; }
    private boolean haveNumbers() { return (features & Features.HAVE_NUMBERS.bits()) != 0; }
    private boolean haveString()  { return (features & Features.HAVE_STRING.bits())  != 0; }
    private boolean haveIO()      { return (features & Features.HAVE_IO.bits())      != 0; }
    private boolean haveUtil()    { return (features & Features.HAVE_UTIL.bits())    != 0; }
    private boolean haveApply()   { return (features & Features.HAVE_APPLY.bits())   != 0; }
    private boolean haveCons()    { return (features & Features.HAVE_CONS.bits())    != 0; }
    private boolean haveCond()    { return (features & Features.HAVE_COND.bits())    != 0; }
    private boolean haveAtom()    { return (features & Features.HAVE_ATOM.bits())    != 0; }
    private boolean haveEq()      { return (features & Features.HAVE_EQ.bits())      != 0; }
    private boolean haveQuote()   { return (features & Features.HAVE_QUOTE.bits())   != 0; }
    private boolean haveLexC()    { return (features & Features.HAVE_LEXC.bits())    != 0; }

    public LambdaJ() {
        this(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null);
    }

    public LambdaJ(int features, TraceLevel trace, TraceConsumer tracer) {
        this(features, trace, tracer, null);
    }

    public LambdaJ(int features, TraceLevel trace, TraceConsumer tracer, Path libDir) {
        this.features = features;
        this.trace = trace;
        this.tracer = tracer != null ? tracer : System.err::println;
        if (libDir != null) this.libDir = libDir;
        else this.libDir = murmelDir;
    }



    /// ## Printer

    /** return an ObjectWriter that transforms \n to the platform default line separator */
    public static ObjectWriter makeWriter(WriteConsumer out) {
        return makeWriter(out, System.lineSeparator());
    }

    /** return an ObjectWriter that transforms \n to the given {@code lineSeparator} */
    public static ObjectWriter makeWriter(WriteConsumer out, String lineSeparator) {
        if ("\r\n".equals(lineSeparator)) return new SExpressionWriter(new UnixToAnyEol(out, "\r\n"));
        if ("\r"  .equals(lineSeparator)) return new SExpressionWriter(new UnixToAnyEol(out, "\r"));

        return new SExpressionWriter(out);
    }

    /** This class will write objects as S-expressions to the given {@link WriteConsumer} w/o any eol translation */
    public static class SExpressionWriter implements ObjectWriter {
        private final WriteConsumer out;

        public SExpressionWriter(WriteConsumer out) { this.out = out; }
        @Override public void printObj(Object o) { printSEx(out, o); }
        @Override public void printEol() { out.print("\n"); }
        @Override public void printString(String s) { out.print(s); }
    }



    /// ## Scanner, symboltable and S-expression parser

    private static boolean isWhiteSpace(int x) { return x == ' ' || x == '\t' || x == '\n' || x == '\r'; }
    private static boolean isSExSyntax(int x) { return x == '(' || x == ')' /*|| x == '.'*/ || x == '\'' || x == '`' || x == ','; }

    private static boolean containsSExSyntaxOrWhiteSpace(String s) {
        for (int i = 0; i < s.length(); i++) {
            final char c;
            if (isSExSyntax(c = s.charAt(i))) return true;
            if (isWhiteSpace(c)) return true;
        }
        return false;
    }

    /** This class will read and parse S-Expressions (while generating symbol table entries)
     *  from the given {@link ReadSupplier} */
    public static class SExpressionParser implements Parser {
        private static class ParseError extends LambdaJError {
            private static final long serialVersionUID = 1L;
            private ParseError(String msg, Object... args) { super(true, msg, args); }
        }

        private final int features;
        private final TraceLevel trace;
        private final TraceConsumer tracer;

        private ReadSupplier in;    // readObj() will read from this
        Path filePath;
        private boolean init;

        private boolean pos;
        private int lineNo = 1, charNo;
        private int prevLineNo = 1, prevCharNo;
        private boolean escape; // is the lookahead escaped
        private boolean tokEscape;
        private int backquote;
        private int look;
        private final char[] token = new char[TOKEN_MAX];
        private Object tok;

        /** Create an S-expression parser (==reader) with all features, no tracing.
         *
         *  @param in a {@link ReadSupplier} that supplies characters,
         *            {@code InputStream::read} won't work because that supplies bytes but not (Unicode-) characters,
         *            {@code Reader::read} will work
         */
        public SExpressionParser(ReadSupplier in) {
            this(in, null);
        }

        public SExpressionParser(ReadSupplier in, Path filePath) {
            this(Features.HAVE_ALL_DYN.bits(), TraceLevel.TRC_NONE, null, in, filePath, true);
        }

        /** Create an S-expression parser (==reader).
         * @param in a {@link ReadSupplier} that supplies characters,
         *            {@code InputStream::read} won't work because that supplies bytes but not (Unicode-) characters,
         *            {@code Reader::read} will work
         * @param eolConversion if true then any EOL will be converted to Unix EOL
         */
        public SExpressionParser(int features, TraceLevel trace, TraceConsumer tracer, ReadSupplier in, Path filePath, boolean eolConversion) {
            this.features = features; this.trace = trace; this.tracer = tracer;
            this.in = eolConversion ? new AnyToUnixEol(in) : in;
            this.filePath = filePath;
        }

        private boolean haveDouble()  { return (features & Features.HAVE_DOUBLE.bits())  != 0; }
        private boolean haveLong()    { return (features & Features.HAVE_LONG.bits())    != 0; }
        private boolean haveString()  { return (features & Features.HAVE_STRING.bits())  != 0; }

        @Override public void setInput(ReadSupplier input) { in = input; init = false; }

        /// Scanner
        private boolean isSpace(int x)  { return !escape && isWhiteSpace(x); }
        private boolean isDQuote(int x) { return !escape && x == '"'; }
        private boolean isBar(int x)    { return !escape && x == '|'; }
        private boolean isHash(int x)   { return !escape && x == '#'; }

        private boolean isSyntax(int x) { return !escape && isSExSyntax(x); }

        private static final Pattern longPattern = Pattern.compile("[-+]?([0-9]|([1-9][0-9]*))");
        private static boolean isLong(String s) {
            if (s == null || s.isEmpty()) return false;
            return longPattern.matcher(s).matches();
        }

        private static final Pattern doublePattern = Pattern.compile(
                "[-+]?"                              // optional-sign
              + "("                                  // either
              + "(((([0-9]+\\.)[0-9]*)|\\.[0-9]+)"   //   either: one-or-more-digits '.' zero-or-more-digits  or: '.' one-or-more-digits
              + "([eE][-+]?[0-9]+)?)"                //   optional: e-or-E optional-sign one-or-more-digits
              + "|"                                  // or
              + "([0-9]+[eE][-+]?[0-9]+)"            //   one-or-more-digits e-or-E optional-sign one-or-more-digits
              + ")");
        private static boolean isDouble(String s) {
            if (s == null || s.isEmpty()) return false;
            return doublePattern.matcher(s).matches();
        }

        /*java.io.PrintWriter debug;
        {
            try {
                debug = new java.io.PrintWriter(Files.newBufferedWriter(Paths.get("scanner.log")));
            } catch (IOException e) { }
        }*/

        /** Translate the various line end sequences \r, \r\n and \n all to \n */
        // todo ist die lineend behandlung erforderlich? vgl EolUtil,  AnyToUnixEol
        private void savePrevPos() { prevLineNo = lineNo; prevCharNo = charNo; }
        private int prev = -1;
        private int readchar() throws IOException {
            final int c = in.read();
            //debug.println(String.format("%d:%d: char %-3d %s", lineNo, charNo, c, Character.isWhitespace(c) ? "" : String.valueOf((char)c))); debug.flush();
            if (c == '\r') {
                prev = c;
                savePrevPos();
                lineNo++;
                charNo = 0;
                return '\n';
            }
            if (c == '\n' && prev == '\r') {
                prev = c;
                return readchar();
            }
            if (c == '\n') {
                prev = c;
                savePrevPos();
                lineNo++;
                charNo = 0;
                return '\n';
            }
            prev = c;
            savePrevPos();
            if (c != EOF) { charNo++; }
            return c;
        }

        private int getchar() {
            return getchar(true);
        }

        private int getchar(boolean handleComment) {
            try {
                tokEscape = escape;
                escape = false;
                int c = readchar();
                if (c == '\\') {
                    escape = true;
                    return readchar();
                }
                if (handleComment && c == ';') {
                    while ((c = readchar()) != '\n' && c != EOF) /* nothing */;
                }
                return c;
            } catch (CharacterCodingException e) {
                throw new ParseError("line %d:%d: characterset conversion error in SExpressionParser: %s", lineNo, charNo, e.toString());
            } catch (Exception e) {
                throw new ParseError("line %d:%d: I/O error in SExpressionParser: %s", lineNo, charNo, e.toString());
            }
        }

        private void skipWs() { while (isSpace(look)) { look = getchar(); } }

        private static final Object CONTINUE = new Object();
        
        /** if we get here then we have already read '#' and look contains the character after #subchar */
        private Object readerMacro(int sub_char) {
            switch (sub_char) {
            // #\ ... character literal
            case '\\':
                final String charOrCharactername = readerMacroToken(sub_char);
                if (charOrCharactername.length() == 1) return charOrCharactername.charAt(0);
                if (isLong(charOrCharactername)) {
                    try {
                        int n = Integer.parseInt(charOrCharactername);
                        if (n > 126) return (char)n;
                    } catch (NumberFormatException e) {
                        throw new ParseError("line %d:%d: '%s' following #\\ is not a valid number", lineNo, charNo, charOrCharactername);
                    }
                }
                for (int i = 0; i < CTRL.length; i++) {
                    if (CTRL[i].equals(charOrCharactername)) return (char)i;
                }
                throw new ParseError("line %d:%d: unrecognized character name %s", lineNo, charNo, charOrCharactername);

            // #| ... multiline comment ending with |#
            case '|':
                int ln = lineNo, cn = charNo;
                while (look != EOF) {
                    // note single & to avoid short-circuiting
                    if (look == '|' & (look = getchar(false)) == '#') {
                        look = getchar();
                        return CONTINUE;
                    }
                }
                throw new ParseError("line %d:%d: EOF in multiline comment", ln, cn);

            // #' ... function, ignore for CL compatibility
            case '\'':
                return CONTINUE;

            // #+... , #-... feature expressions
            case '+':
            case '-':
                final boolean hasFeature = featurep(readObj(true));
                final Object next = readObj(true);
                if (sub_char == '+') return hasFeature ? next : CONTINUE;
                else return hasFeature ? CONTINUE : next;

            case 'b':
            case 'B':
                skipWs();
                return parseLong(readerMacroToken(sub_char), 2);

            case 'o':
            case 'O':
                skipWs();
                return parseLong(readerMacroToken(sub_char), 8);

            case 'x':
            case 'X':
                skipWs();
                return parseLong(readerMacroToken(sub_char), 16);
                
            default:
                look = getchar();
                throw new ParseError("line %d:%d: no dispatch function defined for %s", lineNo, charNo, printChar(sub_char));
            }
        }

        private String readerMacroToken(int macroChar) {
            int index = 0;
            if (look != EOF) {
                token[index++] = (char)look;
                look = getchar(false);
            }
            while (look != EOF && !isSpace(look) && !isSyntax(look)) {
                if (index < TOKEN_MAX) token[index++] = (char)look;
                look = getchar(false);
            }
            final String ret = tokenToString(token, 0, index > SYMBOL_MAX ? SYMBOL_MAX : index);
            if (ret.isEmpty()) throw new ParseError("line %d:%d: EOF after #%c", lineNo, charNo, (int)macroChar);
            return ret;
        }

        private final Object sNot          = intern(new LambdaJSymbol("not"));
        private final Object sAnd          = intern(new LambdaJSymbol("and"));
        private final Object sOr           = intern(new LambdaJSymbol("or"));

        private final ConsCell featureList;
        {
            ConsCell l = null;
            for (String feat: FEATURES) {
                l = new ListConsCell(intern(new LambdaJSymbol(feat)), l);
            }
            // todo os, javaversion, ... zur laufzeit hinzufuegen
            featureList = l;
        }

        private boolean featurep(Object next) {
            //if (!symbolp(next)) throw new ParseError("only symbols are supported as feature expressions, got %s", printSEx(next));
            //return "murmel".equalsIgnoreCase(next.toString());
            if (next != null && symbolp(next)) return some(x -> x == next, featureList);
            else if (consp(next)) {
                if (car(next) == sAnd) return every(this::featurep, cdr(next));
                if (car(next) == sOr) return some(this::featurep, cdr(next));
                if (car(next) == sNot) {
                    if (cdr(next) == null) throw new ParseError("feature expression not: not enough subexpressions, got %s", printSEx(next));
                    if (cddr(next) != null) throw new ParseError("feature expression not: too many subexpressions, got %s", printSEx(next));
                    return !featurep(cadr(next));
                }
            }
            throw new ParseError("unsupported feature expressions, got %s", printSEx(next));
        }

        private static boolean every(Function<Object, Boolean> pred, Object maybeList) {
            if (maybeList == null) return true;
            if (!consp(maybeList)) return pred.apply(maybeList);
            for (Object o: (ConsCell)maybeList) { if (!pred.apply(o)) return false; }
            return true;
        }

        private static boolean some(Function<Object, Boolean> pred, Object maybeList) {
            if (maybeList == null) return false;
            if (!consp(maybeList)) return pred.apply(maybeList);
            for (Object o: (ConsCell)maybeList) { if (pred.apply(o)) return true; }
            return false;
        }

        private static final Object LP = new String("(");
        private static final Object RP = new String(")");
        private static final Object DOT = new String(".");
        private static final Object SQ = new String("'");
        private static final Object BQ = new String("`");
        private static final Object COMMA = new String(",");

        private void readToken() {
            for (;;) {
                int index = 0;
                skipWs();
                tok = null;
                if (look != EOF) {
                    if (isBar(look)) {
                        look = getchar();
                        while (look != EOF && !isBar(look)) {
                            if (index < SYMBOL_MAX) token[index++] = (char) look;
                            look = getchar(false);
                        }
                        if (look == EOF)
                            throw new ParseError("line %d:%d: |-quoted symbol is missing closing |", lineNo, charNo);
                        look = getchar(); // consume trailing |
                        final String s = tokenToString(token, 0, index > SYMBOL_MAX ? SYMBOL_MAX : index);
                        tok = new LambdaJSymbol(s);
                    } else if (isSyntax(look)) {
                        switch (look) {
                        case '(': tok = LP; break;
                        case ')': tok = RP; break;
                        case '\'': tok = SQ; break;
                        case '`': tok = BQ; break;
                        case ',': tok = COMMA; break;
                        default: throw new ParseError("line %d:%d: internal error - unexpected syntax char %c", lineNo, charNo, (char)look);
                        }
                        look = getchar();
                    } else if (haveString() && isDQuote(look)) {
                        do {
                            if (index < TOKEN_MAX) token[index++] = (char) look;
                            look = getchar(false);
                        } while (look != EOF && !isDQuote(look));
                        if (look == EOF)
                            throw new ParseError("line %d:%d: string literal is missing closing \"", lineNo, charNo);
                        look = getchar(); // consume trailing "
                        tok = tokenToString(token, 1, index).intern();
                    } else if (isHash(look)) {
                        look = getchar(false);
                        final int subChar;
                        if (escape) subChar = '\\';
                        else { subChar = look; look = getchar(false); }
                        tok = readerMacro(subChar);
                    } else {
                        while (look != EOF && !isSpace(look) && !isSyntax(look)) {
                            if (index < TOKEN_MAX) token[index++] = (char) look;
                            look = getchar();
                        }
                        String s = tokenToString(token, 0, index);
                        if (!tokEscape && ".".equals(s)) {
                            tok = DOT;
                        } else if (haveDouble() && isDouble(s)) {
                            tok = parseDouble(s);
                        } else if (haveLong() && isLong(s)) {
                            tok = parseLong(s, 10);
                        } else if (haveDouble() && isLong(s)) {
                            tok = parseDouble(s);
                        } else {
                            if (s.length() > SYMBOL_MAX) s = s.substring(0, SYMBOL_MAX);
                            tok = new LambdaJSymbol(s);
                        }
                    }
                }

                if (trace.ge(TraceLevel.TRC_LEX))
                    tracer.println("*** scan  token  |" + tok + '|');

                if (tok != CONTINUE) return;
            }
        }

        private Number parseLong(String s, int radix) {
            try {
                return Long.valueOf(s, radix);
            } catch (NumberFormatException e) {
                throw new ParseError("line %d:%d: '%s' is not a valid number", lineNo, charNo, s);
            }
        }

        private Number parseDouble(String s) {
            try {
                return Double.valueOf(s);
            } catch (NumberFormatException e) {
                throw new ParseError("line %d:%d: '%s' is not a valid number", lineNo, charNo, s);
            }
        }

        private static String tokenToString(char[] b, int first, int end) {
            return new String(b, first, end - first);
        }



        /// A symbol table implemented with a list just because. could easily replaced by a HashMap for better performance.
        private ConsCell symbols;

        // String#equalsIgnoreCase is slow. we could String#toUpperCase all symbols then we could use String#equals
        @Override
        public LambdaJSymbol intern(LambdaJSymbol sym) {
            for (ConsCell s = symbols; s != null; s = (ConsCell)cdr(s)) {
                final LambdaJSymbol _s = (LambdaJSymbol) car(s);
                if (_s.name.equalsIgnoreCase(sym.name))
                    return _s;
            }

            symbols = cons(0, 0, sym, symbols);
            return sym;
        }



        /// S-expression parser
        /** Record line and char numbers in the conses */
        public Object readObj(boolean ignored) {
            this.pos = true;
            final Object ret = readObj();
            this.pos = false;
            return ret;
        }

        @Override
        public Object readObj() {
            if (!init) {
                prev = -1;
                lineNo = 1; charNo = 0;
                look = getchar();
                init = true;
            }
            skipWs();
            final int startLine = lineNo, startChar = charNo;
            readToken();
            //return expand_backquote(readObject(startLine, startChar));
            return readObject(startLine, startChar);
        }

        private final Object sQuote          = intern(new LambdaJSymbol("quote"));
        private final Object sQuasiquote     = intern(new LambdaJSymbol("quasiquote"));
        private final Object sUnquote        = intern(new LambdaJSymbol("unquote"));
        private final Object sUnquote_splice = intern(new LambdaJSymbol("unquote-splice"));
        private final Object sAppend         = intern(new LambdaJSymbol("append"));
        private final Object sList           = intern(new LambdaJSymbol("list"));

        private Object readObject(int startLine, int startChar) {
            if (tok == null) {
                if (trace.ge(TraceLevel.TRC_PARSE)) tracer.println("*** parse list   ()");
                return null;
            }
            if (!tokEscape && tok instanceof LambdaJSymbol && "nil".equalsIgnoreCase(tok.toString())) {
                if (trace.ge(TraceLevel.TRC_TOK)) tracer.println("*** parse symbol nil");
                return null;
            }
            if (symbolp(tok)) {
                if (trace.ge(TraceLevel.TRC_TOK)) tracer.println("*** parse symbol " + tok);
                return intern((LambdaJSymbol)tok);
            }
            if (!tokEscape && tok == RP) {
                throw new LambdaJError(true, "line %d:%d: unexpected ')'", lineNo, charNo);
            }
            if (!tokEscape && tok == LP) {
                try {
                    final Object list = readList(startLine, startChar);
                    if (!tokEscape && tok == DOT) {
                        skipWs();
                        final Object cdr = readList(lineNo, charNo);
                        if (cdr(cdr) != null) throw new ParseError("line %d:%d: illegal end of dotted list: %s", lineNo, charNo, printSEx(cdr));
                        final Object cons = combine(startLine, startChar, list, car(cdr));
                        if (trace.ge(TraceLevel.TRC_PARSE)) tracer.println("*** parse cons   " + printSEx(cons));
                        return cons;
                    }
                    if (trace.ge(TraceLevel.TRC_PARSE)) tracer.println("*** parse list   " + printSEx(list));
                    return list;
                }
                catch (ParseError e) {
                    throw new LambdaJError(e.getMessage() + System.lineSeparator() + "error occurred in S-expression line " + startLine + ':' + startChar + ".." + lineNo + ':' + charNo);
                }
            }
            if (!tokEscape && tok == SQ) {
                skipWs();
                final int _startLine = lineNo, _startChar = charNo;
                readToken();
                return cons(startLine, startChar, sQuote, cons(startLine, startChar, readObject(_startLine, _startChar), null));
            }
            if (!tokEscape && tok == BQ) {
                skipWs();
                final int _startLine = lineNo, _startChar = charNo;
                readToken();
                backquote++;
                final Object exp = readObject(_startLine, _startChar);
                final Object o;
                if (backquote == 1) o = qq_expand(exp);
                else o = cons(startLine, startChar, sQuasiquote, cons(startLine, startChar, exp, null));
                backquote--;
                return o;
            }
            if (!tokEscape && tok == COMMA) {
                if (backquote == 0)
                    throw new LambdaJError("comma is not inside a backquote" + System.lineSeparator() + "error occurred in S-expression line " + startLine + ':' + startChar + ".." + lineNo + ':' + charNo);
                skipWs();
                final boolean splice;
                if (look == '@') { splice = true; look = getchar(); }
                else splice = false;
                final int _startLine = lineNo, _startChar = charNo;
                readToken();
                backquote--;
                final Object o = cons(startLine, startChar, splice ? sUnquote_splice : sUnquote, cons(startLine, startChar, readObject(_startLine, _startChar), null));
                backquote++;
                return o;
            }
            if (trace.ge(TraceLevel.TRC_TOK)) tracer.println("*** parse value  " + tok);
            return tok;
        }

        private Object readList(int listStartLine, int listStartChar) {
            ListConsCell first = null, appendTo = null;
            for (;;) {
                skipWs();
                final int carStartLine = lineNo, carStartChar = charNo;
                readToken();
                if (tok == null) throw new ParseError("line %d:%d: cannot read list. missing ')'?", lineNo, charNo);
                if (!tokEscape && (tok == RP || tok == DOT)) {
                    adjustEnd(first);
                    return first;
                }
                final ListConsCell newCell = cons(listStartLine, listStartChar);
                if (first == null) first = newCell;
                if (appendTo != null) appendTo.rplacd(newCell);
                appendTo = newCell;

                newCell.rplaca(readObject(carStartLine, carStartChar));
                skipWs();
                listStartLine = lineNo; listStartChar = charNo;
            }
        }

        private void adjustEnd(ConsCell c) {
            if (c instanceof SExpConsCell) {
                final SExpConsCell lc = (SExpConsCell)c;
                lc.lineNo = prevLineNo;
                lc.charNo = prevCharNo;
            }
        }



        private ListConsCell cons(int startLine, int startChar, Object car, Object cdr) {
            return pos ? new SExpConsCell(startLine, startChar, lineNo, charNo, car, cdr) : new ListConsCell(car, cdr);
        }

        private ListConsCell cons(int startLine, int startChar) {
            return pos ? new SExpConsCell(startLine, startChar, lineNo, charNo, null, null) : new ListConsCell(null, null);
        }

        /** Append rest at the end of first. If first is a list it will be modified. */
        private ListConsCell combine(int startLine, int startChar, Object first, Object rest) {
            if (consp(first)) return appendToList(startLine, startChar, (ListConsCell)first, rest);
            else return cons(startLine, startChar, first, rest);
        }

        /** Append rest at the end of first, modifying first in the process.
         *  Returns a dotted list unless rest is a proper list. */
        // ist das nconc (destructive concatenate) ?
        private ListConsCell appendToList(int startLine, int startChar, ListConsCell first, Object rest) {
            for (ListConsCell last = first; last != null; last = (ListConsCell) cdr(last)) {
                if (cdr(last) == first) throw new LambdaJError(true, "%s: first argument is a circular list", "appendToList");
                if (cdr(last) == null) {
                    last.rplacd(rest);
                    return first;
                }
                if (!consp(cdr(last))) {
                    last.rplacd(cons(startLine, startChar, last.cdr(), rest));
                    return first;
                }
            }
            throw new LambdaJError(true, "%s: internal error, can't append %s and %s", "appendToList", printSEx(first), printSEx(rest));
        }



        /*
        Object expand_backquote(Object form) {
            if (atom(form))
                return form;

            final ConsCell formCons = (ConsCell)form;
            final Object op = car(formCons);

            if (op == sQuote)
                return form;

            if (op == sUnquote || op == sUnquote_splice) {
                throw new LambdaJError("comma is not inside a backquote");
            }

            if (op == sQuasiquote)
                return qq_expand(cadr(formCons));

            return mapcar(this::expand_backquote, formCons);
        }
        */



        /*
         qq-expand and qq-expand-list are based on "Quasiquotation in Lisp (1999) by Alan Bawden"
         https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227

        (defun qq-expand (x)
          (cond ((null x)
                 nil)
                ((tag-comma? x)
                 (tag-data x))
                ((tag-comma-atsign? x)
                 (error "Illegal"))
                ((tag-backquote? x)
                 (qq-expand
                   (qq-expand (tag-data x))))
                ((consp x)
                 `(append
                    ,(qq-expand-list (car x))
                    ,(qq-expand (cdr x))))
                (t `',x)))
        */
        private Object qq_expand(Object x) {
            if (x == null) return null;
            if (atom(x))
                return quote(x);

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return cadr(xCons);

            if (op == sUnquote_splice)
                throw new LambdaJError("can't splice here");

            if (op == sQuasiquote)
                return qq_expand(qq_expand(cadr(xCons)));

            if (cdr(xCons) == null) return qq_expand_list(op);
            return list(sAppend, qq_expand_list(op), qq_expand(cdr(xCons)));
        }

        /*
        (defun qq-expand-list (x)
          (cond ((tag-comma? x)
                  `(list ,(tag-data x)))
                ((tag-comma-atsign? x)
                 (tag-data x))
                ((tag-backquote? x)
                 (qq-expand-list
                   (qq-expand (tag-data x))))
                ((consp x)
                 `(list
                    (append
                      ,(qq-expand-list (car x))
                      ,(qq-expand (cdr x)))))
                (t `'(,x))))
        */
        private Object qq_expand_list(Object x) {
            if (atom(x))
                return quote(list(x, null));

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return list(sList, cadr(xCons));

            if (op == sUnquote_splice)
                return cadr(xCons);

            if (op == sQuasiquote)
                return qq_expand_list(qq_expand(cadr(xCons)));

            if (cdr(xCons) == null) return list(sList, qq_expand_list(op));
            return list(sList, list(sAppend, qq_expand_list(op), qq_expand(cdr(xCons))));
        }



        private ConsCell quote(Object form) {
            return list(sQuote, form);
        }

        private static ConsCell list(Object o1, Object o2) {
            if (o2 == null)
                return new ListConsCell(o1, null);
            return new ListConsCell(o1, new ListConsCell(o2, null));
        }

        private static ConsCell list(Object o1, Object o2, Object o3) {
            return new ListConsCell(o1, new ListConsCell(o2, new ListConsCell(o3, null)));
        }
    }



    ///
    /// ## Murmel interpreter
    ///

    /// Murmel has a list of reserved words may not be used as a symbol
    private ConsCell reservedWords;

    private void reserve(Object word) { reservedWords = cons(word, reservedWords); }

    /** Throw error if sym is a reserved symbol */
    private void notReserved(final String op, final Object sym) {
        if (sym == null)
            throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", op, "nil");
        if (member(sym, reservedWords))
            throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", op, sym.toString());
    }

    /// Symboltable
    private SymbolTable symtab;

    public static final Object UNASSIGNED = "value is not assigned";          // only relevant in letrec
    private static final Object PSEUDO_SYMBOL = "non existant pseudo symbol"; // to avoid matches on pseudo env entries

    /** Look up the symbols for special forms only once. Also start to build the table of reserved words. */
    private void setSymtab(SymbolTable symtab) {
        this.symtab = symtab;

        // (re-)read the new symtab
        sLambda =                      internReserved("lambda");
        sDynamic =                     intern("dynamic");

        if (haveQuote())  { sQuote   = internReserved("quote"); }
        if (haveCond())   { sCond    = internReserved("cond"); }
        if (haveLabels()) { sLabels  = internReserved("labels"); }
        if (haveApply())  { sApply   = internReserved("apply"); }

        if (haveXtra())   {
            sIf      = internReserved("if");
            sDefine  = internReserved("define");
            sDefun   = internReserved("defun");
            sDefmacro= internReserved("defmacro");
            sLet     = internReserved("let");
            sLetStar = internReserved("let*");
            sLetrec  = internReserved("letrec");
            sSetQ    = internReserved("setq");

            sProgn   = internReserved("progn");

            sLoad    = internReserved("load");
            sRequire = internReserved("require");
        }

        // Lookup only once on first use. The supplier below will do a lookup on first use and then replace itself
        // by another supplier that simply returns the cached value.
        expTrue = () -> { final Object s = makeExpTrue(); expTrue = () -> s; return s; };

        // reset the opencoded primitives, new symboltable means new (blank) environment. #environment may or may not refill these
        ocEval = null;

        topEnv = null;

        traced = null;

        macros.clear();
        modules.clear();
    }

    /** well known symbols for special forms */
    private LambdaJSymbol sLambda, sDynamic, sQuote, sCond, sLabels, sIf, sDefine, sDefun, sDefmacro, sLet, sLetStar, sLetrec, sSetQ, sApply, sProgn, sLoad, sRequire;

    private Supplier<Object> expTrue;

    private Object makeExpTrue() {
        if (haveT()) return symtab.intern(new LambdaJSymbol("t")); // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (haveQuote()) return cons(symtab.intern(new LambdaJSymbol("quote")), cons(symtab.intern(new LambdaJSymbol("t")), null));
        else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
    }

    private LambdaJSymbol intern(String sym) {
        return symtab.intern(new LambdaJSymbol(sym));
    }

    private LambdaJSymbol internReserved(String sym) {
        final LambdaJSymbol ret = symtab.intern(new LambdaJSymbol(sym));
        reserve(ret);
        return ret;
    }

    private abstract static class OpenCodedPrimitive implements Primitive {
        private final LambdaJSymbol symbol;

        private OpenCodedPrimitive(LambdaJSymbol symbol) { this.symbol = symbol; }

        @Override public String toString() { return "#<opencoded primitive: " + symbol + '>'; }
    }
    private OpenCodedPrimitive ocEval;



    /// ### Global environment - define'd symbols go into this list
    private ConsCell topEnv;

    private final Map<Object, ConsCell> macros = new HashMap<>();
    private final Set<Object> modules = new HashSet<>();

    /// ###  eval - the heart of most if not all Lisp interpreters
    private Object eval(Object form, ConsCell env, int stack, int level, int traceLvl) {
        Object func = null;
        Object result = null;
        Deque<Object> traceStack = null;
        ConsCell restore = null;
        boolean isTc = false;
        try {
            stack++;

            tailcall:
            while (true) {
                level++;
                dbgEvalStart(isTc ? "eval TC" : "eval", form, env, stack, level);
                final Object operator;

                /// eval - lookup symbols in the current environment
                if (symbolp(form)) {                 // this line is a convenient breakpoint
                    if (form == null) return null;
                    final ConsCell envEntry = assoceq(form, env);
                    if (envEntry != null) {
                        final Object value = cdr(envEntry);
                        if (value == UNASSIGNED) throw new LambdaJError(true, "%s: '%s' is bound but has no assigned value", "eval", form);
                        result = value; return value;
                    }
                    throw new LambdaJError(true, "%s: '%s' is not bound", "eval", form);
                }

                /// eval - atoms that are not symbols eval to themselves
                else if (atom(form)) {
                    return form;   // this catches nil as well
                }

                /// eval - the form is enclosed in parentheses, either a special form or a function application
                else if (consp(form)) {
                    final ConsCell formCons = (ConsCell)form;
                    operator = car(formCons);      // first element of the of the form should be a symbol or an expression that computes a symbol
                    if (!listp(cdr(formCons))) throw new LambdaJError(true, "%s: expected an operand list to follow operator but got %s", "eval", printSEx(form));
                    final ConsCell arguments = (ConsCell) cdr(formCons);   // list with remaining atoms/ expressions



                    /// eval - special forms

                    /// eval - (quote exp) -> exp
                    if (operator == sQuote) {
                        oneArg("quote", arguments);
                        result = car(arguments);
                        return result;
                    }

                    /// eval - (lambda dynamic? (params...) forms...) -> lambda or closure
                    if (operator == sLambda) {
                        result = "#<lambda>";
                        return makeClosureFromForm(formCons, env);
                    }



                    /// eval - special forms that change the global environment

                    /// eval - (define symbol exp) -> symbol with a side of global environment extension
                    if (operator == sDefine) {
                        twoArgs("define", arguments);
                        final Object symbol = car(arguments);
                        if (!symbolp(symbol)) throw new LambdaJError(true, "%s: not a symbol: %s", "define", printSEx(symbol));
                        notReserved("define", symbol);
                        final ConsCell envEntry = assoceq(symbol, topEnv);

                        // immutable globals: "if (envEntry...)" entkommentieren, dann kann man globals nicht mehrfach neu zuweisen
                        //if (envEntry != null) throw new LambdaJError(true, "%s: '%s' was already defined, current value: %s", "define", symbol, printSEx(cdr(envEntry)));

                        final Object value = eval(cadr(arguments), env, stack, level, traceLvl);
                        if (envEntry == null) insertFront(topEnv, symbol, value);
                        else envEntry.rplacd(value);

                        result = symbol;
                        return result;
                    }

                    /// eval - (defun symbol (params...) forms...) -> symbol with a side of global environment extension
                    // shortcut for (define symbol (lambda (params...) forms...))
                    if (operator == sDefun) {
                        nArgs("defun", arguments, 2);
                        form = list(sDefine, car(arguments), cons(sLambda, cons(cadr(arguments), cddr(arguments))));
                        continue tailcall;
                    }

                    if (operator == sSetQ) {
                        nArgs("setq", arguments, 2);
                        ConsCell pairs = arguments;
                        do {
                            final Object symbol = car(pairs);
                            if (!symbolp(symbol)) throw new LambdaJError(true, "%s: not a symbol: %s", "setq", printSEx(symbol));
                            notReserved("setq", symbol);
                            final ConsCell envEntry = assoceq(symbol, env);

                            pairs = (ConsCell) cdr(pairs);
                            if (pairs == null) throw new LambdaJError(true, "%s: odd number of arguments", "setq");
                            final Object value = eval(car(pairs), env, stack, level, traceLvl);
                            if (envEntry == null)
                                //insertFront(env, symbol, value);
                                throw new LambdaJError(true, "%s: '%s' is not bound", "setq", symbol);
                            else envEntry.rplacd(value);
                            result = value;
                            pairs = (ConsCell) cdr(pairs);
                        } while (pairs != null);
                        return result;
                    }

                    if (operator == sDefmacro) {
                        nArgs("defmacro", arguments, 1);
                        final Object macroName = car(arguments);
                        notReserved("defmacro", macroName);
                        final int arglen = length(arguments);
                        if (arglen == 1) {
                            Object prev = macros.remove(macroName);
                            return prev != null ? macroName : null;
                        }
                        else if (arglen == 3) {
                            final ConsCell closure = makeClosureFromForm(cons(sLambda, cons(cadr(arguments), cddr(arguments))), env);
                            result = macroName;
                            macros.put(result, closure);
                            return result;
                        }
                        else throw new LambdaJError(true, "defmacro: syntax error", printSEx(form));
                    }



                    /// eval - special forms that run expressions

                    /// eval - (eval form) -> object ; this is not really a special form but is handled here for TCO
                    if (operator == ocEval) {
                        nArgs("eval", arguments, 1, 2);
                        form = car(arguments);
                        if (cdr(arguments) == null) env = topEnv; // todo topEnv sind ALLE globals, eval sollte nur predefined globals bekommen
                        else {
                            final Object additionalEnv = cadr(arguments);
                            if (!listp(additionalEnv)) throw new LambdaJError(true, "eval: expected 'env' to be a list but got %s", additionalEnv);
                            env = (ConsCell) append2(additionalEnv, topEnv);
                        }
                        isTc = true; continue tailcall;
                    }

                    /// eval - (if condform form optionalform) -> object
                    if (operator == sIf) {
                        nArgs("if", arguments, 2, 3);
                        if (eval(car(arguments), env, stack, level, traceLvl) != null) {
                            form = cadr(arguments); isTc = true; continue tailcall;
                        } else if (caddr(arguments) != null) {
                            form = caddr(arguments); isTc = true; continue tailcall;
                        } else { result = null; return null; } // condition eval'd to false, no else form

                    /// eval - (load filespec) -> object
                    } else if (operator == sLoad) {
                        nArgs("load", arguments, 1);
                        return loadFile("load", car(arguments));

                    /// eval - (require modulname optfilespec) -> object
                    } else if (operator == sRequire) {
                        nArgs("require", arguments, 1, 2);
                        if (!stringp(car(arguments))) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(arguments));
                        final Object modName = car(arguments);
                        if (!modules.contains(modName)) {
                            Object modFilePath = cadr(arguments);
                            if (modFilePath == null) modFilePath = modName;
                            final Object ret = loadFile("require", modFilePath);
                            modules.add(modName);
                            return ret;
                        }
                        return null;
                    }

                    // "forms" will be set up depending on the special form and then used in "eval a list of forms" below
                    ConsCell forms = null;

                    /// eval - (progn forms...) -> object
                    if (operator == sProgn) {
                        if (!listp(arguments)) throw new LambdaJError(true, "%s: malformed progn: expected a list of forms but got %s", "progn", printSEx(arguments));
                        forms = arguments;
                        // fall through to "eval a list of forms"

                    /// eval - (cond (condform forms...)... ) -> object
                    } else if (operator == sCond) {
                        if (arguments != null)
                            for (Object c: arguments) {
                                if (!listp(c)) throw new LambdaJError(true, "%s: malformed cond: expected a list (condexpr forms...) but got %s", "cond", printSEx(c));
                                if (eval(car(c), env, stack, level, traceLvl) != null) {
                                    forms = (ConsCell) cdr(c);
                                    break;
                                }
                            }

                        if (forms == null) { result = null; return null; } // no condition was true
                        // fall through to "eval a list of forms"

                    /// eval - (labels ((symbol (params...) forms...)...) forms...) -> object
                    } else if (operator == sLabels) {
                        nArgs("labels", arguments, 1);
                        final ListConsCell extEnv = cons(cons(PSEUDO_SYMBOL, UNASSIGNED), env);
                        // stick the functions into the env
                        if (car(arguments) != null)
                            for (Object binding: (ConsCell) car(arguments)) {
                                final ConsCell currentFunc = (ConsCell)binding;
                                final Object currentSymbol = car(currentFunc);
                                notReserved("labels", currentSymbol);
                                final ConsCell lambda = makeClosure(cdr(currentFunc), extEnv);
                                insertFront(extEnv, currentSymbol, lambda);
                            }
                        forms = (ConsCell) cdr(arguments);
                        env = extEnv;
                        // fall through to "eval a list of forms"


                    /// eval - (let optsymbol? (bindings...) bodyforms...) -> object
                    /// eval - (let* optsymbol? (bindings...) bodyforms...) -> object
                    /// eval - (letrec optsymbol? (bindings...) bodyforms...) -> object
                    } else if (operator == sLet || operator == sLetStar || operator == sLetrec) {
                        final boolean letDynamic = (car(arguments)) == sDynamic;
                        if (letDynamic && !(operator == sLetStar)) throw new LambdaJError(true, "%s: malformed %s: dynamic only allowed with let*", operator, operator, form);
                        final boolean letStar  = operator == sLetStar;
                        final boolean letRec   = operator == sLetrec;
                        final boolean namedLet = !letDynamic && car(arguments) != null && symbolp(car(arguments)); // ohne "car(arguments) != null" wuerde die leere liste in "(let () 1)" als loop-symbol nil erkannt

                        final String op = letDynamic ? "let* dynamic" : (namedLet ? "named " : "") + operator;
                        final ConsCell bindingsAndBodyForms = (namedLet || letDynamic) ? (ConsCell)cdr(arguments) : arguments;  // ((bindings...) bodyforms...)

                        final ConsCell bindings = (ConsCell)car(bindingsAndBodyForms);
                        if (!listp(bindings)) throw new LambdaJError(true, "%s: malformed %s: expected a list of bindings but got %s", op, op, printSEx(car(bindingsAndBodyForms)));

                        ConsCell extenv = env;
                        if (bindings != null) {
                            final Set<Object> seen = new HashSet<>();
                            extenv = cons(cons(PSEUDO_SYMBOL, UNASSIGNED), env);
                            for (Object binding : bindings) {
                                if (!consp(binding))
                                    throw new LambdaJError(true, "%s: malformed %s: expected bindings to contain lists but got %s", op, op, printSEx(binding));
                                final Object sym = car(binding);
                                if (!symbolp(sym))
                                    throw new LambdaJError(true, "%s: malformed %s: expected binding to contain a symbol and a form but got %s", op, op, printSEx(binding));
                                notReserved(op, sym);
                                if (!listp(cdr(binding)))
                                    throw new LambdaJError(true, "%s: malformed %s: expected binding to contain a symbol and a form but got %s", op, op, printSEx(binding));
                                if (!letStar) // let allows no duplicate let symbols
                                    if (seen.contains(sym)) throw new LambdaJError(true, "duplicate symbol %s", sym);
                                    else seen.add(sym);

                                ConsCell newBinding = null;
                                if (letDynamic) newBinding = assoceq(sym, topEnv);
                                else if (letRec) newBinding = insertFront(extenv, sym, UNASSIGNED);
                                
                                if (caddr(binding) != null) throw new LambdaJError(true, "%s: malformed %s: illegal variable specification %s", op, op, printSEx(binding));
                                final Object val = eval(cadr(binding), letStar || letRec ? extenv : env, stack, level, traceLvl);
                                if (letDynamic && newBinding != null) {
                                    restore = cons(cons(newBinding, cdr(newBinding)), restore);
                                    newBinding.rplacd(val); // hier ist das zu frueh, das macht effektiv ein let* dynamic
                                }
                                else if (letRec) newBinding.rplacd(val);
                                else extenv = extendEnv(extenv, sym, val);
                            }
                        }
                        forms = (ConsCell)cdr(bindingsAndBodyForms);
                        if (namedLet) {
                            final ConsCell bodyParams = extractParamList(op, bindings);
                            final Object bodyForms = makeClosure(cons(bodyParams, forms), extenv);   // (optsymbol . (lambda (params bodyforms)))
                            insertFront(extenv, car(arguments), bodyForms);
                        }
                        env = extenv;
                        // fall through to "eval a list of forms"

                    } else if (macros.containsKey(operator)) {
                        if (trace.ge(TraceLevel.TRC_FUNC))  tracer.println(pfx(stack, level) + " #<macro " + operator + "> " + printSEx(env));
                        form = mexpand(operator, arguments, stack, level, traceLvl);
                        isTc = true; continue tailcall;
                    }



                    /// eval - function application
                    else {
                        final ConsCell argList;

                        /// eval - apply function to list
                        /// eval - (apply form argform) -> object
                        if (operator == sApply) {
                            twoArgs("apply", arguments);

                            func = eval(car(arguments), env, stack, level, traceLvl);
                            final Object _argList = eval(cadr(arguments), env, stack, level, traceLvl);
                            if (!listp(_argList)) throw new LambdaJError(true, "%s: expected an argument list but got %s", "apply", printSEx(_argList));
                            argList = (ConsCell)_argList;
                            // fall through to "actually perform..."

                        /// eval - function call
                        /// eval - (operatorform argforms...) -> object
                        } else {
                            func = eval(operator, env, stack, level, traceLvl);
                            if (!listp(arguments)) throw new LambdaJError(true, "%s: expected an argument list but got %s", "function application", printSEx(arguments));
                            argList = evlis(arguments, env, stack, level, traceLvl);
                            // fall through to "actually perform..."
                        }

                        /// eval - actually perform the function call that was set up by "apply" or "function call" above
                        traceLvl = traceEnter(func, argList, traceLvl);
                        if (func instanceof OpenCodedPrimitive) {
                            form = cons(func, argList);
                            traceStack = push(func, traceStack);
                            func = null;
                            continue tailcall;

                        } else if (primp(func)) {
                            try { result = applyPrimitive((Primitive) func, argList, stack, level); return result; }
                            catch (LambdaJError e) { throw new LambdaJError(e.getMessage()); }

                        } else if (func instanceof MurmelJavaProgram.CompilerPrimitive) {
                            // compiled function or compiler runtime func
                            try { result = applyCompilerPrimitive((MurmelJavaProgram.CompilerPrimitive) func, argList, stack, level); return result; }
                            catch (LambdaJError e) { throw new LambdaJError(e.getMessage()); }

                        } else if (consp(func) && car(func) == sLambda) {
                            final Object lambda = cdr(func);          // ((params...) (forms...))
                            final ConsCell closure = ((ConsCell)func).closure();
                            if (closure == null) nArgs("lambda application", lambda, 1); // if closure != null then it was created by the special form lambda, no need to check again
                            env = zip(closure != null ? closure : env, car(lambda), argList);

                            if (trace.ge(TraceLevel.TRC_FUNC))  tracer.println(pfx(stack, level) + " #<lambda " + lambda + "> " + printSEx(env));
                            forms = (ConsCell) cdr(lambda);
                            // fall through to "eval a list of forms"

                        } else {
                            throw new LambdaJError(true, "function application: not a primitive or lambda: %s", printSEx(func));
                        }
                    }

                    /// eval - eval a list of forms
                    // todo dotted list wird cce geben
                    for (; forms != null && cdr(forms) != null; forms = (ConsCell) cdr(forms))
                        eval(car(forms), env, stack, level, traceLvl);
                    if (forms != null) {
                        traceStack = push(operator, traceStack);
                        form = car(forms); isTc = true; func = null; continue tailcall;
                    }

                    result = null; return null; // lambda/ progn/ labels/... w/o body

                }

                /// eval - Not a symbol/atom/cons - something is really wrong here. Let's sprinkle some crack on him and get out of here, Dave.
                throw new LambdaJError("eval: cannot eval expression");
            }

        } catch (LambdaJError e) {
            throw new LambdaJError(false, e.getMessage(), form);
        } catch (Exception e) {
            //e.printStackTrace();
            throw new LambdaJError(e, "eval: internal error - caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
        } finally {
            dbgEvalDone(isTc ? "eval TC" : "eval", form, env, stack, level);
            if (func != null) traceLvl = traceExit(func, result, traceLvl);
            Object s;
            if (traceStack != null) {
                while ((s = traceStack.pollLast()) != null) traceLvl = traceExit(s, result, traceLvl);
            }
            for (ConsCell c = restore; c != null; c = (ConsCell) cdr(c)) {
                ConsCell entry = (ConsCell) caar(c);
                entry.rplacd(cdar(c));
            }
        }
    }

    private Object mexpand(Object operator, final ConsCell arguments, int stack, int level, int traceLvl) {
        final ConsCell macroClosure = macros.get(operator);
        final Object lambda = cdr(macroClosure);      // (params . (forms...))
        nArgs("macro expansion", lambda, 2);          // todo sollte unnoetig sein, sollte von defmacro sichergestellt sein (werden?)
        final ConsCell menv = zip(topEnv, car(lambda), arguments);    // todo predef env statt topenv?!?
        Object expansion = null;
        for (Object macroform: (ConsCell) cdr(lambda)) // loop over macro body so that e.g. "(defmacro m (a b) (write 'hallo) `(+ ,a ,b))" will work
            expansion = eval(macroform, menv, stack, level, traceLvl);
        return expansion;
    }

    /** Insert a new symbolentry at the front of env, env is modified in place, address of the list will not change.
     *  Returns the newly created (and inserted) symbolentry (symbol . value) */
    private ConsCell insertFront(ConsCell env, Object symbol, Object value) {
        final ConsCell symbolEntry = cons(symbol, value);
        final Object oldCar = car(env);
        final Object oldCdr = cdr(env);
        env.rplaca(symbolEntry);
        env.rplacd(cons(oldCar, oldCdr));
        return symbolEntry;
    }

    /** Extend env by attaching a new symbolentry at the front of env, env is unchanged.
     *  Returns the extended list with newly created symbolentry (symbol . value) */
    private ListConsCell extendEnv(ConsCell env, Object symbol, Object value) {
        final ListConsCell symbolEntry = cons(symbol, value);
        return cons(symbolEntry, env);
    }

    /** From a list of ((symbol form)...) return the symbols as new a list (symbol...). Throw error if any symbol is a reserved word. */
    private ConsCell extractParamList(String op, final ConsCell bindings) {
        ListConsCell bodyParams = null, insertPos = null;
        if (bindings != null)
            for (Object binding: bindings) {
                final Object symbol = car(binding);
                notReserved(op, symbol);
                if (bodyParams == null) {
                    bodyParams = cons(symbol, null);
                    insertPos = bodyParams;
                } else {
                    insertPos.rplacd(cons(symbol, null));
                    insertPos = (ListConsCell) insertPos.cdr();
                }
            }
        return bodyParams;
    }

    /** build an extended environment for a function invocation:<pre>
     *  loop over params and args
     *    construct a list (param arg)
     *    stick above list in front of the environment
     *  return extended environment</pre> */
    private ConsCell zip(ConsCell env, Object paramList, ConsCell args) {
        if (paramList == null && args == null) return env; // shortcut for no params/ no args

        for (Object params = paramList; params != null; ) {
            // regular param/arg: add to env
            if (consp(params)) {
                if (args == null) throw new LambdaJError(true, "%s: not enough arguments. parameters w/o argument: %s", "function application", printSEx(params));
                env = cons(cons(car(params), car(args)), env);
            }

            // if paramList is a dotted list then the last param will be bound to the list of remaining args
            else {
                env = cons(cons(params, args), env);
                args = null; break;
            }

            params = cdr(params);
            if (params == paramList) throw new LambdaJError(true, "%s: malformed lambda: bindings are a circular list", "function application");

            args = (ConsCell) cdr(args);
            if (args == null) {
                if (consp(params)) throw new LambdaJError(true, "%s: not enough arguments. parameters w/o argument: %s", "function application", printSEx(params));
                else if (params != null) {
                    // paramList is a dotted list, no argument for vararg parm: assign nil
                    env = cons(cons(params, null), env);
                    break;
                }
            }
        }
        if (args != null) throw new LambdaJError(true, "%s: too many arguments. remaining arguments: %s", "function application", printSEx(args));
        return env;
    }

    /** eval a list of forms and return a list of results */
    private ConsCell evlis(ConsCell forms, ConsCell env, int stack, int level, int traceLvl) {
        dbgEvalStart("evlis", forms, env, stack, level);
        ListConsCell head = null;
        ListConsCell insertPos = null;
        if (forms != null)
            for (Object form: forms) {
                final ListConsCell currentArg = cons(eval(form, env, stack, level, traceLvl), null);
                if (head == null) {
                    head = currentArg;
                    insertPos = head;
                }
                else {
                    insertPos.rplacd(currentArg);
                    insertPos = currentArg;
                }
            }
        dbgEvalDone("evlis", forms, head, stack, level);
        return head;
    }

    /** make a lexical closure (if enabled) or lambda from a lambda-form,
     *  considering whether or not "dynamic" was specified after "lambda" */
    private ConsCell makeClosureFromForm(final ConsCell form, ConsCell env) {
        final ConsCell paramsAndForms = (ConsCell) cdr(form);

        if (car(paramsAndForms) == sDynamic) {
            final Object _paramsAndForms = cdr(paramsAndForms);
            nArgs("lambda dynamic", _paramsAndForms, 1);
            symbolArgs("lambda dynamic", car(_paramsAndForms));
            noDuplicates(car(_paramsAndForms));
            return cons(sLambda, _paramsAndForms);
        }
        nArgs("lambda", paramsAndForms, 1);
        symbolArgs("lambda", car(paramsAndForms));
        noDuplicates(car(paramsAndForms));

        if (haveLexC()) return makeClosure(paramsAndForms, env);
        return form;
    }

    /** make a lexical closure (if enabled) or lambda */
    private ConsCell makeClosure(final Object paramsAndForms, ConsCell env) {
        return cons3(sLambda, paramsAndForms, haveLexC() ? env : null);
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (trace.ge(TraceLevel.TRC_FUNC)) tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        try { return primfn.apply(args); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<primitive> throws exception: %s", e.getMessage()); }
    }

    /** in case compiled code calls "(eval)" */
    private Object applyCompilerPrimitive(MurmelJavaProgram.CompilerPrimitive primfn, ConsCell args, int stack, int level) {
        if (trace.ge(TraceLevel.TRC_FUNC)) tracer.println(pfx(stack, level) + " #<compiled function> " + printSEx(args));
        try { return primfn.applyPrimitive(listToArray(args)); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<compiled function> throws exception: %s", e.getMessage()); }
    }


    private Object loadFile(String func, Object argument) {
        if (!stringp(argument)) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(argument));
        final String fileName = (String) argument;
        final SymbolTable prevSymtab = symtab;
        final Path prevPath = ((SExpressionParser)symtab).filePath;
        final Path p = findFile(prevPath, fileName);
        try (final Reader r = Files.newBufferedReader(p)) {
            final SExpressionParser parser = new SExpressionParser(r::read) {
                @Override
                public LambdaJSymbol intern(LambdaJSymbol sym) {
                    return prevSymtab.intern(sym);
                }
            };
            symtab = parser;
            Object result = null;
            for (;;) {
                final Object form = parser.readObj(true);
                if (form == null) break;

                result = eval(form, topEnv, 0, 0, 0);
            }
            return result;
        } catch (IOException e) {
            throw new LambdaJError(true, "load: error reading file '%s': ", e.getMessage());
        }
        finally {
            symtab = prevSymtab;
            ((SExpressionParser)symtab).filePath = prevPath;
        }
    }

    private Path findFile(Path current, String fileName) {
        final Path path;
        if (fileName.toLowerCase().endsWith(".lisp")) path = Paths.get(fileName);
        else path = Paths.get(fileName + ".lisp");
        if (path.isAbsolute()) return path;
        if (current == null) current = Paths.get("dummy");
        Path ret = current.resolveSibling(path);
        if (Files.isReadable(ret)) return ret;
        ret = libDir.resolve(path);
        return ret;
    }


    /// ### debug support - trace and untrace
    private Map<Object, LambdaJSymbol> traced;

    private Object trace(ConsCell symbols) {
        if (symbols == null) return traced == null ? null : new ArraySlice(traced.values().toArray(), 0);
        if (traced == null) traced = new HashMap<>();
        for (Object sym: symbols) {
            if (!symbolp(sym)) throw new LambdaJError(true, "trace: can't trace %s: not a symbol", printSEx(sym));
            final ConsCell envEntry = assoceq(sym, topEnv);
            if (envEntry == null) throw new LambdaJError(true, "trace: can't trace %s: not bound", printSEx(sym));
            traced.put(cdr(envEntry), (LambdaJSymbol) sym);
        }
        return new ArraySlice(traced.values().toArray(), 0);
    }

    private Object untrace(ConsCell symbols) {
        if (symbols == null) { traced = null; return null; }
        ConsCell ret = null;
        if (traced != null) {
            for (Object sym: symbols) {
                if (symbolp(sym)) {
                    final ConsCell envEntry = assoceq(sym, topEnv);
                    if (envEntry != null) {
                        final boolean wasTraced = traced.remove(cdr(envEntry)) != null;
                        if (wasTraced) ret = cons(sym, ret);
                    }
                }
            }
            if (traced.isEmpty()) traced = null;
        }
        return ret;
    }

    /** stack of tco'd function calls */
    private Deque<Object> push(Object op, Deque<Object> traceStack) {
        if (traced == null) return traceStack;
        if (op instanceof LambdaJSymbol) {
            final ConsCell entry = assoceq(op, topEnv);
            if (entry == null) return traceStack;
            op = cdr(entry);
        }
        if (!traced.containsKey(op)) return traceStack;
        if (traceStack == null) traceStack = new ArrayDeque<>();
        traceStack.addLast(op);
        return traceStack;
    }

    private int traceEnter(Object op, ConsCell args, int level) {
        if (traced == null || !traced.containsKey(op)) return level;
        enter(traced.get(op), args, level);
        return level + 1;
    }

    private void enter(Object op, ConsCell args, int level) {
        final StringBuilder sb = new StringBuilder();

        tracePfx(sb, level);

        sb.append('(').append(level+1).append(" enter ").append(op);
        sb.append(printArgs(args));
        sb.append(')');
        tracer.println(sb.toString());
    }

    private static String printArgs(ConsCell args) {
        if (args == null) return "";
        final StringBuilder sb = new StringBuilder();
        sb.append(':');
        for (Object arg: args) {
            sb.append(' ');
            printSEx(sb::append, arg);
        }
        return sb.toString();
    }

    private int traceExit(Object op, Object result, int level) {
        if (traced == null || !traced.containsKey(op)) return level;
        level = level < 1 ? 0 : level-1; // clamp at zero in case a traceEnter() call was lost because of a preceeding exception
        exit(traced.get(op), result, level);
        return level;
    }

    private void exit(Object op, Object result, int level) {
        final StringBuilder sb = new StringBuilder();

        tracePfx(sb, level);

        sb.append('(').append(level+1).append(" exit  ").append(op).append(':').append(' ');
        printSEx(sb::append, result);
        sb.append(')');
        tracer.println(sb.toString());
    }

    private static void tracePfx(StringBuilder sb, int level) {
        final char[] cpfx = new char[level * 2];
        Arrays.fill(cpfx, ' ');
        sb.append(cpfx);
    }



    /// ###  Stats during eval and at the end
    private int nCells;
    private int maxEnvLen;
    private int maxEvalStack;
    private int maxEvalLevel;

    /** spaces printed to the left indicate java stack usage, spaces+asterisks indicate Lisp call hierarchy depth.
     *  due to tail call optimization Java stack usage should be less than Lisp call hierarchy depth. */
    private void dbgEvalStart(String evFunc, Object exp, ConsCell env, int stack, int level) {
        if (trace.ge(TraceLevel.TRC_STATS)) {
            if (maxEvalStack < stack) maxEvalStack = stack;
            if (maxEvalLevel < level) maxEvalLevel = level;
            if (trace.ge(TraceLevel.TRC_EVAL)) {
                evFunc = fmtEvFunc(evFunc);

                final String pfx = pfx(stack, level);
                tracer.println(pfx + " " + evFunc + " (" + stack + '/' + level + ") exp:           " + printSEx(exp));
                if (trace.ge(TraceLevel.TRC_ENV)) {
                    tracer.println(pfx + " -> env size:" + length(env) + " env:     " + printSEx(env));
                }
            }
        }
    }

    private void dbgEvalDone(String evFunc, Object exp, Object env, int stack, int level) {
        if (trace.ge(TraceLevel.TRC_ENVSTATS)) {
            final int envLen = length(env);
            if (maxEnvLen < envLen) maxEnvLen = envLen;
            if (trace.ge(TraceLevel.TRC_EVAL)) {
                evFunc = fmtEvFunc(evFunc);
                final String pfx = pfx(stack, level);
                tracer.println(pfx + " " + evFunc + " (" + stack + '/' + level + ") done, exp was: " + printSEx(exp));
            }
        }
    }

    private static String fmtEvFunc(String func) {
        return (func + "          ").substring(0, 10);
    }

    private static String pfx(int stack, int level) {
        final int stackLen = stack * 2;
        final int tcoLen = 3 + (level - stack) * 2;

        final char[] cpfx = new char[stackLen + tcoLen];
        Arrays.fill(cpfx, 0, stackLen, ' ');
        Arrays.fill(cpfx, stackLen, stackLen + tcoLen, '*');

        return new String(cpfx);
    }



    /// ###  Functions used by interpreter program, a subset is used by interpreted programs as well
    private ListConsCell cons(Object car, Object cdr)                    { nCells++; return new ListConsCell(car, cdr); }
    private ListConsCell cons3(Object car, Object cdr, ConsCell closure) { nCells++; return new ClosureConsCell(car, cdr, closure); }

    private static class ArraySlice extends ConsCell {
        private static class ArraySliceIterator implements Iterator<Object> {
            private final ArraySlice coll;
            private int cursor;

            private ArraySliceIterator(ArraySlice coll) { this.coll = coll; this.cursor = coll.offset; }
            @Override public boolean hasNext() { return cursor != -1; }

            @Override
            public Object next() {
                if (cursor == -1 || coll.arry == null) throw new NoSuchElementException();
                final Object ret = coll.arry[cursor++];
                if (cursor == coll.arry.length)  cursor = -1;
                return ret;
            }
        }

        private static final long serialVersionUID = 1L;

        private final Object[] arry;
        private final int offset;

        private ArraySlice(Object[] arry) {
            if (arry != null && arry.length > 1) { this.arry = arry; offset = 1; }
            else { this.arry = null; offset = -1; }
        }

        private ArraySlice(Object[] arry, int offset) {
            if (arry != null && arry.length > offset) { this.arry = arry; this.offset = offset; }
            else { this.arry = null; this.offset = -1; }
        }

        private ArraySlice(ArraySlice slice) {
            if (slice.arry != null && slice.arry.length > slice.offset) { this.arry = slice.arry; offset = slice.offset + 1; }
            else { this.arry = null; offset = -1; }
        }

        @Override public Object     car() { return (arry == null || arry.length <= offset) ? null : arry[offset]; }
        @Override public ArraySlice cdr() { return (arry == null || arry.length <= offset+1) ? null : new ArraySlice(this); }
        @Override public String toString() { return printSEx(true, false); }
        @Override public Iterator<Object> iterator() { return new ArraySliceIterator(this); }

        private String printSEx(boolean headOfList, boolean escapeAtoms) {
            if (arry == null || arry.length <= offset) return LambdaJ.printSEx(null);
            else {
                final StringBuilder ret = new StringBuilder();
                if (headOfList) ret.append('(');
                boolean first = true;
                for (int i = offset; i < arry.length; i++) {
                    final Object o = arry[i];
                    if (first) first = false;
                    else ret.append(' ');
                    _printSEx(ret::append, arry, o, true, escapeAtoms);
                }
                ret.append(')');
                return ret.toString();
            }
        }
    }

    private static Object   car(ConsCell c)    { return c == null ? null : c.car(); }
    private static Object   car(Object o)      { return o == null ? null
                                                 : o instanceof ListConsCell ? ((ListConsCell)o).car()
                                                 : o instanceof ConsCell ? ((ConsCell)o).car()
                                                 : o instanceof Object[] ? (((Object[])o).length == 0 ? null : ((Object[])o)[0])
                                                 : o instanceof String ? (((String)o).isEmpty() ? null : ((String)o).charAt(0))
                                                 : o instanceof LambdaJSymbol ? ((LambdaJSymbol)o).name.charAt(0)
                                                 : carCdrError("car", o); }

    private static Object carCdrError(String func, Object o) { throw new LambdaJError(true, "%s: expected one pair or symbol or string argument but got %s", func, printSEx(o)); }

    private static Object   caar(ConsCell c)   { return c == null ? null : car(car(c)); }
    private static Object   caadr(ConsCell c)  { return c == null ? null : car(cadr(c)); }
    private static Object   cadr(ConsCell c)   { return c == null ? null : car(cdr(c)); }
    private static Object   cadr(Object o)     { return o == null ? null : car(cdr(o)); }
    private static Object   cadar(ConsCell c)  { return c == null ? null : car(cdar(c)); }
    private static Object   caddr(ConsCell c)  { return c == null ? null : car(cddr(c)); }
    private static Object   caddr(Object o)    { return o == null ? null : car(cddr(o)); }
    private static Object   cadddr(ConsCell o) { return o == null ? null : car(cdddr(o)); }
    private static Object   cadddr(Object o)   { return o == null ? null : car(cdddr(o)); }

    private static Object   cdr(ConsCell c)    { return c == null ? null : c.cdr(); }
    private static Object   cdr(Object o)      { return o == null ? null
                                                 : o instanceof ListConsCell ? ((ListConsCell)o).cdr()
                                                 : o instanceof ConsCell ? ((ConsCell)o).cdr()
                                                 : o instanceof Object[] ? (((Object[])o).length <= 1 ? null : new ArraySlice((Object[])o))
                                                 : o instanceof String ? (((String)o).length() <= 1 ? null : ((String)o).substring(1))
                                                 : o instanceof LambdaJSymbol ? ((LambdaJSymbol)o).name.substring(1)
                                                 : carCdrError("cdr", o); }

    private static Object   cdar(ConsCell c)   { return c == null ? null : cdr(car(c)); }
    private static Object   cdar(Object o)     { return o == null ? null : cdr(car(o)); }
    private static Object   cddr(ConsCell c)   { return c == null ? null : cdr(cdr(c)); }
    private static Object   cddr(Object o)     { return o == null ? null : cdr(cdr(o)); }
    private static Object   cdddr(ConsCell o)  { return o == null ? null : cdr(cddr(o)); }
    private static Object   cdddr(Object o)    { return o == null ? null : cdr(cddr(o)); }

    private static boolean  consp(Object o)    { return o instanceof ConsCell; }
    private static boolean  atom(Object o)     { return !(o instanceof ConsCell); }                // ! consp(x)
    private static boolean  symbolp(Object o)  { return o == null || o instanceof LambdaJSymbol; } // null (aka nil) is a symbol too
    private static boolean  listp(Object o)    { return o == null || o instanceof ConsCell; }      // null (aka nil) is a list too
    private static boolean  primp(Object o)    { return o instanceof Primitive; }
    private static boolean  numberp(Object o)  { return o instanceof Number; }
    private static boolean  stringp(Object o)  { return o instanceof String; }
    private static boolean  floatp(Object o)   { return o instanceof Double; }
    private static boolean  integerp(Object o) { return o instanceof Long; }
    private static boolean  characterp(Object o) { return o instanceof Character; }

    /** return a string with "line x:y..xx:yy: " */
    private static String lineInfo(Object form) {
        if (!(form instanceof SExpConsCell)) return "";
        final SExpConsCell f = (SExpConsCell)form;
        return "line " + f.startLineNo + ':' + f.startCharNo + ".." + f.lineNo + ':' + f.charNo + ':' + ' ';
    }

    private static int length(Object list) {
        if (list == null) return 0;
        int n = 0;
        for (Object ignored: (ConsCell)list) n++;
        return n;
    }

    /** todo this should handle circular and dotted lists but doesn't, todo avoid cce on dotted lists, throw eror instead:
     * (nthcdr 3 '(0 . 1))) -> Error: Attempted to take CDR of 1. */
    private static Object nthcdr(int n, Object list) {
        if (list == null) return null;
        for (; list != null && n-- > 0; list = cdr(list)) /* nothing */;
        return list;
    }

    /** note: searches using object identity (eq), will work for interned symbols, won't reliably work for e.g. numbers */
    private static ConsCell assoceq(Object atom, Object maybeList) {
        if (maybeList == null) return null;
        if (!consp(maybeList)) throw new LambdaJError(true, "%s: expected second argument to be a list but got %s", "assoc", printSEx(maybeList));
        for (Object env: (ConsCell) maybeList) {
            if (env != null) {
                final ConsCell _env = (ConsCell) env;
                if (atom == car(_env)) return _env;
            }
        }
        return null;
    }

    private static ConsCell assoc(Object atom, Object maybeList) {
        if (maybeList == null) return null;
        if (!consp(maybeList)) throw new LambdaJError(true, "%s: expected second argument to be a list but got %s", "assoc", printSEx(maybeList));
        for (Object item: (ConsCell) maybeList) {
            if (item != null) { // ignore null items
                final ConsCell itemAsCons = (ConsCell) item;
                if (cl_eql(atom, car(itemAsCons))) return itemAsCons;
            }
        }
        return null;
    }

    /** this method returns true while Lisp member returns the sublist starting at obj */
    private static boolean member(Object obj, ConsCell list) {
        if (obj == null) return false;
        if (list == null) return false;
        for (Object e: list) if (e == obj) return true;
        return false;
    }

    private Object eval(Object form, Object env) {
        if (!listp(env)) throw new LambdaJError(true, "eval: expected 'env' to be a list but got %s", env);
        return eval(form, env != null ? (ConsCell) append2(env, topEnv) : topEnv, 0, 0, 0);
    }

    private ConsCell list(Object... a) {
        if (a == null || a.length == 0) return null;
        ListConsCell ret = null, insertPos = null;
        for (Object o: a) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                insertPos.rplacd(cons(o, null));
                insertPos = (ListConsCell) insertPos.cdr();
            }
        }
        return ret;
    }

    /** Create a new list by copying lhs and appending rhs. */
    private Object append2(Object lhs, Object rhs) {
        if (lhs == null) return rhs;
        if (!consp(lhs)) throw new LambdaJError(true, "append2: first argument %s is not a list", lhs);
        ListConsCell ret = null, insertPos = null;
        for (Object o: (ConsCell)lhs) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                insertPos.rplacd(cons(o, null));
                insertPos = (ListConsCell) insertPos.cdr();
            }
        }
        insertPos.rplacd(rhs);
        return ret;
    }

    /** append args non destructively, all args except the last are shallow copied, all args except the last must be a list */
    // todo CL macht deep copy bei allen args ausser dem letzten, alle args ausser dem letzten muessen proper lists sein (murmel behandelt dotted und proper lists gleich)
    private Object append(Object... args) {
        if (args == null || args.length == 0) return null;
        if (args.length == 1) return args[0];
        if (!listp(args[0])) throw new LambdaJError(true, "append: first argument %s is not a list", args[0]);
        Object ret = args[0];
        for (int i = 1; i < args.length; i++) {
            ret = append2(ret, args[i]); // todo optimieren: bei n args wird n-1 mal kopiert -> insertpos mitfuehren
        }
        return ret;
    }

    private static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return null;
        if (maybeList instanceof ArraySlice) {
            final ArraySlice slice = (ArraySlice)maybeList;
            if (slice.offset == 0) return slice.arry;
            return Arrays.copyOfRange(slice.arry, slice.offset, slice.arry.length);
        }
        if (!listp(maybeList)) throw new LambdaJError(true, "%s: expected argument to be a list but got %s", "listToArray", printSEx(maybeList));
        final List<Object> ret = new ArrayList<>();
        ((ConsCell) maybeList).forEach(ret::add); // todo forEach behandelt dotted und proper lists gleich -> im interpreter gibt (apply < '(1 2 3 4 . 5)) einen fehler, im compiler nicht
        return ret.toArray();
    }

    private static ConsCell mapcar(UnaryOperator<Object> f, ConsCell l) {
        final ListBuilder b = new ListBuilder();
        Object o = l;
        for (;;) {
            if (o == null) break;
            if (consp(o)) {
                b.append(f.apply(car(o)));
            }
            else {
                b.appendLast(f.apply(o));
                break;
            }
            o = cdr(o);
        }
        return (ConsCell) b.first();
    }

    /** transform {@code ob} into an S-expression, atoms are not escaped */
    private static String printObj(Object ob) {
        if (ob == null) return "nil";
        final StringBuilder sb = new StringBuilder(200);
        _printSEx(sb::append, ob, ob, true, false);
        return sb.toString();
    }

    /** transform {@code ob} into an S-expression, atoms are escaped */
    private static String printSEx(Object obj) {
        if (obj == null) return "nil";
        final StringBuilder sb = new StringBuilder(200);
        _printSEx(sb::append, obj, obj, true, true);
        return sb.toString();
    }

    private static void printSEx(WriteConsumer w, Object obj) {
        _printSEx(w, obj, obj, true, true);
    }

    private static void _printSEx(WriteConsumer sb, Object list, Object obj, boolean headOfList, boolean escapeAtoms) {
        while (true) {
            if (obj == null) {
                sb.print("nil"); return;
            } else if (obj instanceof ArraySlice) {
                sb.print(((ArraySlice)obj).printSEx(headOfList, escapeAtoms)); return;
            } else if (listp(obj)) {
                if (headOfList) sb.print("(");
                final Object first = car(obj);
                if (first == list) {
                    sb.print(headOfList ? "#<this cons>" : "#<this list>");
                } else {
                    _printSEx(sb, first, first, true, escapeAtoms);
                }
                final Object rest = cdr(obj);
                if (rest != null) {
                    if (listp(rest)) {
                        sb.print(" ");
                        if (list == rest) {
                            sb.print("#<circular list>)"); return;
                        } else {
                            obj = rest; headOfList = false; continue;
                        }
                    } else if (headOfList) {
                        sb.print(" . ");
                        _printSEx(sb, list, rest, false, escapeAtoms);
                        sb.print(")");
                        return;
                    } else {
                        sb.print(" . ");
                        _printSEx(sb, list, rest, false, escapeAtoms); // must be an atom
                        sb.print(")");
                        return;
                    }
                } else {
                    sb.print(")");
                    return;
                }
            } else if (escapeAtoms && symbolp(obj)) {
                if (obj.toString().isEmpty()) {
                    sb.print("||");
                    return;
                }
                if (".".equals(obj.toString())) {
                    sb.print("|.|");
                    return;
                }
                if (containsSExSyntaxOrWhiteSpace(obj.toString())) {
                    sb.print("|"); sb.print(escapeSymbol((LambdaJSymbol) obj)); sb.print("|");
                    return;
                }
                sb.print(escapeSymbol((LambdaJSymbol) obj)); return;
            } else if (obj instanceof OpenCodedPrimitive) {
                sb.print(obj.toString()); return;
            } else if (primp(obj)) {
                sb.print("#<primitive>"); return;
            } else if (escapeAtoms && stringp(obj)) {
                sb.print("\""); sb.print(escapeString(obj.toString())); sb.print("\""); return;
            } else if (escapeAtoms && characterp(obj)) {
                sb.print(printChar((int)(Character)obj));
                return;
            } else if (atom(obj)) {
                sb.print(obj.toString()); return;
            } else {
                sb.print("<internal error>"); return;
            }
        }
    }

    private static String printChar(int c) {
        return "#\\"
         + ((c < CTRL.length) ? CTRL[c]
        : (c < 127) ? String.valueOf((char)c)
        : String.valueOf(c));
    }

    private static String escapeSymbol(LambdaJSymbol s) {
        if (s.name == null) return null;
        if (s.name.isEmpty()) return "";

        final StringBuilder ret = new StringBuilder();
        for (char c: s.name.toCharArray()) {
            switch (c) {
            case '|':  ret.append('\\').append('|'); break;
            default: ret.append(c);
            }
        }
        return ret.toString();
    }

    /** prepend " and \ by a \ */
    private static String escapeString(String s) {
        if (s == null) return null;
        if (s.isEmpty()) return "";

        final StringBuilder ret = new StringBuilder();
        for (char c: s.toCharArray()) {
            switch (c) {
            case '\"':  ret.append('\\').append('\"'); break;
            case '\\': ret.append('\\').append('\\'); break;
            default: ret.append(c);
            }
        }
        return ret.toString();
    }



    /// ##  Error checking functions, used by interpreter and primitives
    /** ecactly one argument */
    private static void oneArg(String func, Object a) {
        if (a == null)      throw new LambdaJError(true, "%s: expected one argument but no argument was given", func);
        if (cdr(a) != null) throw new LambdaJError(true, "%s: expected one argument but got extra arg(s) %s", func, printSEx(cdr(a)));
    }

    /** ecactly two arguments */
    private static void twoArgs(String func, Object a) {
        if (a == null)       throw new LambdaJError(true, "%s: expected two arguments but no argument was given", func);
        a = cdr(a);
        if (a == null)  throw new LambdaJError(true, "%s: expected two arguments but only one argument was given", func);
        a = cdr(a);
        if (a != null) throw new LambdaJError(true, "%s: expected two arguments but got extra arg(s) %s", func, printSEx(a));
    }

    /** at least {@code min} args */
    private static void nArgs(String func, Object a, int min) {
        final int actualLength = length(a);
        if (actualLength < min) throw new LambdaJError(true, "%s: expected %d arguments or more but got only %d", func, min, actualLength);
    }

    /** between {@code min} and {@code max} args */
    private static void nArgs(String func, Object a, int min, int max) {
        final int actualLength = length(a);
        if (actualLength < min) {
            if (min == max) throw new LambdaJError(true, "%s: expected %d arguments but got only %d", func, min, actualLength);
            throw new LambdaJError(true, "%s: expected %d to %d arguments but got only %d", func, min, max, actualLength);
        }
        if (actualLength > max) {
            if (min == max) throw new LambdaJError(true, "%s: expected %d arguments but got extra arg(s) %s", func, min, printSEx(nthcdr(max, a)));
            throw new LambdaJError(true, "%s: expected %d to %d arguments but got extra arg(s) %s", func, min, max, printSEx(nthcdr(max, a)));
        }
    }

    /** 'a' must be a symbol or a proper or dotted list of only symbols (empty list is fine, too).
     *  Also 'a' must not contain reserved symbols. */
    private void symbolArgs(String func, Object a) {
        if (symbolp(a)) return;
        if (atom(a)) throw new LambdaJError(true, "%s: malformed %s: expected bindings to be a symbol or list of symbols but got %s", func, func, a);
        final ConsCell start = (ConsCell) a;
        for (;;) {
            if (consp(a) && cdr(a) == start) throw new LambdaJError(true, "%s: malformed %s: circular list of bindings is not allowed", func, func);
            if (!symbolp(car(a))) throw new LambdaJError(true, "%s: expected a symbol or a list of symbols but got %s", func, printSEx(a));
            notReserved(func, car(a));

            a = cdr(a);
            if (a == null) return; // end of a proper list, everything a-ok, move along
            if (atom(a)) {
                if (!symbolp(a)) throw new LambdaJError(true, "%s: expected a symbol or a list of symbols but got %s", func, printSEx(a));
                notReserved(func, a);
                return; // that was the end of a dotted list, everything a-ok, move along
            }
        }
    }

    private static void noDuplicates(Object symList) {
        if (symList == null) return;
        if (!consp(symList)) return;
        final Set<Object> seen = new HashSet<>();
        for (Object o: (ConsCell)symList) {
            if (seen.contains(o)) throw new LambdaJError(true, "duplicate symbol %s", o);
            else seen.add(o);
        }
    }

    ///
    /// ## Summary
    /// That's (almost) all, folks.
    ///
    /// At this point we have reached the end of the Murmel interpreter core, i.e. we have everything needed
    /// to read S-Expressions and eval() them in an environment.
    ///
    /// The rest of this file contains Murmel primitives and driver functions such as interpretExpression/s and main
    /// for interactive use.
    ///
    /// And a compiler Murmel to Java source, classes or jars.



    ///
    /// ## Murmel runtime
    ///

    /// Additional error checking functions used by primitives only.

    /** a must be the empty list */
    private static void noArgs(String func, ConsCell a) {
        if (a != null) throw new LambdaJError(true, "%s: expected no arguments but got %s", func, printSEx(a));
    }

    private static void oneArg(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(true, "%s: expected one argument but no argument was given", func);
        if (cdr(a) != null) throw new LambdaJError(true, "%s: expected one argument but got %s", func, printSEx(a));
    }

    private static void oneOrMoreArgs(String func, ConsCell a) {
        if (a == null) throw new LambdaJError(true, "%s: expected at least one argument but no argument was given", func);
    }

    /** a must be a proper list of only numbers (empty list is fine, too) */
    private static void numberArgs(String func, ConsCell a) {
        if (a == null) return;
        final ConsCell start = a;
        for (; a != null; a = (ConsCell) cdr(a)) {
            if (!numberp(car(a)) || (cdr(a) != null && (cdr(a) == start || !consp(cdr(a)))))
                throw new LambdaJError(true, "%s: expected a proper list of numbers but got %s", func, printSEx(a));
        }
    }

    /** between {@code min} and {@code max} number args */
    private static void numberArgs(String func, ConsCell a, int min, int max) {
        nArgs(func, a, min, max);
        numberArgs(func, a);
    }

    /** one number arg */
    private static void oneNumber(String func, ConsCell a) {
        oneArg(func, a);
        numberArgs(func, a);
    }

    /** at least one number arg */
    private static void oneOrMoreNumbers(String func, ConsCell a) {
        oneOrMoreArgs(func, a);
        numberArgs(func, a);
    }

    /** the given arg must be a LambdaJString */
    private static void stringArg(String func, String arg, ConsCell a) {
        if (!stringp(car(a)))
            throw new LambdaJError(true, "%s: expected %s to be a string but got %s", func, arg, printSEx(car(a)));
    }

    /** a must be a proper list of only strings (empty list is fine, too) */
    private static void stringArgs(String func, ConsCell a) {
        if (a == null) return;
        final ConsCell start = a;
        for (; a != null; a = (ConsCell) cdr(a)) {
            if (!stringp(car(a)) || (cdr(a) != null && (cdr(a) == start || !consp(cdr(a)))))
                throw new LambdaJError(true, "%s: expected a proper list of strings but got %s", func, printSEx(a));
        }
    }

    private TurtleFrame current_frame;

    /** Return {@code a} as a TurtleFrame or current_frame if null, error if {@code a} is not of type frame. */
    private TurtleFrame asFrame(String func, Object a) {
        final TurtleFrame ret;
        if (a == null) {
            ret = current_frame;
        }
        else {
            if (!(a instanceof TurtleFrame)) throw new LambdaJError(true, "%s: expected a frame argument but got %s", func, printSEx(a));
            ret = (TurtleFrame) a;
        }
        if (ret == null) throw new LambdaJError(true, "%s: no frame argument and no current frame", func);
        return ret;
    }

    /** Return {@code a} as a float, error if {@code a} is not a number. */
    private static float asFloat(String func, Object a) {
        if (!(a instanceof Number)) throw new LambdaJError(true, "%s: expected a number argument but got %s", func, printSEx(a));
        return ((Number)a).floatValue();
    }

    /** Return {@code a} as a double, error if {@code a} is not a number. */
    private static double asDouble(String func, Object a) {
        if (!(a instanceof Number)) throw new LambdaJError(true, "%s: expected a number argument but got %s", func, printSEx(a));
        return ((Number)a).doubleValue();
    }

    /** Return {@code a} as an int, error if {@code a} is not a number. */
    private static int asInt(String func, Object a) {
        if (!(a instanceof Number)) throw new LambdaJError(true, "%s: expected a number argument but got %s", func, printSEx(a));
        return ((Number)a).intValue();
    }

    /** Return {@code c} as a Character, error if {@code c} is not a Character. */
    private static Character asChar(String func, Object c) {
        if (!(c instanceof Character)) throw new LambdaJError(true, "%s: expected a character argument but got %s", func, printSEx(c));
        return (Character)c;
    }

    /** Return {@code c} as a String, error if {@code c} is not a string, character or symbol. */
    private static String asString(String func, Object c) {
        if (c == null) return null;
        if (!(c instanceof String) && !(c instanceof Character) && !(c instanceof LambdaJSymbol)) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(c));
        return c.toString();
    }

    /** Return {@code a} cast to a list, error if {@code a} is not a list or nil. */
    private static ConsCell asList(String func, Object a) {
        if (!consp(a)) throw new LambdaJError(true, "%s: expected a non-nil list argument but got %s", func, printSEx(a));
        return (ConsCell)a;
    }



    /// Runtime for Lisp programs, i.e. an environment with primitives and predefined global symbols

    private Object boolResult(boolean b) { return b ? expTrue.get() : null; }

    interface DoubleBiPred {
        boolean test(double d1, double d2);
    }

    /** compare subsequent pairs of the given list of numbers with the given predicate */
    private Object compare(ConsCell args, String opName, DoubleBiPred pred) {
        oneOrMoreNumbers(opName, args);
        Number prev = (Number)car(args);
        for (ConsCell rest = (ConsCell)cdr(args); rest != null; rest = (ConsCell)cdr(rest)) {
            final Number next = (Number)car(rest);
            if (!pred.test(prev.doubleValue(), next.doubleValue())) return null;
            prev = next;
        }
        return expTrue.get();
    }

    /** generate operator for zero or more args */
    private static Object makeAddOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        numberArgs(opName, args);
        if (car(args) == null) return startVal;
        double result = ((Number)car(args)).doubleValue();
        for (args = (ConsCell) cdr(args); args != null; args = (ConsCell) cdr(args))
            result = op.applyAsDouble(result, ((Number)car(args)).doubleValue());
        return result;
    }

    /** generate operator for one or more args */
    private static Object makeSubOp(ConsCell args, String opName, double startVal, DoubleBinaryOperator op) {
        oneOrMoreNumbers(opName, args);
        double result = ((Number)car(args)).doubleValue();
        if (cdr(args) == null) return op.applyAsDouble(startVal, result);
        for (args = (ConsCell) cdr(args); args != null; args = (ConsCell) cdr(args))
            result = op.applyAsDouble(result, ((Number)car(args)).doubleValue());
        return result;
    }

    private static Object cl_rplaca(Object args) {
        twoArgs("rplaca", args);
        final ConsCell l = asList("rplaca", car(args));
        l.rplaca(cadr(args));
        return l;
    }

    private static Object cl_rplacd(Object args) {
        twoArgs("rplacd", args);
        final ConsCell l = asList("rplacd", car(args));
        l.rplacd(cadr(args));
        return l;
    }

    private void write(final Object arg) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "write");
        lispPrinter.printObj(arg);
    }

    private void writeln(final ConsCell arg) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "writeln");
        if (arg != null) {
            lispPrinter.printObj(car(arg));
        }
        lispPrinter.printEol();
    }

    private void lnwrite(final ConsCell arg) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "lnwrite");
        lispPrinter.printEol();
        if (arg != null) {
            lispPrinter.printObj(car(arg));
            lispPrinter.printString(" ");
        }
    }

    /** expand a single macro call */
    private Object macroexpand1(ConsCell args) {
        oneArg("macroexpand-1", args);
        if (!consp(car(args))) return car(args);
        final Object operator = caar(args);
        if (!macros.containsKey(operator)) return car(args);
        final ConsCell arguments = (ConsCell) cdar(args);
        return mexpand(operator, arguments, 0, 0, 0);
    }

    private int gensymCounter;
    private Object gensym(ConsCell args) {
        return new LambdaJSymbol("gensym" + ++gensymCounter);
    }

    private String format(ConsCell a) {
        final String func = "format";
        nArgs(func, a, 2);
        final boolean toString = car(a) == null;
        a = (ConsCell) cdr(a);
        stringArg(func, "second argument", a);
        final String s = (String) car(a);
        final Object[] args = listToArray(cdr(a));
        try {
            if (toString) return EolUtil.anyToUnixEol(String.format(s, args));
            if (!haveIO()) throw new LambdaJError(true, "%s: I/O is disabled", func);
            if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", func);
            lispPrinter.printString(EolUtil.anyToUnixEol(String.format(s, args)));
            return null;
        } catch (IllegalFormatException e) {
            throw new LambdaJError(true, "%s: illegal format string and/ or arguments: %s" + System.lineSeparator() + "error ocurred processing the argument(s) %s", func, e.getMessage(), printSEx(a));
        }
    }

    private String formatLocale(ConsCell a) {
        final String func = "format-locale";
        nArgs(func, a, 3);
        final boolean toString = car(a) == null;
        a = (ConsCell) cdr(a);

        final String locString;
        if (car(a) != null) {
            stringArg(func, "first argument", a);
            locString = (String) car(a);
        } else locString = null;

        stringArg(func, "third argument", (ConsCell) cdr(a));
        final String s = (String) cadr(a);
        final Object[] args = listToArray(cddr(a));
        try {
            if (locString == null) {
                if (toString) return EolUtil.anyToUnixEol(String.format(s, args));
                if (!haveIO()) throw new LambdaJError(true, "%s: I/O is disabled", func);
                if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", func);
                lispPrinter.printString(EolUtil.anyToUnixEol(String.format(s, args)));
                return null;
            }
            final Locale loc = Locale.forLanguageTag(locString);
            if (toString) return EolUtil.anyToUnixEol(String.format(loc, s, args));
            if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", func);
            lispPrinter.printString(EolUtil.anyToUnixEol(String.format(loc, s, args)));
            return null;
        } catch (IllegalFormatException e) {
            throw new LambdaJError(true,
                    "%s: illegal format string and/ or arguments: %s" + System.lineSeparator() + "error ocurred processing the argument(s) %s", func, e.getMessage(), printSEx(a));
        }
    }

    private static long getInternalRealTime() {
        return System.nanoTime();
    }

    private static long getInternalRunTime() {
        return getThreadBean("get-internal-run-time").getCurrentThreadUserTime();
    }

    private static long getInternalCpuTime() {
        return getThreadBean("get-internal-cpu-time").getCurrentThreadCpuTime();
    }

    private static long sleep(ConsCell a) {
        oneArg("sleep", a);
        numberArgs("sleep", a);
        try {
            final long startNanos = System.nanoTime();
            final long nanos = ((Number)car(a)).longValue();
            final long millis = TimeUnit.NANOSECONDS.toMillis(nanos);
            Thread.sleep(millis);
            return System.nanoTime() - startNanos;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new LambdaJError("sleep: got interrupted");
        }
    }

    private static long getUniversalTime() {
        final ZoneId utc = ZoneId.of("UTC");
        final ZonedDateTime ld1900 = ZonedDateTime.of(1900, 1, 1, 0, 0, 0, 0, utc);
        return ld1900.until(ZonedDateTime.now(utc), ChronoUnit.SECONDS);
    }

    private Object getDecodedTime() {
        final Instant now = Clock.systemDefaultZone().instant();
        final ZonedDateTime n = now.atZone(ZoneId.systemDefault());
        final ZoneRules rules = n.getZone().getRules();
        final boolean daylightSavings = rules.isDaylightSavings(now);
        final double offset = -rules.getOffset(now).get(ChronoField.OFFSET_SECONDS) / 3600.0;
        //get-decoded-time <no arguments> => second, minute, hour, date, month, year, day, daylight-p, zone
        return cons(n.getSecond(), cons(n.getMinute(), cons(n.getHour(),
               cons(n.getDayOfMonth(), cons(n.getMonthValue(), cons(n.getYear(), cons(n.getDayOfWeek().getValue() - 1,
               cons(boolResult(daylightSavings), cons(offset, null)))))))));
    }

    private static ThreadMXBean getThreadBean(final String func) {
        final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
        if (threadBean == null)
            throw new LambdaJError(true, "%s: ThreadMXBean not supported in this Java Runtime", func);
        if (!threadBean.isCurrentThreadCpuTimeSupported())
            throw new LambdaJError(true, "%s: ThreadMXBean.getCurrentThreadCpuTime() not supported in this Java Runtime", func);
        return threadBean;
    }



    /// Murmel runtime support for Java FFI - Murmel calls Java
    private static class JavaConstructor implements Primitive {
        private final Constructor<?> constructor;

        private JavaConstructor(Constructor<?> constructor) { this.constructor = constructor; }

        @Override
        public Object apply(ConsCell x) {
            final Object[] args = listToArray(x);
            try { return constructor.newInstance(args); }
            catch (Exception e) { throw new LambdaJError(true, "new %s: %s: %s", constructor.getName(), e.getClass().getName(), e.getMessage()); }
        }
    }

    private static class JavaMethod implements Primitive {
        private final Method method;

        private JavaMethod(Method method) { this.method = method; }

        @Override
        public Object apply(ConsCell x) {
            final Object obj = car(x);
            if (obj != null && !method.getDeclaringClass().isInstance(obj))
                throw new LambdaJError(true, ":: : %s is not an instance of class %s", obj, method.getDeclaringClass().getName());
            final Object[] args = listToArray(cdr(x));
            try { return method.invoke(obj, args); }
            catch (Exception e) { throw new LambdaJError(true, "%s.%s: exception: %s", method.getClass(), method.getName(), e.getMessage()); }
        }
    }

    private static Primitive findJavaMethod(ConsCell x) {
        stringArgs(":: ", x);
        final String className = (String) car(x);
        final String methodName = (String) cadr(x);
        final ArrayList<Class<?>> paramTypes = new ArrayList<>();
        if (cddr(x) != null)
        for (Object arg: (ConsCell)cddr(x)) {
            final String paramType = (String)arg;
            try { paramTypes.add(Class.forName(paramType)); }
            catch (ClassNotFoundException e) { throw new LambdaJError(true, ":: : exception finding parameter class %s: %s : %s", paramType, e.getClass().getName(), e.getMessage()); }
        }
        final Class<?>[] params = paramTypes.isEmpty() ? null : paramTypes.toArray(new Class[0]);
        try {
            final Class<?> clazz = Class.forName(className);
            return "new".equals(methodName)
                    ? new JavaConstructor(clazz.getDeclaredConstructor(params))
                            : new JavaMethod(clazz.getMethod(methodName, params));
        }
        catch (Exception e) { throw new LambdaJError(true, ":: : exception finding method: %s: %s", e.getClass().getName(), e.getMessage()); }
    }



    private ObjectReader lispReader;
    private ObjectWriter lispPrinter;

    public ObjectReader getLispReader()  { return lispReader; }
    public ObjectWriter getLispPrinter() { return lispPrinter; }

    public void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) {
        this.lispReader = lispStdin;
        this.lispPrinter = lispStdout;
    }

    /** build an environment by prepending the previous environment {@code pre} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@code symtab} on the fly */
    private ConsCell environment(ConsCell env) {
        if (haveIO()) {
            final Primitive freadobj =  a -> {
                noArgs("read", a);
                if (lispReader == null) throw new LambdaJError(true, "%s: lispStdin is nil", "read");
                return lispReader.readObj();
            };
            env = addBuiltin("read",    freadobj,
                  addBuiltin("write",   (Primitive) a -> { oneArg("write", a);         write(car(a));    return expTrue.get(); },
                  addBuiltin("writeln", (Primitive) a -> { nArgs("writeln", a, 0, 1);  writeln(a);  return expTrue.get(); },
                  addBuiltin("lnwrite", (Primitive) a -> { nArgs("lnwrite", a, 0, 1);  lnwrite(a);  return expTrue.get(); },
                  env))));

            final Primitive makeFrame = a -> {
                stringArg("make-frame", "first arg", a);
                final String title = car(a).toString();
                numberArgs("make-frame", (ConsCell) cdr(a), 0, 3);
                final TurtleFrame ret = new TurtleFrame(title, (Number)cadr(a), (Number)caddr(a), (Number)cadddr(a));
                current_frame = ret;
                return ret;
            };
            env = addBuiltin("make-frame",    makeFrame,
                  addBuiltin("open-frame",    (Primitive) a -> { nArgs("open-frame",    a, 0, 1); return asFrame("open-frame",    car(a)).open();    },
                  addBuiltin("close-frame",   (Primitive) a -> { nArgs("close-frame",   a, 0, 1); return asFrame("close-frame",   car(a)).close();   },
                  addBuiltin("reset-frame",   (Primitive) a -> { nArgs("reset-frame",   a, 0, 1); return asFrame("reset-frame",   car(a)).reset();   },
                  addBuiltin("clear-frame",   (Primitive) a -> { nArgs("clear-frame",   a, 0, 1); return asFrame("clear-frame",   car(a)).clear();   },
                  addBuiltin("repaint-frame", (Primitive) a -> { nArgs("repaint-frame", a, 0, 1); return asFrame("repaint-frame", car(a)).repaint(); },
                  addBuiltin("flush-frame",   (Primitive) a -> { nArgs("flush-frame", a, 0, 1);   return asFrame("flush-frame",   car(a)).flush(); },

                  // set new current frame, return previous frame
                  addBuiltin("current-frame", (Primitive) a -> { nArgs("current-frame", a, 0, 1); final Object prev = current_frame; if (car(a) != null) current_frame = asFrame("current-frame", car(a)); return prev; },

                  addBuiltin("push-pos",      (Primitive) a -> { nArgs("push-pos",a, 0, 1); return asFrame("push-pos",car(a)).pushPos(); },
                  addBuiltin("pop-pos",       (Primitive) a -> { nArgs("pop-pos", a, 0, 1); return asFrame("pop-pos", car(a)).popPos();  },

                  addBuiltin("pen-up",        (Primitive) a -> { nArgs("pen-up",  a, 0, 1); return asFrame("pen-up",   car(a)).penUp();   },
                  addBuiltin("pen-down",      (Primitive) a -> { nArgs("pen-down",a, 0, 1); return asFrame("pen-down", car(a)).penDown(); },

                  addBuiltin("color",         (Primitive) a -> { nArgs("color",   a, 0, 1); return asFrame("color",   cadr(a)).color  (asInt("color",   car(a))); },
                  addBuiltin("bgcolor",       (Primitive) a -> { nArgs("bgcolor", a, 0, 1); return asFrame("bgcolor", cadr(a)).bgColor(asInt("bgcolor", car(a))); },

                  addBuiltin("text",          (Primitive) a -> { nArgs("text",    a, 1, 2); return asFrame("text",    cadr(a)).text   (car(a).toString()); },

                  addBuiltin("right",         (Primitive) a -> { nArgs("right",   a, 1, 2); return asFrame("right",   cadr(a)).right  (asDouble("right",   car(a))); },
                  addBuiltin("left",          (Primitive) a -> { nArgs("left",    a, 1, 2); return asFrame("left",    cadr(a)).left   (asDouble("left",    car(a))); },
                  addBuiltin("forward",       (Primitive) a -> { nArgs("forward", a, 1, 2); return asFrame("forward", cadr(a)).forward(asDouble("forward", car(a))); },
                  env))))))))))))))))));

            env = addBuiltin("move-to",       (Primitive) a -> { nArgs("move-to", a, 2, 3);  return asFrame("move-to",  caddr(a)).moveTo(asDouble("move-to",  car(a)), asDouble("move-to", cadr(a)));  },
                  addBuiltin("line-to",       (Primitive) a -> { nArgs("line-to", a, 2, 3);  return asFrame("line-to",  caddr(a)).lineTo(asDouble("line-to",  car(a)), asDouble("line-to", cadr(a)));  },
                  addBuiltin("move-rel",      (Primitive) a -> { nArgs("move-rel", a, 2, 3); return asFrame("move-rel", caddr(a)).moveRel(asDouble("move-rel", car(a)), asDouble("move-rel", cadr(a))); },
                  addBuiltin("line-rel",      (Primitive) a -> { nArgs("line-rel", a, 2, 3); return asFrame("line-rel", caddr(a)).lineRel(asDouble("line-rel", car(a)), asDouble("line-rel", cadr(a))); },
                  env))));

            env = addBuiltin("make-bitmap",   (Primitive) a -> { nArgs("make-bitmap",    a, 2, 3); return asFrame("make-bitmap",    caddr(a)).makeBitmap(asInt("make-bitmap",  car(a)), asInt("make-bitmap", cadr(a))); },
                  addBuiltin("discard-bitmap",(Primitive) a -> { nArgs("discard-bitmap", a, 0, 1); return asFrame("discard-bitmap", car(a)).discardBitmap(); },
                  addBuiltin("set-pixel",     (Primitive) a -> { nArgs("set-pixel",      a, 3, 4); return asFrame("set-pixel",      cadddr(a)).setRGB(asInt("set-pixel", car(a)), asInt("set-pixel", cadr(a)), asInt("set-pixel", caddr(a)));  },
                  addBuiltin("rgb-to-pixel",  (Primitive) a -> { nArgs("rgb-to-pixel",   a, 3, 3); return (asInt("rgb-to-pixel", car(a)) << 16)
                                                                                                        | (asInt("rgb-to-pixel", cadr(a)) << 8)
                                                                                                        | (asInt("rgb-to-pixel", caddr(a)));  },
                  addBuiltin("hsb-to-pixel",  (Primitive) a -> { nArgs("hsb-to-pixel",   a, 3, 3); return Color.HSBtoRGB(asFloat("hsb-to-pixel", car(a)),
                                                                                                                         asFloat("hsb-to-pixel", cadr(a)),
                                                                                                                         asFloat("hsb-to-pixel", caddr(a)));  },
                  env)))));
        }

        if (haveString()) {
            env = addBuiltin("stringp",    (Primitive) a -> { oneArg("stringp", a);    return boolResult(stringp(car(a))); },
                  addBuiltin("characterp", (Primitive) a -> { oneArg("characterp", a); return boolResult(characterp(car(a))); },
                  addBuiltin("char-code",  (Primitive) a -> { oneArg("char-code", a);  return (long)(asChar("char-code", car(a))); },
                  addBuiltin("code-char",  (Primitive) a -> { oneArg("code-char", a);  return (char)(asInt("code-char", car(a))); },
                  addBuiltin("string=",    (Primitive) a -> { twoArgs("string=", a);   return boolResult(Objects.equals(asString("string=", car(a)), asString("string=", cadr(a)))); },
                  addBuiltin("string->list", (Primitive) LambdaJ::stringToList,
                  addBuiltin("list->string", (Primitive) LambdaJ::listToString,
                  env)))))));

            if (haveUtil()) {
                env = addBuiltin("format",        (Primitive) this::format,
                      addBuiltin("format-locale", (Primitive) this::formatLocale,
                      env));
            }
        }

        if (haveXtra()) {
            env = addBuiltin(sDynamic, sDynamic, env);

            final LambdaJSymbol sEval = symtab.intern(new LambdaJSymbol("eval"));
            ocEval = new OpenCodedPrimitive(sEval) {
                @Override public Object apply(ConsCell a) {
                    nArgs("eval", a, 1, 2);
                    return eval(car(a), cadr(a));
                }
            };
            env = addBuiltin(sEval, ocEval, env);

            env = addBuiltin("trace", (Primitive) this::trace,
                  addBuiltin("untrace", (Primitive) this::untrace,
                  env));

            env = addBuiltin("macroexpand-1", (Primitive)this::macroexpand1,
                  addBuiltin("gensym", (Primitive)this::gensym,
                  env));

            env = addBuiltin("rplaca", (Primitive) LambdaJ::cl_rplaca,
                  addBuiltin("rplacd", (Primitive) LambdaJ::cl_rplacd,
                  env));
        }

        if (haveT()) {
            final LambdaJSymbol sT = symtab.intern(new LambdaJSymbol("t"));
            env = addBuiltin(sT, sT, env);
            reserve(sT);
        }

        if (haveNil()) {
            final LambdaJSymbol sNil = symtab.intern(new LambdaJSymbol("nil"));
            env = addBuiltin(sNil, null, env);
            reserve(sNil);
        }

        if (haveUtil()) {
            env = addBuiltin("consp",   (Primitive) a -> { oneArg("consp",   a);  return boolResult(consp  (car(a))); },
                  addBuiltin("symbolp", (Primitive) a -> { oneArg("symbolp", a);  return boolResult(symbolp(car(a))); },
                  addBuiltin("listp",   (Primitive) a -> { oneArg("listp",   a);  return boolResult(listp  (car(a))); },
                  addBuiltin("null",    (Primitive) a -> { oneArg("null",    a);  return boolResult(car(a) == null); },
                  addBuiltin("assoc",   (Primitive) a -> { twoArgs("assoc",  a);  return assoc(car(a), car(cdr(a))); },
                  addBuiltin("list",    (Primitive) a -> a,
                  env))))));

            env = addBuiltin("append",  (Primitive) a -> append(listToArray(a)),
                  env);

            env = addBuiltin("internal-time-units-per-second", 1e9,
                  addBuiltin("get-internal-real-time", (Primitive) a -> getInternalRealTime(),
                  addBuiltin("get-internal-run-time",  (Primitive) a -> getInternalRunTime(), // user
                  addBuiltin("get-internal-cpu-time",  (Primitive) a -> getInternalCpuTime(), // user + system
                  addBuiltin("sleep",                  (Primitive) a -> sleep(a),
                  addBuiltin("get-universal-time",     (Primitive) a -> getUniversalTime(), // seconds since 1.1.1900
                  addBuiltin("get-decoded-time",       (Primitive) a -> getDecodedTime(),
                  env)))))));

            env = addBuiltin("fatal", (Primitive) a -> { oneArg("fatal", a); throw new RuntimeException(String.valueOf(car(a))); }, env);

            env = addBuiltin("::", (Primitive) LambdaJ::findJavaMethod, env);

        }

        if (haveAtom()) {
            env = addBuiltin("atom", (Primitive) a -> { oneArg("atom", a); return boolResult(atom(car(a))); },
                  env);
        }

        if (haveNumbers()) {
            env = addBuiltin("numberp", (Primitive) args -> { oneArg("numberp", args); return boolResult(numberp(car(args))); },
                  addBuiltin("floatp", (Primitive) args -> { oneArg("floatp", args); return boolResult(floatp(car(args))); },
                  addBuiltin("integerp", (Primitive) args -> { oneArg("integerp", args); return boolResult(integerp(car(args))); },
                  env)));

            env = addBuiltin("pi",      Math.PI,
                  env);

            env = addBuiltin("fround",   (Primitive) args -> { numberArgs("fround",   args, 1, 1); return cl_round(((Number)car(args))); },
                  addBuiltin("ffloor",   (Primitive) args -> { numberArgs("ffloor",   args, 1, 1); return Math.floor(((Number)car(args)).doubleValue()); },
                  addBuiltin("fceiling", (Primitive) args -> { numberArgs("fceiling", args, 1, 1); return Math.ceil (((Number)car(args)).doubleValue()); },
                  addBuiltin("ftruncate",(Primitive) args -> { numberArgs("ftruncate",args, 1, 1); return cl_truncate((Number)car(args)); },
                  env))));

            env = addBuiltin("round",   (Primitive) args -> { numberArgs("round",   args, 1, 1); return truncate(cl_round(((Number)car(args)))); },
                  addBuiltin("floor",   (Primitive) args -> { numberArgs("floor",   args, 1, 1); return truncate(Math.floor(((Number)car(args)).doubleValue())); },
                  addBuiltin("ceiling", (Primitive) args -> { numberArgs("ceiling", args, 1, 1); return truncate(Math.ceil (((Number)car(args)).doubleValue())); },
                  addBuiltin("truncate",(Primitive) args -> { numberArgs("truncate",args, 1, 1); return truncate(cl_truncate((Number)car(args))); },
                  env))));

            env = addBuiltin("1+",      (Primitive) args -> { oneNumber("1+", args); return inc((Number)car(args)); },
                  addBuiltin("1-",      (Primitive) args -> { oneNumber("1-", args); return dec((Number)car(args)); },
                  env));

            env = addBuiltin("sqrt",    (Primitive) args -> { numberArgs("sqrt",    args, 1, 1); return Math.sqrt (((Number)car(args)).doubleValue()); },
                  addBuiltin("log",     (Primitive) args -> { numberArgs("log",     args, 1, 1); return Math.log  (((Number)car(args)).doubleValue()); },
                  addBuiltin("log10",   (Primitive) args -> { numberArgs("log10",   args, 1, 1); return Math.log10(((Number)car(args)).doubleValue()); },
                  addBuiltin("exp",     (Primitive) args -> { numberArgs("exp",     args, 1, 1); return Math.exp  (((Number)car(args)).doubleValue()); },
                  addBuiltin("expt",    (Primitive) args -> { numberArgs("expt",    args, 2, 2); return Math.pow  (((Number)car(args)).doubleValue(), ((Number)cadr(args)).doubleValue()); },

                  addBuiltin("mod",     (Primitive) args -> { numberArgs("mod",     args, 2, 2); return cl_mod((Number)car(args), (Number)cadr(args)); },
                  addBuiltin("rem",     (Primitive) args -> { numberArgs("rem",     args, 2, 2); return cl_rem((Number)car(args), (Number)cadr(args)); },
                          
                  addBuiltin("signum",  (Primitive) args -> { oneNumber("signum", args); return cl_signum((Number)car(args)); },
                  env))))))));

            env = addBuiltin("=",       (Primitive) args -> compare(args, "=",  (d1, d2) -> d1 == d2),
                  addBuiltin(">",       (Primitive) args -> compare(args, ">",  (d1, d2) -> d1 > d2),
                  addBuiltin(">=",      (Primitive) args -> compare(args, ">=", (d1, d2) -> d1 >= d2),
                  addBuiltin("<",       (Primitive) args -> compare(args, "<",  (d1, d2) -> d1 < d2),
                  addBuiltin("<=",      (Primitive) args -> compare(args, "<=", (d1, d2) -> d1 <= d2),
                  addBuiltin("/=",      (Primitive) args -> compare(args, "/=", (d1, d2) -> d1 != d2),
                  env))))));

            env = addBuiltin("+",       (Primitive) args -> makeAddOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs),
                  addBuiltin("-",       (Primitive) args -> makeSubOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs),
                  addBuiltin("*",       (Primitive) args -> makeAddOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs),
                  addBuiltin("/",       (Primitive) args -> makeSubOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs),
                  env))));
        }

        if (haveEq()) {
            env = addBuiltin("eq", (Primitive) a -> { twoArgs("eq", a);     return boolResult(car(a) == cadr(a)); },
                  addBuiltin("eql", (Primitive) a -> { twoArgs("eql", a);   return boolResult(cl_eql(car(a), cadr(a))); },
                  env));
        }

        if (haveCons()) {
            env = addBuiltin("car",     (Primitive) a -> { oneArg("car", a);    if (car(a) == null) return null; return caar(a); },
                  addBuiltin("cdr",     (Primitive) a -> { oneArg("cdr", a);    if (car(a) == null) return null; return cdar(a); },
                  addBuiltin("cons",    (Primitive) a -> { twoArgs("cons", a);  return cons(car(a), cadr(a)); },
                  env)));
        }

        return env;
    }

    private static boolean cl_eql(Object o1, Object o2) {
        if (o1 == o2) return true;
        if (numberp(o1) && numberp(o2)
            || characterp(o1) && characterp(o2)) return Objects.equals(o1, o2);
        return false;
    }

    private static Number cl_signum(Number n) {
        if (integerp(n)) return n.longValue() == 0 ? 0 : n.longValue() < 0 ? -1 : 1;
        return Math.signum(n.doubleValue());
    }

    /** produce a quotient that has been rounded to the nearest mathematical integer;
     *  if the mathematical quotient is exactly halfway between two integers, (that is, it has the form integer+1/2),
     *  then the quotient has been rounded to the even (divisible by two) integer. */
    private static double cl_round(Number n) {
        return Math.rint(n.doubleValue());
    }

    /** produce a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer of the same sign as the mathematical quotient,
     *  and that has the greatest integral magnitude not greater than that of the mathematical quotient. */
    private static double cl_truncate(Number n) {
        final double d = n.doubleValue();
        return d < 0.0 ? Math.ceil(d) : Math.floor(d);
    }

    private static double cl_mod(Number n1, Number n2) {
        final double x = n1.doubleValue();
        final double y = n2.doubleValue();
        return x - Math.floor(x / y) * y;
    }

    private static double cl_rem(Number n1, Number n2) {
        return n1.doubleValue() % n2.doubleValue();
    }

    /** return the argument w/o decimal places as a long, exception if conversion is not possible */
    private static long truncate(double d) {
        if (Double.isNaN(d)) throw new LambdaJError("value is NaN");
        if (Double.isInfinite(d)) throw new LambdaJError("value is Infinite");
        if (d < Long.MIN_VALUE) throw new LambdaJError("underflow");
        if (d > Long.MAX_VALUE) throw new LambdaJError("overflow");
        return (long)d;
    }

    private static Number inc(Number n) {
        if (n instanceof Long) {
            long l;
            if ((l = n.longValue()) == Long.MAX_VALUE) throw new LambdaJError("1+: overflow");
            return l + 1;
        }
        return n.doubleValue() + 1;
    }

    private static Number dec(Number n) {
        if (n instanceof Long) {
            long l;
            if ((l = n.longValue()) == Long.MIN_VALUE) throw new LambdaJError("1-: underflow");
            return l - 1;
        }
        return n.doubleValue() - 1;
    }

    private static Object listToString(ConsCell a) {
        oneArg("list->string", a);
        final ConsCell l = asList("list->string", car(a));
        final StringBuilder ret = new StringBuilder();
        for (Object c: l) {
            ret.append(asChar("list->string", c));
        }
        return ret.toString();
    }

    private static Object stringToList(ConsCell a) {
        oneArg("string->list", a);
        final ListBuilder ret = new ListBuilder();
        for (char c: asString("string->list", car(a)).toCharArray()) {
            ret.append(c);
        }
        return ret.first;
    }

    private ListConsCell addBuiltin(final String sym, final Object value, ConsCell env) {
        return cons(cons(symtab.intern(new LambdaJSymbol(sym)), value), env);
    }

    private ListConsCell addBuiltin(final LambdaJSymbol sym, final Object value, ConsCell env) {
        return cons(cons(sym, value), env);
    }



    ///
    /// ## Invoking the interpreter
    ///

    /// JMurmel native embed API: Java calls Murmel with getValue() and getFunction()

    /** embed API: interface for compiled lambdas as well as primitives, used for embedding as well as compiled Murmel */
    public interface MurmelFunction { Object apply(Object... args) throws LambdaJError; }

    /** embed API: Return the value of {@code globalSymbol} in the interpreter's current global environment */
    public Object getValue(String globalSymbol) {
        if (topEnv == null) throw new LambdaJError("getValue: not initialized (must interpret *something* first)");
        final ConsCell envEntry = assoceq(symtab.intern(new LambdaJSymbol(globalSymbol)), topEnv);
        if (envEntry != null) return cdr(envEntry);
        throw new LambdaJError(true, "%s: '%s' is not bound", "getValue", globalSymbol);
    }

    private class CallPrimitive implements MurmelFunction {
        final Primitive p;
        CallPrimitive(Primitive p) { this.p = p; }
        @Override public Object apply(Object... args) { return p.apply(list(args)); }
    }

    private class CallLambda implements MurmelFunction {
        final ConsCell lambda;
        final ConsCell env;
        CallLambda(ConsCell lambda) { this.lambda = lambda; this.env = topEnv; }
        @Override
        public Object apply(Object... args) {
            if (env != topEnv) throw new LambdaJError("MurmelFunction.apply: stale function object, global environment has changed");
            return eval(cons(lambda, list(args)), env, 0, 0, 0);
        }
    }

    /** <p>embed API: Return the function {@code funcName}
     *
     *  <p>Function objects of Lambdas will be usable until the interpreter's environment is rebuilt
     *  by a call to interpretExpression/s, eg.<pre>
     *  MurmelFunction f = getFunction("my-function");
     *  interpreter.interpretExpressions("...");
     *  f.apply(1, 2, 3);  // this will throw a "stale function..." Exception
     *  </pre>
     */
    public MurmelFunction getFunction(String funcName) {
        final Object maybeFunction = getValue(funcName);
        if (maybeFunction instanceof Primitive) {
            return new CallPrimitive((Primitive)maybeFunction);
        }
        if (maybeFunction instanceof ConsCell && car(maybeFunction) == sLambda) {
            return new CallLambda((ConsCell)maybeFunction);
        }
        // todo CompilerPrimitive
        throw new LambdaJError(true, "getFunction: not a primitive or lambda: %s", funcName);
    }

    public interface MurmelProgram {
        Object getValue(String globalSymbol);
        MurmelFunction getFunction(String funcName);

        Object body();

        ObjectReader getLispReader();
        ObjectWriter getLispPrinter();
        void setReaderPrinter(ObjectReader reader, ObjectWriter writer);
    }

    /** Turn {@code program} into an interpreted Murmel program: {@code program} will be wrapped in the method
     *  {@link MurmelProgram#body} that can be run multiple times.
     *
     *  Note how this is somewhat similar to {@link MurmelJavaCompiler#formsToJavaClass(String, Iterable, String)}. */
    public MurmelProgram formsToInterpretedProgram(String program, ReadSupplier in, WriteConsumer out) {
        return new MurmelProgram() {
            @Override public Object getValue(String globalSymbol) { return LambdaJ.this.getValue(globalSymbol); }
            @Override public MurmelFunction getFunction(String funcName) { return LambdaJ.this.getFunction(funcName); }

            @Override public ObjectReader getLispReader() { return LambdaJ.this.getLispReader(); }
            @Override public ObjectWriter getLispPrinter() { return LambdaJ.this.getLispPrinter(); }
            @Override public void setReaderPrinter(ObjectReader reader, ObjectWriter writer) { LambdaJ.this.setReaderPrinter(reader, writer); }

            @Override public Object body() {
                return interpretExpressions(new StringReader(program)::read, in, out);
            }
        };
    }



    /// JMurmel JSR-223 embed API - Java calls Murmel with JSR223 eval

    /** <p>evalScript is for JSR-223 support.
     *  <p>First call creates a new parser (parsers contain the symbol table) and inits the global environment
     *  <p>Subsequent calls will re-use the parser (including symbol table) and global environment. */
    @SuppressWarnings("resource")
    public Object evalScript(Reader program, Reader in, Writer out) {
        if (symtab == null) {
            setSymtab(new SExpressionParser(features, trace, tracer, in::read, null, true));
            topEnv = environment(null);
        }
        final Parser scriptParser = (Parser)symtab;
        scriptParser.setInput(program::read);
        setReaderPrinter(new SExpressionParser(in::read), new SExpressionWriter(new WrappingWriter(out)::append));
        Object result = null;
        while (true) {
            final Object exp = (scriptParser instanceof SExpressionParser) ? ((SExpressionParser)scriptParser).readObj(true) : scriptParser.readObj();
            if (exp != null) result = eval(exp, topEnv, 0, 0, 0);
            else return result;
        }
    }



    /// JMurmel native embed API - Java calls Murmel

    /** Build environment, setup symbol table, Lisp reader and writer.
     *  Needs to be called once before eval() and evalScript(), not needed before interpretExpression/s  */
    public SExpressionParser init(ReadSupplier in, WriteConsumer out) {
        final SExpressionParser parser = new SExpressionParser(features, trace, tracer, in, null, true);
        setSymtab(parser);
        final ObjectWriter outWriter = makeWriter(out);
        setReaderPrinter(parser, outWriter);
        topEnv = environment(null);
        nCells = 0; maxEnvLen = 0;
        return parser;
    }

    /** <p>Build environment, read a single S-expression from {@code in}, invoke {@code eval()} and return result.
     *
     *  <p>After the expression was read from {@code in}, the primitive function {@code read} (if used)
     *  will read S-expressions from {@code in} as well,
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpression(ReadSupplier in, WriteConsumer out) {
        final SExpressionParser parser = init(in, out);
        final Object exp = parser.readObj();
        final long tStart = System.nanoTime();
        final Object result = eval(exp, topEnv, 0, 0, 0);
        traceStats(System.nanoTime() - tStart);
        return result;
    }

    /** <p>Build environment, repeatedly read an S-expression from {@code program} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read S-expressions from {@code in}
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpressions(ReadSupplier program, ReadSupplier in, WriteConsumer out) {
        final Parser parser = new SExpressionParser(features, trace, tracer, program, null, true);
        final ObjectReader inReader = new SExpressionParser(features, TraceLevel.TRC_NONE, null, in, null, true);
        final ObjectWriter outWriter = makeWriter(out);
        return interpretExpressions(parser, inReader, outWriter, (_symtab) -> null);
    }

    /** <p>Build environment, repeatedly read an expression from {@code parser} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read expressions from {@code inReader},
     *  and {@code write}/ {@code writeln} will write Objects to {@code out}. */
    public Object interpretExpressions(Parser parser, ObjectReader inReader, ObjectWriter outWriter, CustomEnvironmentSupplier customEnv) {
        nCells = 0; maxEnvLen = 0;
        setSymtab(parser);
        setReaderPrinter(parser, outWriter);
        final ConsCell customEnvironment = customEnv == null ? null : customEnv.customEnvironment(parser);
        final ConsCell env = environment(customEnvironment);
        topEnv = env;
        Object exp = (parser instanceof SExpressionParser) ? ((SExpressionParser)parser).readObj(true) : parser.readObj();
        while (true) {
            final long tStart = System.nanoTime();
            final Object result = eval(exp, env, 0, 0, 0);
            traceStats(System.nanoTime() - tStart);
            exp = (parser instanceof SExpressionParser) ? ((SExpressionParser)parser).readObj(true) : parser.readObj();
            if (exp == null) return result;
        }
    }

    /** print and reset interpreter stats and wall time. preceeded and followed by a newline. */
    private void traceStats(long nanos) {
        if (trace.ge(TraceLevel.TRC_STATS)) {
            tracer.println("");
            tracer.println("*** max eval nesting:  " + maxEvalLevel + " ***");
            tracer.println("*** max stack used:    " + maxEvalStack + " ***");

            tracer.println("*** total ConsCells:   " + nCells + " ***");
            if (trace.ge(TraceLevel.TRC_ENVSTATS)) tracer.println("*** max env length:    " + maxEnvLen + " ***");

            final long millis = (long)(nanos * 0.000001D);
            final String ms = Long.toString(millis) + '.' + ((long) (nanos * 0.001D + 0.5D) - (long) (millis * 1000D));
            tracer.println("*** elapsed wall time: " + ms + "ms ***");
            tracer.println("");

            maxEvalLevel = maxEvalStack = nCells = maxEnvLen = 0;
        }
    }

    /** print stats (wall time) of compiled program. preceeded and followed by a newline. */
    private void traceJavaStats(long nanos) {
        if (trace.ge(TraceLevel.TRC_STATS)) {
            tracer.println("");
            final long millis = (long)(nanos * 0.000001D);
            final String ms = Long.toString(millis) + '.' + ((long) (nanos * 0.001D + 0.5D) - (long) (millis * 1000D));
            tracer.println("*** elapsed wall time: " + ms + "ms ***");
            tracer.println("");
        }
    }



    /// static void main() - run JMurmel from the command prompt (interactive)

    /** static main() function for commandline use of the Murmel interpreter */
    public static void main(String[] args) {
        misc(args);
        final TraceLevel trace = trace(args);
        final int features = features(args);

        final boolean istty       = hasFlag("--tty", args) || null != System.console();
        final boolean repl        = hasFlag("--repl", args);
        final boolean echo        = hasFlag("--echo", args);    // used only in repl
        final boolean printResult = hasFlag("--result", args);  // used only in filemode
        final boolean toJava      = hasFlag("--java", args);
        final boolean toJar       = hasFlag("--jar", args);
        final boolean run         = hasFlag("--run", args);
        final boolean verbose     = hasFlag("--verbose", args);
        final String clsName      = flagValue("--class", args);
        final String outDir       = flagValue("--outdir", args);
        final String libDir       = flagValue("--libdir", args);

        if (argError(args)) {
            System.err.println("LambdaJ: exiting because of previous errors.");
            System.exit(1);
        }

        Path libPath = null;
        if (libDir != null) {
            try {
                libPath = Paths.get(libDir).toAbsolutePath();
                if (!Files.isDirectory(libPath)) {
                    System.err.println("LambdaJ: invalid value for --libdir: " + libDir + " is not a directory");
                    System.exit(1);
                }
                if (!Files.isReadable(libPath)) {
                    System.err.println("LambdaJ: invalid value for --libdir: " + libDir + " is not readable");
                    System.exit(1);
                }
            }
            catch (Exception e) {
                System.err.println("LambdaJ: cannot process --libdir: " + libDir + ": " + e.getMessage());
                System.exit(1);
            }
        }
        final LambdaJ interpreter = new LambdaJ(features, trace, null, libPath);

        final List<Object> history = repl ? new ArrayList<>() : null;

        final List<String> files = args(args);
        if (!files.isEmpty()) {
            if (toJar || toJava) {
                compileFiles(files, toJar, clsName, libPath, outDir);
            }
            else if (run) {
                final SExpressionParser parser = new SExpressionParser(interpreter.features, interpreter.trace, interpreter.tracer,
                        () -> -1, null, true);

                final List<Object> program = new ArrayList<>();
                for (String fileName: files) {
                    if ("--".equals(fileName)) continue;
                    if (verbose) System.out.println("compiling " + fileName + "...");
                    final Path p = Paths.get(fileName);
                    try (final Reader r = Files.newBufferedReader(p)) {
                        while (true) {
                            parser.setInput(r::read);
                            parser.filePath = p;
                            final Object sexp = parser.readObj(true);
                            if (sexp == null) break;
                            program.add(sexp);
                        }
                    } catch (IOException e) {
                        System.err.println();
                        System.err.println(e);
                        System.exit(1);
                    }
                }
                interpreter.init(System.in::read, System.out::print);
                injectCommandlineArgs(interpreter, args);
                runForms(parser, program, interpreter);
                return;
            }
            else {
                interpreter.init(() -> -1, s -> {});
                injectCommandlineArgs(interpreter, args);
                Object result = null;
                for (String fileName: files) {
                    if ("--".equals(fileName)) continue;
                    if (verbose) System.out.println("interpreting " + fileName + "...");
                    final Path p = Paths.get(fileName);
                    try (final Reader r = Files.newBufferedReader(p)) {
                        result = interpretStream(interpreter, r::read, p, printResult, history);
                    } catch (IOException e) {
                        System.err.println();
                        System.err.println(e);
                        System.exit(1);
                    }
                }
                if (result != null) {
                    System.out.println();
                    System.out.println("==> " + result);
                }
            }
        }

        if (repl || (files.isEmpty() && istty)) repl(interpreter, !files.isEmpty(), istty, echo, history, args); // repl() doesn't return

        if (files.isEmpty()) {
            final String consoleCharsetName = System.getProperty("sun.stdout.encoding");
            final Charset  consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);

            if (toJar || run || toJava) {
                final SExpressionParser parser = new SExpressionParser(interpreter.features, interpreter.trace, interpreter.tracer,
                                                                       new InputStreamReader(System.in, consoleCharset)::read, null, true);

                final List<Object> program = new ArrayList<>();
                while (true) {
                    final Object sexp = parser.readObj(true);
                    if (sexp == null) break;
                    program.add(sexp);
                }

                if (toJar) {
                    final String outFile = outDir != null ? (outDir + "/a.jar") : "a.jar";
                    final boolean success = compileToJar(parser, libPath, program, clsName, outFile);
                    if (success) System.out.println("compiled stdin to " + outFile);
                }
                else if (run) {
                    interpreter.setSymtab(parser);
                    ObjectWriter outWriter = makeWriter(System.out::print);
                    interpreter.setReaderPrinter(parser, outWriter);
                    interpreter.topEnv = interpreter.environment(null);
                    injectCommandlineArgs(interpreter, args);
                    runForms(parser, program, interpreter);
                }
                else {
                    final String outFile = clsName;
                    final boolean success = compileToJava(StandardCharsets.UTF_8, parser, libPath, program, clsName, outDir);
                    if (success) System.out.println("compiled stdin to " + (outFile == null ? "MurmelProgram" : outFile.toString()));
                }
            }
            else {
                interpreter.init(() -> -1, s -> {});
                injectCommandlineArgs(interpreter, args);
                final Object result = interpretStream(interpreter, new InputStreamReader(System.in, consoleCharset)::read, null, printResult, null);
                if (result != null) {
                    System.out.println();
                    System.out.println("==> " + result);
                }
            }
        }
    }

    private static Object interpretStream(final LambdaJ interpreter, ReadSupplier prog, Path fileName, final boolean printResult, List<Object> history) {
        try {
            final SExpressionParser parser = (SExpressionParser)interpreter.symtab;
            parser.setInput(prog);
            final ObjectReader inReader = new SExpressionParser(interpreter.features, TraceLevel.TRC_NONE, null, System.in::read, fileName, true);
            final ObjectWriter outWriter = makeWriter(System.out::print);
            interpreter.setReaderPrinter(inReader, outWriter);
            Object result = null;
            for (;;) {
                final Object form = parser.readObj(true);
                if (form == null) break;
                if (history != null) history.add(form);

                final long tStart = System.nanoTime();
                result = interpreter.eval(form, interpreter.topEnv, 0, 0, 0);
                final long tEnd = System.nanoTime();
                interpreter.traceStats(tEnd - tStart);
                if (printResult) {
                    System.out.println();
                    System.out.print("==> "); outWriter.printObj(result); System.out.println();
                }
            }
            return result;
        } catch (LambdaJError e) {
            System.err.println();
            System.err.println(e);
            System.exit(1);
            return null; // notreached
        }
    }

    private static class BoolHolder { boolean value; BoolHolder(boolean value) { this.value = value; }}

    /** Enter REPL, doesn't return */
    private static void repl(final LambdaJ interpreter, boolean isInit, final boolean istty, final boolean echo, List<Object> prevHistory, String[] args) {
        final BoolHolder echoHolder = new BoolHolder(echo);

        if (!echoHolder.value) {
            System.out.println("Enter a Murmel form or :command (or enter :h for command help or :q to exit):");
            System.out.println();
        }

        final String consoleCharsetName = System.getProperty("sun.stdout.encoding");
        final Charset  consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);

        final List<Object> history = prevHistory == null ? new ArrayList<>() : prevHistory;
        SExpressionParser parser = null;
        ObjectWriter outWriter = null;
        ConsCell env = null;
        if (isInit) {
            interpreter.nCells = 0; interpreter.maxEnvLen = 0;
            parser = (SExpressionParser)interpreter.symtab;
            final AnyToUnixEol read = new AnyToUnixEol();
            parser.setInput(() -> read.read(echoHolder.value));
            outWriter = interpreter.lispPrinter;
            env = interpreter.topEnv;
        }
        for (;;) {
            if (!isInit) {
                interpreter.nCells = 0; interpreter.maxEnvLen = 0;
                final AnyToUnixEol read = new AnyToUnixEol();
                parser = new SExpressionParser(interpreter.features, interpreter.trace, interpreter.tracer,
                                               () -> read.read(echoHolder.value), null, false);
                interpreter.setSymtab(parser);
                outWriter = makeWriter(System.out::print);
                interpreter.lispReader = parser; interpreter.lispPrinter = outWriter;
                env = interpreter.environment(null);
                interpreter.topEnv = env;
                injectCommandlineArgs(interpreter, args);
                isInit = true;
            }

            if (!echoHolder.value) {
                System.out.print("JMurmel> ");
                System.out.flush();
            }

            try {
                if (istty) { parser.lineNo = parser.charNo == 0 ? 1 : 0;  parser.charNo = 0; } // if parser.charNo != 0 the next thing the parser reads is the lineseparator following the previous sexp that was not consumed
                final Object exp = parser.readObj(true);

                if (exp == null && parser.look == EOF
                    || exp != null && ":q"  .equalsIgnoreCase(exp.toString())) { System.out.println("bye."); System.out.println();  System.exit(0); }
                if (exp != null) {
                    if (":h"      .equalsIgnoreCase(exp.toString())) { showHelp();  continue; }
                    if (":echo"   .equalsIgnoreCase(exp.toString())) { echoHolder.value = true; continue; }
                    if (":noecho" .equalsIgnoreCase(exp.toString())) { echoHolder.value = false; continue; }
                    if (":env"    .equalsIgnoreCase(exp.toString())) { System.out.println(env.toString()); System.out.println("env length: " + length(env));  System.out.println(); continue; }
                    if (":res"    .equalsIgnoreCase(exp.toString())) { isInit = false; history.clear();  continue; }
                    if (":l"      .equalsIgnoreCase(exp.toString())) { listHistory(history); continue; }
                    if (":w"      .equalsIgnoreCase(exp.toString())) { writeHistory(history, parser.readObj(false)); continue; }
                    if (":java"   .equalsIgnoreCase(exp.toString())) { compileToJava(consoleCharset, parser, interpreter.libDir, history, parser.readObj(false), parser.readObj(false)); continue; }
                    if (":r"      .equalsIgnoreCase(exp.toString())) { runForms(parser, history, interpreter); continue; }
                    if (":jar"    .equalsIgnoreCase(exp.toString())) { compileToJar(parser, interpreter.libDir, history, parser.readObj(false), parser.readObj(false)); continue; }
                    //if (":peek"   .equals(exp.toString())) { System.out.println(new java.io.File(LambdaJ.class.getProtectionDomain().getCodeSource().getLocation().getPath()).getName()); return; }
                    history.add(exp);
                }

                final long tStart = System.nanoTime();
                final Object result = interpreter.eval(exp, env, 0, 0, 0);
                final long tEnd = System.nanoTime();
                interpreter.traceStats(tEnd - tStart);
                System.out.println();
                System.out.print("==> "); outWriter.printObj(result); System.out.println();
            } catch (LambdaJError e) {
                if (istty) {
                    System.out.println();
                    System.out.println(e);
                    System.out.println();
                } else {
                    System.err.println();
                    System.err.println(e);
                    System.exit(1);
                }
            }
        }
    }

    private static void listHistory(List<Object> history) {
        for (Object sexp: history) {
            System.out.println(printSEx(sexp));
        }
    }

    private static void writeHistory(List<Object> history, Object filename) {
        try {
            final Path p = Paths.get(filename.toString());
            Files.createFile(p);
            Files.write(p, history.stream()
                    .map(LambdaJ::printSEx)
                    .collect(Collectors.toList()));
            System.out.println("wrote history to file '" + p + '\'');
        }
        catch (Exception e) {
            System.out.println("history NOT written - error: " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
    }

    private static Path getTmpDir() throws IOException {
        final Path tmpDir = Files.createTempDirectory("jmurmel");
        tmpDir.toFile().deleteOnExit();
        return tmpDir;
    }

    /** compile history to a class and run compiled class.
     *  if className is null "MurmelProgram" will be the class' name */
    private static void runForms(SymbolTable symtab, List<Object> history, LambdaJ interpreter) {
        try {
            final MurmelJavaCompiler c = new MurmelJavaCompiler(symtab, interpreter.libDir, getTmpDir());
            final Class<MurmelProgram> murmelClass = c.formsToJavaClass("MurmelProgram", history, null);
            final MurmelProgram prg = murmelClass.getDeclaredConstructor().newInstance();
            final long tStart = System.nanoTime();
            final Object result = prg.body();
            final long tEnd = System.nanoTime();
            interpreter.traceJavaStats(tEnd - tStart);
            System.out.println();
            System.out.print("==> ");  interpreter.lispPrinter.printObj(result); System.out.println();
        }
        catch (LambdaJError e) {
            System.out.println("history NOT run as Java - error: " + e.getMessage());
        }
        catch (Exception e) {
            System.out.println("history NOT run as Java - error: ");
            e.printStackTrace(System.out);
        }
    }

    // todo refactoren dass jedes einzelne file verarbeitet wird, mit parser statt arraylist, wsl am besten gemeinsam mit packages umsetzen
    private static void compileFiles(final List<String> files, boolean toJar, String clsName, Path libPath, String outDir) {
        SExpressionParser parser = null;
        final List<Object> program = new ArrayList<>();
        for (String fileName: files) {
            if ("--".equals(fileName)) continue;
            final Path p = Paths.get(fileName);
            System.out.println("parsing " + fileName + "...");
            try (final Reader reader = Files.newBufferedReader(p)) {
                if (parser == null) parser = new SExpressionParser(reader::read);
                else parser.setInput(reader::read);
                while (true) {
                    final Object sexp = parser.readObj(true);
                    if (sexp == null) break;
                    program.add(sexp);
                }
            } catch (IOException e) {
                System.err.println();
                System.err.println(e);
                System.exit(1);
            }
        }
        final String outFile;
        final boolean success;
        if (toJar) {
            outFile = outDir != null ? (outDir + "/a.jar") : "a.jar";
            success = compileToJar(parser, libPath, program, clsName, outFile);
        }
        else {
            success = compileToJava(StandardCharsets.UTF_8, parser, libPath, program, clsName, outDir);
            if (clsName == null) clsName = "MurmelProgram";
            if (outDir == null) outDir = ".";
            outFile = outDir + '/' + clsName + ".java";
        }
        if (success) System.out.println("compiled " + files.size() + " file(s) to " + outFile);
    }

    /** compile history to Java source and print or write to a file.
     *  <ul>
     *  <li>if className is null "MurmelProgram" will be the class' name.
     *  <li>if filename is t the compiled Java code will be printed to the screen.
     *  <li>if filename is null the filename will be derived from the className
     *  <li>if filename not null then filename is interpreted as a base directory and the classname (with packages) will be appended
     *  </ul> */
    private static boolean compileToJava(Charset charset, SymbolTable symtab, Path libDir, List<Object> history, Object className, Object filename) {
        final MurmelJavaCompiler c = new MurmelJavaCompiler(symtab, libDir, null);
        final String clsName = className == null ? "MurmelProgram" : className.toString();
        //if (filename == interpreter.symtab.intern(new LambdaJSymbol("t"))) {
        if (filename != null && "t".equalsIgnoreCase(filename.toString())) {
            c.formsToJavaSource(new OutputStreamWriter(System.out, charset), clsName, history);
            return true;
        }

        final Path p;
        if (null == filename) p = Paths.get(clsName.replace('.', '/') + ".java");
        else p = Paths.get(filename.toString() + '/' + clsName.replace('.', '/') + ".java");

        try {
            if (p.getParent() != null) Files.createDirectories(p.getParent());
        }
        catch (Exception e) {
            System.out.println("NOT compiled to Java - error: ");
            e.printStackTrace(System.out);
            return false;
        }

        final CharsetEncoder encoder = StandardCharsets.UTF_8.newEncoder();
        try (final OutputStream os = Files.newOutputStream(p);
             final WrappingWriter writer = new WrappingWriter(new BufferedWriter(new OutputStreamWriter(os, encoder)))) {
            System.out.println("compiling...");
            c.formsToJavaSource(writer, clsName, history);
            System.out.println("compiled to Java file '" + p + '\'');
            return true;
        }
        catch (LambdaJError e) {
            System.out.println("NOT compiled to Java - error: " + e.getMessage());
            return false;
        }
        catch (Exception e) {
            System.out.println("NOT compiled to Java - error: ");
            e.printStackTrace(System.out);
            return false;
        }
    }

    private static boolean compileToJar(SymbolTable symtab, Path libDir, List<Object> history, Object className, Object jarFile) {
        try {
            final MurmelJavaCompiler c = new MurmelJavaCompiler(symtab, libDir, getTmpDir());
            final String jarFileName = jarFile == null ? "a.jar" : jarFile.toString();
            final String clsName = className == null ? "MurmelProgram" : className.toString();
            System.out.println("compiling...");
            c.formsToJavaClass(clsName, history, jarFileName);
            System.out.println("compiled to .jar file '" + jarFileName + '\'');
            return true;
        }
        catch (LambdaJError e) {
            System.out.println("NOT compiled to .jar - error: " + e.getMessage());
            return false;
        }
        catch (Exception e) {
            System.out.println("NOT compiled to .jar - error: ");
            e.printStackTrace(System.out);
            return false;
        }
    }

    private static void misc(String[] args) {
        if (hasFlag("--version", args)) {
            showVersion();
            System.exit(0);
        }

        if (hasFlag("--help", args) || hasFlag("--usage", args)) {
            showVersion();
            System.out.println();
            showUsage();
            System.exit(0);
        }

        if (hasFlag("--help-features", args)) {
            showVersion();
            System.out.println();
            showFeatureUsage();
            System.exit(0);
        }
    }

    private static TraceLevel trace(String[] args) {
        TraceLevel trace = TraceLevel.TRC_NONE;
        if (hasFlag("--trace=stats", args))    trace = TraceLevel.TRC_STATS;
        if (hasFlag("--trace=envstats", args)) trace = TraceLevel.TRC_ENVSTATS;
        if (hasFlag("--trace=eval", args))     trace = TraceLevel.TRC_EVAL;
        if (hasFlag("--trace=env", args))      trace = TraceLevel.TRC_ENV;
        if (hasFlag("--trace", args))          trace = TraceLevel.TRC_LEX;
        return trace;
    }

    private static int features(String[] args) {
        int features = Features.HAVE_ALL_LEXC.bits();
        if (hasFlag("--XX-dyn", args))      features =  Features.HAVE_ALL_DYN.bits();

        if (hasFlag("--no-nil", args))      features &= ~Features.HAVE_NIL.bits();
        if (hasFlag("--no-t", args))        features &= ~Features.HAVE_T.bits();
        if (hasFlag("--no-extra", args))    features &= ~Features.HAVE_XTRA.bits();
        if (hasFlag("--no-number", args))   features &= ~(Features.HAVE_NUMBERS.bits() | Features.HAVE_DOUBLE.bits() | Features.HAVE_LONG.bits());
        if (hasFlag("--no-string", args))   features &= ~Features.HAVE_STRING.bits();
        if (hasFlag("--no-io", args))       features &= ~Features.HAVE_IO.bits();
        if (hasFlag("--no-util", args))     features &= ~Features.HAVE_UTIL.bits();

        if (hasFlag("--no-labels", args))   features &= ~Features.HAVE_LABELS.bits();
        if (hasFlag("--no-cons", args))     features &= ~Features.HAVE_CONS.bits();
        if (hasFlag("--no-cond", args))     features &= ~Features.HAVE_COND.bits();
        if (hasFlag("--no-apply", args))    features &= ~Features.HAVE_APPLY.bits();

        if (hasFlag("--no-atom", args))     features &= ~Features.HAVE_ATOM.bits();
        if (hasFlag("--no-eq", args))       features &= ~Features.HAVE_EQ.bits();
        if (hasFlag("--no-quote", args))    features &= ~Features.HAVE_QUOTE.bits();

        if (hasFlag("--min+", args))        features =  Features.HAVE_MINPLUS.bits();
        if (hasFlag("--min", args))         features =  Features.HAVE_MIN.bits();
        if (hasFlag("--lambda+", args))     features =  Features.HAVE_LAMBDAPLUS.bits();
        if (hasFlag("--lambda", args))      features =  Features.HAVE_LAMBDA.bits();

        return features;
    }

    private static boolean hasFlag(String flag, String[] args) {
        for (int i = 0; i < args.length; i++) {
            final String arg = args[i];
            if ("--".equals(arg)) return false;
            if (flag.equals(arg)) {
                args[i] = null; // consume the arg
                return true;
            }
        }
        return false;
    }

    private static String flagValue(String flag, String[] args) {
        for (int i = 0; i < args.length; i++) {
            final String arg = args[i];
            if ("--".equals(arg)) return null;
            if (flag.equals(arg)) {
                if (args.length < i+2) {
                    System.err.println("LambdaJ: commandline argument " + flag + " requires a value");
                    return null;
                }
                args[i] = null; // consume the arg
                final String ret = args[i+1];
                args[i+1] = null;
                return ret;
            }
        }
        return null;
    }

    private static boolean argError(String[] args) {
        boolean err = false;
        for (String arg: args) {
            if ("--".equals(arg)) return err;
            if (arg != null && arg.startsWith("-")) {
                System.err.println("LambdaJ: unknown commandline argument " + arg + " or missing value");
                System.err.println("use '--help' to show available commandline arguments");
                err = true;
            }
        }
        return err;
    }

    /** extract arguments for JMurmel from the commandline that are not flags,
     *  arguments before "--" are for JMurmel, arguments after "--" are for the Murmel program. */
    private static List<String> args(String[] args) {
        final ArrayList<String> ret = new ArrayList<>();
        for (String arg: args) {
            if ("--".equals(arg)) return ret;
            if (arg != null) ret.add(arg);
        }
        return ret;
    }

    private static void injectCommandlineArgs(LambdaJ intp, String[] args) {
        int n = 0;
        for (String arg: args) {
            n++;
            if ("--".equals(arg)) break;
        }

        intp.insertFront(intp.topEnv, intp.symtab.intern(new LambdaJSymbol("*command-line-argument-list*")), new ArraySlice(args, n));
    }

    private static void showVersion() {
        System.out.println(ENGINE_VERSION);
    }

    private static void showHelp() {
        System.out.println("Available commands:\n"
        + "  :h ............................. this help screen\n"
        + "  :echo .......................... print forms to screen before eval'ing\n"
        + "  :noecho ........................ don't print forms\n"
        + "  :env ........................... list current global environment\n"
        + "  :res ........................... 'CTRL-ALT-DEL' the REPL, i.e. reset global environment, clear history\n"
        + "\n"
        + "  :l ............................. print history to the screen\n"
        + "  :w filename .................... write history to a new file with the given filename\n"
        + "\n"
        + "  :r ............................. compile history to Java class 'MurmelProgram' and run it\n"
        + "\n"
        + "  :java classname t .............. compile history to Java class 'classname' and print to the screen\n"
        + "  :java classname nil ............ compile history to Java class 'classname' and save to a file based on 'classname' in current directory\n"
        + "  :java classname directory ...... compile history to Java class 'classname' and save to a file based on 'classname' in directory 'directory'\n"
        + "\n"
        + "  :jar  classname jarfilename .... compile history to jarfile 'jarfile' containing Java class 'classname'\n"
        + "                                   the generated jar needs jmurmel.jar in the same directory to run\n"
        + "\n"
        + "  If 'classname' is nil then 'MurmelProgram' will be used as the classname (in the Java default package).\n"
        + "  If 'jarfilename' is nil then 'a.jar' will be used as the jar file name.\n"
        + "  classname, directory and jarfilename may need to be enclosed in double quotes if they contain spaces or are longer than SYMBOL_MAX (" + SYMBOL_MAX + ")\n"
        + "\n"
        + "  :q ............................. quit JMurmel\n");
    }

    // for updating the usage message edit the file usage.txt and copy/paste its contents here between double quotes
    private static void showUsage() {
        System.out.println("Usage:\n"
                + "\n"
                + "java -jar jmurmel.jar <commandline flags>... <source files>... '--' args-for-program\n"
                + "\n"
                + "Commandline flags are:\n"
                + "\n"
                + "Misc flags:\n"
                + "\n"
                + "-- ...............  '--' must be used to indicate:\n"
                + "                    commandline arguments after this will be passed\n"
                + "                    to the program\n"
                + "\n"
                + "--version ........  Show version and exit\n"
                + "--help ...........  Show this message and exit\n"
                + "--help-features ..  Show advanced commandline flags to disable various\n"
                + "                    Murmel language elements (interpreter only)\n"
                + "--libdir <dir> ...  (load filespec) also searches in this directory,\n"
                + "                    default is the directory containing jmurmel.jar.\n"
                + "--verbose ........  List files given on the commandline as they are interpreted.\n"
                + "\n"
                + "--java ...........  Compile input files to Java source 'MurmelProgram.java'\n"
                + "--jar ............  Compile input files to jarfile 'a.jar' containing\n"
                + "                    the class MurmelProgram. The generated jar needs\n"
                + "                    jmurmel.jar in the same directory to run.\n"
                + "--run ............  Compile and run\n"
                + "--class <name> ...  Use 'name' instead of 'MurmelProgram' as the classname\n"
                + "                    in generated .java- or .jar files\n"
                + "--outdir <dir> ...  Save .java or .jar files to 'dir' instead of current dir\n"
                + "\n"
                + "--result .........  Print the result of the last form.\n"
                + "--tty ............  By default JMurmel will enter REPL only if there\n"
                + "                    are no filenames given on the commandline and\n"
                + "                    stdin is a tty.\n"
                + "                    --tty will make JMurmel enter REPL anyways,\n"
                + "                    i.e. print prompt and results, support :commands and\n"
                + "                    continue after runtime errors.\n"
                + "                    Useful e.g. for Emacs' (run-lisp).\n"
                + "--repl ...........  Same as --tty but terminate after runtime errors.\n"
                + "\n"
                + "Flags for REPL:\n"
                + "--echo ...........  Echo all input while reading\n"
                + "--trace=stats ....  Print stack and memory stats after each form\n"
                + "--trace=envstats .  Print stack, memory and environment stats after each form\n"
                + "--trace=eval .....  Print internal interpreter info during executing programs\n"
                + "--trace=env ......  Print more internal interpreter info executing programs\n"
                + "--trace ..........  Print lots of internal interpreter info during\n"
                + "                    reading/ parsing/ executing programs");
    }

    private static void showFeatureUsage() {
        System.out.println("Feature flags:\n"
                + "\n"
                + "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n"
                + "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n"
                + "--no-extra ....  no special forms 'eval', 'if', 'define', 'defun',\n"
                + "                 'letrec', 'progn'\n"
                + "--no-number ...  no number support\n"
                + "--no-string ...  no string support\n"
                + "--no-io .......  no primitive functions read/ write/ writeln/\n"
                + "                 format/ format-locale\n"
                + "--no-util .....  no primitive functions consp/ symbolp/ listp/ null/ assoc/\n"
                + "                 format/ format-locale\n"
                + "                 no time related primitives\n"
                + "\n"
                + "--min+ ........  turn off all above features, leaving a Lisp\n"
                + "                 with 10 primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, cons, car, cdr, lambda, apply, cond, labels\n"
                + "\n"
                + "--no-apply ....  no special form 'apply'\n"
                + "--no-labels ...  no special form 'labels' (hint: use Y-combinator instead)\n"
                + "\n"
                + "--min .........  turn off all above features, leaving a Lisp with\n"
                + "                 8 special forms/ primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, cons, car, cdr, lambda, cond\n"
                + "\n"
                + "--no-cons .....  no primitive functions cons/ car/ cdr\n"
                + "--no-cond .....  no special form 'cond'\n"
                + "\n"
                + "--lambda+ .....  turn off pretty much everything except Lambda calculus,\n"
                + "                 leaving a Lisp with 4 primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote\n"
                + "                   atom, eq, lambda\n"
                + "\n"
                + "--no-atom .....  no primitive function 'atom'\n"
                + "--no-eq .......  no primitive function 'eq'\n"
                + "--no-quote ....  no special form quote\n"
                + "\n"
                + "--lambda ......  turns off yet even more stuff, leaving I guess\n"
                + "                 bare bones Lambda calculus:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   lambda\n"
                + "\n"
                + "\n"
                + "--XX-dyn ......  Use dynamic environments instead of Murmel's\n"
                + "                 lexical closures with dynamic global environment.\n"
                + "                 WARNING: This flag is for experimentation purposes only\n"
                + "                          and may be removed in future versions.\n"
                + "                          Use at your own discretion.\n"
                + "                          Using --XX-dyn JMurmel will no longer implement Murmel\n"
                + "                          and your programs may silently compute different\n"
                + "                          results!");
    }



    ///
    /// ## class MurmelJavaProgram
    /// class MurmelJavaProgram - base class for compiled Murmel programs

    /** Base class for compiled Murmel programs, contains Murmel runtime as well as embed API support for compiled Murmel programs. */
    public abstract static class MurmelJavaProgram implements MurmelProgram {

        public interface CompilerGlobal {
            Object get();
        }

        public static final CompilerGlobal UNASSIGNED = () -> { throw new LambdaJError(false, "unassigned value"); };

        public interface CompilerPrimitive {
            Object applyPrimitive(Object... args);
        }

        private static class MurmelFunctionCall {
            MurmelFunction next;
            Object[] args;
        }

        private final LambdaJ intp = new LambdaJ();

        protected MurmelJavaProgram() {
            intp.init(() -> -1, System.out::print);
            intp.setReaderPrinter(new SExpressionParser(Features.HAVE_ALL_DYN.bits(), TraceLevel.TRC_NONE, null, System.in::read, null, true), intp.getLispPrinter());
            _t = intern("t");
        }



        /// JMurmel native embed API - Java calls compiled Murmel
        @Override public final ObjectReader getLispReader()  { return intp.getLispReader(); }
        @Override public final ObjectWriter getLispPrinter() { return intp.getLispPrinter(); }
        @Override public final void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) { intp.setReaderPrinter(lispStdin, lispStdout); }
        @Override public abstract Object getValue(String globalSymbol);

        @Override
        public final MurmelFunction getFunction(String func) {
            final Object maybeFunction = getValue(func);
            if (maybeFunction instanceof MurmelFunction) {
                return args -> funcall((MurmelFunction)maybeFunction, args);
            }
            if (maybeFunction instanceof CompilerPrimitive) {
                return args -> funcall((CompilerPrimitive)maybeFunction, args);
            }
            throw new LambdaJError(true, "getFunction: not a primitive or lambda: %s", func);
        }

        protected abstract Object runbody();
        @Override public Object body() {
            try {
                return runbody();
            }
            catch (LambdaJError e) {
                return rterror(e);
            }
        }

        public final Object rterror(LambdaJError e) {
            throw new LambdaJError(e.getMessage() + "\nError occured in " + loc);
        }



        /// predefined global variables
        public static final Object _nil = null;
        public final Object _t;
        public static final Object _pi = Math.PI;

        /// predefined aliased global variables
        // itups doesn't have a leading _ because it is avaliable under an alias name
        public static final Object itups = 1e9;
        // *COMMAND-LINE-ARGUMENT-LIST*
        public ConsCell commandlineArgumentList;

        /// predefined primitives
        public final Object   _car     (Object... args) { oneArg("car",        args.length); return car(args[0]); }
        public final Object   _cdr     (Object... args) { oneArg("cdr",        args.length); return cdr(args[0]); }
        public final ConsCell _cons    (Object... args) { twoArg("cons",       args.length); return cons(args[0], args[1]); }

        public final Object   _rplaca  (Object... args) { return cl_rplaca(arraySlice(args)); }
        public final Object   _rplacd  (Object... args) { return cl_rplacd(arraySlice(args)); }

        public final Object _eval      (Object... args) { onetwoArg("eval",    args.length); return intp.eval(args[0], args.length == 2 ? args[1] : null); }
        public final Object _eq        (Object... args) { twoArg("eq",         args.length); return args[0] == args[1] ? _t : null; }
        public final Object _eql       (Object... args) { twoArg("eql",        args.length); return cl_eql(args[0], args[1]) ? _t : null; }
        public final Object _null      (Object... args) { oneArg("null",       args.length); return args[0] == null ? _t : null; }

        public final Object _write     (Object... args) { oneArg("write",      args.length); intp.write(args[0]); return _t; }
        public final Object _writeln   (Object... args) { oneOptArg("writeln", args.length); intp.writeln(arraySlice(args)); return _t; }
        public final Object _lnwrite   (Object... args) { oneOptArg("lnwrite", args.length); intp.lnwrite(arraySlice(args)); return _t; }

        public final Object _atom      (Object... args) { oneArg("atom",       args.length); return atom      (args[0]) ? _t : null; }
        public final Object _consp     (Object... args) { oneArg("consp",      args.length); return consp     (args[0]) ? _t : null; }
        public final Object _listp     (Object... args) { oneArg("listp",      args.length); return listp     (args[0]) ? _t : null; }
        public final Object _symbolp   (Object... args) { oneArg("symbolp",    args.length); return symbolp   (args[0]) ? _t : null; }
        public final Object _numberp   (Object... args) { oneArg("numberp",    args.length); return numberp   (args[0]) ? _t : null; }
        public final Object _stringp   (Object... args) { oneArg("stringp",    args.length); return stringp   (args[0]) ? _t : null; }
        public final Object _characterp(Object... args) { oneArg("characterp", args.length); return characterp(args[0]) ? _t : null; }
        public final Object _integerp  (Object... args) { oneArg("integerp",   args.length); return integerp  (args[0]) ? _t : null; }
        public final Object _floatp    (Object... args) { oneArg("floatp",     args.length); return floatp    (args[0]) ? _t : null; }

        public final ConsCell _assoc   (Object... args) { twoArg("assoc",      args.length); return assoc(args[0], args[1]); }
        public final ConsCell _list    (Object... args) { return arraySlice(args); }
        public final Object   _append  (Object... args) { return intp.append(args); }

        public final double   _fround   (Object... args) { oneArg("fround",      args.length); return cl_round(dbl(args[0])); }
        public final double   _ffloor   (Object... args) { oneArg("ffloor",      args.length); return Math.floor(dbl(args[0])); }
        public final double   _fceiling (Object... args) { oneArg("fceiling",    args.length); return Math.ceil (dbl(args[0])); }
        public final double   _ftruncate(Object... args) { oneArg("ftruncate",   args.length); return cl_truncate(dbl(args[0])); }

        public final long     _round   (Object... args) { oneArg("round",      args.length); return truncate(cl_round(dbl(args[0]))); }
        public final long     _floor   (Object... args) { oneArg("floor",      args.length); return truncate(Math.floor(dbl(args[0]))); }
        public final long     _ceiling (Object... args) { oneArg("ceiling",    args.length); return truncate(Math.ceil (dbl(args[0]))); }
        public final long     _truncate(Object... args) { oneArg("truncate",   args.length); return truncate(cl_truncate(dbl(args[0]))); }

        public final Object   charInt  (Object... args) { oneArg("char-code",  args.length); return (long)asChar("char-code", args[0]); }
        public final Object   intChar  (Object... args) { oneArg("code-char",  args.length); return (char)asInt("code-char", args[0]); }
        public final Object   stringeq (Object... args) { twoArg("string=",   args.length); return Objects.equals(asString("string=", args[0]), asString("string=", args[1])) ? _t : null; }
        public final Object   stringToList (Object... args) { oneArg("string->list",   args.length); return LambdaJ.stringToList(arraySlice(args)); }
        public final Object   listToString (Object... args) { oneArg("list->string",   args.length); return LambdaJ.listToString(arraySlice(args)); }

        public final Object   inc      (Object... args) { oneArg("1+",         args.length); number(args[0]); return LambdaJ.inc((Number)args[0]); }
        public final Object   dec      (Object... args) { oneArg("1-",         args.length); number(args[0]); return LambdaJ.dec((Number)args[0]); }

        public final double   _sqrt    (Object... args) { oneArg("sqrt",       args.length); return Math.sqrt (dbl(args[0])); }
        public final double   _log     (Object... args) { oneArg("log",        args.length); return Math.log  (dbl(args[0])); }
        public final double   _log10   (Object... args) { oneArg("log10",      args.length); return Math.log10(dbl(args[0])); }
        public final double   _exp     (Object... args) { oneArg("exp",        args.length); return Math.exp  (dbl(args[0])); }
        public final Number   _signum  (Object... args) { oneArg("signum",     args.length); number(args[0]); return cl_signum((Number)(args[0])); }
        public final double   _expt    (Object... args) { twoArg("expt",       args.length); return Math.pow  (dbl(args[0]), dbl(args[1])); }
        public final double   _mod     (Object... args) { twoArg("mod",        args.length); return cl_mod(dbl(args[0]), dbl(args[1])); }
        public final double   _rem     (Object... args) { twoArg("rem",        args.length); return cl_rem(dbl(args[0]), dbl(args[1])); }

        /// predefined aliased primitives
        // the following don't have a leading _ because they are avaliable (in the environment) under alias names
        public final double add     (Object... args) { if (args.length > 0) { double ret = dbl(args[0]); for (int i = 1; i < args.length; i++) ret += dbl(args[i]); return ret; } return 0.0; }
        public final double mul     (Object... args) { if (args.length > 0) { double ret = dbl(args[0]); for (int i = 1; i < args.length; i++) ret *= dbl(args[i]); return ret; } return 1.0; }

        public final double sub     (Object... args) { onePlusArg("-", args.length);
                                                       if (args.length == 1) return 0.0 - dbl(args[0]);
                                                       double ret = dbl(args[0]); for (int i = 1; i < args.length; i++) ret -= dbl(args[i]); return ret; }
        public final double quot    (Object... args) { onePlusArg("/", args.length);
                                                       if (args.length == 1) return 1.0 / dbl(args[0]);
                                                       double ret = dbl(args[0]); for (int i = 1; i < args.length; i++) ret /= dbl(args[i]); return ret; }

        public final Object numbereq(Object... args) { return compare("=", args,  (d1, d2) -> d1 == d2); }
        public final Object lt      (Object... args) { return compare("<", args,  (d1, d2) -> d1 <  d2); }
        public final Object le      (Object... args) { return compare("<=", args, (d1, d2) -> d1 <= d2); }
        public final Object ge      (Object... args) { return compare(">=", args, (d1, d2) -> d1 >= d2); }
        public final Object gt      (Object... args) { return compare(">", args,  (d1, d2) -> d1 >  d2); }
        public final Object ne      (Object... args) { return compare(">", args,  (d1, d2) -> d1 != d2); }

        public final Object format             (Object... args) { return intp.format(arraySlice(args)); }
        public final Object formatLocale       (Object... args) { return intp.formatLocale(arraySlice(args)); }
        //public final Object macroexpand1       (Object... args) { return intp.macroexpand1(arraySlice(args)); }
        public final Object _gensym (Object... args) { return intp.gensym(null); }

        public final Object getInternalRealTime(Object... args) { return LambdaJ.getInternalRealTime(); }
        public final Object getInternalRunTime (Object... args) { return LambdaJ.getInternalRunTime(); }
        public final Object getInternalCpuTime (Object... args) { return LambdaJ.getInternalCpuTime(); }
        public final Object sleep              (Object... args) { return LambdaJ.sleep(arraySlice(args)); }
        public final Object getUniversalTime   (Object... args) { return LambdaJ.getUniversalTime(); }
        public final Object getDecodedTime     (Object... args) { return intp.getDecodedTime(); }

        public final Object jambda             (Object... args) { return findJavaMethod(arraySlice(args)); }

        public final Object _trace             (Object... args) { return intp.trace(arraySlice(args)); }
        public final Object _untrace           (Object... args) { return intp.untrace(arraySlice(args)); }

        public final Object makeFrame          (Object... args) {
            final ConsCell a = arraySlice(args);
            stringArg("make-frame", "first arg", a);
            final String title = args[0].toString();
            numberArgs("make-frame", (ConsCell) cdr(a), 0, 3);
            final TurtleFrame ret = new TurtleFrame(title, (Number)cadr(a), (Number)caddr(a), (Number)cadddr(a));
            intp.current_frame = ret;
            return ret;
        }

        public final Object openFrame          (Object... args) { final ConsCell a = arraySlice(args); nArgs("open-frame",    a, 0, 1); return intp.asFrame("open-frame", car(a)).open(); }
        public final Object closeFrame         (Object... args) { final ConsCell a = arraySlice(args); nArgs("close-frame",   a, 0, 1); return intp.asFrame("close-frame",   car(a)).close();   }
        public final Object resetFrame         (Object... args) { final ConsCell a = arraySlice(args); nArgs("reset-frame",   a, 0, 1); return intp.asFrame("reset-frame",   car(a)).reset();   }
        public final Object clearFrame         (Object... args) { final ConsCell a = arraySlice(args); nArgs("clear-frame",   a, 0, 1); return intp.asFrame("clear-frame",   car(a)).clear();   }
        public final Object repaintFrame       (Object... args) { final ConsCell a = arraySlice(args); nArgs("repaint-frame", a, 0, 1); return intp.asFrame("repaint-frame", car(a)).repaint(); }
        public final Object flushFrame         (Object... args) { final ConsCell a = arraySlice(args); nArgs("flush-frame",   a, 0, 1); return intp.asFrame("flush-frame",   car(a)).flush(); }

        // set new current frame, return previous frame
        public final Object currentFrame       (Object... args) { final ConsCell a = arraySlice(args); nArgs("current-frame", a, 0, 1); final Object prev = intp.current_frame; if (car(a) != null) intp.current_frame = intp.asFrame("current-frame", car(a)); return prev; }

        public final Object pushPos            (Object... args) { final ConsCell a = arraySlice(args); nArgs("push-pos",a, 0, 1); return intp.asFrame("push-pos",car(a)).pushPos(); }
        public final Object popPos             (Object... args) { final ConsCell a = arraySlice(args); nArgs("pop-pos", a, 0, 1); return intp.asFrame("pop-pos", car(a)).popPos();  }

        public final Object penUp              (Object... args) { final ConsCell a = arraySlice(args); nArgs("pen-up",  a, 0, 1); return intp.asFrame("pen-up",   car(a)).penUp();   }
        public final Object penDown            (Object... args) { final ConsCell a = arraySlice(args); nArgs("pen-down",a, 0, 1); return intp.asFrame("pen-down", car(a)).penDown(); }

        public final Object color              (Object... args) { final ConsCell a = arraySlice(args); nArgs("color",   a, 0, 1); return intp.asFrame("color",   cadr(a)).color  (asInt("color",   car(a))); }
        public final Object bgColor            (Object... args) { final ConsCell a = arraySlice(args); nArgs("bgcolor", a, 0, 1); return intp.asFrame("bgcolor", cadr(a)).bgColor(asInt("bgcolor", car(a))); }

        public final Object text               (Object... args) { final ConsCell a = arraySlice(args); nArgs("text",    a, 1, 2); return intp.asFrame("text",    cadr(a)).text   (car(a).toString()); }

        public final Object right              (Object... args) { final ConsCell a = arraySlice(args); nArgs("right",   a, 1, 2); return intp.asFrame("right",   cadr(a)).right  (asDouble("right",   car(a))); }
        public final Object left               (Object... args) { final ConsCell a = arraySlice(args); nArgs("left",    a, 1, 2); return intp.asFrame("left",    cadr(a)).left   (asDouble("left",    car(a))); }
        public final Object forward            (Object... args) { final ConsCell a = arraySlice(args); nArgs("forward", a, 1, 2); return intp.asFrame("forward", cadr(a)).forward(asDouble("forward", car(a))); }

        public final Object moveTo             (Object... args) { final ConsCell a = arraySlice(args); nArgs("move-to", a, 2, 3);  return intp.asFrame("move-to",  caddr(a)).moveTo(asDouble("move-to",  car(a)), asDouble("move-to", cadr(a)));  }
        public final Object lineTo             (Object... args) { final ConsCell a = arraySlice(args); nArgs("line-to", a, 2, 3);  return intp.asFrame("line-to",  caddr(a)).lineTo(asDouble("line-to",  car(a)), asDouble("line-to", cadr(a)));  }
        public final Object moveRel            (Object... args) { final ConsCell a = arraySlice(args); nArgs("move-rel", a, 2, 3); return intp.asFrame("move-rel", caddr(a)).moveRel(asDouble("move-rel", car(a)), asDouble("move-rel", cadr(a))); }
        public final Object lineRel            (Object... args) { final ConsCell a = arraySlice(args); nArgs("line-rel", a, 2, 3); return intp.asFrame("line-rel", caddr(a)).lineRel(asDouble("line-rel", car(a)), asDouble("line-rel", cadr(a))); }

        public final Object makeBitmap         (Object... args) { final ConsCell a = arraySlice(args); nArgs("make-bitmap",     a, 2, 3); return intp.asFrame("make-bitmap",    caddr(a)).makeBitmap(asInt("make-bitmap",  car(a)), asInt("make-bitmap", cadr(a)));  }
        public final Object discardBitmap      (Object... args) { final ConsCell a = arraySlice(args); nArgs("discard-bitmap",  a, 0, 1); return intp.asFrame("discard-bitmap", car(a))  .discardBitmap();   }

        public final Object setPixel           (Object... args) { final ConsCell a = arraySlice(args); nArgs("set-pixel",       a, 3, 4); return intp.asFrame("set-pixel",      cadddr(a)).setRGB(asInt("set-pixel",  car(a)), asInt("set-pixel", cadr(a)), asInt("set-pixel", caddr(a)));  }
        public final Object rgbToPixel         (Object... args) { threeArgs("rgb-to-pixel", args.length); numbers(args[0], args[1], args[2]);
                                                                  final int r = asInt("rgb-to-pixel", args[0]);
                                                                  final int g = asInt("rgb-to-pixel", args[1]);
                                                                  final int b = asInt("rgb-to-pixel", args[2]);
                                                                  return (r<<16) | (g<<8) | b; }
        public final Object hsbToPixel         (Object... args) { threeArgs("hsb-to-pixel", args.length); numbers(args[0], args[1], args[2]);
                                                                  final float hue = asFloat("hsb-to-pixel", args[0]);
                                                                  final float sat = asFloat("hsb-to-pixel", args[1]);
                                                                  final float bri = asFloat("hsb-to-pixel", args[2]);
                                                                  return Color.HSBtoRGB(hue, sat, bri); }
        public final Object _fatal             (Object... args) { oneArg("fatal", args.length); throw new RuntimeException(String.valueOf(args[0])); }



        /// Helpers that the Java code compiled from Murmel will use, i.e. compiler intrinsics
        public final LambdaJSymbol intern(String symName) {
            return intp.symtab.intern(new LambdaJSymbol(symName));
        }

        public static ConsCell arraySlice(Object[] o, int offset) {
            return offset >= o.length ? null : new ArraySlice(o, offset);
        }

        public static ConsCell arraySlice(Object[] o) {
            return arraySlice(o, 0);
        }



        /** Primitives are in the environment as (CompilerPrimitive)... . Compiled code that calls primitives will
         *  actually call this overload and not funcall(Object, Object...) that contains the TCO thunking code. */
        public static Object funcall(CompilerPrimitive fn, Object... args) {
            return fn.applyPrimitive(args);
        }

        public static Object tailcall(CompilerPrimitive fn, Object... args) {
            return funcall(fn, args);
        }

        /** used for (apply sym form) */
        public static Object applyHelper(CompilerPrimitive fn, Object argList) {
            return funcall(fn, toArray(argList));
        }

        /** used for (apply sym form) */
        public static Object applyTailcallHelper(CompilerPrimitive fn, Object argList) {
            return funcall(fn, toArray(argList));
        }



        /** used for function calls, and also for let, labels, progn */
        public static Object funcall(MurmelFunction fn, Object... args) {
            Object r = fn.apply(args);
            while (r instanceof MurmelFunctionCall) {
                final MurmelFunctionCall functionCall = (MurmelFunctionCall)r;
                r = functionCall.next.apply(functionCall.args);
            }
            return r;
        }

        public static Object funcall(Object fn, Object... args) {
            if (fn instanceof MurmelFunction)    return funcall((MurmelFunction)fn, args);
            if (fn instanceof CompilerPrimitive) return funcall((CompilerPrimitive)fn, args);
            if (fn instanceof Primitive)         return ((Primitive)fn).apply(arraySlice(args));
            throw new LambdaJError(true, "not a function: %s", fn);
        }

        private final MurmelFunctionCall tailcall = new MurmelFunctionCall();
        /** used for function calls */
        public Object tailcall(Object fn, Object... args) {
            if (fn instanceof MurmelFunction)    {
                final MurmelFunctionCall tailcall = this.tailcall;
                tailcall.next = (MurmelFunction)fn;
                tailcall.args = args;
                return tailcall;
            }
            if (fn instanceof CompilerPrimitive) return funcall((CompilerPrimitive)fn, args);
            if (fn instanceof Primitive)         return funcall(fn, args);
            throw new LambdaJError(true, "not a function: %s", fn);
        }

        /** used for (apply sym form) */
        public static Object applyHelper(Object fn, Object argList) {
            return funcall(fn, toArray(argList));
        }

        /** used for (apply sym form) */
        public Object applyTailcallHelper(Object fn, Object argList) {
            return tailcall(fn, toArray(argList));
        }



        private static Object[] toArray(Object o) {
            if (o == null)
                return new Object[0];
            if (o instanceof Object[])
                return (Object[])o;
            return listToArray(o);
        }



        /** used by _cons() and by code generated from quotedFormToJava() */
        public static ConsCell cons(Object car, Object cdr)  { return new ListConsCell(car, cdr); }



        // car cdr ... bis gt waren special forms und der generierte code enthielt aufrufe
        // -> car, cdr, cons usw waren "open coded", koennte man wieder machen
        private static Object car (Object l)  { return LambdaJ.car(l); }
        private static Object cdr (Object l)  { return LambdaJ.cdr(l); }

        private static double dbl(Object n) {
            number(n);
            return ((Number)n).doubleValue();
        }

        private Object compare(String op, Object[] args, DoubleBiPred pred) {
            oneOrMoreNumbers(op, args);
            Number prev = (Number)args[0];
            final int length = args.length;
            for (int i = 1; i < length; i++) {
                final Number next = (Number)args[i];
                if (!pred.test(prev.doubleValue(), next.doubleValue())) return null;
                prev = next;
            }
            return _t;
        }



        /** zero or one argument */
        private static void oneOptArg(String expr, int argCount) {
            if (argCount > 1) throw new LambdaJError(true, "%s: too many arguments", expr);
        }

        /** one or more arguments */
        private static void onePlusArg(String expr, int argCount) {
            if (argCount < 1) throw new LambdaJError(true, "%s: not enough arguments", expr);
        }

        /** exactly one argument */
        private static void oneArg(String expr, int argCount) {
            if (1 != argCount) argError(expr, 1, argCount);
        }

        private static void onetwoArg(String expr, int argCount) {
            if (argCount < 1) throw new LambdaJError(true, "%s: not enough arguments", expr);
            if (argCount > 2) throw new LambdaJError(true, "%s: too many arguments", expr);
        }

        private static void twoArg(String expr, int argCount) {
            if (2 != argCount) argError(expr, 2, argCount);
        }

        private static void threeArgs(String expr, int argCount) {
            if (3 != argCount) argError(expr, 3, argCount);
        }

        public static void argCheck(String expr, int paramCount, int argCount) {
            if (paramCount != argCount) argError(expr, paramCount, argCount);
        }

        public static void argCheckVarargs(String expr, int paramCount, int argCount) {
            if (argCount < paramCount - 1) argError(expr, paramCount - 1, argCount);
        }

        private static void argError(String expr, int expected, int actual) {
            if (expected > actual) throw new LambdaJError(true, "%s: not enough arguments", expr);
            if (expected < actual) throw new LambdaJError(true, "%s: too many arguments", expr);
        }



        /** error if any arg is not of type number */
        private static void oneOrMoreNumbers(String expr, Object[] args) {
            final int length = args.length;
            if (length == 0) throw new LambdaJError(true, "%s: not enough arguments", expr);
            for (int i = 0; i < length; i++) {
                number(args[i]);
            }
        }

        /** error if n is not of type number */
        private static void number(Object n) {
            if (!(n instanceof Number)) notANumber(n);
        }

        /** error if any arg is not of type number */
        private static void numbers(Object n1, Object n2, Object n3) {
            number(n1);
            number(n2);
            number(n3);
        }

        private static void notANumber(Object n) {
            throw new LambdaJError(true, "not a number: %s", printSEx(n));
        }



        public String loc;

        /** main() will be called from compiled Murmel code */
        @SuppressWarnings("unused")
        protected static void main(MurmelJavaProgram program) {
            program.loc = "<unknown>";
            try {
                final Object result = program.body();
                if (result != null) {
                    System.out.println();
                    System.out.print("==> "); program._write(result);
                    System.out.println();
                    //System.exit(0); don't call exit this wouldn't wait for open frames
                }
            } catch (LambdaJError e) {
                System.err.println(e.getMessage());
                System.exit(1);
            }
        }
    }



    ///
    /// ## class MurmelJavaCompiler
    /// class MurmelJavaCompiler - compile Murmel to Java or to a in-memory Class-object and optionally to a .jar file
    ///
    public static class MurmelJavaCompiler {
        private LambdaJ.SymbolTable st;
        private final Path libDir;
        private final JavaCompilerHelper javaCompiler;
        private LambdaJ intp;

        public MurmelJavaCompiler(SymbolTable st, Path libDir, Path outPath) {
            this.st = st;
            this.libDir = libDir;
            this.javaCompiler = new JavaCompilerHelper(outPath);
            for (String s: reservedWords) {
                reservedSymbols.add(intern(s));
            }
        }

        private LambdaJ interpreter() {
            if (intp == null) {
                intp = new LambdaJ(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null, libDir);
                intp.init(() -> -1, System.out::print);
                intp.setSymtab(st);
                intp.topEnv = intp.environment(null);
            }
            return intp;
        }



        private static final String[] reservedWords = {
                "nil", "t",
                "lambda", "quote", "cond", "labels", "if", "define", "defun", "let", "let*", "letrec",
                "apply", "progn",
        };

        private final Collection<LambdaJSymbol> reservedSymbols = new ArrayList<>();

        private void notReserved(LambdaJSymbol sym) {
            if (sym == null)
                throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", "compile", "nil");
            if (reservedSymbols.contains(sym))
                throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", "compile", sym.toString());
        }

        private void notReserved(Object sym) {
            if (sym == null)
                throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", "compile", "nil");
            requireSymbol(sym);
            if (reservedSymbols.contains(sym))
                throw new LambdaJError(true, "%s: can't use reserved word %s as a symbol", "compile", sym.toString());
        }



        /// Environment for compiled Murmel:
        /// * nil, t, pi
        /// * car, cdr, cons, rplaca, rplacd
        /// * eval, eq, eql, null, intern, write, writeln, lnwrite
        /// * atom, consp, listp, symbolp, numberp, stringp, characterp, integerp, floatp
        /// * assoc, list, append
        /// * round, floor, ceiling, truncate
        /// * fround, ffloor, fceiling, ftruncate
        /// * sqrt, log, log10, exp, expt, mod, rem, signum
        /// * +, *, -, /, =, <, <=, >=, > are handled as special forms (inlined for performance) and are primitives as well (for apply)
        /// * internal-time-units-per-second
        /// * get-internal-real-time, get-internal-run-time, get-internal-cpu-time, sleep, get-universal-time, get-decoded-time
        /// * format, format-locale, char-code, code-char, string=, string->list, list->string
        ///
        private static final String[] globalvars = { "nil", "t", "pi" };
        private static final String[][] aliasedGlobals = {
            { "internal-time-units-per-second", "itups" },
            { "*command-line-argument-list*", "commandlineArgumentList" },
        };
        private static final String[] primitives = {
                "car", "cdr", "cons", "rplaca", "rplacd",
                "eval", "eq", "eql", "null", "write", "writeln", "lnwrite",
                "atom", "consp", "listp", "symbolp", "numberp", "stringp", "characterp", "integerp", "floatp",
                "assoc", "list", "append",
                "round", "floor", "ceiling", "truncate",
                "fround", "ffloor", "fceiling", "ftruncate",
                "sqrt", "log", "log10", "exp", "expt", "mod", "rem", "signum",
                "gensym", "trace", "untrace",
                "fatal",
        };
        private static final String[][] aliasedPrimitives = {
            {"+", "add"}, {"*", "mul"}, {"-", "sub"}, {"/", "quot"},
            {"=", "numbereq"}, {"<=", "le"}, {"<", "lt"}, {">=", "ge"}, {">", "gt"}, { "/=", "ne" },
            {"1+", "inc"}, {"1-", "dec"},
            {"format", "format"}, {"format-locale", "formatLocale" }, {"char-code", "charInt"}, {"code-char", "intChar"}, 
            {"string=", "stringeq"}, {"string->list", "stringToList"}, {"list->string", "listToString"},
            //{ "macroexpand-1", "macroexpand1" },
            {"get-internal-real-time", "getInternalRealTime" }, {"get-internal-run-time", "getInternalRunTime" }, {"get-internal-cpu-time", "getInternalCpuTime" },
            {"sleep", "sleep" }, {"get-universal-time", "getUniversalTime" }, {"get-decoded-time", "getDecodedTime" },
            { "::", "jambda" },

            { "make-frame", "makeFrame" }, { "open-frame", "openFrame"}, { "close-frame", "closeFrame" },
            { "reset-frame", "resetFrame" }, { "clear-frame", "clearFrame" }, { "repaint-frame", "repaintFrame" }, { "flush-frame", "flushFrame" },
            { "current-frame", "currentFrame" },
            { "push-pos", "pushPos" }, { "pop-pos", "popPos" }, { "pen-up", "penUp" }, { "pen-down", "penDown" },
            { "color", "color" }, { "bgcolor", "bgColor" }, { "text", "text" },
            { "right", "right" }, { "left", "left" }, { "forward", "forward" },
            { "move-to", "moveTo" }, { "line-to", "lineTo" }, { "move-rel", "moveRel" }, { "line-rel", "lineRel" },
            { "make-bitmap", "makeBitmap" }, { "discard-bitmap", "discardBitmap" },
            { "set-pixel", "setPixel" },
            { "rgb-to-pixel", "rgbToPixel" }, { "hsb-to-pixel", "hsbToPixel" },
        };



        /// Wrappers to compile Murmel to a Java class and optionally a .jar

        public Class <MurmelProgram> formsToJavaClass(String unitName, Iterable<Object> forms, String jarFileName) throws Exception {
            final Iterator<Object> i = forms.iterator();
            final ObjectReader r = () -> i.hasNext() ? i.next() : null;
            return formsToJavaClass(unitName, r, jarFileName);
        }

        /** Compile the Murmel compilation {@code forms} to a Java class for a standalone application with a "public static void main()" */
        public Class <MurmelProgram> formsToJavaClass(String unitName, ObjectReader forms, String jarFileName) throws Exception {
            final StringWriter w = new StringWriter();
            formsToJavaSource(w, unitName, forms);
            //System.err.print(w.toString());
            return javaCompiler.javaToClass(unitName, w.toString(), jarFileName);
        }



        /// Wrappers to compile Murmel to Java source

        public void formsToJavaSource(Writer ret, String unitName, Iterable<Object> forms) {
            final Iterator<Object> i = forms.iterator();
            final ObjectReader r = () -> i.hasNext() ? i.next() : null;
            formsToJavaSource(ret, unitName, r);
        }

        /** Compile the Murmel compilation unit to Java source for a standalone application class {@code unitName}
         *  with a "public static void main()" */
        public void formsToJavaSource(Writer w, String unitName, ObjectReader forms) {
            ConsCell predefinedEnv = null;
            for (String   global: globalvars)        predefinedEnv = extenvIntern(intern(global),   '_' + global,   predefinedEnv);
            for (String[] alias:  aliasedGlobals)    predefinedEnv = extenvIntern(intern(alias[0]), alias[1], predefinedEnv);
            for (String   prim:   primitives)        predefinedEnv = extenvprim(prim, mangle(prim, 0), predefinedEnv);
            for (String[] alias:  aliasedPrimitives) predefinedEnv = extenvprim(alias[0], alias[1], predefinedEnv);

            final WrappingWriter ret = new WrappingWriter(w);

            final String clsName;
            final int dotpos = unitName.lastIndexOf('.');
            if (dotpos == -1) {
                clsName = unitName;
            } else {
                ret.append("package ").append(unitName.substring(0, dotpos)).append(";\n\n");
                clsName = unitName.substring(dotpos+1);
            }
            ret.append("import io.github.jmurmel.LambdaJ;\n"
                     + "import io.github.jmurmel.LambdaJ.*;\n\n"
                     + "public class ").append(clsName).append(" extends MurmelJavaProgram {\n"
                     + "    protected ").append(clsName).append(" rt() { return this; }\n\n"
                     + "    public static void main(String[] args) {\n"
                     + "        ").append(clsName).append(" program = new ").append(clsName).append("();\n"
                     + "        program.commandlineArgumentList = arraySlice(args);\n"
                     + "        main(program);\n"
                     + "    }\n\n");

            final ArrayList<Object> bodyForms = new ArrayList<>();
            final StringBuilder globals = new StringBuilder();
            final ObjectReader _forms = (forms instanceof SExpressionParser) ? () -> ((SExpressionParser)forms).readObj(true) : forms;

            /// first pass: emit toplevel define/ defun forms
            passTwo = false;
            implicitDecl = new HashSet<>();
            ConsCell globalEnv = predefinedEnv;
            Object form;
            while (null != (form = _forms.readObj())) {
                try {
                    globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, form);
                }
                catch (LambdaJError e) {
                    throw new LambdaJError(false, e.getMessage(), form);
                }
                catch (Exception e) {
                    throw new LambdaJError(e, "formToJava: internal error - caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
                }
            }

            if (!implicitDecl.isEmpty()) {
                throw new LambdaJError("undefined symbols: " + implicitDecl);
            }
            implicitDecl = null;

            interpreter().macros.clear(); // on pass2 macros will be re-interpreted at the right place so that illegal macro forward-refences are caught

            // generate getValue() for embed API
            ret.append("    @Override public Object getValue(String symbol) {\n"
                     + "        switch (symbol) {\n")
               .append(globals)
               .append("        }\n");

            ret.append("        switch (symbol) {\n");
            for (String   global: globalvars)        ret.append("        case \"").append(global)  .append("\": return _").append(global).append(";\n");
            for (String[] alias:  aliasedGlobals)    ret.append("        case \"").append(alias[0]).append("\": return ") .append(alias[1]).append(";\n");
            for (String   prim:   primitives)        ret.append("        case \"").append(prim)    .append("\": return (CompilerPrimitive)rt()::_").append(prim).append(";\n");
            for (String[] alias:  aliasedPrimitives) ret.append("        case \"").append(alias[0]).append("\": return (CompilerPrimitive)rt()::").append(alias[1]).append(";\n");
            ret.append("        default: throw new LambdaJError(true, \"%s: '%s' is undefined\", \"getValue\", symbol);\n"
                     + "        }\n"
                     + "    }\n\n"
                     + "    // toplevel forms\n"
                     + "    protected Object runbody() {\n        Object result0;\n");

            /// second pass: emit toplevel forms that are not define or defun as well as the actual assignments for define/ defun
            passTwo = true;
            formsToJava(ret, bodyForms, globalEnv, globalEnv, 0, true);

            ret.append("    }\n"
                     + "}\n");
            ret.flush();
        }

        private ConsCell toplevelFormToJava(WrappingWriter ret, ArrayList<Object> bodyForms, StringBuilder globals, ConsCell globalEnv, Object form) {
            if (consp(form)) { // todo toplevel progn inline expandieren?
                final Object op = car(form);
                if (op == interpreter().sDefine) {
                    globalEnv = defineToJava(ret, (ConsCell) form, globalEnv);
                    interpreter().eval(form, null);
                } else if (op == interpreter().sDefun) {
                    globalEnv = defunToJava(ret, (ConsCell) form, globalEnv);
                    interpreter().eval(form, null);
                } else if (op == interpreter().sDefmacro) {
                    final Object sym = cadr(form);
                    notReserved(sym);
                    interpreter().eval(form, null);
                }
            }

            if (consp(form) && interpreter().macros.containsKey(car(form))) {
                final Object expansion = interpreter().mexpand(car(form), (ConsCell) cdr(form), 0, 0, 0);
                globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, expansion);
            } else if (consp(form) && car(form) == interpreter().sLoad) {
                nArgs("load", cdr(form), 1);
                globalEnv = loadFile(true, "load", ret, cadr(form), null, globalEnv, -1, false, bodyForms, globals);
            } else if (consp(form) && car(form) == interpreter().sRequire) {
                nArgs("require", cdr(form), 1, 2);
                if (!stringp(cadr(form))) throw new LambdaJError(true, "%s: expected a string argument but got %s", "require", printSEx(cdr(form)));
                final Object modName = cadr(form);
                if (!interpreter().modules.contains(modName)) {
                    Object modFilePath = caddr(form);
                    if (modFilePath == null) modFilePath = modName;
                    globalEnv = loadFile(true,"require", ret, modFilePath, null, globalEnv, -1, false, bodyForms, globals);
                    interpreter().modules.add(modName);
                }
            }
            else bodyForms.add(form);

            if (consp(form) && (car(form) == interpreter().sDefine || car(form) == interpreter().sDefun))
                globals.append("        case \"").append(cadr(form)).append("\": return ").append(javasym(cadr(form), globalEnv)).append(";\n");
            return globalEnv;
        }


        /** extend the environment by putting (symbol mangledsymname) in front of {@code prev},
         *  symbols that are reserved words throw an error. */
        private ConsCell extenv(Object symbol, int sfx, ConsCell prev) {
            requireSymbol(symbol);
            final LambdaJSymbol sym = (LambdaJSymbol)symbol;
            notReserved(sym);
            return extenvIntern(sym, mangle(symbol.toString(), sfx), prev);
        }

        private static void requireSymbol(Object symbol) {
            if (symbol != null && !(symbol instanceof LambdaJSymbol)) throw new LambdaJError(true, "not a symbol: %s", symbol);
        }

        /** extend environment w/o reserved word check */
        private static ConsCell extenvIntern(LambdaJSymbol sym, String javaName, ConsCell env) {
            return cons(cons(sym, javaName), env);
        }

        private ConsCell extenvprim(String symname, String javaName, ConsCell env) {
            final LambdaJSymbol sym = intern(symname);
            return extenvIntern(sym, "((CompilerPrimitive)rt()::" + javaName + ')', env);
        }



        private LambdaJSymbol intern(String symname) {
            if (symname == null) symname = "nil";
            return st.intern(new LambdaJSymbol(symname));
        }

        /** replace chars that are not letters */
        private static String mangle(String symname, int sfx) {
            final StringBuilder mangled = new StringBuilder();
            for (char c: symname.toCharArray()) {
                if (c == '_' || c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') mangled.append(c);
                else mangled.append('_').append((int)c).append('_');
            }
            return '_' + mangled.toString() + (sfx == 0 ? "" : sfx);
        }

        private boolean passTwo;
        private Set<String> implicitDecl;
        private String javasym(Object form, ConsCell env) {
            if (form == null) form = intern("nil");
            final ConsCell symentry = assoceq(form, env);
            if (symentry == null) {
                if (passTwo) throw new LambdaJError(true, "undefined symbol %s", form.toString());
                System.err.println("implicit declaration of " + form);
                implicitDecl.add(form.toString());
                return mangle(form.toString(), 0) + ".get()"; // on pass 1 assume that undeclared variables are forward references to globals
            }
            else if (!passTwo) implicitDecl.remove(form.toString());
            final String javasym;
            if (listp(cdr(symentry))) javasym = (String)cadr(symentry); // function: symentry is (sym . (javasym . (params...)))
            else javasym = (String)cdr(symentry);
            return javasym;
        }

        private void notDefined(String func, Object sym, ConsCell env) {
            final ConsCell prevEntry = assoceq(sym, env);
            if (prevEntry != null) {
                notReserved((LambdaJSymbol) car(prevEntry));
                throw new LambdaJError(true, "%s: can't redefine symbol %s", func, sym);
            }
        }

        private void defined(String func, Object sym, ConsCell env) {
            if (sym == null) sym = intern("nil");
            final ConsCell symentry = assoceq(sym, env);
            if (symentry == null) throw new LambdaJError(true, "%s: undefined symbol %s", func, sym.toString());
        }



        /** Emit a member for {@code symbol} and a function that assigns {@code form} to {@code symbol}.
         *  @param form a list (define symbol form) */
        private ConsCell defineToJava(WrappingWriter sb, ConsCell form, ConsCell env) {
            final Object sym = cadr(form);

            notReserved(sym);
            notDefined("define", sym, env);
            final String javasym = mangle(sym.toString(), 0);
            env = extenvIntern((LambdaJSymbol) sym, javasym + ".get()", env); // ggf. die methode define_javasym OHNE javasym im environment generieren, d.h. extenvIntern erst am ende dieser methode

            sb.append("    // ").append(lineInfo(form)).append("(define ").append(sym).append(" form)\n"
                    + "    public CompilerGlobal ").append(javasym).append(" = UNASSIGNED;\n");

            sb.append("    public Object define_").append(javasym).append("() {\n"
                    + "        loc = \"").append(lineInfo(form))/*.append(printSEx(cadr(form)))*/.append("\";\n"
                    + "        if (").append(javasym).append(" != UNASSIGNED) rterror(new LambdaJError(\"duplicate define\"));\n"
                    + "        try { final Object value = "); formToJava(sb, caddr(form), env, env, 0, false); sb.append(";\n"
                    + "        ").append(javasym).append(" = () -> value; }\n"
                    + "        catch (LambdaJError e) { rterror(e); }\n"
                    + "        return intern(\"").append(sym).append("\");\n"
                    + "    }\n\n");
            return env;
        }

        /** @param form a list (defun symbol ((symbol...) forms...)) */
        private ConsCell defunToJava(WrappingWriter sb, ConsCell form, ConsCell env) {
            final Object sym = cadr(form);
            final Object params = caddr(form);
            final Object body = cdddr(form);

            notReserved(sym);
            notDefined("defun", sym, env);
            final String javasym = mangle(sym.toString(), 0);
            env = extenvIntern((LambdaJSymbol) sym, javasym + ".get()", env);

            sb.append("    // ").append(lineInfo(form)).append("(defun ").append(sym).append(' ').append(printSEx(params)).append(" forms...)\n"
                    + "    private CompilerGlobal ").append(javasym).append(" = UNASSIGNED;\n");

            sb.append("    public LambdaJSymbol defun_").append(javasym).append("() {\n"
                    + "        loc = \"").append(lineInfo(form))/*.append(printSEx(cadr(form)))*/.append("\";\n"
                    + "        if (").append(javasym).append(" != UNASSIGNED) rterror(new LambdaJError(\"duplicate defun\"));\n"
                    + "        final MurmelFunction func = (args0) -> {\n");
            final ConsCell extenv = params(sb, params, env, 0, javasym);
            sb.append("        Object result0;\n");
            formsToJava(sb, (ConsCell)body, extenv, env, 0, false);
            sb.append("        };\n"
                    + "        ").append(javasym).append(" = () -> func;\n"
                    + "        return intern(\"").append(sym).append("\");\n"
                    + "    }\n\n");

            return env;
        }



        /// formsToJava - compile a list of Murmel forms to Java source
        /** generate Java code for a list of forms */
        private void formsToJava(WrappingWriter ret, Iterable<Object> forms, ConsCell env, ConsCell topEnv, int rsfx, boolean topLevel) {
            if (forms == null) {
                // e.g. the body of an empty lambda or function
                ret.append("        return null;\n");
                return;
            }

            final Iterator<Object> it = forms.iterator();
            while (it.hasNext()) {
                final Object form = it.next();
                //ret.append("        // ").append(lineInfo(form));      stringToJava(ret, printSEx(form)); ret.append('\n'); // nicht 2x stringToJava(ret, printSEx(form)) 
                ret.append("        loc = \"").append(lineInfo(form)); stringToJava(ret, printSEx(form)); ret.append("\";\n");
                if (it.hasNext()) ret.append("        result").append(rsfx).append(" = ");
                else              ret.append("        return ");
                formToJava(ret, form, env, topEnv, rsfx, !topLevel && !it.hasNext());
                ret.append(";\n");
            }
        }

        /// formToJava - compile a Murmel form to Java source. Note how this is somehow similar to eval:
        private void formToJava(WrappingWriter sb, Object form, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            try {

                /// * symbols
                if (symbolp(form)) {
                    sb.append(javasym(form, env));  return;
                }
                /// * atoms that are not symbols
                if (atom(form)) {
                    atomToJava(sb, form);  return;
                }

                if (consp(form)) {
                    final Object op = car(form);
                    final Object args = cdr(form);

//                    // * some functions and operators are compiled inline:
//                    //     - number operators
//                    if (isSymbol(op, "+")) { addDbl(sb, "+", 0.0, args, env, rsfx); return; }
//                    if (isSymbol(op, "*")) { addDbl(sb, "*", 1.0, args, env, rsfx); return; }
//                    if (isSymbol(op, "-")) { subDbl(sb, "-", 0.0, args, env, rsfx); return; }
//                    if (isSymbol(op, "/")) { subDbl(sb, "/", 1.0, args, env, rsfx); return; }
//
//                    //     - number compare operators
//                    if (isSymbol(op, "="))  { compareNum(sb, "numbereq", args, env, rsfx); return; }
//                    if (isSymbol(op, "<"))  { compareNum(sb, "lt",       args, env, rsfx); return; }
//                    if (isSymbol(op, "<=")) { compareNum(sb, "le",       args, env, rsfx); return; }
//                    if (isSymbol(op, ">=")) { compareNum(sb, "ge",       args, env, rsfx); return; }
//                    if (isSymbol(op, ">"))  { compareNum(sb, "gt",       args, env, rsfx); return; }
//
//                    //     - cons, car, cdr
//                    if (isSymbol(op, "car"))  { sb.append("car(");  formToJava(sb, car(args), env, rsfx); sb.append(")"); return; }
//                    if (isSymbol(op, "cdr"))  { sb.append("cdr(");  formToJava(sb, car(args), env, rsfx); sb.append(")"); return; }
//                    if (isSymbol(op, "cons")) { sb.append("cons("); formToJava(sb, car(args), env, rsfx); sb.append(", "); formToJava(sb, cadr(args), env, rsfx); sb.append(')'); return; }
//
//                    //     - eq, not
//                    if (isSymbol(op, "eq"))   { compareOp(sb, "==", car(args), cadr(args), env, rsfx); return; }
//                    if (isSymbol(op, "null")) { compareOp(sb, "==", car(args), null, env, rsfx); return; }



                    /// * special forms:

                    ///     - quote
                    if (interpreter().sQuote == op) { quotedFormToJava(sb, car(args)); return; }

                    ///     - if
                    if (interpreter().sIf == op) {
                        formToJava(sb, car(args), env, topEnv, rsfx, false); sb.append(" != null\n        ? "); formToJava(sb, cadr(args), env, topEnv, rsfx, isLast);
                        if (caddr(args) != null) { sb.append("\n        : "); formToJava(sb, caddr(args), env, topEnv, rsfx, isLast); }
                        else sb.append("\n        : null");
                        return;
                    }

                    ///     - cond
                    if (interpreter().sCond == op) {
                        sb.append("false ? null\n");
                        for (Object cond: (ConsCell)args) {
                            sb.append("        : ("); formToJava(sb, car(cond), env, topEnv, rsfx, false); sb.append(" != null)\n        ? ");
                            prognToJava(sb, (ConsCell)cdr(cond), env, topEnv, rsfx+1, isLast);
                        }
                        sb.append("\n        : null");
                        return;
                    }

                    ///     - lambda
                    if (interpreter().sLambda == op) {
                        lambdaToJava(sb, args, env, topEnv, rsfx+1);
                        return;
                    }

                    if (interpreter().sSetQ == op) {
                        sb.append("_fatal(\"setq is not yet supported\")");
                        return;
                    }
                    if (interpreter().sDefine == op) {
                        if (rsfx != 0) throw new LambdaJError("define as non-toplevel form is not yet implemented");
                        final Object sym = cadr(form);
                        notReserved(sym); // todo notreserved und defined muesste eigentlich durch pass1 erledigt sein
                        defined("define", sym, env);
                        final String javasym = mangle(cadr(form).toString(), 0);
                        sb.append("define_").append(javasym).append("()");
                        return;
                    }
                    if (interpreter().sDefun == op) {
                        if (rsfx != 0) throw new LambdaJError("defun as non-toplevel form is not yet implemented");
                        final Object sym = cadr(form);
                        notReserved(sym); // todo notreserved und defined muesste eigentlich durch pass1 erledigt sein
                        defined("define", sym, env);
                        final String javasym = mangle(cadr(form).toString(), 0);
                        sb.append("defun_").append(javasym).append("()");
                        return;
                    }
                    if (interpreter().sDefmacro == op) {
                        if (rsfx != 0) throw new LambdaJError("defmacro as non-toplevel form is not yet implemented");
                        final Object sym = cadr(form);
                        Object result = interpreter().eval(form, null);
                        if (result != null) sb.append("intern(\"").append(sym).append("\")");
                        else sb.append("null");
                        return;
                    }

                    ///     - apply
                    if (interpreter().sApply == op) {
                        sb.append(isLast ? "applyTailcallHelper(" : "applyHelper(");
                        formToJava(sb, car(args), env, topEnv, rsfx, false);
                        sb.append("\n        , ");
                        formToJava(sb, cadr(args), env, topEnv, rsfx, false);
                        sb.append(')');
                        return;
                    }

                    ///     - progn
                    if (interpreter().sProgn == op) {
                        prognToJava(sb, (ConsCell)args, env, topEnv, rsfx+1, isLast);
                        return;
                    }

                    ///     - labels: (labels ((symbol (params...) forms...)...) forms...) -> object
                    // note how labels is similar to let: let binds values to symbols, labels binds functions to symbols
                    if (interpreter().sLabels == op) {
                        labelsToJava(sb, (ConsCell)args, env, topEnv, rsfx+1, isLast);
                        return;
                    }

                    ///     - let: (let ((sym form)...) forms...) -> Object
                    ///     - named let: (let sym ((sym form)...) forms...) -> Object
                    if (interpreter().sLet == op) {
                        letToJava(sb, args, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let*: (let* ((sym form)...) forms...) -> Object
                    ///     - named let*: (let sym ((sym form)...) forms...) -> Object
                    if (interpreter().sLetStar == op) {
                        letStarToJava(sb, args, env, topEnv, rsfx+1, isLast);
                        return;
                    }


                    ///     - letrec:       (letrec ((sym form)...) forms) -> Object
                    ///     - named letrec: (letrec sym ((sym form)...) forms) -> Object
                    if (interpreter().sLetrec == op) {
                        letrecToJava(sb, args, env, topEnv, rsfx+1, isLast);
                        return;
                    }

                    if (interpreter().sLoad == op) {
                        nArgs("load", args, 1);
                        // todo aenderungen im environment gehen verschuett, d.h. define/defun funktioniert nur bei toplevel load, nicht hier
                        loadFile(false, "load", sb, car(args), env, topEnv, rsfx, isLast, null, null);
                        return;
                    }

                    if (interpreter().sRequire == op) {
                        // pass1 has replaced all toplevel (require)s with the file contents
                        throw new LambdaJError("require as non-toplevel form is not implemented");
                    }
                    
                    /// * macro expansion
                    if (intp != null && intp.macros.containsKey(op)) {
                        final Object expansion = interpreter().mexpand(op, (ConsCell) args, 0, 0, 0);
                        formToJava(sb, expansion, env, topEnv, rsfx, isLast);
                        return;
                    }

                    /// * function call
                    sb.append(isLast ? "tailcall(" : "funcall(");
                    formToJava(sb, op, env, topEnv, rsfx, false);
                    if (args != null) {
                        for (Object arg: (ConsCell)args) {
                            sb.append("\n        , ");
                            formToJava(sb, arg, env, topEnv, rsfx, false);
                        }
                    }
                    sb.append(')');
                    return;
                }
                throw new LambdaJError("compile-to-java: form not implemented: " + printSEx(form));

            }
            catch (LambdaJError e) {
                throw new LambdaJError(false, e.getMessage(), form);
            }
            catch (Exception e) {
                //e.printStackTrace();
                throw new LambdaJError(e, "formToJava: internal error - caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
            }
        }

        /** write atoms that are not symbols */
        private static void atomToJava(WrappingWriter sb, Object form) {
            if (form instanceof Long) sb.append(Long.toString((Long) form)).append('L');
            else if (form instanceof Character) {
                final char c = ((Character) form);
                switch (c) {
                case '\r': sb.append("'\\r'"); break;
                case '\n': sb.append("'\\n'"); break;
                case '\t': sb.append("'\\t'"); break;
                case '\'': sb.append("'\\''"); break;
                case '\\': sb.append("'\\''"); break;
                default:
                    if (c >= 32 && c < 127) sb.append('\'').append(c).append('\'');
                    else sb.append((String.format("'\\u%04X'", (int)c)));
                }
            }
            //else if (form instanceof String) sb.append("new String(\"").append(form).append("\")"); // new Object so that (eql "a" "a") is nil (Common Lisp allows both nil and t). otherwise the reader must intern strings as well
            else if (form instanceof String) { sb.append('"'); stringToJava(sb, (String)form); sb.append('"'); }
            else sb.append(printSEx(form));
        }

        private static void stringToJava(WrappingWriter sb, String s) {
            if (s == null)   { sb.append("null"); return; }
            if (s.isEmpty()) { sb.append("\"\""); return; }
            
            for (char c: s.toCharArray()) {
                switch (c) {
                case '\"': sb.append("\\\""); break;
                case '\\': sb.append("\\\\");   break;
                case '\r': sb.append("\\r");  break;
                case '\n': sb.append("\\n");  break;
                case '\t': sb.append("\\t");  break;
                default:
                    if (c >= 32 && c < 127) sb.append(c);
                    else sb.append((String.format("\\u%04X", (int)c)));
                }
            }
        }

        /** args = ((sym...) form...) */
        private void lambdaToJava(WrappingWriter sb, final Object args, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append("(MurmelFunction)(args").append(rsfx).append(" -> {\n        Object result").append(rsfx).append(";\n");
            final String expr = "(lambda " + printSEx(car(args)) + ')';
            env = params(sb, car(args), env, rsfx, expr);
            formsToJava(sb, (ConsCell)cdr(args), env, topEnv, rsfx, false);
            sb.append("        })");
        }

        private int ignoredCounter = 0;
        
        private void prognToJava(WrappingWriter sb, ConsCell forms, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (cdr(forms) == null) formToJava(sb, car(forms), env, topEnv, rsfx, isLast);
            else {
                sb.append((isLast ? "tailcall(" : "funcall(")).append("(MurmelFunction)(Object... ignored").append(ignoredCounter++).append(") -> {\n        Object result").append(rsfx).append(";\n");
                formsToJava(sb, forms, env, topEnv, rsfx, false);
                sb.append("        }, (Object[])null)");
            }
        }

        /** args = (formsym (sym...) form...) */
        // beim aufruf aus labels koennte das ein lambda sein ohne "private final Object xxx = this", wird aber auch fuer named let benutzt 
        private void labelToJava(WrappingWriter sb, final Object args, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append("new MurmelFunction() {\n");
            env = extenv(car(args), rsfx, env);
            sb.append("        private final Object ").append(javasym(car(args), env)).append(" = this;\n"); // "Object o = (MurmelFunction)this::apply" is the same as "final Object x = this"  
            sb.append("        public Object apply(Object... args").append(rsfx).append(") {\n        Object result").append(rsfx).append(";\n");
            env = params(sb, cadr(args), env, rsfx, car(args).toString());
            formsToJava(sb, (ConsCell)cddr(args), env, topEnv, rsfx, false);
            sb.append("        } }");
        }

        /** args = (((symbol (sym...) form...)...) form...) */
        private void labelsToJava(WrappingWriter sb, final ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (args == null) throw new LambdaJError(true, "%s: malformed %s: expected at least one argument", "labels", "labels");
            
            final Object localFuncs = car(args);
            if (localFuncs == null || (cddr(args) == null && atom(cadr(args)))) {
                // no local functions or body is one single atom
                prognToJava(sb, (ConsCell)cdr(args), env, topEnv, rsfx, isLast);
                return;
            }

            sb.append(isLast ? "tailcall(" : "funcall(");
            sb.append("new MurmelFunction() {\n");

            final ConsCell params = paramList(localFuncs);
            for (Object localFunc: params) {
                env = extenv(localFunc, rsfx, env);
            }

            for (Object symbolParamsAndBody: (ConsCell) localFuncs) {
                sb.append("        private final Object ").append(javasym(car(symbolParamsAndBody), env)).append(" = ");
                labelToJava(sb, symbolParamsAndBody, env, topEnv, rsfx+1);
                sb.append(";\n");
            }

            sb.append("        @Override public Object apply(Object... args) {\n");
            sb.append("        Object result").append(rsfx).append(";\n");
            formsToJava(sb, (ConsCell)cdr(args), env, topEnv, rsfx, false); // todo isLast statt false? oder .apply() statt tailcall/funcall?
            sb.append("        } } )");
        }

        /** let and named let */
        private void letToJava(WrappingWriter sb, final Object args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final ConsCell bindings;
            if (car(args) instanceof LambdaJSymbol) {
                // named let
                final ConsCell params = paramList(cadr(args));
                sb.append(isLast ? "tailcall(" : "funcall("); // todo kein tailcall/funcall bei leerem body?
                labelToJava(sb, cons(car(args), cons(params, cddr(args))), env, topEnv, rsfx+1);
                bindings = (ConsCell)cadr(args);
            }
            else {
                // regular let
                final ConsCell params = paramList(car(args));
                sb.append(isLast ? "tailcall(" : "funcall("); // todo kein tailcall/funcall bei leerem body?
                lambdaToJava(sb, cons(params, cdr(args)), env, topEnv, rsfx+1);
                bindings = (ConsCell)(car(args));
            }
            if (bindings != null)
                for (Object binding: bindings) {
                    sb.append("\n        , ");
                    if (caddr(binding) != null) throw new LambdaJError(true, "%s: malformed %s: illegal variable specification %s", "let", "let", printSEx(binding));
                    formToJava(sb, cadr(binding), env, topEnv, rsfx, false);
                }
            sb.append(')');
        }

        /** let* and named let* */
        private void letStarToJava(WrappingWriter sb, final Object args, ConsCell _env, ConsCell topEnv, int rsfx, boolean isLast) {
            ConsCell env = _env;
            final ConsCell bindings, bodyForms;
            final LambdaJSymbol name;
            sb.append(isLast ? "tailcall(" : "funcall(");

            if (car(args) instanceof LambdaJSymbol) {
                // named let*: (let* sym ((sym form)...) forms...) -> Object
                name = (LambdaJSymbol)car(args);
                sb.append("new MurmelFunction() {\n"
                        + "        private final Object ").append(javasym(name, extenv(name, rsfx, env))).append(" = this;\n"
                        + "        @Override public Object apply(Object... args").append(rsfx).append(") {\n        Object result").append(rsfx).append(";\n");
                bindings = (ConsCell)(cadr(args));
                bodyForms = (ConsCell) cddr(args);
            }
            else {
                // regular let*: (let* ((sym form)...) forms...) -> Object
                name = null;
                sb.append("(MurmelFunction)(args").append(rsfx).append(") -> {\n        Object result").append(rsfx).append(";\n");
                bindings = (ConsCell)(car(args));
                bodyForms = (ConsCell) cdr(args);
            }

            if (bindings != null) {
                final Set<String> seen = new HashSet<>();
                for (Object binding : bindings) {
                    final String javaname = mangle(car(binding).toString(), rsfx);
                    if (seen.contains(javaname)) sb.append("        ");
                    else {
                        sb.append("        Object ");
                        seen.add(javaname);
                    }
                    sb.append(javaname).append(" = ");
                    if (caddr(binding) != null) throw new LambdaJError(true, "%s: malformed %s: illegal variable specification %s", "let*", "let*", printSEx(binding));
                    formToJava(sb, cadr(binding), env, topEnv, rsfx, false);
                    env = extenv(car(binding), rsfx, env);
                    sb.append(";\n");
                }
            }
            if (name != null) env = extenv(name, rsfx, env);
            formsToJava(sb, bodyForms, env, topEnv, rsfx, false);
            if (name != null) sb.append("        } } )");
            else              sb.append("        } )");
        }

        /** args = ([name] ((symbol form)...) forms...) */
        private void letrecToJava(WrappingWriter sb, Object args, ConsCell _env, ConsCell topEnv, int rsfx, boolean isLast) {
            ConsCell env = _env;

            final LambdaJSymbol name;
            
            sb.append(isLast ? "tailcall(" : "funcall(");
            sb.append("new MurmelFunction () {\n");

            if (car(args) instanceof LambdaJSymbol) {
                // named let*: (let* sym ((sym form)...) forms...) -> Object
                name = (LambdaJSymbol) car(args);
                args = cdr(args);
            }
            else name = null;
            
            final ConsCell params = paramList(car(args));
            if (params != null) {
                final Set<Object> seen = new HashSet<>();
                for (Object letVar : params) {
                    if (seen.contains(letVar)) throw new LambdaJError(true, "duplicate symbol %s", letVar);
                    seen.add(letVar);
                    env = extenv(letVar, rsfx, env);
                    final String letVarName = javasym(letVar, env);
                    sb.append("        private Object ").append(letVarName).append(" = null;\n");
                }
            }
            
            if ((car(args)) != null)
                for (Object letVarAndForm: (ConsCell)(car(args))) {
                    final Object letVar = car(letVarAndForm);
                    final String letVarName = javasym(letVar, env);
                    sb.append("        { ").append(letVarName).append(" = ");
                    if (caddr(letVarAndForm) != null) throw new LambdaJError(true, "%s: malformed %s: illegal variable specification %s", "letrec", "letrec", printSEx(letVarAndForm));
                    formToJava(sb, cadr(letVarAndForm), env, topEnv, rsfx, false);
                    sb.append("; }\n");
                }

            if (name != null) {
                env = extenv(name, rsfx, env);
                sb.append("        private final Object ").append(javasym(name, env)).append(" = this;\n");
            }
            sb.append("        @Override public Object apply(Object... args) {\n");
            sb.append("        Object result").append(rsfx).append(";\n");
            formsToJava(sb, (ConsCell)cdr(args), env, topEnv, rsfx, isLast);
            sb.append("        } } )");
        }



        /** from a list of bindings extract a new list of symbols: ((symbol1 form1)...) -> (symbol1...) */
        // todo vgl. LambdaJ.extractParamList()
        private static ConsCell paramList(Object bindings) {
            if (bindings == null) return null;
            ConsCell params = null; ConsCell insertPos = null;
            for (Object binding: (ConsCell)bindings) {
                if (params == null) { params = cons(null, null);          insertPos = params; }
                else                { insertPos.rplacd(cons(null, null)); insertPos = (ConsCell) insertPos.cdr(); }
                insertPos.rplaca(car(binding));
            }
            return params;
        }

        /** generate variables from arg array */
        private ConsCell params(WrappingWriter sb, Object paramList, ConsCell env, int rsfx, String expr) {
            if (paramList == null) {
                sb.append("        argCheck(\"").append(expr).append("\", 0, args").append(rsfx).append(".length);\n");
                return env;
            }

            if (symbolp(paramList)) {
                // (lambda a forms...) - style varargs
            }
            else if (!properList(paramList)) {
                sb.append("        argCheckVarargs(\"").append(expr).append("\", ").append(length(paramList)).append(", args").append(rsfx).append(".length);\n");
            }
            else sb.append("        argCheck(\"").append(expr).append("\", ").append(length(paramList)).append(", args").append(rsfx).append(".length);\n");

            final Set<Object> seen = new HashSet<>();
            int n = 0;
            for (Object params = paramList; params != null; ) {
                if (consp(params)) {
                    final Object param = car(params);
                    if (seen.contains(param)) throw new LambdaJError(true, "duplicate symbol %s", param);
                    seen.add(param);
                    env = extenv(param, rsfx, env);
                    sb.append("        final Object ").append(javasym(param, env)).append(" = args").append(rsfx).append("[").append(n++).append("];\n");
                }

                else if (symbolp(params)) {
                    if (seen.contains(params)) throw new LambdaJError(true, "duplicate symbol %s", params);
                    seen.add(params);
                    env = extenv(params, rsfx, env);
                    if (n == 0) sb.append("        final Object ").append(javasym(params, env)).append(" = args").append(rsfx).append(";\n");
                    else        sb.append("        final Object ").append(javasym(params, env)).append(" = arraySlice(args").append(rsfx).append(", ").append(n).append(");\n");
                    return env;
                }

                params = cdr(params);
            }
            return env;
        }

        private ConsCell loadFile(boolean pass1, String func, WrappingWriter sb, Object argument, ConsCell _env, ConsCell topEnv, int rsfx, boolean isLast, ArrayList<Object> bodyForms, StringBuilder globals) {
            if (!stringp(argument)) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(argument));
            final String fileName = (String) argument;
            final SymbolTable prevSymtab = st;
            final Path prevPath = ((SExpressionParser)(interpreter().symtab)).filePath;
            final Path p = interpreter().findFile(prevPath, fileName);
            try (final Reader r = Files.newBufferedReader(p)) {
                final SExpressionParser parser = new SExpressionParser(r::read) {
                    @Override
                    public LambdaJSymbol intern(LambdaJSymbol sym) {
                        return prevSymtab.intern(sym);
                    }
                };
                st = parser;
                interpreter().symtab = parser;
                for (;;) {
                    final Object form = parser.readObj(true);
                    if (form == null) break;

                    if (pass1) topEnv = toplevelFormToJava(sb, bodyForms, globals, topEnv, form);
                    else formToJava(sb, form, _env, topEnv, rsfx, isLast);
                }
                return topEnv;
            } catch (IOException e) {
                throw new LambdaJError(true, "load: error reading file '%s': ", e.getMessage());
            }
            finally {
                st = prevSymtab;
                interpreter().symtab = prevSymtab;
                ((SExpressionParser)(interpreter().symtab)).filePath = prevPath;
            }
        }

        private static boolean properList(Object params) {
            if (params == null) return true;
            if (!listp(params)) return false;
            for (;;) {
                if (params == null) return true;
                Object rest = cdr(params);
                if (!listp(cdr(params))) return false;
                params = rest;
            }
        }

//        /** generate boolean op for one or two args */
//        private void compareOp(WrappingWriter sb, String pred, Object lhs, Object rhs, ConsCell env, int rsfx) {
//            sb.append('(').append('(');
//            formToJava(sb, lhs, env, rsfx);
//            sb.append(' ').append(pred).append(' ');
//            if (rhs == null) sb.append("null"); else formToJava(sb, rhs, env, rsfx);
//            sb.append(") ").append(" ? _t : null)");
//        }
//
//        /** compare two numbers */
//        private void compareNum(WrappingWriter sb, String pred, Object args, ConsCell env, int rsfx) {
//            sb.append(pred).append('('); formToJava(sb, car(args), env, rsfx); sb.append(", "); formToJava(sb, cadr(args), env, rsfx); sb.append(')');
//        }
//
//        /** generate double operator for zero or more number args */
//        private void addDbl(WrappingWriter sb, String op, double start, Object args, ConsCell env, int rsfx) {
//            sb.append('(').append(start);
//            if (args != null) for (Object arg: (ConsCell)args) { sb.append(' ').append(op).append(' '); asDouble(sb, arg, env, rsfx); }
//            sb.append(')');
//        }
//
//        /** generate double operator for one or more number args */
//        private void subDbl(WrappingWriter sb, String op, double start, Object args, ConsCell env, int rsfx) {
//            sb.append('(');
//            if (cdr(args) == null) { sb.append(start).append(' ').append(op).append(' '); asDouble(sb, car(args), env, rsfx); }
//            else {
//                asDouble(sb, car(args), env, rsfx);
//                for (Object arg: (ConsCell)cdr(args)) { sb.append(' ').append(op).append(' '); asDouble(sb, arg, env, rsfx); }
//            }
//            sb.append(')');
//        }
//
//        /** eval form and change to double */
//        private void asDouble(WrappingWriter sb, Object form, ConsCell env, int rsfx) {
//            if (form == null) throw new LambdaJError("not a number: nil");
//            if (form instanceof Long) sb.append(form.toString()).append('.').append('0');
//            else if (form instanceof Double) sb.append(form.toString());
//            else { sb.append("dbl("); formToJava(sb, form, env, rsfx); sb.append(')'); }
//        }

        private static void quotedFormToJava(WrappingWriter sb, Object form) {
            if (form == null || "nil".equals(form.toString())) { sb.append("null"); }

            else if (symbolp(form)) { sb.append("intern(\"").append(form.toString()).append("\")"); }
            else if (atom(form))    { atomToJava(sb, form); }

            else if (consp(form)) {
                // use a builder to avoid stackoverflow at runtime on long lists. nested lists still are recursive at compiletime as well as runtime, tough
                sb.append("new LambdaJ.ListBuilder()");
                for (Object o = form; ; o = cdr(o)) {
                    if (cdr(o) != null) {
                        sb.append("\n        .append("); quotedFormToJava(sb, car(o)); sb.append(')');
                        if (!consp(cdr(o))) {
                            sb.append("\n        .appendLast("); quotedFormToJava(sb, cdr(o)); sb.append(')');
                            break;
                        }
                    }
                    else {
                        sb.append("\n        .append("); quotedFormToJava(sb, car(o)); sb.append(')');
                        break;
                    }
                }
                sb.append("\n        .first()");
            }

            else throw new LambdaJError("quote: internal error");
        }



        private static ConsCell cons(Object car, Object cdr) {
            return new ListConsCell(car, cdr);
        }
    }
}


/// ## class JavaCompilerHelper
/// class JavaCompilerHelper - a helper class that wraps the Java system compiler in tools.jar,
/// used by MurmelJavaCompiler to compile the generated Java to an in-memory class and optionally a .jar file.
class JavaCompilerHelper {
    private static final java.util.Map<String, String> ENV = Collections.singletonMap("create", "true");
    private MurmelClassLoader murmelClassLoader;

    JavaCompilerHelper(Path outPath) {
        AccessController.doPrivileged((PrivilegedAction<?>) () -> {
            murmelClassLoader = new MurmelClassLoader(outPath);
            return null;
        });
    }

    @SuppressWarnings("unchecked")
    Class<LambdaJ.MurmelProgram> javaToClass(String className, String javaSource, String jarFileName) throws Exception {
        final Class<LambdaJ.MurmelProgram> program = (Class<LambdaJ.MurmelProgram>) javaToClass(className, javaSource);
        if (jarFileName == null) {
            cleanup();
            return program;
        }

        final Manifest mf = new Manifest();
        mf.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        mf.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_TITLE, LambdaJ.ENGINE_NAME);
        mf.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_VERSION, LambdaJ.ENGINE_VERSION);
        mf.getMainAttributes().put(Attributes.Name.MAIN_CLASS, className);
        mf.getMainAttributes().put(Attributes.Name.CLASS_PATH, new java.io.File(LambdaJ.class.getProtectionDomain().getCodeSource().getLocation().getPath()).getName());

        /*
        try (final JarOutputStream jar = new JarOutputStream(new FileOutputStream(jarFileName), mf)) {
            final String[] dirs = className.split("\\.");
            final StringBuilder path = new StringBuilder();
            for (int i = 0; i < dirs.length; i++) {
                path.append(dirs[i]);
                if (i == dirs.length - 1) {
                    final JarEntry entry = new JarEntry(path.toString() + ".class");
                    jar.putNextEntry(entry);
                    jar.write(murmelClassLoader.getBytes(className));

                    // add classes for Murmel lambdas
                    Class<?>[] nestedClasses = program.getClasses();
                    for (Class<?> clazz: nestedClasses) {
                        jar.write(murmelClassLoader.getBytes(clazz.getName()));
                    }
                }
                else {
                    path.append('/');
                    final JarEntry entry = new JarEntry(path.toString());
                    jar.putNextEntry(entry);
                }
                jar.closeEntry();
            }
        }
        */
        final Path zipPath = Paths.get(jarFileName);
        final URI uri = URI.create("jar:" + zipPath.toUri());

        Files.deleteIfExists(zipPath);

        try (final FileSystem zipfs = FileSystems.newFileSystem(uri, ENV)) {
            Files.createDirectory(zipfs.getPath("META-INF/"));

            try (final OutputStream out = Files.newOutputStream(zipfs.getPath("META-INF/MANIFEST.MF"))) {
                mf.write(out);
            }
            copyFolder(murmelClassLoader.getOutPath(), zipfs.getPath("/"));
        }
        cleanup();

        return program;
    }

    void cleanup() throws IOException {
        //System.out.println("cleanup " + murmelClassLoader.getOutPath().toString());
        try (final Stream<Path> files = Files.walk(murmelClassLoader.getOutPath())) {
            // delete directory including files and sub-folders
            files.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    //.peek(f -> System.out.println("delete " + f.toString()))
                    .forEach(File::deleteOnExit);
        }
    }

    private static void copyFolder(Path src, Path dest) throws IOException {
        try (final Stream<Path> stream = Files.walk(src)) {
            stream.forEachOrdered(sourcePath -> {
                try {
                    final Path subSource = src.relativize(sourcePath);
                    final Path dst = dest.resolve(subSource.toString());
                    //System.out.println(sourcePath.toString() + " -> " + dst.toString());
                    if (!sourcePath.equals(src)) {
                        Files.copy(sourcePath, dst);
                    }
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            });
        }
    }

    /** Compile Java sourcecode of class {@code className} to Java bytecode */
    Class<?> javaToClass(String className, String javaSource) throws Exception {
        final JavaCompiler comp = ToolProvider.getSystemJavaCompiler();
        if (comp == null) throw new LambdaJ.LambdaJError(true, "compilation of class %s failed. No compiler is provided in this environment. Perhaps you are running on a JRE rather than a JDK?", className);
        try (final StandardJavaFileManager fm = comp.getStandardFileManager(null, null, null)) {
            final List<String> options = Collections.singletonList("-g"/*, "-source", "1.8", "-target", "1.8"*/);
            fm.setLocation(StandardLocation.CLASS_OUTPUT, Collections.singletonList(murmelClassLoader.getOutPath().toFile()));
            //                                     out       diag  opt      classes
            final CompilationTask c = comp.getTask(null, fm, null, options, null, Collections.singletonList(new JavaSourceFromString(className, javaSource)));
            if (c.call()) {
                return Class.forName(className, true, murmelClassLoader);
            }
            throw new LambdaJ.LambdaJError(true, "compilation of class %s failed", className);
        }
    }
}

class JavaSourceFromString extends SimpleJavaFileObject {
    /**
     * The source code of this "file".
     */
    private final String code;

    /**
     * Constructs a new JavaSourceFromString.
     * @param name the name of the compilation unit represented by this file object
     * @param code the source code for the compilation unit represented by this file object
     */
    JavaSourceFromString(String name, String code) {
        super(java.net.URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE);
        this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return code;
    }
}

class MurmelClassLoader extends ClassLoader {
    private final Path outPath;

    MurmelClassLoader(Path outPath) { this.outPath = outPath; }

    @Override
    public Class<?> findClass(String name) throws ClassNotFoundException {
        try {
            final byte[] ba = getBytes(name);
            if (ba == null) return super.findClass(name);
            return defineClass(name, ba, 0, ba.length);
        }
        catch (IOException e) {
            throw new ClassNotFoundException(e.getMessage());
        }
    }

    Path getOutPath() { return outPath; }

    private byte[] getBytes(String name) throws IOException {
        final String path = name.replace('.', '/');
        final Path p = outPath.resolve(Paths.get(path + ".class"));
        if (!Files.isReadable(p)) return null;
        return Files.readAllBytes(p);
    }
}

class EolUtil {
    private EolUtil() {}

    /**
     * <p>From https://stackoverflow.com/questions/3776923/how-can-i-normalize-the-eol-character-in-java/27930311
     *
     * <p>Accepts a string and returns the string with all end-of-lines
     * normalized to a \n.  This means \r\n and \r will both be normalized to \n.
     * <p>
     *     Impl Notes:  Although regex would have been easier to code, this approach
     *     will be more efficient since it's purpose built for this use case.  Note we only
     *     construct a new StringBuilder and start appending to it if there are new end-of-lines
     *     to be normalized found in the string.  If there are no end-of-lines to be replaced
     *     found in the string, this will simply return the input value.
     *
     * @param inputValue input value that may or may not contain new lines
     * @return the input value that has new lines normalized
     */
    static String anyToUnixEol(String inputValue){
        if (inputValue == null) return null;
        if (inputValue.isEmpty()) return "";

        StringBuilder stringBuilder = null;
        int index = 0;
        final int len = inputValue.length();

        while (index < len) {
            if (inputValue.charAt(index) == '\r') {
                stringBuilder = new StringBuilder();
                break;
            }
            index++;
        }
        if (stringBuilder == null) return inputValue;

        // we get here if we just read a '\r'
        // build up the string builder so it contains all the prior characters
        stringBuilder.append(inputValue, 0, index);
        if ((index + 1 < len) && inputValue.charAt(index + 1) == '\n') {
            // this means we encountered a \r\n  ... move index forward one more character
            index++;
        }
        stringBuilder.append('\n');
        index++;
        while (index < len) {
            final char c = inputValue.charAt(index);
            if (c == '\r') {
                if ((index + 1 < len) && inputValue.charAt(index + 1) == '\n') {
                    // this means we encountered a \r\n  ... move index forward one more character
                    index++;
                }
                stringBuilder.append('\n');
            } else {
                stringBuilder.append(c);
            }
            index++;
        }

        return stringBuilder.toString();
    }
}

/** A wrapping {@link LambdaJ.ReadSupplier} that reads from {@code in} or System.in.
 *  When reading from System.in sun.stdout.encoding will be used.
 *  Various lineendings will all be translated to '\n'.
 *  Optionally echoes input to System.out, various lineendings will be echoed as the system default line separator. */
class AnyToUnixEol implements LambdaJ.ReadSupplier {
    private static final Charset consoleCharset;

    static {
        final String consoleCharsetName = System.getProperty("sun.stdout.encoding");
        consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);
    }

    private final LambdaJ.ReadSupplier in;
    private int prev = -1;

    AnyToUnixEol() { this(null); }
    AnyToUnixEol(LambdaJ.ReadSupplier in) { this.in = in == null ? new InputStreamReader(System.in, consoleCharset)::read : in; }

    @Override
    public int read() throws IOException { return read(false); }

    public int read(boolean echo) throws IOException {
        final int c = in.read();
        if (c == '\r') {
            prev = c;
            if (echo) System.out.print(System.lineSeparator());
            return '\n';
        }
        if (c == '\n' && prev == '\r') {
            prev = c;
            return read(echo);
        }
        if (c == '\n') {
            prev = c;
            if (echo) System.out.print(System.lineSeparator());
            return '\n';
        }
        prev = c;
        if (echo && c != -1) System.out.print((char)c);
        return c;
    }
}

/** A wrapping {@link LambdaJ.WriteConsumer} that translates '\n' to the given line separator {@code eol}. */
class UnixToAnyEol implements LambdaJ.WriteConsumer {
    final LambdaJ.WriteConsumer wrapped;
    final String eol;

    UnixToAnyEol(LambdaJ.WriteConsumer wrapped, String eol) {
        this.wrapped = wrapped;
        this.eol = eol;
    }

    @Override
    public void print(String s) {
        if (s == null
            || s.isEmpty()
            || (s.charAt(0) != '\n' && s.charAt(s.length() - 1) != '\n' && s.indexOf('\n') == -1)) {
            // fast path for null, empty string or strings w/o '\n'
            // the check for '\n' also has a fast path for strings beginning or ending with '\n'
            wrapped.print(s); return;
        }
        final int len = s.length();
        for (int index = 0; index < len; index++) {
            final char c = s.charAt(index);
            if (c == '\n') wrapped.print(eol);
            else wrapped.print(String.valueOf(c));
        }
    }
}

/** Wrap a java.io.Writer, methods throw unchecked LambdaJError, also add {@code append()} methods for basic data types. */
class WrappingWriter extends Writer {
    private final Writer wrapped;

    WrappingWriter(Writer w) { wrapped = w; }

    @Override public WrappingWriter append(CharSequence c) { final String s = String.valueOf(c); write(s, 0, s.length()); return this; }
    @Override public WrappingWriter append(char c)         { final String s = String.valueOf(c); write(s, 0, s.length()); return this; }
    public           WrappingWriter append(int n)          { final String s = String.valueOf(n); write(s, 0, s.length()); return this; }
    public           WrappingWriter append(long l)         { final String s = String.valueOf(l); write(s, 0, s.length()); return this; }
    public           WrappingWriter append(double d)       { final String s = String.valueOf(d); write(s, 0, s.length()); return this; }
    public           WrappingWriter append(Object o)       { final String s = String.valueOf(o); write(s, 0, s.length()); return this; }

    @Override
    public void write(String s, int off, int len) {
        try { wrapped.write(s, off, len); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override
    public void write(char[] cbuf, int off, int len) {
        try { wrapped.write(cbuf, off, len); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override
    public void flush() {
        try { wrapped.flush(); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override
    public void close() {
        try { wrapped.close(); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }
}

/** A frame (window) with methods to draw lines and print text. */
class TurtleFrame {
    private static final Color[] colors = {
        Color.white,        //  0
        Color.black,        //  1
        Color.red,          //  2
        Color.green,        //  3
        Color.blue,         //  4
        Color.pink,         //  5
        Color.orange,       //  6
        Color.yellow,       //  7
        Color.magenta,      //  8
        Color.cyan,         //  9
        Color.darkGray,     // 10
        Color.gray,         // 11
        Color.lightGray,    // 12

        Color.red.darker(),
        Color.green.darker(),
        Color.blue.darker(),
    };

    private static class Text {
        private final double x, y;
        private final String s;
        Text(double x, double y, String s) { this.x = x; this.y = y; this.s = s; }
    }
    private static class Pos {
        private final double x, y, angle;
        private Pos(TurtleFrame f) {
            x = f.x;
            y = f.y;
            angle = f.angle;
        }
    }

    private final int padding;

    private int bgColor /*= 0*/;
    private int color = 1;
    private final List<Object> lines = new ArrayList<>();
    private final List<Text> texts = new ArrayList<>();
    private final Deque<Pos> posStack = new ArrayDeque<>();

    private double x, y, angle;
    private boolean draw;

    private final Object minMaxLock = new Object();
    private double xmin, ymin, xmax, ymax;
    private double dirtyxl, dirtyyl, dirtyxr, dirtyyu;

    private boolean open;
    private final Frame f;
    private final LineComponent component;

    TurtleFrame(String title, Number width, Number height, Number padding) {
        f = new Frame(title);
        f.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                close();
            }
        });

        final int w, h;
        if (width != null && width.intValue() > 0) w = width.intValue();
        else w = Toolkit.getDefaultToolkit().getScreenSize().width / 2;
        if (height != null && height.intValue() > 0) h = height.intValue();
        else h = Toolkit.getDefaultToolkit().getScreenSize().height / 2;

        if (padding != null && padding.intValue() >= 0) this.padding = padding.intValue();
        else this.padding = 40;

        component = new LineComponent(w, h);
        f.add(component, BorderLayout.CENTER);

        draw = true;
    }

    @Override
    public String toString() { return "#<frame \"" + f.getTitle() + "\">"; }

    TurtleFrame open() {
        if (open) { repaint(); return this; }
        f.pack();
        f.setVisible(true);
        open = true;
        return this;
    }

    TurtleFrame close() {
        if (!open) return this;
        f.dispose();
        open = false;
        return this;
    }

    TurtleFrame reset() {
        draw = true;
        x = y = angle = 0.0;
        bgColor = 0; color = 1;
        return this;
    }

    TurtleFrame clear() {
        reset();
        synchronized (minMaxLock) { xmin = xmax = ymin = ymax = 0.0; }
        allclean();
        synchronized (lines) {
            lines.clear();
            texts.clear();
            posStack.clear();
            if (bitmap != null) {
                final Graphics2D g = bitmap.createGraphics();
                g.setBackground(new Color(0, 0, 0, 0));
                g.clearRect(0, 0, bitmap.getWidth(), bitmap.getHeight());
                g.dispose();
            }
        }
        return repaint();
    }

    TurtleFrame repaint() {
        if (open) {
            alldirty(); f.repaint(); allclean();
        }
        return this;
    }

    TurtleFrame flush() {
        if (open) {
            final int w = component.getWidth();
            final int h = component.getHeight();

            final int x, width, y, height;
            synchronized (minMaxLock) {
                if (dirtyxl == xmin || dirtyxr == xmax || dirtyyl == ymin || dirtyyu == ymax) {
                    x = 0;  width = w;  y = 0;  height = h;
                }
                else {
                    final double fac, xoff, yoff;
                    fac = fact(w, h);

                    xoff = 0 - xmin + (w / fac - (xmax - xmin)) / 2.0;
                    yoff = 0 - ymin + (h / fac - (ymax - ymin)) / 2.0;

                    x = trX(fac, xoff, dirtyxl);
                    width = trX(fac, xoff, dirtyxr) - x + 1;

                    y = h - trY(fac, yoff, dirtyyu);
                    height = h - trY(fac, yoff, dirtyyl) - y + 1;
                }
            }

            //System.out.println("calling repaint x=" + x + ", y=" + y + ", w=" + width + ", h=" + height);
            component.repaint(0, x, y, width, height);

            allclean();
        }
        return this;
    }



    TurtleFrame pushPos() {
        posStack.addLast(new Pos(this));
        return this;
    }

    TurtleFrame popPos() {
        final Pos next = posStack.removeLast();
        x = next.x;
        y = next.y;
        angle = next.angle;
        return this;
    }



    TurtleFrame color(int newColor) {
        validateColor(newColor);
        if (newColor == color) return this;
        color = newColor;
        synchronized (lines) { lines.add(colors[newColor]); }
        return this;
    }

    TurtleFrame bgColor(int newColor) { validateColor(newColor);  bgColor = newColor; return this; }

    private static void validateColor(int color) {
        if (color >= 0 && color < colors.length) return;
        throw new IllegalArgumentException("Invalid color " + color + ", valid range: 0.." + (colors.length - 1));
    }

    TurtleFrame moveTo(double newx, double newy) {
        if (x == newx && y == newy) return this;

        synchronized (minMaxLock) {
            if (x != newx) {
                if (newx < xmin) { xmin = newx; alldirty(); }
                if (newx > xmax) { xmax = newx; alldirty(); }
                x = newx;
            }
            if (y != newy) {
                if (newy < ymin) { ymin = newy; alldirty(); }
                if (newy > ymax) { ymax = newy; alldirty(); }
                y = newy;
            }
            calcdirty();
        }
        return this;
    }

    TurtleFrame lineTo(double newx, double newy) {
        if (x != newx || y != newy) {
            synchronized (lines) { lines.add(new Line2D.Double(x, y, newx, newy)); }
            moveTo(newx, newy);
        }
        return this;
    }

    TurtleFrame moveRel(double dx, double dy) { return moveTo(x + dx, y + dy); }
    TurtleFrame lineRel(double dx, double dy) { return lineTo(x + dx, y + dy); }
    TurtleFrame text(String s) { synchronized (lines) { texts.add(new Text(x, y, s)); return this; } }
    TurtleFrame penUp() { draw = false; return this; }
    TurtleFrame penDown() { draw = true; return this; }
    TurtleFrame left(double angleDiff) { angle += angleDiff; return this; }
    TurtleFrame right(double angleDiff) { angle -= angleDiff; return this; }

    TurtleFrame forward(double length) {
        if (length != 0.0) {
            final double newx = x + Math.cos(Math.toRadians(angle)) * length;
            final double newy = y + Math.sin(Math.toRadians(angle)) * length;
            if (draw) lineTo(newx, newy); else moveTo(newx, newy);
        }
        return this;
    }



    private void allclean() { dirtyxl = dirtyxr = x; dirtyyl = dirtyyu = y; }
    private void alldirty() { dirtyxl = xmin; dirtyxr = xmax; dirtyyl = ymin; dirtyyu = ymax; }
    private void calcdirty() {
        if (x < dirtyxl) dirtyxl = x;
        else if (x > dirtyxr) dirtyxr = x;
        if (y < dirtyyl) dirtyyl = y;
        else if (y > dirtyyu) dirtyyu = y;
    }



    private double fact(final int w, final int h) {
        final double xfac = (w-2*padding) / (xmax - xmin);
        final double yfac = (h-2*padding) / (ymax - ymin);

        return xfac < yfac ? xfac : yfac;
    }

    private static int trX(double fac, double xoff, double x) {
        return (int)((x + xoff) * fac);
    }

    private static int trY(double fac, double yoff, double y) {
        return (int)((y + yoff) * fac);
    }

    private double factBitmap(final int w, final int h) {
        final double xfac = ((double)w-2*padding) / bitmap.getWidth();
        final double yfac = ((double)h-2*padding) / bitmap.getHeight();

        return xfac < yfac ? xfac : yfac;
    }



    private BufferedImage bitmap;

    TurtleFrame makeBitmap(int width, int height) {
        bitmap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        return this;
    }

    TurtleFrame discardBitmap() {
        bitmap = null;
        return this;
    }

    TurtleFrame setRGB(int x, int y, int rgb) {
        bitmap.setRGB(x, bitmap.getHeight() - y - 1, rgb);
        return this;
    }



    private class LineComponent extends Component {
        private static final long serialVersionUID = 1L;

        LineComponent(int width, int height) {
            setPreferredSize(new Dimension(width, height));
        }

        @Override
        public void paint(Graphics g) {
            //System.out.println("paint x=" + g.getClipBounds().x + ", y=" + g.getClipBounds().y + ", w=" + g.getClipBounds().width + ", h=" + g.getClipBounds().height);

            final int w = getWidth();
            final int h = getHeight();

            g.setColor(colors[bgColor]);
            g.fillRect(0, 0, w, h);
            if (w < 2*padding || h < 2*padding)
                return;

            if (bitmap != null) {
                final double fac = factBitmap(w, h);
                final int xoff = (int)((w - bitmap.getWidth() * fac) / 2.0);
                final int yoff = (int)((h - bitmap.getHeight() * fac) / 2.0);
                g.drawImage(bitmap, xoff, yoff, w - 2*xoff, h - 2*yoff, null);
            }

            if (lines.isEmpty() && texts.isEmpty()) return;

            final double fac, xoff, yoff;
            synchronized (minMaxLock) {
                fac = fact(w, h);

                xoff = 0 - xmin + (w / fac - (xmax - xmin)) / 2.0;
                yoff = 0 - ymin + (h / fac - (ymax - ymin)) / 2.0;
            }

            synchronized (lines) {
                g.setColor(Color.black);
                for (Object o : lines) {
                    if (o instanceof Color) {
                        g.setColor((Color)o);
                    }
                    else /*if (o instanceof Line2D.Double)*/ {
                        final Line2D.Double line = (Line2D.Double)o;
                        //System.out.println("line x1=" + trX(fac, xoff, line.getX1()) + " y1=" + (h - trY(fac, yoff, line.getY1())) + " x2=" + trX(fac, xoff, line.getX2()) + " y2=" + (h - trY(fac, yoff, line.getY2())));

                        g.drawLine(
                            trX(fac, xoff, line.getX1()),
                            h - trY(fac, yoff, line.getY1()),
                            trX(fac, xoff, line.getX2()),
                            h - trY(fac, yoff, line.getY2())
                        );
                    }
                }

                g.setColor(Color.black);
                for (Text text: texts) {
                    g.drawString(text.s, trX(fac, xoff, text.x), h - trY(fac, yoff, text.y));
                }
                //g.drawRect(padding, padding, w - 2*padding, h - 2*padding);
            }
        }
    }
}
