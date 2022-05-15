/* LambdaJ is Copyright (C) 2020-2022 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

package io.github.jmurmel;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.io.*;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.*;
import java.math.BigDecimal;
import java.math.BigInteger;
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
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.zone.ZoneRules;
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
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
 *  <ul><li>{@code lispReader} is an {@link SExpressionReader} that reads using a {@link ReadSupplier} (which defaults to {@link System#in})
 *      <li>{@code lispPrinter} is an {@link LambdaJ.SExpressionWriter} that prints using a {@link WriteConsumer} (which defaults to {@link System#out})
 *  </ul>
 *
 *  If you want to read/ write S-expressions from streams other than {@link System#in}/ {@link System#out} then do something like<pre>
 *  intp.setReaderPrinter(intp.new SExpressionReader(() -&gt; myReader::myFunctionThatReturnsCharsAsInt), intp.getLispPrinter());</pre>
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

    /// ## Public Java constants, interfaces and an exception class to use the interpreter from Java

    public static final String LANGUAGE_VERSION = "1.2";
    public static final String ENGINE_NAME = "JMurmel: Java based implementation of Murmel";
    public static final String ENGINE_VERSION;

    static {
        String versionInfo;
        final ClassLoader cl = LambdaJ.class.getClassLoader();
        final URL url = cl.getResource("META-INF/jmurmelversioninfo.properties");
        if (url == null) versionInfo = "unknown";
        else {
            try (InputStream is = url.openStream()) {
                final Properties manifest = new Properties();
                manifest.load(is);
                versionInfo = manifest.getProperty("Engine-Version", "unknown");
            } catch (IOException e) {
                versionInfo = "error";
            }
        }
        ENGINE_VERSION = versionInfo;
    }

    /** Max length of symbols*/
    public static final int SYMBOL_MAX = 30;

    /** Max length of string literals */
    public static final int TOKEN_MAX = 2000;


    /** Main building block for Lisp-lists */
    public abstract static class ConsCell implements Iterable<Object>, Serializable {
        private static final long serialVersionUID = 1L;

        public static ConsCell cons(Object car, Object cdr) { return new ListConsCell(car, cdr); }

        public ConsCell copy() { throw new UnsupportedOperationException("copy not supported on " + getClass().getSimpleName()); }
        public Object shallowCopyCdr() { throw new UnsupportedOperationException("shallowCopyCdr not supported on " + getClass().getSimpleName()); }

        public abstract Object car();
        public ConsCell rplaca(Object car) { throw new UnsupportedOperationException("rplaca not supported on " + getClass().getSimpleName()); }

        public abstract Object cdr();
        public ConsCell rplacd(Object cdr) { throw new UnsupportedOperationException("rplacd not supported on " + getClass().getSimpleName()); }

        ConsCell closure() { return null; }
        /** return a string with "line x:y..xx:yy: " if {@code form} is an {@link SExpConsCell} that contains line info */
        String lineInfo() { return ""; }
    }

    /** A murmel symbol name */
    public static class LambdaJSymbol implements Serializable, Writeable {
        private static final long serialVersionUID = 1L;
        final String name;
        final WellknownSymbol wellknownSymbol;
        ConsCell macro;

        public LambdaJSymbol(String symbolName) {
            name = Objects.requireNonNull(symbolName, "can't use null symbolname");
            wellknownSymbol = WellknownSymbol.none;
        }

        public LambdaJSymbol(String symbolName, boolean wellknown) {
            name = Objects.requireNonNull(symbolName, "can't use null symbolname");
            wellknownSymbol = wellknown ? WellknownSymbol.of(symbolName) : WellknownSymbol.none;
            assert wellknownSymbol != null : "enum value for wellknown symbol " + symbolName + " not found";
        }

        public boolean wellknown() { return wellknownSymbol != WellknownSymbol.none; }
        public boolean specialForm() { return wellknownSymbol.kind == WellknownSymbolKind.SF; }
        public boolean primitive() { return wellknownSymbol.kind == WellknownSymbolKind.PRIM; }

        @Override
        public void printSEx(WriteConsumer out, boolean escapeAtoms) {
            final String name = this.name;
            if (!escapeAtoms) { out.print(name); return; }

            if (name.isEmpty()) { out.print("||"); return; }
            if (".".equals(name)) { out.print("|.|"); return; }
            final char firstChar = name.charAt(0);
            if (firstChar == '|' || firstChar == '"' || firstChar == '#'
                || containsSExSyntaxOrWhiteSpace(name)
                || isDouble(name) || isLong(name) || isCLDecimalLong(name)) {
                out.print("|"); out.print(escapeSymbol(this)); out.print("|"); return;
            }
            out.print(escapeSymbol(this));
        }

        private static boolean containsSExSyntaxOrWhiteSpace(String s) {
            for (int i = 0; i < s.length(); i++) {
                final char c;
                if (isSExSyntax(c = s.charAt(i))) return true;
                if (isWhiteSpace(c)) return true;
                if ('\\' == c) return true;
                if (!(c >= 32 && c <= 126 || Character.isAlphabetic(c))) return true;
            }
            return false;
        }

        @Override public String toString() { return name; }

        @Override public int hashCode() { return name.hashCode(); }
        @Override public boolean equals(Object o) { return o == this || o instanceof LambdaJSymbol && name.equals(((LambdaJSymbol)o).name); }

        private static String escapeSymbol(LambdaJSymbol s) {
            final String name = s.name;
            if (name == null) return null;
            if (name.isEmpty()) return "";

            final StringBuilder ret = new StringBuilder();
            final int len = name.length();
            for (int i = 0; i < len; i++) {
                final char c = name.charAt(i);
                switch (c) {
                    case '|':  ret.append("\\|"); break;
                    case '\\': ret.append("\\\\"); break; 
                    default: ret.append(c);
                }
            }
            return ret.toString();
        }
    }

    public interface SymbolTable {
        LambdaJSymbol intern(LambdaJSymbol symbol);
        default LambdaJSymbol intern(String symbolName) { return intern(new LambdaJSymbol(symbolName)); }
    }

    @FunctionalInterface public interface ReadSupplier { int read() throws IOException; }
    @FunctionalInterface public interface WriteConsumer { void print(String s); }
    @FunctionalInterface public interface TraceConsumer { void println(String msg); }

    @FunctionalInterface public interface ObjectReader {
        Object readObj(Object eof);
        
        /** if {@code recordPos == true} then it would be desirable to reconrd file/line positions inside the objects */
        default Object readObj(boolean recordPos, Object eof) { return readObj(eof); }
        default void setInput(ReadSupplier input, Path filePath) { throw new UnsupportedOperationException("this ObjectReader does not support changing input"); }
        default Path getFilePath() { return null; }
    }

    public interface ObjectWriter {
        void printObj(Object o, boolean printEscape);
        default void printObj(Object o) { printObj(o, true); }
        default void printString(String s) { printObj(s, false); }
        void printEol();
    }

    /** if an atom implements this interface then {@link #printSEx(WriteConsumer, boolean)} will be used by the Murmel primitive {@code write} */
    @FunctionalInterface public interface Writeable {
        /** will be used by the Murmel primitive {@code write} */
        void printSEx(WriteConsumer out, boolean escapeAtoms);
    }

    @FunctionalInterface public interface Primitive extends Writeable { 
        Object applyPrimitive(ConsCell x);
        default Object applyPrimitiveVarargs(Object... args) { return applyPrimitive(arraySlice(args, 0)); }
        @Override default void printSEx(WriteConsumer out, boolean ignored) { out.print("#<primitive>"); }
    }

    @FunctionalInterface public interface CustomEnvironmentSupplier {
        ConsCell customEnvironment(SymbolTable symtab);
    }

    public static class LambdaJError extends RuntimeException {
        public static final long serialVersionUID = 1L;

        public LambdaJError(String msg)                                    { super(msg, null, false, false); }
        public LambdaJError(boolean format,  String msg, Object... params) { super((format ? String.format(msg, params) : msg) + getErrorExp(params), null, false, false); }
        public LambdaJError(Throwable cause, String msg, Object... params) { super(String.format(msg, params) + getErrorExp(params), cause); }

        @Override public String toString() { return "Error: " + getMessage(); }

        private static String getErrorExp(Object[] params) {
            final Object exp;
            if (params != null && params.length > 0 && (exp = params[params.length-1]) instanceof ConsCell)
                return System.lineSeparator() + "error occurred in " + ((ConsCell) exp).lineInfo() + printSEx(exp);
            return "";
        }
    }


    /// ## Data types used by interpreter program as well as interpreted programs

    private abstract static class AbstractListBuilder<T extends AbstractListBuilder<T>> {
        private Object first;
        private Object last;

        /** add an item at the end of the list */
        @SuppressWarnings("unchecked")
        public T append(Object elem) {
            final ConsCell newCell = cons(elem, null);
            if (first == null) {
                last = first = newCell;
            }
            else if (last instanceof ConsCell) {
                ((ConsCell) last).rplacd(newCell);
                last = newCell;
            }
            else throw new LambdaJ.LambdaJError("can't append another element to dotted list");
            return (T)this;
        }

        @SuppressWarnings("unchecked")
        public T appendElements(Object... elems) {
            final int length = elems.length;
            for (int i = 0; i < length; i++) append(elems[i]);
            return (T)this;
        }

        /** add an item at the end of the list to create a dotted list.
         *  Once {@code #appendLast(Object)} has been invoked subsequent invocations
         *  of {@code #appendLast(Object)} and/ or {@link #append(Object)} will result in an error as dotted lists can't be appended to. */
        @SuppressWarnings("unchecked")
        public T appendLast(Object lastElem) {
            if (first == null) {
                last = first = lastElem;
            }
            else if (last instanceof ConsCell) {
                ((ConsCell) last).rplacd(lastElem);
                last = lastElem;
            }
            else throw new LambdaJ.LambdaJError("can't append another last element to dotted list");
            return (T)this;
        }

        /** return the constructed list so far */
        public Object first() { return first; }

        abstract ConsCell cons(Object car, Object cdr);
    }

    /** Builder class for constructing lists, also used by compiled Murmel */
    public static final class ListBuilder extends AbstractListBuilder<ListBuilder> {
        @Override ConsCell cons(Object car, Object cdr) { return new ListConsCell(car, cdr); }

        public static ConsCell list(Object... elems) {
            if (elems == null || elems.length == 0) return null;
            if (elems.length == 1) return new ListConsCell(elems[0], null);
            return (ConsCell)new ListBuilder().appendElements(elems).first();
        }
    }

    private final class CountingListBuilder extends AbstractListBuilder<CountingListBuilder> {
        @Override ConsCell cons(Object car, Object cdr) { return LambdaJ.this.cons(car, cdr); }
    }

    private abstract static class AbstractConsCell extends ConsCell {
        private static class ListConsCellIterator implements Iterator<Object> {
            private final AbstractConsCell coll;
            private Iterator<Object> delegate;
            private Object cursor;

            private ListConsCellIterator(AbstractConsCell coll) { this.coll = coll; cursor = coll; }
            @Override public boolean hasNext() {
                if (delegate != null) return delegate.hasNext();
                return cursor != null;
            }

            @Override
            public Object next() {
                if (delegate != null) return delegate.next();
                final Object _cursor;
                if ((_cursor = cursor) == null) throw new NoSuchElementException();
                if (_cursor instanceof ArraySlice) {
                    // a ListConsCell based list can contain an ArraySlice as the last cdr
                    // (i.e. a list starts as conses and is continued by an ArraySlice.
                    // An ArraySlice can not be continued by conses
                    delegate = ((ArraySlice)_cursor).iterator();
                    return delegate.next();
                }
                if (_cursor instanceof AbstractConsCell) {
                    final AbstractConsCell list = (AbstractConsCell)_cursor;
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

        private AbstractConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
        @Override public String toString() { return printSEx(this, false); }
        @Override public Iterator<Object> iterator() { return new ListConsCellIterator(this); }

        @Override public Object shallowCopyCdr() { if (consp(cdr)) cdr = ((ConsCell)cdr).copy(); return cdr; }

        @Override public Object car() { return car; }
        @Override public ConsCell rplaca(Object car) { this.car = car; return this; }

        @Override public Object cdr() { return cdr; }
        @Override public ConsCell rplacd(Object cdr) { this.cdr = cdr; return this; }
        
        void adjustEnd(int endLineNo, int endCharNo) {}
    }

    private static final class ListConsCell extends AbstractConsCell {
        private static final long serialVersionUID = 1L;
        private ListConsCell(Object car, Object cdr) { super(car, cdr); }

        @Override public ConsCell copy() { return cons(car(), cdr()); }
    }

    private static final class SExpConsCell extends AbstractConsCell {
        private static final long serialVersionUID = 1L;
        private final transient Path path;
        private final int startLineNo, startCharNo;
        private int lineNo, charNo;
        
        @Override public SExpConsCell copy() { return new SExpConsCell(path, startLineNo, startCharNo, lineNo, charNo, car(), cdr()); }

        private SExpConsCell(Path path, int startLine, int startChar, int line, int charNo, Object car, Object cdr)    {
            super(car, cdr);
            this.path = path; this.startLineNo = startLine; this.startCharNo = startChar; this.lineNo = line; this.charNo = charNo;
        }

        @Override void adjustEnd(int lineNo, int charNo) { this.lineNo = lineNo; this.charNo = charNo; }
        String lineInfo() { return (path == null ? "line " : path.toString() + ':') + startLineNo + ':' + startCharNo + ".." + lineNo + ':' + charNo + ':' + ' '; }
    }

    private static final class ClosureConsCell extends AbstractConsCell {
        private static final long serialVersionUID = 1L;
        private final ConsCell closure; // only used for Lambdas with lexical environments. doesn't waste space because Java object sizes are multiples of 8 and this uses an otherwise unused slot
        private ClosureConsCell(Object car, Object cdr, ConsCell closure)    { super(car, cdr); this.closure = closure; }

        @Override ConsCell closure() { return closure; }
    }

    private static class ArraySlice extends ConsCell {
        private static class ArraySliceIterator implements Iterator<Object> {
            private final Object[] arry;
            private final int len;
            private int cursor;

            private ArraySliceIterator(ArraySlice coll) { this.arry = coll.arry; this.len = arry.length; this.cursor = coll.offset; }
            @Override public boolean hasNext() { return cursor != -1; }

            @Override
            public Object next() {
                if (cursor == -1 || arry == null) throw new NoSuchElementException();
                final Object ret = arry[cursor++];
                if (cursor == len)  cursor = -1;
                return ret;
            }
        }

        private static final long serialVersionUID = 1L;

        private final Object[] arry;
        private final int offset;

        /** {@link #arraySlice} should be preferred because it will return {@code null} instead of an "null" ArraySlice */
        private ArraySlice(Object[] arry) {
            if (arry != null && arry.length > 1) { this.arry = arry; offset = 1; }
            else { this.arry = null; offset = -1; }
        }

        /** {@link #arraySlice} should be preferred because it will return {@code null} instead of an "null" ArraySlice */
        private ArraySlice(Object[] arry, int offset) {
            if (arry != null && arry.length > offset) { this.arry = arry; this.offset = offset; }
            else { this.arry = null; this.offset = -1; }
        }

        /** {@link #arraySlice} should be preferred because it will return {@code null} instead of an "null" ArraySlice */
        private ArraySlice(ArraySlice slice) {
            if (slice.arry != null && slice.arry.length > slice.offset) { this.arry = slice.arry; offset = slice.offset + 1; }
            else { this.arry = null; offset = -1; }
        }

        @Override public Object     car() { return isNil() ? null : arry[offset]; }
        @Override public ConsCell rplaca(Object car) { arry[offset] = car; return this; }

        @Override public ArraySlice cdr() { return arry == null || arry.length <= offset+1 ? null : new ArraySlice(this); }

        @Override public String toString() { return printSEx(true, false); }
        @Override public Iterator<Object> iterator() { return new ArraySliceIterator(this); }

        String printSEx(boolean headOfList, boolean escapeAtoms) {
            final Object[] arry;
            final int alen, offset;
            if ((arry=this.arry) == null || (alen = arry.length) <= (offset=this.offset)) return LambdaJ.printSEx(null);
            else {
                final StringBuilder ret = new StringBuilder();
                final WriteConsumer append = ret::append;
                if (headOfList) ret.append('(');
                boolean first = true;
                for (int i = offset; i < alen; i++) {
                    if (first) first = false;
                    else ret.append(' ');

                    final Object obj;
                    if ((obj=arry[i]) == this) ret.append("#<this list>");
                    else _printSEx(append, arry, obj, true, escapeAtoms);
                }
                ret.append(')');
                return ret.toString();
            }
        }

        private boolean isNil() { return arry == null || arry.length <= offset; }

        Object[] listToArray() {
            if (offset == 0) return arry;
            if (offset >= arry.length) return EMPTY_ARRAY;
            return Arrays.copyOfRange(arry, offset, arry.length);
        }
    }



    /// ## Infrastructure
    private static final int EOF = -1;
    public static final Object[] EMPTY_ARRAY = new Object[0];
    private static final Class<?>[] EMPTY_CLASS_ARRAY = new Class[0];

    private static final String[] FEATURES = {
            "murmel", "murmel-" + LANGUAGE_VERSION, "jvm", "ieee-floating-point"
    };

    final ConsCell featuresEnvEntry;

    static final String[] CTRL = {
            "Nul", "Soh", "Stx", "Etx", "Eot", "Enq", "Ack", "Bel", "Backspace", "Tab", "Newline",
            "Vt", "Page", "Return", "So", "Si", "Dle", "Dc1", "Dc2", "Dc3", "Dc4",
            "Nak", "Syn", "Etb", "Can", "Em", "Sub", "Esc", "Fs", "Gs", "Rs",
            "Us"
    };

    /** installation directory */
    static final Path murmelDir;
    static {
        Path path;
        try {
            final Path p = Paths.get(LambdaJ.class.getProtectionDomain().getCodeSource().getLocation().toURI());
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
        }
        catch (URISyntaxException e) {
            System.out.println("cannot get Murmel dir: " + e.getMessage());
            path = Paths.get(".");
        }
        murmelDir = path;
    }

    /** additional directory for load and require, default is installation directory, see {@link #murmelDir} */
    private final Path libDir;

    public enum TraceLevel {
        TRC_NONE, TRC_STATS, TRC_ENVSTATS, TRC_EVAL, TRC_FUNC, TRC_ENV, TRC_PARSE, TRC_TOK, TRC_LEX;
        public boolean ge(TraceLevel l) { return ordinal() >= l.ordinal(); }
    }
    private final TraceLevel trace;
    private final boolean traceOn;
    private final boolean traceFunc;

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

        HAVE_FFI,            // jmethod and jproxy
        
        HAVE_NUMBERS,        // numbers, +-<>..., numberp, without it the remaining datatypes are symbols and cons-cells (lists)

        HAVE_DOUBLE,         // turns on Double support in the reader, you'll want NUMBERS as well
        HAVE_LONG,           // turns on Long support in the reader, you'll want NUMBERS as well
        HAVE_STRING,         // turns on String support in the reader and string literals and string related functions in the interpreter

        HAVE_IO,             // read/ write, without it only the result will be printed
        HAVE_GUI,            // turtle and bitmap graphics
        HAVE_UTIL,           // consp, symbolp, listp, null, assoc

        HAVE_LEXC,           // use lexical environments with dynamic global environment

        /** untyped lambda calculus with dynamic environments, S-expressions, that's all */
        HAVE_LAMBDA     { @Override public int bits() { return 0; } },
        HAVE_LAMBDAPLUS { @Override public int bits() { return HAVE_LAMBDA.bits() | HAVE_QUOTE.bits() | HAVE_ATOM.bits() | HAVE_EQ.bits(); } },
        HAVE_MIN        { @Override public int bits() { return HAVE_LAMBDAPLUS.bits() | HAVE_CONS.bits() | HAVE_COND.bits(); } },
        HAVE_MINPLUS    { @Override public int bits() { return HAVE_MIN.bits() | HAVE_APPLY.bits() | HAVE_LABELS.bits() | HAVE_NIL.bits() | HAVE_T.bits(); } },
        HAVE_ALL_DYN    { @Override public int bits() { return HAVE_MINPLUS.bits() | HAVE_XTRA.bits() | HAVE_FFI.bits()
                                                               | HAVE_NUMBERS.bits()| HAVE_DOUBLE.bits() | HAVE_LONG.bits()
                                                               | HAVE_STRING.bits() | HAVE_IO.bits() | HAVE_GUI.bits() | HAVE_UTIL.bits(); } },
        HAVE_ALL_LEXC   { @Override public int bits() { return HAVE_ALL_DYN.bits() | HAVE_LEXC.bits(); } }
        ;

        public int bits() { return 1 << ordinal(); }
    }

    private final int features;

    private boolean haveLabels()  { return (features & Features.HAVE_LABELS.bits())  != 0; }
    private boolean haveNil()     { return (features & Features.HAVE_NIL.bits())     != 0; }
    private boolean haveT()       { return (features & Features.HAVE_T.bits())       != 0; }
    private boolean haveXtra()    { return (features & Features.HAVE_XTRA.bits())    != 0; }
    private boolean haveFFI()     { return (features & Features.HAVE_FFI.bits())     != 0; }
    private boolean haveNumbers() { return (features & Features.HAVE_NUMBERS.bits()) != 0; }
    private boolean haveString()  { return (features & Features.HAVE_STRING.bits())  != 0; }
    private boolean haveIO()      { return (features & Features.HAVE_IO.bits())      != 0; }
    private boolean haveGui()     { return (features & Features.HAVE_GUI.bits())     != 0; }
    private boolean haveUtil()    { return (features & Features.HAVE_UTIL.bits())    != 0; }
    private boolean haveApply()   { return (features & Features.HAVE_APPLY.bits())   != 0; }
    private boolean haveCons()    { return (features & Features.HAVE_CONS.bits())    != 0; }
    private boolean haveCond()    { return (features & Features.HAVE_COND.bits())    != 0; }
    private boolean haveAtom()    { return (features & Features.HAVE_ATOM.bits())    != 0; }
    private boolean haveEq()      { return (features & Features.HAVE_EQ.bits())      != 0; }
    private boolean haveQuote()   { return (features & Features.HAVE_QUOTE.bits())   != 0; }
    private boolean haveLexC()    { return (features & Features.HAVE_LEXC.bits())    != 0; }

    /** constructor with all features, no tracing */
    public LambdaJ() {
        this(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null);
    }

    public LambdaJ(SymbolTable symtab) {
        this(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null, symtab, null);
    }

    /** constructor */
    public LambdaJ(int features, TraceLevel trace, TraceConsumer tracer) {
        this(features, trace, tracer, null, null);
    }

    /** constructor */
    public LambdaJ(int features, TraceLevel trace, TraceConsumer tracer, SymbolTable symtab, Path libDir) {
        this.features = features;

        this.trace = trace;
        traceOn = trace != TraceLevel.TRC_NONE;
        traceFunc = trace.ge(TraceLevel.TRC_FUNC);

        this.tracer = tracer != null ? tracer : System.err::println;
        this.symtab = symtab == null ? new ListSymbolTable() : symtab;
        if (libDir != null) this.libDir = libDir;
        else this.libDir = murmelDir;
        if (features != Features.HAVE_ALL_LEXC.bits()) speed = 0;

        this.featuresEnvEntry = cons(intern("*features*"), makeFeatureList());

        /* Look up the symbols for special forms only once. Also start to build the table of reserved words. */
        sT =                           haveT() ? internWellknown("t") : intern("t");
        sNil =                         haveNil() ? internWellknown("nil") : intern("nil");
        sLambda =                      internWellknown("lambda");

        if (haveQuote())  { sQuote   = internWellknown("quote"); }   else sQuote = null;
        if (haveCond())   { sCond    = internWellknown("cond"); }    else sCond = null;
        if (haveLabels()) { sLabels  = internWellknown("labels"); }  else sLabels = null;

        if (haveXtra())   {
            sDynamic = internWellknown("dynamic");

            sIf      = internWellknown("if");
            sDefine  = internWellknown("define");
            sDefun   = internWellknown("defun");
            sDefmacro= internWellknown("defmacro");
            sLet     = internWellknown("let");
            sLetStar = internWellknown("let*");
            sLetrec  = internWellknown("letrec");

            sMultipleValueBind = internWellknown("multiple-value-bind");
            sMultipleValueCall = internWellknown("multiple-value-call");
            sSetQ    = internWellknown("setq");

            sProgn   = internWellknown("progn");

            sLoad    = internWellknown("load");
            sRequire = internWellknown("require");
            sProvide = internWellknown("provide");

            sDeclaim = internWellknown("declaim");
        }
        else sDynamic = sIf = sDefine = sDefun = sDefmacro = sLet = sLetStar = sLetrec = sMultipleValueBind = sMultipleValueCall = sSetQ = sProgn = sLoad = sRequire = sProvide
             = sDeclaim = null;

        if (haveUtil()) {
            sNull =    internWellknown("null");
            sList =    internWellknown("list");
            sListStar= internWellknown("list*");
            sAppend =  internWellknown("append");
            sEql =     internWellknown("eql");
        }
        else sNull = sList = sListStar = sAppend = sEql = null;

        if (haveNumbers()) {
            sInc =     internWellknown("1+");
            sDec =     internWellknown("1-");

            sMod =     internWellknown("mod");
            sRem =     internWellknown("rem");

            sNeq =     internWellknown("=");
            sLt  =     internWellknown("<");
            sLe  =     internWellknown("<=");
            sGe  =     internWellknown(">=");
            sGt  =     internWellknown(">");
            sNe  =     internWellknown("/=");

            sAdd =     internWellknown("+");
            sMul =     internWellknown("*");
            sSub =     internWellknown("-");
            sDiv =     internWellknown("/");
        }
        else sInc = sDec = sMod = sRem = sNeq = sLt = sLe = sGe = sGt = sNe = sAdd = sMul = sSub = sDiv = null;

        if (haveEq()) sEq = internWellknown("eq"); else sEq = null;

        if (haveCons()) {
            sCar =     internWellknown("car");
            sCdr =     internWellknown("cdr");
            sCons =    internWellknown("cons");
        }
        else sCar = sCdr = sCons = null;

        // Lookup only once on first use. The supplier below will do a lookup on first use and then replace itself
        // by another supplier that simply returns the cached value.
        expTrue = () -> { final Object s = makeExpTrue(); expTrue = () -> s; return s; };
    }

    private ConsCell makeFeatureList() {
        ConsCell l = null;
        for (String feat: FEATURES) {
            l = new ListConsCell(intern(feat), l);
        }
        return l;
    }


    /// ## Printer

    /** create an ObjectWriter that transforms \n to the platform default line separator */
    public static ObjectWriter makeWriter(WriteConsumer out) {
        return makeWriter(out, System.lineSeparator());
    }

    /** create an ObjectWriter that transforms \n to the given {@code lineSeparator} */
    public static ObjectWriter makeWriter(WriteConsumer out, String lineSeparator) {
        if ("\r\n".equals(lineSeparator)) return new SExpressionWriter(new UnixToAnyEol(out, "\r\n"));
        if ("\r"  .equals(lineSeparator)) return new SExpressionWriter(new UnixToAnyEol(out, "\r"));

        return new SExpressionWriter(out);
    }

    /** this class will write objects as S-expressions to the given {@link WriteConsumer} w/o any eol translation */
    private static class SExpressionWriter implements ObjectWriter {
        private final WriteConsumer out;

        public SExpressionWriter(WriteConsumer out) { this.out = out; }

        @Override public void printObj(Object o, boolean printEscape) { printSEx(out, o, printEscape); }
        @Override public void printEol() { out.print("\n"); }
        @Override public void printString(String s) { out.print(s); }
    }



    /// ## Scanner, symboltable and S-expression reader

    static class ListSymbolTable implements SymbolTable {
        /// A symbol table implemented with a list just because. could easily replaced by a HashMap for better performance.
        private ConsCell symbols;

        // String#equalsIgnoreCase is slow. we could String#toUpperCase all symbols then we could use String#equals
        @Override
        public LambdaJSymbol intern(LambdaJSymbol sym) {
            final String symName = sym.name;
            for (ConsCell s = symbols; s != null; s = (ConsCell)cdr(s)) {
                final LambdaJSymbol _s = (LambdaJSymbol) car(s);
                if (_s.name.equalsIgnoreCase(symName))
                    return _s;
            }

            symbols = ConsCell.cons(sym, symbols);
            return sym;
        }

        @Override
        public LambdaJSymbol intern(String symName) {
            for (ConsCell s = symbols; s != null; s = (ConsCell)cdr(s)) {
                final LambdaJSymbol _s = (LambdaJSymbol) car(s);
                if (_s.name.equalsIgnoreCase(symName))
                    return _s;
            }

            final LambdaJSymbol ret = new LambdaJSymbol(symName);
            symbols = ConsCell.cons(ret, symbols);
            return ret;
        }
    }

    public static ObjectReader makeReader(ReadSupplier in) { return new SExpressionReader(in, new ListSymbolTable(), null); }
    public static ObjectReader makeReader(ReadSupplier in, SymbolTable symtab, ConsCell featuresEnvEntry) { return new SExpressionReader(in, symtab, featuresEnvEntry); }
    final SExpressionReader makeReader(ReadSupplier in, Path path) { return new SExpressionReader(in, symtab, featuresEnvEntry, path); }

    static boolean isWhiteSpace(int x) { return x == ' ' || x == '\t' || x == '\n' || x == '\r'; }
    static boolean isSExSyntax(int x) { return x == '(' || x == ')' /*|| x == '.'*/ || x == '\'' || x == '`' || x == ','; }

    private static final Pattern LONG_PATTERN = Pattern.compile("[-+]?([0-9]|([1-9][0-9]*))");
    static boolean isLong(String s) {
        if (s == null || s.isEmpty()) return false;
        return LONG_PATTERN.matcher(s).matches();
    }

    private static final Pattern CL_DECIMAL_LONG_PATTERN = Pattern.compile("[-+]?([0-9]|([1-9][0-9]*\\.))");
    static boolean isCLDecimalLong(String s) {
        if (s == null || s.isEmpty()) return false;
        return CL_DECIMAL_LONG_PATTERN.matcher(s).matches();
    }

    private static final Pattern DOUBLE_PATTERN = Pattern.compile(
    "[-+]?"                                // optional-sign
    + "("                                  // either
    + "(([0-9]+\\.[0-9]+)"                 //   zero-or-more-digits  '.' one-or-more-digits
    + "([eE][-+]?[0-9]+)?)"                //   optional: e-or-E optional-sign one-or-more-digits
    + "|"                                  // or
    + "([0-9]+[eE][-+]?[0-9]+)"            //   one-or-more-digits e-or-E optional-sign one-or-more-digits
    + ")");
    static boolean isDouble(String s) {
        if (s == null || s.isEmpty()) return false;
        return DOUBLE_PATTERN.matcher(s).matches();
    }

    /** This class will read and parse S-Expressions (while generating symbol table entries)
     *  from the given {@link ReadSupplier} */
    static class SExpressionReader implements ObjectReader {
        private static class ParseError extends LambdaJError {
            private static final long serialVersionUID = 1L;
            private ParseError(String msg, Object... args) { super(true, msg, args); }
        }

        private final int features;
        private final TraceLevel trace;
        private final TraceConsumer tracer;

        private final SymbolTable st;
        private final ConsCell featuresEnvEntry;

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
        SExpressionReader(ReadSupplier in, SymbolTable st, ConsCell featuresEnvEntry) {
            this(in, st, featuresEnvEntry, null);
        }

        SExpressionReader(ReadSupplier in, SymbolTable st, ConsCell featuresEnvEntry, Path filePath) {
            this(Features.HAVE_ALL_DYN.bits(), TraceLevel.TRC_NONE, null, st, featuresEnvEntry, in, filePath);
        }

        /** Create an S-expression parser (==reader).
         * @param in a {@link ReadSupplier} that supplies characters,
         *            {@code InputStream::read} won't work because that supplies bytes but not (Unicode-) characters,
         *            {@code Reader::read} will work
         *
         */
        SExpressionReader(int features, TraceLevel trace, TraceConsumer tracer, SymbolTable st, ConsCell featuresEnvEntry, ReadSupplier in, Path filePath) {
            this.features = features; this.trace = trace; this.tracer = tracer;
            this.st = st;
            this.in = in;
            this.filePath = filePath;

            sNot          = intern("not");
            sAnd          = intern("and");
            sOr           = intern("or");

            sQuote          = intern("quote");
            sQuasiquote     = intern("quasiquote");
            sUnquote        = intern("unquote");
            sUnquote_splice = intern("unquote-splice");
            sAppend         = intern("append");
            sList           = intern("list");
            sListStar       = intern("list*");
            sCons           = intern("cons");
            sNil            = intern("nil");
            
            this.featuresEnvEntry = featuresEnvEntry;
        }

        // this is really only useful for the repl. If parser.charNo != 0 the next thing the parser reads is the lineseparator following the previous sexp that was not consumed.
        void resetPos() { lineNo = charNo == 0 ? 1 : 0;  charNo = 0; }

        private boolean haveDouble()  { return (features & Features.HAVE_DOUBLE.bits())  != 0; }
        private boolean haveLong()    { return (features & Features.HAVE_LONG.bits())    != 0; }
        private boolean haveString()  { return (features & Features.HAVE_STRING.bits())  != 0; }
        private boolean haveNil()     { return (features & Features.HAVE_NIL.bits())     != 0; }

        @Override public void setInput(ReadSupplier input, Path filePath) { in = input; this.filePath = filePath; lineNo = 1; charNo = 0; }
        @Override public Path getFilePath() { return filePath; }

        /// Scanner
        private boolean isSpace(int x)  { return !escape && isWhiteSpace(x); }
        private boolean isDQuote(int x) { return !escape && x == '"'; }
        private boolean isBar(int x)    { return !escape && x == '|'; }
        private boolean isHash(int x)   { return !escape && x == '#'; }

        private boolean isSyntax(int x) { return !escape && isSExSyntax(x); }

        /*java.io.PrintWriter debug;
        {
            try {
                debug = new java.io.PrintWriter(Files.newBufferedWriter(Paths.get("scanner.log")));
            } catch (IOException e) { }
        }*/

        private int prev = -1;
        private int readchar() throws IOException {
            final int c = in.read();
            //debug.println(String.format("%d:%d: char %-3d %s", lineNo, charNo, c, Character.isWhitespace(c) ? "" : String.valueOf((char)c))); debug.flush();
            if (c == '\r') {
                prev = '\r';
                lineNo++;
                charNo = 0;
                return '\n';
            }
            if (c == '\n' && prev == '\r') {
                // current char is a \n, previous char was a \r which was returned as a \n.
                // Therefore the current \n is silently dropped, return the next char.
                prev = '\n';
                return readchar();
            }
            if (c == '\n') {
                prev = '\n';
                lineNo++;
                charNo = 0;
                return '\n';
            }
            prev = c; prevLineNo = lineNo; prevCharNo = charNo;
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
                throw new ParseError("characterset conversion error in SExpressionReader: %s", e.toString());
            } catch (Exception e) {
                throw new ParseError("I/O error in SExpressionReader: %s", e.toString());
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
                        return (char) Integer.parseInt(charOrCharactername);
                    } catch (NumberFormatException e) {
                        throw new ParseError("'%s' following #\\ is not a valid number", charOrCharactername);
                    }
                }
                for (int i = 0; i < CTRL.length; i++) {
                    if (CTRL[i].equals(charOrCharactername)) return (char)i;
                }
                throw new ParseError("unrecognized character name %s", charOrCharactername);

            // #| ... multiline comment ending with |#
            // or #! ... !# to make hashbang scripts possible
            case '|':
            case '!':
                final int ln = lineNo, cn = charNo;
                while (look != EOF) {
                    // note single & to avoid short-circuiting
                    if (look == sub_char & (look = getchar(false)) == '#') {
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
                final boolean hasFeature = featurep(readObj(null));
                final Object next = readObj(null);
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
                throw new ParseError("no dispatch function defined for %s", printChar(sub_char));
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
            final String ret = tokenToString(token, 0, Math.min(index, SYMBOL_MAX));
            if (ret.isEmpty()) throw new ParseError("EOF after #%c", macroChar);
            return ret;
        }

        private final Object sNot;
        private final Object sAnd;
        private final Object sOr; 

        private boolean featurep(Object next) {
            if (next != null && symbolp(next)) return some(x -> x == next, cdr(featuresEnvEntry));
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

        enum Token {
            LP,    // (
            RP,    // )
            DOT,   // .
            SQ,    // '
            BQ,    // `
            COMMA, // ,
        }

        /** setup {@code tok} with the next token which will be:
         * <ul>
         *     <li>one of the values of {@link Token}
         *     <li>an atom
         *     <li>an interned symbol
         *     <li>{@code null} to indicate EOF
         * </ul> */
        private void readToken() {
            final int eof = LambdaJ.EOF;
            for (;;) {
                int index = 0;
                skipWs();
                tok = null;
                if (look == eof) {
                    if (trace.ge(TraceLevel.TRC_LEX)) tracer.println("*** scan  EOF");
                    return;
                }

                if (isBar(look)) {
                    look = getchar();
                    while (look != eof && !isBar(look)) {
                        if (index < SYMBOL_MAX) token[index++] = (char) look;
                        look = getchar(false);
                    }
                    if (look == eof)
                        throw new ParseError("|-quoted symbol is missing closing |");
                    look = getchar(); // consume trailing |
                    final String s = tokenToString(token, 0, Math.min(index, SYMBOL_MAX));
                    tok = intern(s);
                } else if (isSyntax(look)) {
                    switch (look) {
                        case '(':  tok = Token.LP; break;
                        case ')':  tok = Token.RP; break;
                        case '\'': tok = Token.SQ; break;
                        case '`':  tok = Token.BQ; break;
                        case ',':  tok = Token.COMMA; break;
                        default: throw new ParseError("internal error - unexpected syntax char %c", (char)look);
                    }
                    look = getchar();
                } else if (haveString() && isDQuote(look)) {
                    do {
                        if (index < TOKEN_MAX) token[index++] = (char) look;
                        look = getchar(false);
                    } while (look != eof && !isDQuote(look));
                    if (look == eof)
                        throw new ParseError("string literal is missing closing \"");
                    look = getchar(); // consume trailing "
                    tok = tokenToString(token, 1, index).intern();
                } else if (isHash(look)) {
                    look = getchar(false);
                    final int subChar;
                    if (escape) subChar = '\\';
                    else { subChar = look; look = getchar(false); }
                    tok = readerMacro(subChar);
                } else {
                    boolean escapeSeen = false;
                    while (look != eof && !isSpace(look) && !isSyntax(look)) {
                        if (escape) escapeSeen = true;
                        if (index < TOKEN_MAX) token[index++] = (char) look;
                        look = getchar();
                    }
                    String s = tokenToString(token, 0, index);
                    if (!tokEscape && ".".equals(s)) {
                        tok = Token.DOT;
                    } else if (!escapeSeen && haveDouble() && isDouble(s)) {
                        tok = parseDouble(s);
                    } else if (!escapeSeen && haveLong() && isLong(s)) {
                        tok = parseLong(s, 10);
                    } else if (!escapeSeen && haveDouble() && isLong(s)) {
                        tok = parseDouble(s);
                    } else if (!escapeSeen && (haveDouble() || haveLong()) && isCLDecimalLong(s)) {
                        // reject CL-style 123. for "123 in radix 10" - Murmel doesn't support changing reader radix,
                        // and non-lispers may think digits followed by a dot are floating point numbers (as is the case in most programming languages)
                        throw new ParseError("digits followed by '.' to indicate 'integer in radix' 10 is not supported. Digits followed by '.' without decimal numbers to indicate 'floating point' also is not supported.");
                    } else {
                        if (s.length() > SYMBOL_MAX) s = s.substring(0, SYMBOL_MAX);
                        tok = intern(s);
                    }
                }

                if (trace.ge(TraceLevel.TRC_LEX)) tracer.println("*** scan  token  |" + tok + '|');

                if (tok != CONTINUE) return;
            }
        }

        private static Number parseLong(String s, int radix) {
            try {
                return Long.valueOf(s, radix);
            } catch (NumberFormatException e) {
                throw new ParseError("'%s' is not a valid number", s);
            }
        }

        private static Number parseDouble(String s) {
            try {
                return Double.valueOf(s);
            } catch (NumberFormatException e) {
                throw new ParseError("'%s' is not a valid number", s);
            }
        }

        private static String tokenToString(char[] b, int first, int end) {
            return new String(b, first, end - first);
        }


        private LambdaJSymbol intern(String sym) {
            return st.intern(sym);
        }


        /// S-expression parser
        /** Record line and char numbers in the conses */
        @Override
        public Object readObj(boolean recordPos, Object eof) {
            this.pos = recordPos;
            final Object ret = readObj(eof);
            this.pos = false;
            return ret;
        }

        @Override
        public Object readObj(Object eof) {
            if (!init) {
                prev = -1;
                lineNo = 1; charNo = 0;
                look = getchar();
                init = true;
            }
            skipWs();
            final int startLine = lineNo, startChar = charNo;
            try {
                readToken();
                //return expand_backquote(readObject(startLine, startChar));
                return readObject(startLine, startChar, eof);
            }
            catch (ParseError pe) {
                throw errorReaderError(pe.getMessage() + posInfo());
            }
        }

        private final Object sQuote;
        private final Object sQuasiquote;
        private final Object sUnquote;
        private final Object sUnquote_splice;
        private final Object sAppend;
        private final Object sList;
        private final Object sListStar;
        private final Object sCons;
        private final Object sNil;

        private Object readObject(int startLine, int startChar, Object eof) {
            if (tok == null) {
                if (trace.ge(TraceLevel.TRC_PARSE)) tracer.println("*** parse list   ()");
                return eof;
            }
            if (tok == sNil) {
                if (trace.ge(TraceLevel.TRC_TOK)) tracer.println("*** parse symbol nil");
                if (haveNil()) return null;
                else return tok;
            }
            if (symbolp(tok)) {
                if (trace.ge(TraceLevel.TRC_TOK)) tracer.println("*** parse symbol " + tok);
                return tok;
            }
            if (!tokEscape) {
                if (tok == Token.RP) throw new ParseError("unexpected ')'");
                if (tok == Token.LP) {
                    try {
                        final Object list = readList(startLine, startChar, eof);
                        if (!tokEscape && tok == Token.DOT) {
                            skipWs();
                            final Object cdr = readList(lineNo, charNo, eof);
                            if (cdr == null) throw new ParseError("illegal end of dotted list: nothing appears after . in list");
                            if (cdr(cdr) != null) throw new ParseError("illegal end of dotted list: %s", printSEx(cdr));
                            final Object cons = combine(startLine, startChar, list, car(cdr));
                            if (trace.ge(TraceLevel.TRC_PARSE)) tracer.println("*** parse cons   " + printSEx(cons));
                            return cons;
                        }
                        if (trace.ge(TraceLevel.TRC_PARSE)) tracer.println("*** parse list   " + printSEx(list));
                        return list;
                    }
                    catch (ParseError e) {
                        errorReaderError(e.getMessage() + posInfo(startLine, startChar));
                    }
                }
                if (tok == Token.SQ) {
                    skipWs();
                    final int _startLine = lineNo, _startChar = charNo;
                    readToken();
                    return cons(startLine, startChar, sQuote, cons(startLine, startChar, readObject(_startLine, _startChar, eof), null));
                }
                if (tok == Token.BQ) {
                    skipWs();
                    final int _startLine = lineNo, _startChar = charNo;
                    readToken();
                    final Object o;
                    try {
                        backquote++;
                        final Object exp = readObject(_startLine, _startChar, eof);
                        if (backquote == 1) {
                            o = qq_expand(exp);
                            //System.out.println("bq expansion in:  (backquote " + printSEx(exp) + ')');
                            //System.out.println("bq expansion out: " + printSEx(o));
                            //System.out.println();
                        }
                        else o = cons(startLine, startChar, sQuasiquote, cons(startLine, startChar, exp, null));
                    }
                    finally { backquote--; }
                    return o;
                }
                if (tok == Token.COMMA) {
                    if (backquote == 0) errorReaderError("comma is not inside a backquote" + posInfo(startLine, startChar));
                    skipWs();
                    final boolean splice;
                    if (look == '@') { splice = true; look = getchar(); }
                    else splice = false;
                    final int _startLine = lineNo, _startChar = charNo;
                    readToken();
                    final Object o;
                    try {
                        backquote--;
                        o = cons(startLine, startChar, splice ? sUnquote_splice : sUnquote, cons(startLine, startChar, readObject(_startLine, _startChar, eof), null));
                    }
                    finally { backquote++; }
                    return o;
                }
            }
            if (trace.ge(TraceLevel.TRC_TOK)) tracer.println("*** parse value  " + tok);
            return tok;
        }

        private String posInfo(int startLine, int startChar) {
            return System.lineSeparator() + "error occurred in " + (filePath == null ? "line " : filePath.toString() + ':') + startLine + ':' + startChar + ".." + lineNo + ':' + charNo;
        }

        private String posInfo() {
            return System.lineSeparator() + "error occurred in " + (filePath == null ? "line " : filePath.toString() + ':') + lineNo + ':' + charNo;
        }

        private Object readList(int listStartLine, int listStartChar, Object eof) {
            AbstractConsCell first = null, appendTo = null;
            for (;;) {
                skipWs();
                final int carStartLine = lineNo, carStartChar = charNo;
                readToken();
                if (tok == null) throw new ParseError("cannot read list. missing ')'?");
                if (!tokEscape && (tok == Token.RP || tok == Token.DOT)) {
                    if (first != null) first.adjustEnd(prevLineNo, prevCharNo);
                    return first;
                }
                final AbstractConsCell newCell = cons(listStartLine, listStartChar);
                if (first == null) first = newCell;
                if (appendTo != null) appendTo.rplacd(newCell);
                appendTo = newCell;

                newCell.rplaca(readObject(carStartLine, carStartChar, eof));
                skipWs();
                listStartLine = lineNo; listStartChar = charNo;
            }
        }


        private ConsCell cons(int startLine, int startChar, Object car, Object cdr) {
            return pos ? new SExpConsCell(filePath, startLine, startChar, lineNo, charNo, car, cdr) : new ListConsCell(car, cdr);
        }

        private AbstractConsCell cons(int startLine, int startChar) {
            return pos ? new SExpConsCell(filePath, startLine, startChar, lineNo, charNo, null, null) : new ListConsCell(null, null);
        }

        /** Append rest at the end of first. If first is a list it will be modified. */
        private ConsCell combine(int startLine, int startChar, Object first, Object rest) {
            if (consp(first)) return nconc2((ConsCell)first, rest);
            else return cons(startLine, startChar, first, rest);
        }

        /** Append rest at the end of first, modifying first in the process.
         *  Returns a dotted list unless rest is a proper list. This works like a two arg nconc. */
        private static ConsCell nconc2(ConsCell first, Object rest) {
            for (ConsCell last = first; ; last = (ConsCell) cdr(last)) {
                if (cdr(last) == first) throw new LambdaJError(true, "%s: first argument is a circular list", "appendToList");
                if (cdr(last) == null) {
                    last.rplacd(rest);
                    return first;
                }
            }
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
            if (symbolp(x))
                return quote(x);
            if (atom(x))
                return x;

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return cadr(xCons);

            if (op == sUnquote_splice)
                throw new LambdaJError("can't splice here");

            if (op == sQuasiquote)
                return qq_expand(qq_expand(cadr(xCons)));

            if (cdr(xCons) == null) return qq_expand_list(op);

            //return list(sAppend, qq_expand_list(op), qq_expand(cdr(xCons)));
            return optimizedAppend(qq_expand_list(op), qq_expand(cdr(xCons)));
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
            if (x == null) return list(sList, sNil);
            if (symbolp(x))
                return list(sList, quote(x));
            if (atom(x))
                return list(sList, x);

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return list(sList, cadr(xCons));

            if (op == sUnquote_splice)
                return cadr(xCons);

            if (op == sQuasiquote)
                return qq_expand_list(qq_expand(cadr(xCons)));

            if (cdr(xCons) == null) return list(sList, qq_expand_list(op));

            //return list(sList, list(sAppend, qq_expand_list(op), qq_expand(cdr(xCons))));
            return list(sList, optimizedAppend(qq_expand_list(op), qq_expand(cdr(xCons))));
        }

        /** create a form that will append lhs and rhs: "(append lhs rhs)"
         * For some special case the form will be optimized:
         *
         * (append (list lhsX) (list rhsX...))  -> (list lhsX rhsX...)
         * (append (list lhsX) (list* rhsX...)) -> (list* lhsX rhsX...)
         * (append (list lhsX) (cons rhsX...))  -> (list* lhsX rhsX...)
         * (append (list lhsX) rhs)             -> (cons lhsX rhs)  
         * (append lhs (list rhsX))             -> (append lhs (cons rhsX nil))
         */
        private ConsCell optimizedAppend(Object lhs, Object rhs) {
            if (consp(lhs) && car(lhs) == sList) {
                assert cddr(lhs) == null: "expected single argument list call, got: " + lhs;

                if (consp(rhs)) {
                    final Object carRhs = car(rhs);
                    if (carRhs == sList)     return new ListConsCell(sList,     new ListConsCell(cadr(lhs), cdr(rhs)));
                    if (carRhs == sListStar
                        || carRhs == sCons)  return new ListConsCell(sListStar, new ListConsCell(cadr(lhs), cdr(rhs)));
                }

                return list(sCons, cadr(lhs), rhs);
            }

            if (consp(rhs) && car(rhs) == sList && cddr(rhs) == null)
                return list(sAppend, lhs, list(sCons, cadr(rhs), null));

            return list(sAppend, lhs, rhs);
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

    /// Murmel has a list of reserved words may not be used as a symbol: t, nil and special forms

    /** Throw error if sym is a reserved symbol */
    void notReserved(final String op, final LambdaJSymbol sym) {
        if (reserved(sym)) errorReserved(op, sym);
    }

    boolean reserved(LambdaJSymbol sym) {
        return sym == null || sym == sT || sym.specialForm();
    }


    /// Symboltable
    private final SymbolTable symtab;
    public SymbolTable getSymbolTable() { return symtab; }

    private static final Object UNASSIGNED = "#<value is not assigned>";          // only relevant in letrec
    private static final ConsCell NO_VALUES = new ListConsCell("no multiple values", null);
    private static final Object PSEUDO_SYMBOL = "non existant pseudo symbol"; // to avoid matches on pseudo env entries
    private static final Object NOT_HANDLED = "cannot opencode";

    /** well known symbols for the reserved symbols t and nil, and for the special forms */
    final LambdaJSymbol sT, sNil, sLambda, sDynamic, sQuote, sCond, sLabels, sIf, sDefine, sDefun, sDefmacro,
            sLet, sLetStar, sLetrec, sMultipleValueBind, sMultipleValueCall,
            sSetQ, sProgn, sLoad, sRequire, sProvide,
            sDeclaim;

    /** well known symbols for some primitives */
    final LambdaJSymbol sNeq, sNe, sLt, sLe, sGe, sGt, sAdd, sMul, sSub, sDiv, sMod, sRem,
            sCar, sCdr, sCons, sEq, sEql, sNull, sInc, sDec, sAppend, sList, sListStar;

    enum WellknownSymbolKind { SF, PRIM, OC_PRIM, SYMBOL}
    enum WellknownSymbol {
        none("", null),
        sT("t", WellknownSymbolKind.SYMBOL), sNil("nil", WellknownSymbolKind.SYMBOL), sLambda("lambda", WellknownSymbolKind.SF), sDynamic("dynamic", WellknownSymbolKind.SYMBOL),
        sQuote("quote", WellknownSymbolKind.SF), sCond("cond", WellknownSymbolKind.SF), sLabels("labels", WellknownSymbolKind.SF), sIf("if", WellknownSymbolKind.SF),
        sDefine("define", WellknownSymbolKind.SF), sDefun("defun", WellknownSymbolKind.SF), sDefmacro("defmacro", WellknownSymbolKind.SF),
        sLet("let", WellknownSymbolKind.SF), sLetStar("let*", WellknownSymbolKind.SF), sLetrec("letrec", WellknownSymbolKind.SF),
        sMultipleValueBind("multiple-value-bind", WellknownSymbolKind.SF), sMultipleValueCall("multiple-value-call", WellknownSymbolKind.SF),
        sSetQ("setq", WellknownSymbolKind.SF), sProgn("progn", WellknownSymbolKind.SF), sLoad("load", WellknownSymbolKind.SF), sRequire("require", WellknownSymbolKind.SF), sProvide("provide", WellknownSymbolKind.SF),
        sDeclaim("declaim", WellknownSymbolKind.SF),

        sNeq("=", WellknownSymbolKind.PRIM), sNe("/=", WellknownSymbolKind.PRIM), sLt("<", WellknownSymbolKind.PRIM), sLe("<=", WellknownSymbolKind.PRIM), sGe(">=", WellknownSymbolKind.PRIM), sGt(">", WellknownSymbolKind.PRIM), sAdd("+", WellknownSymbolKind.PRIM), sMul("*", WellknownSymbolKind.PRIM), sSub("-", WellknownSymbolKind.PRIM), sDiv("/", WellknownSymbolKind.PRIM), sMod("mod", WellknownSymbolKind.PRIM), sRem("rem", WellknownSymbolKind.PRIM),
        sCar("car", WellknownSymbolKind.PRIM), sCdr("cdr", WellknownSymbolKind.PRIM), sCons("cons", WellknownSymbolKind.PRIM), sEq("eq", WellknownSymbolKind.PRIM), sEql("eql", WellknownSymbolKind.PRIM), sNull("null", WellknownSymbolKind.PRIM), sInc("1+", WellknownSymbolKind.PRIM), sDec("1-", WellknownSymbolKind.PRIM), sAppend("append", WellknownSymbolKind.PRIM), sList("list", WellknownSymbolKind.PRIM), sListStar("list*", WellknownSymbolKind.PRIM);
    
        private final String sym;
        final WellknownSymbolKind kind;
        WellknownSymbol(String sym, WellknownSymbolKind kind) { this.sym = sym; this.kind = kind; }

        static WellknownSymbol of(String name) {
            for (WellknownSymbol s: values()) {
                if (s.sym.equalsIgnoreCase(name)) return s;
            }
            return null;
        }
    }

    private Supplier<Object> expTrue;

    private Object makeExpTrue() {
        if (haveT()) return sT; // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (haveQuote()) return cons(sQuote, cons(sT, null));
        else throw new LambdaJError("truthiness needs support for 't' or 'quote'");
    }

    final LambdaJSymbol intern(String sym) {
        return symtab.intern(sym);
    }

    final LambdaJSymbol internWellknown(String sym) {
        final LambdaJSymbol ret = symtab.intern(new LambdaJSymbol(sym, true));
        assert ret.wellknownSymbol != WellknownSymbol.none : "cannot intern wellknown symbol " + sym + ": was already interned as regular symbol";
        return ret;
    }

    private static class OpenCodedPrimitive implements Primitive {
        private final LambdaJSymbol symbol;

        private OpenCodedPrimitive(LambdaJSymbol symbol) { this.symbol = symbol; }

        @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
        @Override public String toString() { return "#<opencoded primitive: " + symbol + '>'; }
        @Override public Object applyPrimitive(ConsCell a) { throw errorInternal("unexpected"); }
    }
    private OpenCodedPrimitive ocEval, ocApply;



    /// ### Global environment - define'd symbols go into this list
    private ConsCell topEnv;

    /** Build environment, setup Lisp reader and writer.
     *  Needs to be called once before {@link #eval(Object, ConsCell, int, int, int)} */
    ObjectReader init(ReadSupplier in, WriteConsumer out) {
        final SExpressionReader parser = new SExpressionReader(features, trace, tracer, symtab, featuresEnvEntry, in, null);
        final ObjectWriter outWriter = makeWriter(out);
        return init(parser, outWriter, null);
    }

    private ObjectReader init(ObjectReader inReader, ObjectWriter outWriter, ConsCell customEnv) {
        setReaderPrinter(inReader, outWriter);
        environment(customEnv);
        nCells = 0; maxEnvLen = 0;
        return inReader;
    }

    void clearMacros() {
        if (symtab == null) return;
        for (Object entry: ((ListSymbolTable)symtab).symbols) {
            if (entry != null) ((LambdaJSymbol)entry).macro = null;
        }
    }

    final Set<Object> modules = new HashSet<>();
    int speed = 1; // changed by (declaim (optimize (speed...


    /// ###  eval - the heart of most if not all Lisp interpreters
    private Object eval(Object form, ConsCell env, int stack, int level, int traceLvl) {
        Object func = null;
        Object result = null;  /* should be assigned even if followed by a "return" because it will be used in the "finally" clause*/
        Deque<Object> traceStack = null;
        ConsCell restore = null;
        boolean isTc = false;
        try {
            stack++;

            tailcall:
            while (true) {
                if (Thread.interrupted()) throw new InterruptedException("got interrupted");

                level++;
                if (traceOn) dbgEvalStart(isTc ? "eval TC" : "eval", form, env, stack, level);

                /// eval - lookup symbols in the current environment
                if (form == null) return result = null;
                if (symbolp(form)) {
                    final ConsCell envEntry = assq(form, env);
                    if (envEntry != null) {
                        final Object value = cdr(envEntry);
                        if (value == UNASSIGNED) throw new LambdaJError(true, "%s: '%s' is bound but has no assigned value", "eval", printSEx(form));
                        return result = value;
                    }
                    throw new LambdaJError(true, "%s: '%s' is not bound", "eval", printSEx(form));
                }

                /// eval - atoms that are not symbols eval to themselves
                if (atom(form)) return result = form;

                /// eval - form is not an atom - must be a cons (nonempty list) containing either a special form or a function application
                final ConsCell ccForm = (ConsCell)form;

                final Object operator = car(ccForm);      // first element of the of the form should be a symbol or an expression that computes a symbol
                if (operator == null) throw new LambdaJError(true, "function application: not a primitive or lambda: nil");

                final ConsCell ccArguments = listOrMalformed("eval", cdr(ccForm));   // list with remaining atoms/ expressions

                final boolean funcall;
                ConsCell ccForms = null;


                final LambdaJSymbol symOperator; // will be the car of the form as a LambdaJSymbol if it is a symbol, null otherwise
                if (symbolp(operator)) switch ((symOperator = (LambdaJSymbol)operator).wellknownSymbol) {
                /// eval - special forms

                /// eval - (quote exp) -> exp
                case sQuote: {
                    oneArg("quote", ccArguments);
                    return result = car(ccArguments);
                }

                /// eval - (lambda dynamic? (params...) forms...) -> lambda or closure
                case sLambda: {
                    result = "#<lambda>";
                    return makeClosureFromForm(ccForm, env);
                }

                case sSetQ: {
                    return result = evalSetq(ccArguments, env, stack, level, traceLvl);
                }

                case sDefmacro: {
                    return result = car(ccArguments);
                }

                case sDeclaim: {
                    return result = evalDeclaim(level, ccArguments);
                }


                /// eval - special forms that change the global environment

                /// eval - (define symbol exp) -> symbol with a side of global environment extension
                case sDefine: {
                    varargs1_2("define", ccArguments);
                    final LambdaJSymbol symbol = symbolOrMalformed("define", car(ccArguments));
                    final ConsCell envEntry = assq(symbol, topEnv);

                    // immutable globals: "if (envEntry...)" entkommentieren, dann kann man globals nicht mehrfach neu zuweisen
                    //if (envEntry != null) throw new LambdaJError(true, "%s: '%s' was already defined, current value: %s", "define", symbol, printSEx(cdr(envEntry)));

                    final Object value = eval(cadr(ccArguments), env, stack, level, traceLvl);
                    if (envEntry == null) insertFront(topEnv, symbol, value);
                    else envEntry.rplacd(value);

                    return result = symbol;
                }

                /// eval - (defun symbol (params...) forms...) -> symbol with a side of global environment extension
                // shortcut for (define symbol (lambda (params...) forms...))
                case sDefun: {
                    varargsMin("defun", ccArguments, 2);
                    form = list(sDefine, car(ccArguments), cons(sLambda, cons(cadr(ccArguments), cddr(ccArguments))));
                    isTc = true; continue tailcall;
                }


                /// eval - special forms that run expressions

                /// eval - (if condform form optionalform) -> object
                case sIf: {
                    varargsMinMax("if", ccArguments, 2, 3);
                    if (eval(car(ccArguments), env, stack, level, traceLvl) != null) {
                        form = cadr(ccArguments); isTc = true; continue tailcall;
                    } else if (caddr(ccArguments) != null) {
                        form = caddr(ccArguments); isTc = true; continue tailcall;
                    } else { return result = null; } // condition eval'd to false, no else form
                }

                /// eval - (load filespec) -> object
                case sLoad: {
                    oneArg("load", ccArguments);
                    return result = loadFile("load", car(ccArguments));
                }

                /// eval - (require modulename optfilespec) -> object
                case sRequire: {
                    return result = evalRequire(ccArguments);
                }

                /// eval - (provide modulename) -> nil
                case sProvide: {
                    return result = evalProvide(ccArguments);
                }



                // the case clauses below will set "funcall" to false and set up "ccForms" depending on the special form.
                // "ccForms" will then be used in "eval a list of forms" below

                /// eval - (progn forms...) -> object
                case sProgn: {
                    ccForms = ccArguments;
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (cond (condform forms...)... ) -> object
                case sCond: {
                    if (ccArguments != null) for (Object c: ccArguments) {
                        if (!listp(c)) errorMalformed("cond", "a list (condexpr forms...)", c);
                        if (eval(car(c), env, stack, level, traceLvl) != null) {
                            ccForms = (ConsCell) cdr(c);
                            break;
                        }
                    }

                    if (ccForms == null) return result = null; // no condition was true or the true condition has no code
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (labels ((symbol (params...) forms...)...) forms...) -> object
                case sLabels: {
                    final ConsCell[] formsAndEnv = evalLabels(ccArguments, env);
                    ccForms = formsAndEnv[0];
                    env = formsAndEnv[1];
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (let {optsymbol | dynamic}? (bindings...) bodyforms...) -> object
                /// eval - (let* {optsymbol | dynamic}? (bindings...) bodyforms...) -> object
                /// eval - (letrec optsymbol? (bindings...) bodyforms...) -> object
                case sLet:
                case sLetStar:
                case sLetrec: {
                    final ConsCell[] formsAndEnv = evalLet(operator, ccArguments, env, restore, stack, level, traceLvl);
                    ccForms = formsAndEnv[0];
                    env = formsAndEnv[1];
                    restore = formsAndEnv[2];
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (multiple-value-bind (symbols...) values-form bodyforms...) -> object
                case sMultipleValueBind: {
                    final ConsCell[] formsAndEnv = evalMultipleValueBind(ccArguments, env, stack, level, traceLvl);
                    ccForms = formsAndEnv[0];
                    env = formsAndEnv[1];
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                default:
                    // check if we forgot to handle a special form. All special forms (except multiple-value-call) should be handled in the cases above.
                    assert !symOperator.specialForm() || symOperator.wellknownSymbol == WellknownSymbol.sMultipleValueCall: ccForm.lineInfo() + "unexpected special form " + symOperator;

                    // check if expandForm() has expanded all macros and make sure that expandForm() is used prior to any eval() call with a form that may contain macro calls
                    assert symOperator.macro == null: ccForm.lineInfo() + "unexpanded macro call: " + symOperator;
                    
                    funcall = true;
                }
                else {
                    symOperator = null;
                    funcall = true;
                }



                /// eval - function application
                if (funcall) {

                    /// eval - (eval form) -> object
                    if (operator == ocEval) {
                        varargsMinMax("eval", ccArguments, 1, 2);
                        form = expandForm(car(ccArguments));
                        if (cdr(ccArguments) == null) env = topEnv; // todo unterschied zu kompiliertem eval: topEnv sind ALLE globals, kompiliertes eval sieht nur predefined globals
                        else {
                            final Object additionalEnv = cadr(ccArguments);
                            if (!listp(additionalEnv)) errorMalformed("eval", "'env' to be a list", additionalEnv);
                            env = (ConsCell) append2(additionalEnv, topEnv);
                        }
                        isTc = true; continue tailcall;
                    }

                    final ConsCell argList;

                    /// eval - apply a function to an argument list
                    /// eval - (apply form argform) -> object
                    if (operator == ocApply) {
                        twoArgs("apply", ccArguments);
                        final Object funcOrSymbol = car(ccArguments);
                        func = symbolp(funcOrSymbol) ? eval(funcOrSymbol, env, stack, level, traceLvl) : funcOrSymbol; // could add the same performance cheat as in function call below
                        argList = listOrMalformed("apply", cadr(ccArguments));
                        if (speed >= 1 && symbolp(funcOrSymbol)) {
                            result = evalOpencode((LambdaJSymbol) funcOrSymbol, argList);
                            if (result != NOT_HANDLED) return result;
                        }
                        // fall through to "actually perform..."

                        /// eval - multiple-value-call
                        /// eval - (multiple-value-call function-form values-form*) -> object
                    } else if (operator == sMultipleValueCall) {
                        varargs1("multiple-value-call", ccArguments);
                        final Object funcOrSymbol = car(ccArguments);
                        func = eval(funcOrSymbol, env, stack, level, traceLvl); // could add the same performance cheat as in function call below
                        ConsCell allArgs = null;
                        final Object valueForms = cdr(ccArguments);
                        if (valueForms != null) for (Object valueForm: listOrMalformed("multiple-value-call", valueForms)) {
                            values = NO_VALUES;
                            final Object prim = eval(valueForm, env, stack, level, traceLvl);
                            final ConsCell newValues = values == NO_VALUES ? cons(prim, null) : values;
                            if (newValues != null) allArgs = listOrMalformed("multiple-value-call", append2(allArgs, newValues));
                        }
                        argList = allArgs;
                        if (speed >= 1 && symbolp(funcOrSymbol)) {
                            result = evalOpencode((LambdaJSymbol) funcOrSymbol, argList);
                            if (result != NOT_HANDLED) return result;
                        }
                        // fall through to "actually perform..."

                        /// eval - function call
                        /// eval - (operatorform argforms...) -> object
                    } else if (speed >= 1 && symOperator != null && symOperator.wellknown()) {
                        // (mostly) respect evaluation order: operator should be eval'd before arguments.
                        // Generally the operator could be an undefined symbol, and we want that to fail before evaluation the arguments,
                        // e.g. if "when" was not defined as a macro then "(when (< i 10) (loop (1+ i)))" should fail and not make an endless recursion.
                        // Cheat here to gain performance: wellknown symbols are known to exist and will be looked up later out of order if needed
                        // skip eval with an expensive assq call for now.
                        argList = evlis(ccArguments, env, stack, level, traceLvl);
                        result = evalOpencode(symOperator, argList);
                        if (result != NOT_HANDLED) return result;

                        func = cdr(assq(operator, env));
                        // fall through to "actually perform..."
                    } else {
                        func = eval(operator, env, stack, level, traceLvl);
                        argList = evlis(ccArguments, env, stack, level, traceLvl);
                        // fall through to "actually perform..."
                    }

                    /// eval - actually perform the function call that was set up by "apply" or "multiple-value-call" or "function call" above
                    if (traced != null) traceLvl = traceEnter(func, argList, traceLvl);
                    values = NO_VALUES;
                    if (func instanceof OpenCodedPrimitive) {
                        form = cons(func, argList);
                        if (traced != null) traceStack = push(func, traceStack);
                        func = null;
                        isTc = true; continue tailcall;

                    } else if (func instanceof Primitive) {
                        return result = applyPrimitive((Primitive) func, argList, stack, level);

                    } else if (consp(func) && car(func) == sLambda) {
                        final Object lambda = cdr(func);          // ((params...) (forms...))
                        final ConsCell closure = ((ConsCell)func).closure();
                        if (closure == null) varargs1("lambda application", lambda); // if closure != null then it was created by the special form lambda, no need to check again
                        env = zip(car(lambda), argList, closure != null ? closure : env, true);

                        if (traceFunc)  tracer.println(pfx(stack, level) + " #<lambda " + lambda + "> " + printSEx(argList));
                        ccForms = (ConsCell) cdr(lambda);
                        // fall through to "eval a list of forms"

                    } else if (func instanceof MurmelJavaProgram.CompilerPrimitive) {
                        // compiled function or compiler runtime func
                        return result = applyCompilerPrimitive((MurmelJavaProgram.CompilerPrimitive) func, argList, stack, level);

                    } else {
                        throw new LambdaJError(true, "function application: not a primitive or lambda: %s", printSEx(func));
                    }
                }

                /// eval - eval a list of forms
                if (ccForms != null) {
                    for (; cdr(ccForms) != null; ccForms = listOrMalformed("lambda application", cdr(ccForms))) {
                        eval(car(ccForms), env, stack, level, traceLvl);
                    }
                    if (traced != null) traceStack = push(operator, traceStack);
                    form = car(ccForms); func = null; values = NO_VALUES; isTc = true; continue tailcall;
                }

                return result = null; // lambda/ progn/ labels/... w/o body
            }

        } catch (LambdaJError | InterruptedException e) {
            throw new LambdaJError(false, e.getMessage(), form); // todo bei InterruptedException Thread.interrupt()?
        } catch (Exception e) {
            //e.printStackTrace();
            throw errorInternal(e, "eval: caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
        } finally {
            if (traceOn) dbgEvalDone(isTc ? "eval TC" : "eval", form, env, stack, level);
            if (traced != null && func != null) traceLvl = traceExit(func, result, traceLvl);
            Object s;
            if (traceStack != null) {
                while ((s = traceStack.pollLast()) != null) traceLvl = traceExit(s, result, traceLvl);
            }
            for (ConsCell c = restore; c != null; c = (ConsCell) cdr(c)) {
                final ConsCell entry = (ConsCell) caar(c);
                entry.rplacd(cdar(c));
            }
        }
    }

    final Object eval(Object form, ConsCell env) {
        return eval(form, env != null ? (ConsCell) append2(env, topEnv) : topEnv, 0, 0, 0);
    }

    final Object expandAndEval(Object form, ConsCell env) {
        if (form == null) return null;
        final Object expansion = expandForm(form);
        if (consp(expansion) && car(expansion) == sProgn) {
            return expandAndEvalForms(listOrMalformed("progn", cdr(expansion)), env);
        }
        return eval(expansion, env);
    }

    private Object expandAndEvalForms(ConsCell forms, ConsCell env) {
        Object result = null;
        for (ConsCell rest = forms; rest != null; rest = listOrMalformed("progn", cdr(rest))) {
            result = expandAndEval(car(rest), env);
        }
        return result;
    }

    /** expand all macros within a form and do some syntax checks. Macro-expansion is done in a copy, i.e. form will not be modified. */
    Object expandForm(Object form) {
        if (atom(form)) return form;
        final ConsCell ccForm = ((ConsCell)form).copy();
        final Object op = car(ccForm);
        if (op == null) throw new LambdaJError(true, "function application: not a primitive or lambda: nil");
        final ConsCell ccArgs = cdrShallowCopyList("eval", ccForm);

        if (symbolp(op)) {
            final LambdaJSymbol symOp = (LambdaJSymbol)op;
            if (symOp.specialForm()) switch (symOp.wellknownSymbol) {
            case sQuote:
                oneArg("quote", ccArgs);  return form;

            case sLambda:
                varargsMin("lambda", ccArgs, 1);
                if (car(ccArgs) == sDynamic) expandForms("lambda", cddrShallowCopyList("lambda", ccArgs));
                else expandForms("lambda", cdrShallowCopyList("lambda", ccArgs));
                return ccForm;

            case sIf:
                varargsMinMax("if", ccArgs, 2, 3);
                expandForms("if", ccArgs);
                return ccForm;

            case sCond:
                if (ccArgs != null) expandForms("cond", ccArgs); return ccForm;

            case sProgn:
                if (ccArgs == null) return null;
                if (cdr(ccArgs) == null) return expandForm(car(ccArgs));
                expandForms("progn", ccArgs);
                return ccForm;

            case sLabels:
                for (ConsCell i = listOrMalformed("labels", carShallowCopyList("labels", ccArgs)); i != null; i = cdrShallowCopyList("labels", i)) {
                    final ConsCell localFunc = listOrMalformed("labels", carShallowCopyList("labels", i));
                    varargsMin("labels", localFunc, 2);
                    final LambdaJSymbol funcSymbol = symbolOrMalformed("labels", car(localFunc));
                    if (funcSymbol.macro != null) throw new LambdaJError(true, "local function %s is also a macro which would shadow the local function", funcSymbol, localFunc);
                    if (cddr(localFunc) != null) {
                        final ConsCell body = cddrShallowCopyList("labels", localFunc);
                        expandForms("labels", body);
                    }
                }
                if (cdr(ccArgs) != null) expandForms("labels", ccArgs.shallowCopyCdr());
                return ccForm;

            case sDefine: {
                varargs1_2("define", ccArgs);
                if (cdr(ccArgs) != null) {
                    final ConsCell valueForm = cdrShallowCopyList("define", ccArgs);
                    valueForm.rplaca(expandForm(car(valueForm)));
                }
                return ccForm;
            }

            case sDefun: {
                varargsMin("defun", ccArgs, 2);
                if (cddr(ccArgs) != null) expandForms("defun", cddrShallowCopyList("defun", ccArgs));
                return ccForm;
            }

            case sDefmacro:
                varargs1("defmacro", ccArgs);
                if (cddr(ccArgs) != null) expandForms("defmacro", cddrShallowCopyList("defmacro", ccArgs));
                evalDefmacro(ccArgs, topEnv, form);
                return ccForm;

            case sLet:
            case sLetStar:
            case sLetrec:
                final String sfName = symOp.toString();
                final ConsCell bindingsAndBody;
                if (car(ccArgs) != null && symbolp(car(ccArgs))) bindingsAndBody = cdrShallowCopyList(sfName, ccArgs);
                else bindingsAndBody = ccArgs;
                if (car(bindingsAndBody) != null) {
                    final ConsCell bindings = carShallowCopyList(sfName, bindingsAndBody);
                    for (ConsCell i = bindings; i != null; i = cdrShallowCopyList(sfName, i)) {
                        if (consp(car(i))) {
                            if (consp(cadar(i))) {
                                final ConsCell binding = carShallowCopyList(sfName, i);
                                final ConsCell valueFormList = cdrShallowCopyList(sfName, binding);
                                valueFormList.rplaca(expandForm(car(valueFormList)));
                            }
                        }
                    }
                }
                if (cdr(bindingsAndBody) != null) {
                    final ConsCell bodyCopy = cdrShallowCopyList(sfName, bindingsAndBody);
                    expandForms(sfName, bodyCopy);
                }
                return ccForm;

            case sMultipleValueBind:
                varargsMin("multiple-value-bind", ccArgs, 2);
                expandForms("multiple-value-bind", ccArgs.shallowCopyCdr());
                return ccForm;

            case sSetQ:
                for (ConsCell pairs = ccArgs; pairs != null; pairs = cdrShallowCopyList("setq", pairs)) {
                    symbolOrMalformed("setq", car(pairs));
                    if (cdr(pairs) == null) errorMalformed("setq", "odd number of arguments");
                    pairs = cdrShallowCopyList("setq", pairs);
                    pairs.rplaca(expandForm(car(pairs)));
                }
                return ccForm;

            case sDeclaim:
            case sLoad:
            case sRequire:
            case sProvide:
                return form; // no macroexpansion in declaim, load, require, provide forms. Todo or should there be?

            case sMultipleValueCall:
                varargs1("multiple-value-call", ccArgs);
                expandForms("multiple-value-call", ccArgs);
                return ccForm;

            default:
                assert false: ccForm.lineInfo() + "special form " + symOp + " is not implemented";
            }

            // not a special form, must be a function or macro application
            if (symOp.macro != null) {
                final Object expansion = macroexpandImpl(ccForm);
                assert cadr(values) != null: ccForm.lineInfo() + "macro " + symOp + " was not expanded - secondary value is nil, form was " + form;
                assert expansion != ccForm: ccForm.lineInfo() + "macro " + symOp + " was not expanded - expansion == ccForm, form was " + form;
                values = NO_VALUES;
                return expandForm(expansion);
            }
        }
        expandForms("function application", ccForm);
        return ccForm;
    }

    private static ConsCell carShallowCopyList(String sfName, ConsCell bindingsAndBody) {
        final ConsCell carCopy = listOrMalformed(sfName, car(bindingsAndBody)).copy();
        bindingsAndBody.rplaca(carCopy);
        return carCopy;
    }

    private static ConsCell cdrShallowCopyList(String sfName, ConsCell i) {
        return listOrMalformed(sfName, i.shallowCopyCdr());
    }

    private static ConsCell cddrShallowCopyList(String sfName, ConsCell i) {
        return listOrMalformed(sfName, listOrMalformed(sfName, i.shallowCopyCdr()).shallowCopyCdr());
    }

    /** expand all elements in the list ccForms. The first conscell is modified in place, subsequent conscells are copied.  */
    private void expandForms(String func, Object forms) {
        expandForms(func, listOrMalformed(func, forms));
    }

    private void expandForms(String func, ConsCell ccForms) {
        for (ConsCell i = ccForms; i != null; i = cdrShallowCopyList(func, i)) {
            i.rplaca(expandForm(car(i)));
        }
    }

    static ConsCell listOrMalformed(String op, Object args) {
        if (!listp(args)) errorMalformed(op, "a list", args);
        return (ConsCell)args;
    }

    final LambdaJSymbol symbolOrMalformed(String op, Object maybeSymbol) {
        if (!symbolp(maybeSymbol)) errorMalformed(op, "a symbol", maybeSymbol);
        final LambdaJSymbol symbol = (LambdaJSymbol)maybeSymbol;
        notReserved(op, symbol);
        return symbol;
    }

    private Object evalSetq(ConsCell arguments, ConsCell env, int stack, int level, int traceLvl) {
        Object res = null;
        for (ConsCell pairs = arguments; pairs != null; ) {
            final LambdaJSymbol symbol = symbolOrMalformed("setq", car(pairs));
            final ConsCell envEntry = assq(symbol, env);

            pairs = (ConsCell) cdr(pairs);
            if (pairs == null) errorMalformed("setq", "odd number of arguments");
            final Object value = eval(car(pairs), env, stack, level, traceLvl);
            if (envEntry == null)
                insertFront(env, symbol, value);
                //throw new LambdaJError(true, "%s: '%s' is not bound", "setq", symbol);
            else envEntry.rplacd(value);
            res = value;
            pairs = (ConsCell) cdr(pairs);
        }
        return res;
    }

    private void evalDefmacro(ConsCell arguments, ConsCell env, Object form) {
        varargs1("defmacro", arguments);
        final LambdaJSymbol macroName = symbolOrMalformed("defmacro", car(arguments));
        final int arglen = length(arguments);
        if (arglen == 1) {
            macroName.macro = null;
        }
        else if (arglen >= 3) {
            final ListConsCell paramsAndBody = cons(cadr(arguments), cddr(arguments));
            macroName.macro = makeClosureFromForm(cons(sLambda, paramsAndBody), env);
        }
        else throw errorMalformed("defmacro", printSEx(form));
    }

    private Object evalRequire(ConsCell arguments) {
        varargsMinMax("require", arguments, 1, 2);
        if (!stringp(car(arguments))) errorMalformed("require", "a string argument", arguments);
        final Object modName = car(arguments);
        if (!modules.contains(modName)) {
            Object modFilePath = cadr(arguments);
            if (modFilePath == null) modFilePath = modName;
            final Object ret = loadFile("require", modFilePath);
            if (!modules.contains(modName)) throw new LambdaJError(true, "require'd file '%s' does not provide '%s'", modFilePath, modName);
            return ret;
        }
        return null;
    }

    private Object evalProvide(ConsCell arguments) {
        oneArg("provide", arguments);
        if (!stringp(car(arguments))) errorMalformed("provide", "a string argument", arguments);
        final Object modName = car(arguments);
        modules.add(modName);
        return null;
    }

    Object evalDeclaim(int level, ConsCell arguments) {
        if (level != 1) errorMalformed("declaim", "must be a toplevel form");
        if (caar(arguments) == intern("optimize")) {
            final Object rest = cdar(arguments);
            final ConsCell speedCons = assq(intern("speed"), rest);
            if (speedCons != null) {
                final Object speed = cadr(speedCons);
                if (!numberp(speed)) throw new LambdaJError(true, "declaim: argument to optimize must be a number, found %s", speed);
                this.speed = ((Number)speed).intValue();
            }
        }
        return null;
    }

    private ConsCell[] evalLabels(ConsCell arguments, ConsCell env) {
        varargs1("labels", arguments);
        final ListConsCell extEnv = acons(PSEUDO_SYMBOL, UNASSIGNED, env);
        // stick the functions into the env
        if (car(arguments) != null)
            for (Object binding: (ConsCell) car(arguments)) {
                if (!consp(binding)) errorMalformed("labels", "a list (symbol (params...) forms...)", binding);
                final ConsCell currentFunc = (ConsCell)binding;
                final LambdaJSymbol currentSymbol = symbolOrMalformed("labels", car(currentFunc));
                final ConsCell lambda = makeClosure(cdr(currentFunc), extEnv);
                insertFront(extEnv, currentSymbol, lambda);
            }
        return new ConsCell[]{ (ConsCell) cdr(arguments), extEnv};
    }


    private ConsCell[] evalLet(Object operator, final ConsCell arguments, ConsCell env, ConsCell restore, int stack, int level, int traceLvl) {
        final boolean letStar  = operator == sLetStar;
        final boolean letRec   = operator == sLetrec;
        final Object maybeLoopSymbol = car(arguments);
        final boolean letDynamic = maybeLoopSymbol == sDynamic;
        if (letDynamic && letRec) throw errorMalformed(operator.toString(), "dynamic is not allowed with letrec");
        final boolean namedLet = !letDynamic && maybeLoopSymbol != null && symbolp(maybeLoopSymbol); // ohne "maybeLoopSymbol != null" wuerde die leere liste in "(let () 1)" als loop-symbol nil erkannt
        final LambdaJSymbol loopSymbol = namedLet ? (LambdaJSymbol)maybeLoopSymbol : null;

        final ConsCell bindingsAndBodyForms = namedLet || letDynamic ? (ConsCell)cdr(arguments) : arguments;  // ((bindings...) bodyforms...)

        final Object bindings = car(bindingsAndBodyForms);
        if (!listp(bindings)) throw errorMalformed(getOp(operator, letDynamic, namedLet), "a list of bindings", bindings);
        final ConsCell ccBindings = (ConsCell)bindings;

        ConsCell params = null;
        ConsCell extenv = env;
        if (ccBindings != null) {
            final boolean useLookup = !(letStar && !letDynamic || cdr(ccBindings) == null); // lookup is not needed for let* or a single binding
            final ArrayList<Object> seen = useLookup ? new ArrayList<>() : null;
            ConsCell newValues = null; // used for let dynamic
            ConsCell insertPos = null; // used for named let
            if (letRec) extenv = acons(PSEUDO_SYMBOL, UNASSIGNED, env);
            for (Object binding : ccBindings) {
                final LambdaJSymbol sym;
                final Object bindingForm;

                if (symbolp(binding)) {
                    sym = (LambdaJSymbol)binding;
                    bindingForm = null;
                } else if (consp(binding) && symbolp(car(binding)) && listp(cdr(binding))) {
                    if (cddr(binding) != null) throw errorMalformedFmt(getOp(operator, letDynamic, namedLet), "illegal variable specification %s", printSEx(binding));
                    sym = (LambdaJSymbol)car(binding);
                    bindingForm = cadr(binding);
                } else {
                    throw errorMalformed(getOp(operator, letDynamic, namedLet), "bindings to contain lists and/or symbols", binding);
                }

                // don't use notReserved(), this way getOp() only allocates space for string concatenation if needed to actually display an error message
                if (reserved(sym)) errorReserved(getOp(operator, letDynamic, namedLet), sym);
                if (sym == loopSymbol) errorMalformedFmt(getOp(operator, letDynamic, namedLet), "can't use loop symbol %s as a variable", sym);

                final boolean isNewSymbol;
                if (useLookup) {
                    // let and letrec don't allow duplicate symbols
                    isNewSymbol = !seen.contains(sym);
                    if (isNewSymbol) seen.add(sym);
                    if (!letStar) { // let and letrec don't allow duplicate symbols
                        if (!isNewSymbol) throw errorMalformedFmt(getOp(operator, letDynamic, namedLet), "duplicate symbol %s", sym);
                    }
                }
                else {
                    isNewSymbol = true; // ignored/ not needed for "let*", true for a single binding
                }

                ConsCell newBinding = null;
                if (letDynamic) newBinding = assq(sym, topEnv);
                else if (letRec) newBinding = insertFront(extenv, sym, UNASSIGNED);

                final Object val = bindingForm == null ? null : eval(bindingForm, letStar || letRec ? extenv : env, stack, level, traceLvl);
                if (letDynamic && newBinding != null) {
                    if (isNewSymbol) restore = acons(newBinding, cdr(newBinding), restore);
                    if (letStar) newBinding.rplacd(val); // das macht effektiv ein let* dynamic
                    else newValues = acons(newBinding, val, newValues);
                }
                else if (letRec) newBinding.rplacd(val);
                else extenv = acons(sym, val, extenv);
                
                if (namedLet) {
                    if (params == null) {
                        params = cons(sym, null);
                        insertPos = params;
                    } else {
                        insertPos.rplacd(cons(sym, null));
                        insertPos = (ConsCell) insertPos.cdr();
                    }
                }
            }
            if (newValues != null) for (Object o: newValues) {
                final ConsCell c = (ConsCell)o;
                ((ConsCell)car(c)).rplacd(cdr(c));
            }
        }
        final ConsCell bodyForms = (ConsCell)cdr(bindingsAndBodyForms);
        if (namedLet) {
            extenv = acons(loopSymbol, UNASSIGNED, extenv);
            final Object closure = makeClosure(cons(params, bodyForms), extenv);
            ((ConsCell)extenv.car()).rplacd(closure);
        }
        return new ConsCell[] {bodyForms, extenv, restore};
    }

    private static String getOp(Object operator, boolean letDynamic, boolean namedLet) {
        return letDynamic ? operator + " dynamic" : (namedLet ? "named " : "") + operator;
    }

    private ConsCell[] evalMultipleValueBind(final ConsCell bindingsAndBodyForms, ConsCell env, int stack, int level, int traceLvl) {
        varargsMin("multiple-value-bind", bindingsAndBodyForms, 2);
        values = NO_VALUES;
        final Object prim = eval(cadr(bindingsAndBodyForms), env, stack, level, traceLvl);
        final ConsCell newValues = values == NO_VALUES ? cons(prim, null) : values;
        values = NO_VALUES;
        final ConsCell extEnv = zip(car(bindingsAndBodyForms), newValues, env, false);
        return new ConsCell[] { listOrMalformed("multiple-value-bind", cddr(bindingsAndBodyForms)), extEnv };
    }

    Object evalMacro(Object operator, final ConsCell macroClosure, final ConsCell arguments, int stack, int level, int traceLvl) {
        if (traceFunc)  tracer.println(pfx(stack, level) + " #<macro " + operator + "> " + printSEx(arguments));

        final Object lambda = cdr(macroClosure);      // (params . (forms...))
        final ConsCell menv = zip(car(lambda), arguments, topEnv, true);
        Object expansion = null;
        for (Object macroform: (ConsCell) cdr(lambda)) // loop over macro body so that e.g. "(defmacro m (a b) (write 'hallo) `(+ ,a ,b))" will work
            expansion = eval(macroform, menv, stack, level, traceLvl);
        return expansion;
    }

    private Object evalOpencode(LambdaJSymbol op, ConsCell args) {
        // bringt ein bisserl performance: 1x weniger eval und environment lookup mit assq()
        // wenn in einem eigenen pass 1x arg checks gemacht wuerden,
        // koennten die argchecks hier wegfallen und muessten nicht ggf. immer wieder in einer schleife wiederholt werden.

        if (op == null || !op.primitive()) return NOT_HANDLED;

        switch (op.wellknownSymbol) {
        case sAdd:  { return addOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs); }
        case sMul:  { return addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs); }
        case sSub:  { return subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs); }
        case sDiv:  { return subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs); }

        case sMod:  { twoArgs("mod", args); return cl_mod(toDouble("mod", car(args)), toDouble("mod", cadr(args))); }
        case sRem:  { twoArgs("rem", args); return toDouble("rem", car(args)) % toDouble("rem", cadr(args)); }

        case sNeq:  { return compare(args, "=",  (d1, d2) -> d1 == d2); }
        case sNe:   { return compare(args, "/=", (d1, d2) -> d1 != d2); }
        case sLt:   { return compare(args, "<",  (d1, d2) -> d1 <  d2);  }
        case sLe:   { return compare(args, "<=", (d1, d2) -> d1 <= d2); }
        case sGe:   { return compare(args, ">=", (d1, d2) -> d1 >= d2); }
        case sGt:   { return compare(args, ">",  (d1, d2) -> d1 >  d2);  }

        case sCar:  { oneArg ("car",  args);  return caar(args); }
        case sCdr:  { oneArg ("cdr",  args);  return cdar(args); }
        case sCons: { twoArgs("cons", args);  return cons(car(args), cadr(args)); }

        case sEq:   { twoArgs("eq",   args);  return boolResult(car(args) == cadr(args)); }
        case sEql:  { twoArgs("eql",  args);  return boolResult(eql(car(args), cadr(args))); }
        case sNull: { oneArg ("null", args);  return boolResult(car(args) == null); }

        case sInc:  { oneArg("1+", args);  return inc(car(args)); }
        case sDec:  { oneArg("1-", args);  return dec(car(args)); }

        case sAppend:   { return append(args); }
        case sList:     { return args; }
        case sListStar: { return listStar(args); }

        default:    return NOT_HANDLED;
        }
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

    /** build an extended environment for a function invocation:<pre>
     *  loop over params and args
     *    construct a cons (param . arg)
     *    stick above list in front of the environment
     *  return extended environment</pre>
     *  
     *  Similar to CL pairlis, but {@code #zip} will also pair the last cdr of a dotted list with the rest of {@code args},
     *  e.g. (zip '(a b . c) '(1 2 3 4 5)) -> ((a . 1) (b . 2) (c 3 4 5)) */
    private ConsCell zip(Object paramList, ConsCell args, ConsCell env, boolean match) {
        if (paramList == null && args == null) return env; // shortcut for no params/ no args

        for (Object params = paramList; params != null; ) {
            // regular param/arg: add to env
            if (consp(params)) {
                if (match && args == null) throw new LambdaJError(true, "%s: not enough arguments. Parameters w/o argument: %s", "function application", printSEx(params));
                env = acons(car(params), car(args), env);
            }

            // if paramList is a dotted list then the last param will be bound to the list of remaining args
            else {
                env = acons(params, args, env);
                if (match) { args = null; break; }
            }

            params = cdr(params);
            if (params == paramList) errorMalformed("lambda", "bindings are a circular list");

            args = (ConsCell) cdr(args);
            if (args == null) {
                if (consp(params)) {
                    if (match) throw new LambdaJError(true, "%s: not enough arguments. Parameters w/o argument: %s", "function application", printSEx(params));
                    else env = acons(params, null, env);
                }
                else if (params != null) {
                    // paramList is a dotted list, no argument for vararg parm: assign nil
                    env = acons(params, null, env);
                    break;
                }
            }
        }
        if (match && args != null) throw new LambdaJError(true, "%s: too many arguments. Remaining arguments: %s", "function application", printSEx(args));
        return env;
    }

    /** eval a list of forms and return a list of results */
    private ConsCell evlis(ConsCell forms, ConsCell env, int stack, int level, int traceLvl) {
        if (traceOn) dbgEvalStart("evlis", forms, env, stack, level);
        ListConsCell head = null;
        ListConsCell insertPos = null;
        for (ConsCell rest = forms; rest != null; rest = (ConsCell)cdr(rest)) {
            final ListConsCell currentArg = cons(eval(car(rest), env, stack, level, traceLvl), null);
            if (head == null) {
                head = currentArg;
                insertPos = head;
            }
            else {
                insertPos.rplacd(currentArg);
                insertPos = currentArg;
            }
        }
        if (traceOn) dbgEvalDone("evlis", forms, head, stack, level);
        return head;
    }

    /** make a lexical closure (if enabled) or lambda from a lambda-form,
     *  considering whether or not "dynamic" was specified after "lambda" */
    private ConsCell makeClosureFromForm(final ConsCell form, ConsCell env) {
        final ConsCell paramsAndForms = (ConsCell) cdr(form);

        if (car(paramsAndForms) == sDynamic) {
            final Object _paramsAndForms = cdr(paramsAndForms);
            varargs1("lambda dynamic", _paramsAndForms);
            symbolArgs("lambda dynamic", car(_paramsAndForms));
            noDuplicates("lambda dynamic", car(_paramsAndForms));
            return cons(sLambda, _paramsAndForms);
        }
        varargs1("lambda", paramsAndForms);
        symbolArgs("lambda", car(paramsAndForms));
        noDuplicates("lambda", car(paramsAndForms));

        if (haveLexC()) return makeClosure(paramsAndForms, env);
        return form;
    }

    private static void noDuplicates(String func, Object symList) {
        if (symList == null) return;
        if (!consp(symList)) return;
        final HashSet<Object> seen = new HashSet<>();
        for (Object o: (ConsCell)symList) if (!seen.add(o)) errorMalformedFmt(func, "duplicate symbol %s", o);
    }

    /** make a lexical closure (if enabled) or lambda */
    private ConsCell makeClosure(final Object paramsAndForms, ConsCell env) {
        return cons3(sLambda, paramsAndForms, haveLexC() ? env : null);
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        try { return primfn.applyPrimitive(args); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<primitive> throws exception: %s", e.getMessage()); }
    }

    /** in case compiled code calls "(eval)" */
    private Object applyCompilerPrimitive(MurmelJavaProgram.CompilerPrimitive primfn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<compiled function> " + printSEx(args));
        try { return primfn.applyCompilerPrimitive(listToArray(args)); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<compiled function> throws exception: %s", e.getMessage()); }
    }


    private Object loadFile(String func, Object argument) {
        final Path p = findFile(func, lispReader, argument);
        try (Reader r = Files.newBufferedReader(p)) {
            final SExpressionReader parser = new SExpressionReader(r::read, symtab, featuresEnvEntry, p);
            final Object eof = "EOF";
            Object result = null;
            for (;;) {
                final Object form = parser.readObj(true, eof);
                if (form == eof) break;

                result = expandAndEval(form, null);
            }
            return result;
        } catch (IOException e) {
            throw new LambdaJError(true, "load: error reading file '%s': ", e.getMessage());
        }
    }

    final Path findFile(String func, ObjectReader lispReader, Object argument) {
        if (!stringp(argument)) errorMalformed(func, "a string argument", printSEx(argument));
        final String filename = (String)argument;
        Path current = lispReader.getFilePath();
        final Path path;
        if (filename.toLowerCase().endsWith(".lisp")) path = Paths.get(filename);
        else path = Paths.get(filename + ".lisp");
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
            if (((LambdaJSymbol)sym).specialForm()) {
                throw new LambdaJError(true, "trace: can't trace %s: it is a special form", printSEx(sym));
            }
            final ConsCell envEntry = assq(sym, topEnv);
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
                    final ConsCell envEntry = assq(sym, topEnv);
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
            if (((LambdaJSymbol)op).specialForm()) return traceStack;
            final ConsCell entry = assq(op, topEnv);
            if (entry == null) return traceStack;
            op = cdr(entry);
        }
        if (!traced.containsKey(op)) return traceStack;
        if (traceStack == null) traceStack = new ArrayDeque<>();
        traceStack.addLast(op);
        return traceStack;
    }

    private int traceEnter(Object op, ConsCell args, int level) {
        final LambdaJSymbol sym;
        if (traced == null || null == (sym = traced.get(op))) return level;
        enter(sym, args, level);
        return level + 1;
    }

    private void enter(Object op, ConsCell args, int level) {
        final StringBuilder sb = new StringBuilder();

        tracePfx(sb, level);

        sb.append('(').append(level+1).append(" enter ").append(op);
        printArgs(sb, args);
        sb.append(')');
        tracer.println(sb.toString());
    }

    private static void printArgs(StringBuilder sb, ConsCell args) {
        if (args == null) return;
        sb.append(':');
        final WriteConsumer append = sb::append;
        for (Object arg: args) {
            sb.append(' ');
            printSEx(append, arg);
        }
    }

    private int traceExit(Object op, Object result, int level) {
        final LambdaJSymbol sym;
        if (traced == null || null == (sym = traced.get(op))) return level;
        level = level < 1 ? 0 : level-1; // clamp at zero in case a traceEnter() call was lost because of a preceeding exception
        exit(sym, result, level);
        return level;
    }

    private void exit(Object op, Object result, int level) {
        final StringBuilder sb = new StringBuilder();

        tracePfx(sb, level);

        sb.append('(').append(level+1).append(" exit  ").append(op).append(": ");
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
                tracer.println(pfx + ' ' + evFunc + " (" + stack + '/' + level + ") exp:           " + printSEx(exp));
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
                tracer.println(pfx + ' ' + evFunc + " (" + stack + '/' + level + ") done, exp was: " + printSEx(exp));
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



    /// ###  (Mostly) Lisp-like functions used by interpreter program, a subset is used by interpreted programs as well
    final   ListConsCell cons(Object car, Object cdr)                    { nCells++; return new ListConsCell(car, cdr); }
    private ConsCell cons3(Object car, Object cdr, ConsCell closure) { nCells++; return new ClosureConsCell(car, cdr, closure); }
    private ListConsCell acons(Object key, Object datum, ConsCell alist) { return cons(cons(key, datum), alist); }

    private static Object carCdrError(String func, Object o) { throw new LambdaJError(true, "%s: expected one pair or symbol or string argument but got %s", func, printSEx(o)); }

    static Object   car(ConsCell c)    { return c == null ? null : c.car(); }
    static Object   car(Object o)      { //assert o == null || o instanceof ConsCell: "car: argument ist " + o.getClass().getSimpleName();
                                         return o == null ? null
                                                 : o instanceof ListConsCell ? ((ListConsCell)o).car()
                                                 : o instanceof ConsCell ? ((ConsCell)o).car()
                                                 : o instanceof Object[] ? ((Object[])o).length == 0 ? null : ((Object[])o)[0]
                                                 : o instanceof String ? ((String)o).isEmpty() ? null : ((String)o).charAt(0)
                                                 : o instanceof LambdaJSymbol ? ((LambdaJSymbol)o).name.isEmpty() ? null : ((LambdaJSymbol)o).name.charAt(0)
                                                 : carCdrError("car", o); }

    static Object   caar(ConsCell c)   { return c == null ? null : car(car(c)); }

    static Object   cadr(ConsCell c)   { return c == null ? null : car(cdr(c)); }
    static Object   cadr(Object o)     { return o == null ? null : car(cdr(o)); }

    static Object   cadar(ConsCell c)  { return c == null ? null : car(cdar(c)); }

    static Object   caddr(ConsCell c)  { return c == null ? null : car(cddr(c)); }

    static Object   cadddr(ConsCell o) { return o == null ? null : car(cdddr(o)); }

    static Object   cdr(ConsCell c)    { return c == null ? null : c.cdr(); }
    static Object   cdr(Object o)      { return o == null ? null
                                                 : o instanceof ListConsCell ? ((ListConsCell)o).cdr()
                                                 : o instanceof ConsCell ? ((ConsCell)o).cdr()
                                                 : o instanceof Object[] ? ((Object[])o).length <= 1 ? null : new ArraySlice((Object[])o)
                                                 : o instanceof String ? ((String)o).length() <= 1 ? null : ((String)o).substring(1)
                                                 : o instanceof LambdaJSymbol ? ((LambdaJSymbol)o).name.length() <= 1 ? null : ((LambdaJSymbol)o).name.substring(1)
                                                 : carCdrError("cdr", o); }

    static Object   cdar(ConsCell c)   { return c == null ? null : cdr(car(c)); }

    static Object   cddr(ConsCell c)   { return c == null ? null : cdr(cdr(c)); }
    static Object   cddr(Object o)     { return o == null ? null : cdr(cdr(o)); }

    static Object   cdddr(ConsCell o)  { return o == null ? null : cdr(cddr(o)); }

    // todo ggf. spezialfall arrayslice behandeln
    private static Object   nthcdr(int n, Object list) {
        if (list == null) return null;
        if (n <= 0) return list;
        for (; list != null && n-- > 0; list = cdr(list)) /* nothing */;
        return list;
    }

    /*private static ConsCell mapcar(UnaryOperator<Object> f, ConsCell l) {
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
    }*/

    static boolean eql(Object o1, Object o2) {
        if (o1 == o2) return true;
        if (numberp(o1) && numberp(o2) || characterp(o1) && characterp(o2))
            return Objects.equals(o1, o2);
        return false;
    }

    static boolean  consp(Object o)      { return o instanceof ConsCell; } // todo ggf. redundaten check auf SExpConsCell vorschalten?
    static boolean  atom(Object o)       { return !(o instanceof ConsCell); }                // ! consp(x)
    static boolean  symbolp(Object o)    { return o == null || o instanceof LambdaJSymbol; } // null (aka nil) is a symbol too
    static boolean  listp(Object o)      { return o == null || o instanceof ConsCell; }      // null (aka nil) is a list too

    static boolean  numberp(Object o)    { return o instanceof Long || o instanceof Double
                                                  || o instanceof Number; }
    static boolean  stringp(Object o)    { return o instanceof String; }
    static boolean  floatp(Object o)     { return o instanceof Double; } // todo float, BigDecimal?
    static boolean  integerp(Object o)   { return o instanceof Long; } // todo Byte, Short, Integer, BigInteger?
    static boolean  characterp(Object o) { return o instanceof Character; }

    // these *should* have no usages as these checks would be superfluous
    // the purpose of these functions is: if such extra checks were made then this would be discovered during testing
    static boolean  consp(ConsCell ignored)  { throw errorInternal("consp(ConsCell c) should NOT be called"); }
    static boolean  listp(ConsCell ignored)  { throw errorInternal("listp(ConsCell c) should NOT be called"); }

    static ConsCell arraySlice(Object[] o, int offset) { return o == null || offset >= o.length ? null : new ArraySlice(o, offset); }
    static ConsCell arraySlice(Object... elems) {
        if (elems == null || elems.length == 0) return null;
        return new ArraySlice(elems, 0);
    }

    private ConsCell list(Object... a) {
        if (a == null || a.length == 0) return null;
        ConsCell ret = null, insertPos = null;
        for (Object o: a) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                insertPos.rplacd(cons(o, null));
                insertPos = (ConsCell) insertPos.cdr();
            }
        }
        return ret;
    }

    static int length(Object list) {
        if (list == null) return 0;
        int n = 0;
        for (Object ignored: (ConsCell)list) n++;
        return n;
    }

    /** this method returns true while Lisp member returns the sublist starting at obj */
    /*private static boolean member(Object obj, ConsCell list) {
        if (obj == null) return false;
        if (list == null) return false;
        for (Object e: list) if (e == obj) return true;
        return false;
    }*/

    /** return the cons whose car is eq to {@code atom}
     *  note: searches using object identity (eq), will work for interned symbols, won't reliably work for e.g. numbers */
    static ConsCell assq(Object atom, Object maybeList) {
        if (maybeList == null) return null;
        return assq(atom, requireCons("assq", maybeList));
    }

    static ConsCell assq(Object atom, ConsCell ccList) {
        if (ccList == null) return null;
        for (Object entry: ccList) {
            if (entry != null) {
                final ConsCell ccEntry = (ConsCell) entry;
                if (atom == car(ccEntry)) return ccEntry;
            }
        }
        return null;
    }

    /** return the cons whose car is eql to {@code atom} */
    static ConsCell assoc(Object atom, Object maybeList) {
        if (maybeList == null) return null;
        final ConsCell ccList = requireCons("assoc", maybeList);
        for (Object entry: ccList) {
            if (entry != null) { // ignore null items
                final ConsCell ccEntry = (ConsCell) entry;
                if (eql(atom, car(ccEntry))) return ccEntry;
            }
        }
        return null;
    }

    /** append args non destructively, all args except the last are shallow copied, all args except the last must be a list */
    // todo CL macht deep copy bei allen args ausser dem letzten, alle args ausser dem letzten muessen proper lists sein (murmel behandelt dotted und proper lists gleich)
    private Object append(ConsCell args) {
        if (args == null) return null;
        if (cdr(args) == null) return car(args);
        if (!listp(car(args))) throw new LambdaJError(true, "append: first argument %s is not a list", car(args));

        while (args != null && car(args) == null) args = (ConsCell)cdr(args); // skip leading nil args if any

        ConsCell current = args;
        CountingListBuilder lb = null;
        for (; cdr(current) != null; current = (ConsCell)cdr(current)) {
            final Object o = car(current);
            if (o == null) continue;
            if (!consp(o)) throw new LambdaJError(true, "append: argument is not a list: %s", printSEx(o));
            if (lb == null) lb = new CountingListBuilder();
            for (Object obj: (ConsCell)o) lb.append(obj);
        }
        if (lb == null) return car(args);
        lb.appendLast(car(current));
        return lb.first();
    }

    /** Create a new list by copying lhs and appending rhs. Faster (?) 2 argument version of {@link #append(ConsCell)} for internal use. */
    private Object append2(Object lhs, Object rhs) {
        if (lhs == null) return rhs;
        if (!consp(lhs)) throw new LambdaJError(true, "append2: first argument %s is not a list", lhs);
        if (rhs == null) return lhs;
        ConsCell ret = null, insertPos = null;
        for (Object o: (ConsCell)lhs) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                insertPos.rplacd(cons(o, null));
                insertPos = (ConsCell) insertPos.cdr();
            }
        }
        insertPos.rplacd(rhs);
        return ret;
    }

    static Number cl_signum(Object n) {
        if (n instanceof Double) return Math.signum((Double)n);
        if (n instanceof Long)       { return (long)Long.signum((Long)n); } 
        if (n instanceof Byte)       { return (long)Integer.signum((int) (Byte)n); }
        if (n instanceof Short)      { return (long)Integer.signum((int) (Short)n); }
        if (n instanceof Integer)    { return (long)Integer.signum((Integer)n); }
        //if (n instanceof BigInteger) { return ((BigInteger)n).signum(); }
        //if (n instanceof BigDecimal) { return ((BigDecimal)n).signum(); }

        return Math.signum(toDouble("signum", n));
    }

    /** produce a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer
     *  of the same sign as the mathematical quotient,
     *  and that has the greatest integral magnitude not greater than that of the mathematical quotient. */
    static double cl_truncate(double d) {
        return d < 0.0 ? Math.ceil(d) : Math.floor(d);
    }

    /** note that the Java modulo operator {@code %} works differently */
    static double cl_mod(double x, double y) {
        return x - Math.floor(x / y) * y;
    }

    static Number inc(Object n) {
        if (n instanceof Double) return ((Double)n) + 1;
        if (n instanceof Long) {
            final long l;
            if ((l = (Long) n) == Long.MAX_VALUE) throw new LambdaJError("1+: overflow");
            return l + 1;
        }
        if (n instanceof Byte) return ((Byte)n).longValue() + 1;
        if (n instanceof Short) return ((Short)n).longValue() + 1;
        if (n instanceof Integer) return ((Integer)n).longValue() + 1;
        return toDouble("1+", n) + 1;
    }

    static Number dec(Object n) {
        if (n instanceof Double) return ((Double)n) - 1;
        if (n instanceof Long) {
            final long l;
            if ((l = (Long) n) == Long.MIN_VALUE) throw new LambdaJError("1-: underflow");
            return l - 1;
        }
        if (n instanceof Byte) return ((Byte)n).longValue() - 1;
        if (n instanceof Short) return ((Short)n).longValue() - 1;
        if (n instanceof Integer) return ((Integer)n).longValue() - 1;
        return toDouble("1-", n) - 1;
    }


    /// ###  Misc. helpers and printing of S-expressions

    /** convert a (possibly empty aka nil/ null) list to a (possibly empty) Object[] */
    static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return EMPTY_ARRAY;
        if (maybeList instanceof ArraySlice) {
            return ((ArraySlice)maybeList).listToArray();
        }
        if (!consp(maybeList)) throw new LambdaJError(true, "%s: expected argument to be a list but got %s", "listToArray", printSEx(maybeList));
        final List<Object> ret = new ArrayList<>();
        ((ConsCell) maybeList).forEach(ret::add); // todo forEach behandelt dotted und proper lists gleich -> im interpreter gibt (apply < '(1 2 3 4 . 5)) einen fehler, im compiler nicht
        //for (Object rest = maybeList; rest != null; rest = cdr(rest)) ret.add(car(rest));
        return ret.toArray();
    }

    /** transform {@code obj} into an S-expression, atoms are escaped */
    static String printSEx(Object obj) {
        return printSEx(obj, true);
    }

    static String printSEx(Object obj, boolean printEscape) {
        if (obj == null) return "nil";
        final StringBuilder sb = new StringBuilder();
        _printSEx(sb::append, obj, obj, true, printEscape);
        return sb.toString();
    }

    static void printSEx(WriteConsumer w, Object obj) {
        _printSEx(w, obj, obj, true, true);
    }

    static void printSEx(WriteConsumer w, Object obj, boolean printEscape) {
        _printSEx(w, obj, obj, true, printEscape);
    }

    static void _printSEx(WriteConsumer sb, Object list, Object obj, boolean headOfList, boolean escapeAtoms) {
        while (true) {
            if (obj == null) {
                sb.print("nil"); return;
            } else if (obj instanceof ArraySlice) {
                sb.print(((ArraySlice)obj).printSEx(headOfList, escapeAtoms)); return;
            } else if (consp(obj)) {
                if (headOfList) sb.print("(");
                final Object first = car(obj);
                if (first == list) {
                    sb.print(headOfList ? "#<this cons>" : "#<this list>");
                } else {
                    _printSEx(sb, first, first, true, escapeAtoms);
                }
                final Object rest = cdr(obj);
                if (rest != null) {
                    if (consp(rest)) {
                        sb.print(" ");
                        if (list == rest) {
                            sb.print("#<circular list>)"); return;
                        } else {
                            obj = rest; headOfList = false; continue;
                        }
                    } else {
                        sb.print(" . ");
                        _printSEx(sb, list, rest, false, escapeAtoms);
                        sb.print(")");
                        return;
                    }
                } else {
                    sb.print(")");
                    return;
                }
            } else if (obj instanceof Writeable) {
                ((Writeable) obj).printSEx(sb, escapeAtoms); return;
            } else if (escapeAtoms && stringp(obj)) {
                sb.print("\""); sb.print(escapeString(obj.toString())); sb.print("\""); return;
            } else if (escapeAtoms && characterp(obj)) {
                sb.print(printChar((int)(Character)obj)); return;
            } else {
                sb.print(obj.toString()); return;
            }
        }
    }

    static String printChar(int c) {
        return "#\\"
         + (c < CTRL.length ? CTRL[c]
        : c < 127 ? String.valueOf((char)c)
        : String.valueOf(c));
    }

    /** prepend " and \ by a \ */
    static String escapeString(String s) {
        if (s == null) return null;
        if (s.isEmpty()) return "";

        final StringBuilder ret = new StringBuilder();
        final int len = s.length();
        for (int i = 0; i < len; i++) {
            final char c = s.charAt(i);
            switch (c) {
            case '\"':  ret.append("\\\""); break;
            case '\\': ret.append("\\\\"); break;
            default: ret.append(c);
            }
        }
        return ret.toString();
    }



    /// ##  Error "handlers"

    static RuntimeException errorReaderError(String msg) {
        throw new LambdaJError(msg);
    }

    static RuntimeException errorNotImplemented(String msg, Object... args) {
        throw new LambdaJError(true, msg, args);
    }

    static RuntimeException errorInternal(String msg, Object... args) {
        throw new LambdaJError(true, "internal error - " + msg, args);
    }

    static RuntimeException errorInternal(Throwable t, String msg, Object... args) {
        throw new LambdaJError(t, "internal error - " + msg, args);
    }

    static RuntimeException errorMalformed(String func, String msg) {
        throw new LambdaJError(true, "%s: malformed %s: %s", func, func, msg);
    }

    static RuntimeException errorMalformedFmt(String func, String msg, Object... params) {
        return errorMalformed(func, String.format(msg, params));
    }

    static RuntimeException errorMalformed(String func, String expected, Object actual) {
        throw new LambdaJError(true, "%s: malformed %s: expected %s but got %s", func, func, expected, printSEx(actual));
    }

    static void errorReserved(final String op, final Object sym) {
        errorMalformedFmt(op, "can't use reserved word %s as a symbol", sym == null ? "nil" : sym);
    }

    static RuntimeException errorNotANumber(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a number argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorNotAnIntegralNumber(String func, Object n) {
        throw new LambdaJError(true, "%s: expected an integral number argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorOverflow(String func, String targetType, Object n) {
        throw new LambdaJError(true, "%s: value cannot be represented as a %s: %s", func, targetType, String.valueOf(n));
    }

    static void errorArgCount(String func, int expectedMin, int expectedMax, int actual, Object form) {
        final String argPhrase = expectedMin == expectedMax
                ? expectedArgPhrase(expectedMin)
                : expectedMin + " to " + expectedMax + " arguments";

        if (actual < expectedMin) {
            throw new LambdaJError(true, "%s: expected %s but %s", func, argPhrase, actualArgPhrase(actual));
        }
        if (actual > expectedMax) {
            throw new LambdaJError(true, "%s: expected %s but got extra arg(s) %s", func, argPhrase, printSEx(nthcdr(expectedMax, form)));
        }
        assert false: "errorArgCount was called, but there is no error";
    }

    static void errorVarargsCount(String func, int min, int actual) {
        throw new LambdaJError(true, "%s: expected %s or more but %s", func, expectedArgPhrase(min), actualArgPhrase(actual));
    }

    private static String expectedArgPhrase(int expected) {
        return expected == 0 ? "no arguments" : expected == 1 ? "one argument" : expected == 2 ? "two arguments" : expected + " arguments";
    }

    private static String actualArgPhrase(int actual) {
        return actual == 0 ? "no argument was given" : actual == 1 ? "only one argument was given" : "got only " + actual;
    }



    /// ##  Error checking functions, used by interpreter and primitives

    /** a must be the empty list */
    private static void noArgs(String func, ConsCell a) {
        if (a != null) errorArgCount(func, 0, 0, 1, a);
    }

    /** ecactly one argument */
    static void oneArg(String func, ConsCell a) {
        if (a == null)      errorArgCount(func, 1, 1, 0, null);
        if (cdr(a) != null) errorArgCount(func, 1, 1, 2, a);
    }

    /** ecactly two arguments */
    static void twoArgs(String func, ConsCell a) {
        if (a == null) errorArgCount(func, 2, 2, 0, null);
        Object _a = cdr(a);
        if (_a == null) errorArgCount(func, 2, 2, 1, a);
        _a = cdr(_a);
        if (_a != null) errorArgCount(func, 2, 2, 3, a);
    }

    /** varargs, at least one arg */
    private static void varargs1(String func, Object a) {
        if (!consp(a)) errorMalformed(func, "an argument list", a); // todo consp check should probably done at callsite
        varargs1(func, (ConsCell)a);
    }
    static void varargs1(String func, ConsCell a) {
        if (a == null) errorVarargsCount(func, 1, 0);
    }

    static void varargs1_2(String func, ConsCell a) {
        if (a == null || cddr(a) != null) errorArgCount(func, 1, 2, length(a), a);
    }

    /** varargs, at least {@code min} args */
    static void varargsMin(String func, ConsCell a, int min) {
        final Object x = nthcdr(min-1, a);
        if (x == null) errorVarargsCount(func, min, length(a));
    }

    /** varargs, between {@code min} and {@code max} args */
    static void varargsMinMax(String func, ConsCell a, int min, int max) {
        if (min == 0 && a == null) return;
        final Object x = nthcdr(min-1, a);
        final int n = min == 0 ? 0 : min-1;
        if (x == null || nthcdr(max-n, x) != null) errorArgCount(func, min, max, length(a), a);
    }

    /** check that 'a' is a symbol or a proper or dotted list of only symbols (empty list is fine, too).
     *  Also 'a' must not contain reserved symbols. */
    private void symbolArgs(String func, Object a) {
        if (symbolp(a)) return;
        if (atom(a)) errorMalformed(func, "bindings to be a symbol or list of symbols", a);
        final ConsCell start = (ConsCell) a;
        for (;;) {
            if (consp(a) && cdr(a) == start) errorMalformed(func, "circular list of bindings is not allowed");
            if (!symbolp(car(a))) errorMalformed(func, "a symbol or a list of symbols", a);
            notReserved(func, (LambdaJSymbol)car(a));

            a = cdr(a);
            if (a == null) return; // end of a proper list, everything a-ok, move along
            if (atom(a)) {
                if (!symbolp(a)) errorMalformed(func, "a symbol or a list of symbols", a);
                notReserved(func, (LambdaJSymbol)a);
                return; // that was the end of a dotted list, everything a-ok, move along
            }
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


    /** at least one arg, the first arg must be a non-nil string */
    private static void stringArg(String func, String arg, ConsCell a) {
        if (!stringp(car(a)))
            throw new LambdaJError(true, "%s: expected %s to be a string but got %s", func, arg, printSEx(car(a)));
    }

    /** Return {@code a} as a TurtleFrame or current_frame if null, error if {@code a} is not of type frame. */
    private TurtleFrame requireFrame(String func, Object a) {
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


    /** convert {@code a} to a float, error if {@code a} is not a number and/ or cannot be represented as a float (reducing precision is allowed). */
    private static float toFloat(String func, Object a) {
        final Number n = requireNumber(func, a);

        final float ret = n.floatValue();
        if (n instanceof BigInteger || n instanceof BigDecimal) {
            if (Float.isNaN(ret)) errorOverflow(func, "float", a);
            return ret;
        }
        final double dbl = n.doubleValue();
        if (dbl > Float.MAX_VALUE || dbl < Float.MIN_VALUE) errorOverflow(func, "float", a);
        return ret;
    }

    /** convert {@code a} to a double, error if {@code a} is not a number and/ or cannot be represented as a float (reducing precision is allowed). */
    static double toDouble(String func, Object a) {
        final Number n = requireNumber(func, a);

        final double ret = n.doubleValue();
        if (n instanceof BigInteger || n instanceof BigDecimal) {
            if (Double.isNaN(ret)) errorOverflow(func, "double", a);
            return ret;
        }
        return ret;
    }

    /** convert {@code a} to an int, error if {@code a} is not a number. */
    private static int toInt(String func, Object a) {
        return requireIntegralNumber(func, a, Integer.MIN_VALUE, Integer.MAX_VALUE).intValue();
    }

    private static Number requireNumberOrNull(String func, Object a) {
        if (a == null) return null;
        return requireNumber(func, a);
    }

    /** error if n is not of type number */
    static Number requireNumber(String func, Object n) {
        if (n instanceof Long)    return (Long)n;
        if (n instanceof Double)  return (Double) n;
        if (n instanceof Number)  return (Number)n;
        throw errorNotANumber(func, n);
    }

    static Number requireIntegralNumber(String func, Object n, long minIncl, long maxIncl) {
        if (n == null) errorNotANumber(func, null);
        if (n instanceof Long)    return (Long)n;
        if (n instanceof Double)  { return requireIntegralNumber(func, (Double) n, n, minIncl, maxIncl); }
        if (n instanceof Byte)    return (Byte)n;
        if (n instanceof Short)   return (Short)n;
        if (n instanceof Integer) return (Integer)n;
        if (n instanceof Float)   { return requireIntegralNumber(func, (double) (Float) n, n, minIncl, maxIncl); }
        if (n instanceof Number)  { return requireIntegralNumber(func, toDouble(func, n), n, minIncl, maxIncl); }
        throw errorNotANumber(func, n);
    }

    private static Number requireIntegralNumber(String func, double d, Object originalValue, long minIncl, long maxIncl) {
        // see https://stackoverflow.com/questions/9898512/how-to-test-if-a-double-is-an-integer
        if (Math.rint(d) == d && !Double.isInfinite(d) && d >= minIncl && d <= maxIncl) return d;
        throw errorNotAnIntegralNumber(func, originalValue);
    }


    /** Return {@code c} as a Character, error if {@code c} is not a Character. */
    private static Character requireChar(String func, Object c) {
        if (!(c instanceof Character)) throw new LambdaJError(true, "%s: expected a character argument but got %s", func, printSEx(c));
        return (Character)c;
    }

    /** Return {@code c} as a String, error if {@code c} is not a string, character or symbol. */
    static String requireStringOrNull(String func, Object c) {
        if (c == null) return null;
        return requireString(func, c);
    }

    // todo requireString steht im widerspruch zu stringp
    private static String requireString(String func, Object c) {
        if (!(c instanceof String) && !(c instanceof Character) && !(c instanceof LambdaJSymbol)) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(c));
        return c.toString();
    }

    /** Return {@code a} cast to a list, error if {@code a} is not a list or is nil. */
    static ConsCell requireCons(String func, Object a) {
        if (!consp(a)) throw new LambdaJError(true, "%s: expected a cons argument but got %s", func, printSEx(a));
        return (ConsCell)a;
    }

    static LambdaJSymbol requireNonNilSymbol(String func, Object a) {
        if (a == null || !symbolp(a)) throw new LambdaJError(true, "%s: expected a nonnil symbol argument but got %s", func, printSEx(a));
        return (LambdaJSymbol)a;
    }

    /** Return {@code a} cast to a list, error if {@code a} is not a list or is nil. */
    static ConsCell requireList(String func, Object a) {
        if (a == null) return null;
        if (!consp(a)) throw new LambdaJError(true, "%s: expected a list argument but got %s", func, printSEx(a));
        return (ConsCell)a;
    }



    /// Runtime for Lisp programs, i.e. an environment with primitives and predefined global symbols

    private Object boolResult(boolean b) { return b ? expTrue.get() : null; }

    interface DoubleBiPred {
        boolean test(double d1, double d2);
    }

    /** compare subsequent pairs of the given list of numbers with the given predicate */
    private Object compare(ConsCell args, String opName, DoubleBiPred pred) {
        varargs1(opName, args);
        Object prev = car(args);
        for (ConsCell rest = (ConsCell)cdr(args); rest != null; rest = (ConsCell)cdr(rest)) {
            final Object next = car(rest);
            if (!pred.test(toDouble(opName, prev), toDouble(opName, next))) return null;
            prev = next;
        }
        return expTrue.get();
    }

    /** operator for zero or more args */
    private static Object addOp(ConsCell _args, String opName, double startVal, DoubleBinaryOperator op) {
        if (car(_args) == null) return startVal;
        ConsCell args = _args;
        double result = toDouble(opName, car(args));

        for (;;) {
            final Object next = cdr(args);
            if (!listp(next) || next == _args) // todo nested loop check
                throw new LambdaJError(true, "%s: expected a proper list of numbers but got %s", opName, printSEx(_args));
            args = (ConsCell) next;
            if (args == null) break;
            result = op.applyAsDouble(result, toDouble(opName, car(args)));
        }
        return result;
    }

    /** operator for one or more args */
    private static Object subOp(ConsCell _args, String opName, double startVal, DoubleBinaryOperator op) {
        varargs1(opName, _args);
        ConsCell args = _args;
        double result = toDouble(opName, car(args));

        if (cdr(args) == null) return op.applyAsDouble(startVal, result);

        for (;;) {
            final Object next = cdr(args);
            if (!listp(next) || next == args) // todo nested loop check
                throw new LambdaJError(true, "%s: expected a proper list of numbers but got %s", opName, printSEx(_args));
            args = (ConsCell) next;
            if (args == null) break;
            result = op.applyAsDouble(result, toDouble(opName, car(args)));
        }
        return result;
    }

    private static double quot12(String func, ConsCell args) {
        final double lhs = toDouble(func, car(args));
        return cdr(args) == null ? lhs : lhs / toDouble(func, cadr(args));
    }

    /** return the argument w/o decimal places as a long, exception if conversion is not possible */
    // todo assert Math.rint(d) == d vgl #requireIntegralNumber()
    static long checkedToLong(double d) {
        if (Double.isInfinite(d)) throw new LambdaJError("value is Infinite");
        if (Double.isNaN(d)) throw new LambdaJError("value is NaN");
        if (d < Long.MIN_VALUE) throw new LambdaJError("underflow");
        if (d > Long.MAX_VALUE) throw new LambdaJError("overflow");
        return (long)d;
    }



    private static ConsCell cl_rplaca(ConsCell args) {
        twoArgs("rplaca", args);
        return requireCons("rplaca", car(args)).rplaca(cadr(args));
    }

    private static ConsCell cl_rplacd(ConsCell args) {
        twoArgs("rplacd", args);
        return requireCons("rplacd", car(args)).rplacd(cadr(args));
    }

    private static Object listToString(ConsCell a) {
        oneArg("list->string", a);
        return listToStringImpl(requireList("list->string", car(a)));
    }

    static String listToStringImpl(ConsCell l) {
        if (l == null) return null;
        final StringBuilder ret = new StringBuilder();
        for (Object c: l) ret.append(requireChar("list->string", c)); // todo cyclecheck
        return ret.toString();
    }

    private Object stringToList(ConsCell a) {
        oneArg("string->list", a);
        final String s = requireStringOrNull("string->list", car(a));
        if (s == null) return null;
        final CountingListBuilder ret = new CountingListBuilder();
        final int len = s.length();
        for (int i = 0; i < len; i++) ret.append(s.charAt(i));
        return ret.first();
    }

    private Object listStar(ConsCell args) {
        varargs1("list*", args);
        if (cdr(args) == null) return car(args);
        if (cddr(args) == null) return cons(car(args), cadr(args));
        final CountingListBuilder b = new CountingListBuilder();
        for (; cdr(args) != null; args = (ConsCell)cdr(args)) {
            b.append(car(args));
        }
        b.appendLast(car(args));
        return b.first();
    }



    final void write(final Object arg, boolean printEscape) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "write");
        lispPrinter.printObj(arg, printEscape);
    }

    final void writeln(final ConsCell arg, boolean printEscape) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "writeln");
        if (arg != null) {
            lispPrinter.printObj(car(arg), printEscape);
        }
        lispPrinter.printEol();
    }

    final void lnwrite(final ConsCell arg, boolean printEscape) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "lnwrite");
        lispPrinter.printEol();
        if (arg != null) {
            lispPrinter.printObj(car(arg), printEscape);
            lispPrinter.printString(" ");
        }
    }

    final String format(ConsCell a) {
        return format(false, a);
    }

    final String formatLocale(ConsCell a) {
        return format(true, a);
    }

    final String format(boolean locale, ConsCell a) {
        final String func = locale ? "format-locale" : "format";
        varargsMin(func, a, locale ? 3 : 2);
        final boolean toString = car(a) == null;
        a = (ConsCell) cdr(a);

        final String locString;
        if (locale) {
            if (car(a) != null) {
                stringArg(func, "first argument", a);
                locString = (String)car(a);
            } else locString = null;
            a = (ConsCell)cdr(a);
        }
        else locString = null;

        stringArg(func, locale ? "third argument" : "second argument", a);
        final String s = (String) car(a);
        final Object[] args = listToArray(cdr(a));
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
            throw new LambdaJError(true, "%s: illegal format string and/ or arguments: %s" + System.lineSeparator() + "error ocurred processing the argument(s) %s", func, e.getMessage(), printSEx(a));
        }
    }



    static long getInternalRealTime(ConsCell a) {
        noArgs("get-internal-real-time", a);
        return System.nanoTime();
    }

    static long getInternalRunTime(ConsCell a) {
        noArgs("get-internal-run-time", a);
        return getThreadBean("get-internal-run-time").getCurrentThreadUserTime();
    }

    static long getInternalCpuTime(ConsCell a) {
        noArgs("get-internal-cpu-time", a);
        return getThreadBean("get-internal-cpu-time").getCurrentThreadCpuTime();
    }

    private static ThreadMXBean getThreadBean(final String func) {
        final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
        if (threadBean == null)
            throw new LambdaJError(true, "%s: ThreadMXBean not supported in this Java Runtime", func);
        if (!threadBean.isCurrentThreadCpuTimeSupported())
            throw new LambdaJError(true, "%s: ThreadMXBean.getCurrentThreadCpuTime() not supported in this Java Runtime", func);
        return threadBean;
    }

    static Object sleep(ConsCell a) {
        oneArg("sleep", a);
        try {
            final long millis = (long)(toDouble("sleep", car(a)) * 1e3D);
            Thread.sleep(millis);
            return null;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new LambdaJError("sleep: got interrupted");
        }
    }

    static long getUniversalTime(ConsCell a) {
        noArgs("get-universal-time", a);
        final ZoneId utc = ZoneId.of("UTC");
        final ZonedDateTime ld1900 = ZonedDateTime.of(1900, 1, 1, 0, 0, 0, 0, utc);
        return ld1900.until(ZonedDateTime.now(utc), ChronoUnit.SECONDS);
    }

    final Object getDecodedTime(ConsCell a) {
        noArgs("get-decoded-time", a);
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

    /** expand a single macro call */
    final Object macroexpand1(ConsCell args) {
        oneArg("macroexpand-1", args);
        final Object maybeMacroCall = car(args);
        if (!consp(maybeMacroCall)) return maybeMacroCall;
        return macroexpandImpl((ConsCell) maybeMacroCall);
    }

    final Object macroexpandImpl(ConsCell args) {
        final LambdaJSymbol macroSymbol = requireNonNilSymbol("macroexpand-1", car(args));
        final ConsCell macroClosure = macroSymbol.macro;
        if (macroClosure == null) {
            values = cons(args, (cons(null, null)));
            return args;
        }
        final ConsCell arguments = (ConsCell) cdr(args);
        final Object expansion = evalMacro(macroSymbol, macroClosure, arguments, 0, 0, 0);
        values = cons(expansion, (cons(sT, null)));
        return expansion;
    }

    private int gensymCounter;
    final Object gensym(ConsCell args) {
        noArgs("gensym", args);
        return new LambdaJSymbol("#:g" + ++gensymCounter);
    }



    /// Murmel runtime support for Java FFI - Murmel calls Java
    private static class JavaConstructor implements Primitive, MurmelJavaProgram.CompilerPrimitive {
        private final Constructor<?> constructor;
        private final UnaryOperator<Object>[] argConv;

        private JavaConstructor(Constructor<?> constructor, Class<?>[] params) {
            this.constructor = constructor;
            final int paramCount = constructor.getParameterCount();

            argConv = new UnaryOperator[paramCount];
            if (params != null) for (int i = 0; i < paramCount; i++) {
                final String paramClassName = params[i].getName();
                final Object[] entry = classByName.get(paramClassName);
                if (entry != null) argConv[i] = (UnaryOperator<Object>) entry[2];
            }
        }

        @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
        @Override public String toString() { return "#<Java constructor: " + constructor.getName() + '>'; }

        @Override public Object applyPrimitive(ConsCell x) { return applyCompilerPrimitive(listToArray(x)); }

        @Override
        public Object applyCompilerPrimitive(Object... args) {
            final String name = "new " + constructor.getDeclaringClass().getName();
            javaCallArgCheck(name, argConv, args);
            try { return constructor.newInstance(args); }
            catch (InvocationTargetException ite) { throw new LambdaJError(true, "%s: %s", name, ite.getTargetException().toString()); }
            catch (Exception e)                   { throw new LambdaJError(true, "%s: %s", name, e.toString()); }
        }
    }

    private static class JavaMethod implements Primitive, MurmelJavaProgram.CompilerPrimitive {
        @FunctionalInterface private interface Invoker { Object invoke(Object... args) throws Throwable; }

        private final Method method;
        private final Invoker invoke;
        private final UnaryOperator<Object>[] argConv;

        private JavaMethod(Method method, Class<?>[] params) {
            this.method = method;
            int paramCount = method.getParameterCount();
            final boolean isStatic = Modifier.isStatic(method.getModifiers());
            if (!isStatic) paramCount++; // this + parameters

            final int skipThis = isStatic ? 0 : 1;
            argConv = new UnaryOperator[paramCount];
            if (params != null) for (int i = 0; i < paramCount-skipThis; i++) {
                final String paramClassName = params[i].getName();
                final Object[] entry = classByName.get(paramClassName);
                if (entry != null) argConv[i+skipThis] = (UnaryOperator<Object>) entry[2];
            }

            try {
                final MethodHandle mh = MethodHandles.publicLookup().unreflect(method);
                switch (paramCount) {
                    case 0:  invoke = args -> mh.invoke();  break;
                    case 1:  invoke = args -> mh.invoke(args[0]);  break;
                    case 2:  invoke = args -> mh.invoke(args[0], args[1]);  break;
                    case 3:  invoke = args -> mh.invoke(args[0], args[1], args[2]);  break;
                    case 4:  invoke = args -> mh.invoke(args[0], args[1], args[2], args[3]);  break;
                    case 5:  invoke = args -> mh.invoke(args[0], args[1], args[2], args[3], args[4]);  break;
                    case 6:  invoke = args -> mh.invoke(args[0], args[1], args[2], args[3], args[4], args[5]);  break;
                    case 7:  invoke = args -> mh.invoke(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);  break;
                    default: invoke = mh::invokeWithArguments; // that's slow
                }
            }
            catch (IllegalAccessException iae) {
                throw new LambdaJError(iae, "can not access ", method.getDeclaringClass().getSimpleName(), method.getName());
            }
        }

        @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
        @Override public String toString() { return "#<Java method: " + method.getDeclaringClass().getName() + '.' + method.getName() + '>'; }

        @Override public Object applyPrimitive(ConsCell x) { return applyCompilerPrimitive(listToArray(x)); }

        @Override
        public Object applyCompilerPrimitive(Object... args) {
            final Method method = this.method;
            javaCallArgCheck(method.getName(), argConv, args);
            if (!Modifier.isStatic(method.getModifiers()) && !method.getDeclaringClass().isInstance(args[0]))
                throw new LambdaJError(true, "jmethod: %s is not an instance of class %s", args[0], method.getDeclaringClass().getName());

            try { return invoke.invoke(args); }
            catch (Throwable t) { throw new LambdaJError(true, "%s.%s: %s", method.getDeclaringClass().getName(), method.getName(), t.toString()); }
        }
    }

    /** check the number of args vs. number of parameters, and then convert argument types from Murmel to Java */
    static void javaCallArgCheck(String name, UnaryOperator<Object>[] argConv, Object[] args) {
        final int paramCount = argConv.length;
        final int argCount = args == null ? 0 : args.length;
        if (paramCount != argCount) errorArgCount(name, paramCount, paramCount, argCount, null);

        if (args != null) for (int i = 0; i < argCount; i++) {
            final UnaryOperator<Object> conv = argConv[i];
            if (conv != null) args[i] = conv.apply(args[i]);
        }
    }

    /** find a constructor, static or instance method from the given class with the given name and parameter classes if any. */
    static Primitive findMethod(String className, String methodName, Iterable<?> paramClassNames) {
        final ArrayList<Class<?>> paramClasses = new ArrayList<>(10);
        if (paramClassNames != null) for (Object paramClassName: paramClassNames) {
            final String strParamClassName = (String)paramClassName;
            try { paramClasses.add(findClass(strParamClassName)); }
            catch (ClassNotFoundException e) { throw new LambdaJError(true, "jmethod: exception finding parameter class %s: %s", strParamClassName, e.toString()); }
        }
        final Class<?>[] params = paramClasses.isEmpty() ? null : paramClasses.toArray(EMPTY_CLASS_ARRAY);
        try {
            final Class<?> clazz = findClass(className);
            return "new".equals(methodName)
                    ? new JavaConstructor(clazz.getDeclaredConstructor(params), params)
                    : new JavaMethod(clazz.getMethod(methodName, params), params);
        }
        catch (Exception e) { throw new LambdaJError(true, "jmethod: exception finding method: %s", e.getMessage()); }
    }

    static final Map<String, Object[]> classByName = new HashMap<>(64);
    static {
        classByName.put("boolean",   new Object[] { boolean.class,   "toBoolean",   (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });
        classByName.put("byte",      new Object[] { byte.class,      "toByte",      (UnaryOperator<Object>)(MurmelJavaProgram::toByte)});
        classByName.put("short",     new Object[] { short.class,     "toShort",     (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });
        classByName.put("int",       new Object[] { int.class,       "toInt",       (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });
        classByName.put("long",      new Object[] { long.class,      "toLong",      (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });
        classByName.put("float",     new Object[] { float.class,     "toFloat",     (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });
        classByName.put("double",    new Object[] { double.class,    "toDouble",    (UnaryOperator<Object>)(MurmelJavaProgram::toDouble)});

        classByName.put("char",      new Object[] { char.class,      "requireChar", (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });

        // todo die boxed typen sollten nil/null als null durchreichen, Object/Number/String reichen null bereits durch, Boolean passt wsl auch
        classByName.put("Object",    new Object[] { Object.class,    null,                   null });                                                           aliasJavaLang("Object");
        classByName.put("Number",    new Object[] { Number.class,    "requireNumberOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireNumberOrNull) }); aliasJavaLang("Number");
        classByName.put("Boolean",   new Object[] { Boolean.class,   "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });           aliasJavaLang("Boolean");
        classByName.put("Byte",      new Object[] { Byte.class,      "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte) });              aliasJavaLang("Byte");
        classByName.put("Short",     new Object[] { Short.class,     "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });             aliasJavaLang("Short");
        classByName.put("Integer",   new Object[] { Integer.class,   "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });               aliasJavaLang("Integer");
        classByName.put("Long",      new Object[] { Long.class,      "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });              aliasJavaLang("Long");
        classByName.put("Float",     new Object[] { Float.class,     "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });             aliasJavaLang("Float");
        classByName.put("Double",    new Object[] { Double.class,    "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble) });            aliasJavaLang("Double");

        classByName.put("Character", new Object[] { Character.class, "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });         aliasJavaLang("Character");
        classByName.put("String",    new Object[] { String.class,    "requireStringOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireStringOrNull) }); aliasJavaLang("String");
    }

    private static void aliasJavaLang(String existing) {
        classByName.put("java.lang." + existing, classByName.get(existing));
    }

    /** find and load the class given by the (possibly abbreviated) name {@code clsName} */
    static Class<?> findClass(String clsName) throws ClassNotFoundException {
        final Object[] entry = classByName.get(clsName);
        if (entry != null) return (Class<?>)entry[0];
        return Class.forName(clsName);
    }

    private static class DynamicProxy implements InvocationHandler {
        private final Map<Method, MurmelFunction> methods;

        public DynamicProxy(Map<Method, MurmelFunction> methods) {
            this.methods = methods;
        }

        @Override
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            final MurmelFunction func = methods.get(method);
            if (func == null) throw new LambdaJError(true, "no function for method %s", method.getName());
            if (args == null) return func.apply();
            else return func.apply(args);
        }

    }

    Object makeProxy(ConsCell args) {
        varargsMin("jproxy", args, 1);
        final String intf = requireString("jproxy", car(args));
        try {
            final Class<?> clazz = findClass(intf);
            final Map<Method, MurmelFunction> methodToMurmelFunction = new HashMap<>();
            final Map<String, Method> nameToMethod = new HashMap<>();

            final MurmelFunction notImplemented = a -> { throw new LambdaJError(false, "method is not implemented"); };
            for (Method m: Object.class.getMethods()) {
                methodToMurmelFunction.put(m, notImplemented);
                nameToMethod.put(m.getName(), m);
            }
            for (Method m: clazz.getMethods()) {
                methodToMurmelFunction.put(m, notImplemented);
                nameToMethod.put(m.getName(), m);
            }

            final String asString = "#<Java proxy: " + clazz.getName() + ">";
            methodToMurmelFunction.put(nameToMethod.get("toString"), a -> asString);
            methodToMurmelFunction.put(Writeable.class.getMethod("printSEx", WriteConsumer.class, boolean.class),
                                       a -> {final WriteConsumer out = (WriteConsumer) a[0]; out.print(asString); return null;});

            for (ConsCell lst = requireList("jproxy", cdr(args)); lst != null; ) {
                if (cdr(lst) == null) throw new LambdaJError(false, "jproxy: odd number of method/functions");
                final String name = requireString("jproxy", car(lst));

                final Object form = cadr(lst);
                if (form == null) throw new LambdaJError(true, "jproxy: not a function: nil");
                final Method method = nameToMethod.get(name);
                if (method == null) throw new LambdaJError(true, "jproxy: method %s does not exist in interface %s or is not accessible", name, intf);
                methodToMurmelFunction.put(method, getFunction(null, form));

                lst = (ConsCell)cddr(lst);
            }
            return Proxy.newProxyInstance(LambdaJ.class.getClassLoader(), new Class<?>[] { clazz, Writeable.class }, new DynamicProxy(methodToMurmelFunction));
        }
        catch (ClassNotFoundException | NoSuchMethodException e) {
            throw new LambdaJError(true, "exception loading class %s", intf);
        }
    }

    ConsCell values;

    TurtleFrame current_frame;

    private ObjectReader lispReader;
    private ObjectWriter lispPrinter;

    /** return the current stdin */
    public ObjectReader getLispReader()  { return lispReader; }

    /** return the current stdout */
    public ObjectWriter getLispPrinter() { return lispPrinter; }

    /** set new stdin/stdout */
    public void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) {
        this.lispReader = lispStdin;
        this.lispPrinter = lispStdout;
    }


    /** build an environment by prepending the previous environment {@code env} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@link #symtab} on the fly */
    private void environment(ConsCell env) {
        if (haveIO()) {
            final Primitive freadobj =  a -> {
                noArgs("read", a);
                if (lispReader == null) throw new LambdaJError(true, "%s: lispStdin is nil", "read");
                return lispReader.readObj(null); // todo eof als parameter
            };
            env = addBuiltin("read",    freadobj,
                  addBuiltin("write",   (Primitive) a -> { varargsMinMax("write",   a, 1, 2);  write  (car(a), cdr(a) == null || cadr(a) != null);  return expTrue.get(); },
                  addBuiltin("writeln", (Primitive) a -> { varargsMinMax("writeln", a, 0, 2);  writeln(a,      cdr(a) == null || cadr(a) != null);  return expTrue.get(); },
                  addBuiltin("lnwrite", (Primitive) a -> { varargsMinMax("lnwrite", a, 0, 2);  lnwrite(a,      cdr(a) == null || cadr(a) != null);  return expTrue.get(); },
                  env))));
        }

        if (haveGui()) {
            final Primitive makeFrame = a -> {
                varargsMinMax("make-frame", a, 1, 4);
                final String title = requireStringOrNull("make-frame", car(a));
                final TurtleFrame ret = new TurtleFrame(title, requireNumberOrNull("make-frame", cadr(a)), requireNumberOrNull("make-frame", caddr(a)), requireNumberOrNull("make-frame", cadddr(a)));
                current_frame = ret;
                return ret;
            };
            env = addBuiltin("make-frame",    makeFrame,
                  addBuiltin("open-frame",    (Primitive) a -> { varargsMinMax("open-frame",    a, 0, 1); return requireFrame("open-frame",    car(a)).open();    },
                  addBuiltin("close-frame",   (Primitive) a -> { varargsMinMax("close-frame",   a, 0, 1); return requireFrame("close-frame",   car(a)).close();   },
                  addBuiltin("reset-frame",   (Primitive) a -> { varargsMinMax("reset-frame",   a, 0, 1); return requireFrame("reset-frame",   car(a)).reset();   },
                  addBuiltin("clear-frame",   (Primitive) a -> { varargsMinMax("clear-frame",   a, 0, 1); return requireFrame("clear-frame",   car(a)).clear();   },
                  addBuiltin("repaint-frame", (Primitive) a -> { varargsMinMax("repaint-frame", a, 0, 1); return requireFrame("repaint-frame", car(a)).repaint(); },
                  addBuiltin("flush-frame",   (Primitive) a -> { varargsMinMax("flush-frame",   a, 0, 1); return requireFrame("flush-frame",   car(a)).flush();   },

                  // set new current frame, return previous frame
                  addBuiltin("current-frame", (Primitive) a -> { varargsMinMax("current-frame", a, 0, 1); final Object prev = current_frame; if (car(a) != null) current_frame = requireFrame("current-frame", car(a)); return prev; },

                  addBuiltin("push-pos",      (Primitive) a -> { varargsMinMax("push-pos",a, 0, 1); return requireFrame("push-pos",car(a)).pushPos(); },
                  addBuiltin("pop-pos",       (Primitive) a -> { varargsMinMax("pop-pos", a, 0, 1); return requireFrame("pop-pos", car(a)).popPos();  },

                  addBuiltin("pen-up",        (Primitive) a -> { varargsMinMax("pen-up",  a, 0, 1); return requireFrame("pen-up",   car(a)).penUp();   },
                  addBuiltin("pen-down",      (Primitive) a -> { varargsMinMax("pen-down",a, 0, 1); return requireFrame("pen-down", car(a)).penDown(); },

                  addBuiltin("color",         (Primitive) a -> { varargsMinMax("color",   a, 1, 2); return requireFrame("color",   cadr(a)).color  (toInt("color",   car(a))); },
                  addBuiltin("bgcolor",       (Primitive) a -> { varargsMinMax("bgcolor", a, 1, 2); return requireFrame("bgcolor", cadr(a)).bgColor(toInt("bgcolor", car(a))); },

                  addBuiltin("text",          (Primitive) a -> { varargsMinMax("text",    a, 1, 2); return requireFrame("text",    cadr(a)).text   (car(a).toString()); },

                  addBuiltin("right",         (Primitive) a -> { varargsMinMax("right",   a, 1, 2); return requireFrame("right",   cadr(a)).right  (toDouble("right",   car(a))); },
                  addBuiltin("left",          (Primitive) a -> { varargsMinMax("left",    a, 1, 2); return requireFrame("left",    cadr(a)).left   (toDouble("left",    car(a))); },
                  addBuiltin("forward",       (Primitive) a -> { varargsMinMax("forward", a, 1, 2); return requireFrame("forward", cadr(a)).forward(toDouble("forward", car(a))); },
                  env))))))))))))))))));

            env = addBuiltin("move-to",       (Primitive) a -> { varargsMinMax("move-to", a, 2, 3);  return requireFrame("move-to",  caddr(a)).moveTo(toDouble("move-to",  car(a)), toDouble("move-to", cadr(a)));  },
                  addBuiltin("line-to",       (Primitive) a -> { varargsMinMax("line-to", a, 2, 3);  return requireFrame("line-to",  caddr(a)).lineTo(toDouble("line-to",  car(a)), toDouble("line-to", cadr(a)));  },
                  addBuiltin("move-rel",      (Primitive) a -> { varargsMinMax("move-rel", a, 2, 3); return requireFrame("move-rel", caddr(a)).moveRel(toDouble("move-rel", car(a)), toDouble("move-rel", cadr(a))); },
                  addBuiltin("line-rel",      (Primitive) a -> { varargsMinMax("line-rel", a, 2, 3); return requireFrame("line-rel", caddr(a)).lineRel(toDouble("line-rel", car(a)), toDouble("line-rel", cadr(a))); },
                  env))));

            env = addBuiltin("make-bitmap",   (Primitive) a -> { varargsMinMax("make-bitmap",    a, 2, 3); return requireFrame("make-bitmap",    caddr(a)).makeBitmap(toInt("make-bitmap",  car(a)), toInt("make-bitmap", cadr(a))); },
                  addBuiltin("discard-bitmap",(Primitive) a -> { varargsMinMax("discard-bitmap", a, 0, 1); return requireFrame("discard-bitmap", car(a)).discardBitmap(); },
                  addBuiltin("set-pixel",     (Primitive) a -> { varargsMinMax("set-pixel",      a, 3, 4); return requireFrame("set-pixel",      cadddr(a)).setRGB(toInt("set-pixel", car(a)), toInt("set-pixel", cadr(a)), toInt("set-pixel", caddr(a)));  },
                  addBuiltin("rgb-to-pixel",  (Primitive) a -> { varargsMinMax("rgb-to-pixel",   a, 3, 3);
                                                                 return (long)(int)(toInt("rgb-to-pixel", car(a)) << 16
                                                                                  | toInt("rgb-to-pixel", cadr(a)) << 8
                                                                                  | toInt("rgb-to-pixel", caddr(a))); },
                  addBuiltin("hsb-to-pixel",  (Primitive) a -> { varargsMinMax("hsb-to-pixel",   a, 3, 3);
                                                                 return (long)Color.HSBtoRGB(toFloat("hsb-to-pixel", car(a)),
                                                                                             toFloat("hsb-to-pixel", cadr(a)),
                                                                                             toFloat("hsb-to-pixel", caddr(a)));  },
                  env)))));
        }

        if (haveString()) {
            env = addBuiltin("stringp",    (Primitive) a -> { oneArg("stringp", a);    return boolResult(stringp(car(a))); },
                  addBuiltin("characterp", (Primitive) a -> { oneArg("characterp", a); return boolResult(characterp(car(a))); },
                  addBuiltin("char-code",  (Primitive) a -> { oneArg("char-code", a);  return (long) requireChar("char-code", car(a)); },
                  addBuiltin("code-char",  (Primitive) a -> { oneArg("code-char", a);  return (char) toInt("code-char", car(a)); },
                  addBuiltin("string=",    (Primitive) a -> { twoArgs("string=", a);   return boolResult(Objects.equals(requireStringOrNull("string=", car(a)), requireStringOrNull("string=", cadr(a)))); },
                  addBuiltin("string->list", (Primitive) this::stringToList,
                  addBuiltin("list->string", (Primitive) LambdaJ::listToString,
                  env)))))));

            if (haveUtil()) {
                env = addBuiltin("format",        (Primitive) this::format,
                      addBuiltin("format-locale", (Primitive) this::formatLocale,
                      env));
            }
        }

        if (haveApply()) {
            final LambdaJSymbol sApply = intern("apply");
            ocApply = new OpenCodedPrimitive(sApply);
            env = addBuiltin(sApply, ocApply, env);
        }

        if (haveXtra()) {
            env = addBuiltin(sDynamic, sDynamic, env);

            final LambdaJSymbol sEval = intern("eval");
            ocEval = new OpenCodedPrimitive(sEval);
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

            env = addBuiltin("values", (Primitive) a -> { values = a; return car(values); }, env);
        }

        if (haveT()) {
            final LambdaJSymbol sT = internWellknown("t");
            env = addBuiltin(sT, sT, env);
        }

        if (haveNil()) {
            env = addBuiltin(internWellknown("nil"), null, env);
        }

        if (haveUtil()) {
            env = cons(featuresEnvEntry, env);

            env = addBuiltin("consp",   (Primitive) a -> { oneArg("consp",   a);  return boolResult(consp  (car(a))); },
                  addBuiltin("symbolp", (Primitive) a -> { oneArg("symbolp", a);  return boolResult(symbolp(car(a))); },
                  addBuiltin("listp",   (Primitive) a -> { oneArg("listp",   a);  return boolResult(listp  (car(a))); },
                  addBuiltin("null",    (Primitive) a -> { oneArg("null",    a);  return boolResult(car(a) == null); },
                  addBuiltin("assoc",   (Primitive) a -> { twoArgs("assoc",  a);  return assoc(car(a), cadr(a)); },
                  addBuiltin("assq",    (Primitive) a -> { twoArgs("assq",   a);  return assq(car(a), cadr(a)); },
                  addBuiltin("list",    (Primitive) a -> a,
                  addBuiltin("list*",   (Primitive) this::listStar,
                  addBuiltin("append",  (Primitive) this::append,
                  addBuiltin("eql",     (Primitive) a -> { twoArgs("eql",    a);  return boolResult(eql(car(a), cadr(a))); },
                  env))))))))));

            env = addBuiltin("internal-time-units-per-second", 1e9,
                  addBuiltin("get-internal-real-time", (Primitive) LambdaJ::getInternalRealTime,
                  addBuiltin("get-internal-run-time",  (Primitive) LambdaJ::getInternalRunTime, // user
                  addBuiltin("get-internal-cpu-time",  (Primitive) LambdaJ::getInternalCpuTime, // user + system
                  addBuiltin("sleep",                  (Primitive) LambdaJ::sleep,
                  addBuiltin("get-universal-time",     (Primitive) LambdaJ::getUniversalTime, // seconds since 1.1.1900
                  addBuiltin("get-decoded-time",       (Primitive) this::getDecodedTime,
                  env)))))));

            env = addBuiltin("fatal", (Primitive) a -> { oneArg("fatal", a); throw new RuntimeException(String.valueOf(car(a))); }, env);
        }
        
        if (haveFFI()) {
            env = addBuiltin("jmethod", (Primitive) x -> { varargsMin("jmethod", x, 2); return findMethod(requireString("jmethod", car(x)), requireString("jmethod", cadr(x)), requireList("jmethod", cddr(x))); },
                  addBuiltin("jproxy",  (Primitive)this::makeProxy, env));
        }

        if (haveAtom()) {
            env = addBuiltin("atom", (Primitive) a -> { oneArg("atom", a); return boolResult(atom(car(a))); },
                  env);
        }

        if (haveNumbers()) {
            env = addBuiltin("numberp",  (Primitive) args -> { oneArg("numberp", args);  return boolResult(numberp(car(args))); },
                  addBuiltin("floatp",   (Primitive) args -> { oneArg("floatp", args);   return boolResult(floatp(car(args))); },
                  addBuiltin("integerp", (Primitive) args -> { oneArg("integerp", args); return boolResult(integerp(car(args))); },
                  env)));

            env = addBuiltin("pi",      Math.PI,
                  env);

            env = addBuiltin("fround",   (Primitive) args -> { varargsMinMax("fround",   args, 1, 2); return Math.rint  (quot12("fround", args)); },
                  addBuiltin("ffloor",   (Primitive) args -> { varargsMinMax("ffloor",   args, 1, 2); return Math.floor (quot12("ffloor", args)); },
                  addBuiltin("fceiling", (Primitive) args -> { varargsMinMax("fceiling", args, 1, 2); return Math.ceil  (quot12("fceiling", args)); },
                  addBuiltin("ftruncate",(Primitive) args -> { varargsMinMax("ftruncate",args, 1, 2); return cl_truncate(quot12("ftruncate", args)); },

                  addBuiltin("round",   (Primitive) args -> { varargsMinMax("round",   args, 1, 2); return checkedToLong(Math.rint  (quot12("round", args))); },
                  addBuiltin("floor",   (Primitive) args -> { varargsMinMax("floor",   args, 1, 2); return checkedToLong(Math.floor (quot12("floor", args))); },
                  addBuiltin("ceiling", (Primitive) args -> { varargsMinMax("ceiling", args, 1, 2); return checkedToLong(Math.ceil  (quot12("ceiling", args))); },
                  addBuiltin("truncate",(Primitive) args -> { varargsMinMax("truncate",args, 1, 2); return checkedToLong(cl_truncate(quot12("truncate", args))); },
                  env))))))));

            env = addBuiltin("1+",      (Primitive) args -> { oneArg("1+", args); return inc(car(args)); },
                  addBuiltin("1-",      (Primitive) args -> { oneArg("1-", args); return dec(car(args)); },

                  addBuiltin("sqrt",    (Primitive) args -> { oneArg ("sqrt",    args); return Math.sqrt (toDouble("srtq", car(args))); },
                  addBuiltin("log",     (Primitive) args -> { oneArg ("log",     args); return Math.log  (toDouble("log", car(args))); },
                  addBuiltin("log10",   (Primitive) args -> { oneArg ("log10",   args); return Math.log10(toDouble("log10", car(args))); },
                  addBuiltin("exp",     (Primitive) args -> { oneArg ("exp",     args); return Math.exp  (toDouble("exp", car(args))); },
                  addBuiltin("expt",    (Primitive) args -> { twoArgs("expt",    args); return Math.pow  (toDouble("expt", car(args)), toDouble("expt", cadr(args))); },

                  addBuiltin("mod",     (Primitive) args -> { twoArgs("mod",     args); return cl_mod(toDouble("mod", car(args)), toDouble("mod", cadr(args))); },
                  addBuiltin("rem",     (Primitive) args -> { twoArgs("rem",     args); return toDouble("rem", car(args)) % toDouble("rem", cadr(args)); },
                          
                  addBuiltin("signum",  (Primitive) args -> { oneArg("signum", args); return cl_signum(car(args)); },
                  env))))))))));

            env = addBuiltin("=",       (Primitive) args -> compare(args, "=",  (d1, d2) -> d1 == d2),
                  addBuiltin(">",       (Primitive) args -> compare(args, ">",  (d1, d2) -> d1 > d2),
                  addBuiltin(">=",      (Primitive) args -> compare(args, ">=", (d1, d2) -> d1 >= d2),
                  addBuiltin("<",       (Primitive) args -> compare(args, "<",  (d1, d2) -> d1 < d2),
                  addBuiltin("<=",      (Primitive) args -> compare(args, "<=", (d1, d2) -> d1 <= d2),
                  addBuiltin("/=",      (Primitive) args -> compare(args, "/=", (d1, d2) -> d1 != d2),

                  addBuiltin("+",       (Primitive) args -> addOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs),
                  addBuiltin("-",       (Primitive) args -> subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs),
                  addBuiltin("*",       (Primitive) args -> addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs),
                  addBuiltin("/",       (Primitive) args -> subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs),
                  env))))))))));
        }

        if (haveEq()) {
            env = addBuiltin("eq", (Primitive) a -> { twoArgs("eq", a);     return boolResult(car(a) == cadr(a)); },
                  env);
        }

        if (haveCons()) {
            env = addBuiltin(sCar,     (Primitive) a -> { oneArg("car", a);    return caar(a); },
                  addBuiltin(sCdr,     (Primitive) a -> { oneArg("cdr", a);    return cdar(a); },
                  addBuiltin(sCons,    (Primitive) a -> { twoArgs("cons", a);  return cons(car(a), cadr(a)); },
                  env)));
        }

        topEnv = env;
    }

    private ListConsCell addBuiltin(final String sym, final Object value, ConsCell env) {
        return acons(intern(sym), value, env);
    }

    private ListConsCell addBuiltin(final LambdaJSymbol sym, final Object value, ConsCell env) {
        return acons(sym, value, env);
    }



    ///
    /// ## Invoking the interpreter
    ///

    /// JMurmel native embed API: Java calls Murmel with getValue() and getFunction()

    /** embed API: interface for compiled lambdas as well as primitives and jmethods, used for embedding as well as compiled Murmel */
    public interface MurmelFunction { Object apply(Object... args) throws Exception; }

    /** embed API: Return the value of {@code globalSymbol} in the interpreter's current global environment */
    public Object getValue(String globalSymbol) {
        if (topEnv == null) throw new LambdaJError("getValue: not initialized (must interpret *something* first)");
        final ConsCell envEntry = assq(intern(globalSymbol), topEnv);
        if (envEntry != null) return cdr(envEntry);
        throw new LambdaJError(true, "%s: '%s' is not bound", "getValue", globalSymbol);
    }

    private class CallLambda implements MurmelFunction {
        private final ConsCell lambda;
        private final ConsCell env;
        CallLambda(ConsCell lambda, ConsCell topEnv) { this.lambda = lambda; this.env = topEnv; }
        @Override public Object apply(Object... args) { return applyClosure(lambda, args, env); }
    }
    
    Object applyClosure(ConsCell closure, Object[] args, ConsCell originalTopEnv) {
        if (originalTopEnv != topEnv) throw new LambdaJError("MurmelFunction.apply: stale function object, global environment has changed");
        return eval(cons(closure, arraySlice(args, 0)), originalTopEnv, 0, 0, 0);
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
        return getFunction(funcName, getValue(funcName));
    }

    private MurmelFunction getFunction(String funcName, Object maybeFunction) {
        if (maybeFunction instanceof MurmelJavaProgram.CompilerPrimitive)       { return ((MurmelJavaProgram.CompilerPrimitive)maybeFunction)::applyCompilerPrimitive; }
        if (maybeFunction instanceof Primitive)                                 { return ((Primitive)maybeFunction)::applyPrimitiveVarargs; }
        if (maybeFunction instanceof ConsCell && car(maybeFunction) == sLambda) { return new CallLambda((ConsCell)maybeFunction, topEnv); }
        if (maybeFunction instanceof MurmelFunction)                            { return args -> MurmelJavaProgram.funcall((MurmelFunction)maybeFunction, args); /* must use the TCO trampoline */ }

        throw new LambdaJError(true, "getFunction: not a primitive or lambda: %s", funcName != null ? funcName : printSEx(maybeFunction));
    }

    public interface MurmelProgram {
        Object getValue(String globalSymbol);
        MurmelFunction getFunction(String funcName);

        Object body();

        void setCommandlineArgumentList(ConsCell argList);
        ObjectReader getLispReader();
        ObjectWriter getLispPrinter();
        void setReaderPrinter(ObjectReader reader, ObjectWriter writer);
    }

    /** Turn {@code program} into an interpreted Murmel program: {@code program} will be wrapped in the method
     *  {@link MurmelProgram#body} that can be run multiple times. */
    public MurmelProgram formsToInterpretedProgram(String program, ReadSupplier in, WriteConsumer out) {
        return new MurmelProgram() {
            @Override public Object getValue(String globalSymbol) { return LambdaJ.this.getValue(globalSymbol); }
            @Override public MurmelFunction getFunction(String funcName) { return LambdaJ.this.getFunction(funcName); }

            @Override public void setCommandlineArgumentList(ConsCell args) { insertFront(topEnv, intern("*command-line-argument-list*"), args); }
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
    public Object evalScript(Reader program, Reader in, Writer out) {
        if (topEnv == null) {
            lispReader = new SExpressionReader(features, trace, tracer, symtab, featuresEnvEntry, in::read, null);
            environment(null);
        }
        final ObjectReader scriptParser = lispReader;
        scriptParser.setInput(program::read, null);
        setReaderPrinter(new SExpressionReader(in::read, symtab, featuresEnvEntry), new SExpressionWriter(new WrappingWriter(out)::append));
        final Object eof = "EOF";
        Object result = null;
        Object exp;
        while ((exp = scriptParser.readObj(true, eof)) != eof) {
            result = expandAndEval(exp, null);
        }
        return result;
    }



    /// JMurmel native embed API - Java calls Murmel

    /** <p>Build environment, read a single S-expression from {@code in}, invoke {@code eval()} and return result.
     *
     *  <p>After the expression was read from {@code in}, the primitive function {@code read} (if used)
     *  will read S-expressions from {@code in} as well,
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpression(ReadSupplier in, WriteConsumer out) {
        final ObjectReader parser = init(in, out);
        final Object exp = parser.readObj(true, null);
        final long tStart = System.nanoTime();
        final Object result = eval(exp, topEnv, 0, 0, 0); // todo solls hier auch expandForm() geben? oder diese methode überhaupt küblen? interpretExpressions() kann fast dasselbe
        traceStats(System.nanoTime() - tStart);
        return result;
    }

    /** <p>Build environment, repeatedly read an S-expression from {@code programSupplier} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read S-expressions from {@code in}
     *  and {@code write}/ {@code writeln} will write S-Expressions to {@code out}. */
    public Object interpretExpressions(ReadSupplier programSupplier, ReadSupplier in, WriteConsumer out) {
        final ObjectReader program = new SExpressionReader(features, trace, tracer, symtab, featuresEnvEntry, programSupplier, null);
        final ObjectReader inReader = new SExpressionReader(features, TraceLevel.TRC_NONE, null, symtab, featuresEnvEntry, in, null);
        final ObjectWriter outWriter = makeWriter(out);
        return interpretExpressions(program, inReader, outWriter, null);
    }

    /** <p>Build environment, repeatedly read an expression from {@code program} and invoke {@code eval()} until EOF,
     *  return result of last expression.
     *
     *  <p>The primitive function {@code read} (if used) will read expressions from {@code inReader},
     *  and {@code write}/ {@code writeln} will write Objects to {@code out}. */
    public Object interpretExpressions(ObjectReader program, ObjectReader inReader, ObjectWriter outWriter, CustomEnvironmentSupplier customEnv) {
        final ConsCell customEnvironment = customEnv == null ? null : customEnv.customEnvironment(symtab);
        init(inReader, outWriter, customEnvironment);
        final Object eof = "EOF";
        Object result = null;
        Object exp;
        while ((exp = program.readObj(true, eof)) != eof) {
            final long tStart = System.nanoTime();
            result = expandAndEval(exp, null);
            traceStats(System.nanoTime() - tStart);
        }
        return result;
    }

    /** print and reset interpreter stats and wall time. preceeded and followed by a newline. */
    private void traceStats(long nanos) {
        if (trace.ge(TraceLevel.TRC_STATS)) {
            tracer.println("");
            tracer.println("*** max Murmel evaluator recursion: " + maxEvalLevel + " ***");
            tracer.println("*** max eval() on Java stack:       " + maxEvalStack + " ***");

            tracer.println("*** total ConsCells:                " + nCells + " ***");
            if (trace.ge(TraceLevel.TRC_ENVSTATS)) tracer.println("*** max env length:                 " + maxEnvLen + " ***");

            final long millis = (long)(nanos * 0.000001D);
            final String ms = Long.toString(millis) + '.' + ((long) (nanos * 0.001D + 0.5D) - (long) (millis * 1000D));
            tracer.println("*** elapsed wall time:              " + ms + "ms ***");
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

    enum Action { INTERPRET, TO_JAVA, TO_JAR, COMPILE_AND_RUN, }

    static class Exit extends RuntimeException {
        final int rc;
        Exit(int rc) { super(null, null, true, true); this.rc = rc; }
    }

    private static final Exit EXIT_SUCCESS =       new Exit(0);

    private static final Exit EXIT_PROGRAM_ERROR = new Exit(1);

    private static final Exit EXIT_CMDLINE_ERROR = new Exit(128);
    private static final Exit EXIT_IO_ERROR =      new Exit(129);
    private static final Exit EXIT_RUNTIME_ERROR = new Exit(255);

    /** static main() function for commandline use of the Murmel interpreter */
    public static void main(String[] args) {
        final int rc = mainInternal(args);
        // if rc == 0 then don't System.exit() but simply return from main so that the program will only end after all TurtleFrames have been closed
        if (rc != 0) System.exit(rc);
    }

    static int mainInternal(String[] args) {
        try {
            final boolean finalResult = finalResult(args);
            final boolean script = hasFlag("--script", args, false);
            final boolean error = handleScript(args);
            final boolean scriptFlagError;
            if (script && (hasFlag("--repl", args, false) || hasFlag("--tty", args, false))) {
                scriptFlagError = true;
                System.err.println("LambdaJ: when using --script neither --repl nor --tty may be used as well");
            }
            else scriptFlagError = false;


            misc(args);
            final Action action = action(args);
            final TraceLevel trace = trace(args);
            final int features = features(args);

            final boolean istty = hasFlag("--tty", args) || null != System.console();
            final boolean repl = hasFlag("--repl", args);
            final boolean echo = hasFlag("--echo", args);    // used only in repl
            final boolean printResult = hasFlag("--result", args);  // print individual results of toplevel forms, used only when interpreting files given on the commandline or interpreting piped input
            final boolean verbose = hasFlag("--verbose", args);
            final String clsName = flagValue("--class", args);
            final String outDir = flagValue("--outdir", args);
            final String libDir = flagValue("--libdir", args);

            if (argError(args) || error || scriptFlagError) {
                System.err.println("LambdaJ: exiting because of previous errors.");
                throw EXIT_CMDLINE_ERROR;
            }

            final Path libPath = getLibPath(libDir);

            final LambdaJ interpreter = new LambdaJ(features, trace, null, null, libPath);

            final List<Object> history = repl ? new ArrayList<>() : null;

            // process files given on the commandline
            final List<String> files = args(args);
            try {
                if (!files.isEmpty()) {
                    switch (action) {
                    case INTERPRET:
                        interpreter.init(() -> -1, s -> {});
                        injectCommandlineArgs(interpreter, args);
                        Object result = null;
                        for (String fileName : files) {
                            if ("--".equals(fileName)) continue;
                            if (verbose) System.out.println("interpreting " + fileName + "...");
                            final Path p = Paths.get(fileName);
                            try (Reader r = Files.newBufferedReader(p)) {
                                result = interpretStream(interpreter, r::read, p, printResult, history);
                            }
                        }
                        if (finalResult && !printResult && result != null) {
                            System.out.println();
                            System.out.println("==> " + result);
                        }
                        if (script) exit(result);
                        break;
                    case TO_JAVA:
                        final boolean javaSuccess = compileFiles(files, false, clsName, libPath, outDir);
                        if (!istty && !javaSuccess) throw EXIT_RUNTIME_ERROR;
                        break;
                    case TO_JAR:
                        final boolean jarSuccess = compileFiles(files, true, clsName, libPath, outDir);
                        if (!istty && !jarSuccess) throw EXIT_RUNTIME_ERROR;
                        break;
                    case COMPILE_AND_RUN:
                        final Object res = compileAndRunFiles(files, interpreter, args, verbose, finalResult);
                        if (script) exit(res);
                        break;
                    }
                }
            }
            catch (IOException e) {
                System.err.println();
                System.err.println(e);
                throw EXIT_IO_ERROR;
            }

            // repl() doesn't return
            if (files.isEmpty() && istty || repl) repl(interpreter, !files.isEmpty(), istty, echo, history, args);

            if (files.isEmpty()) {
                final String consoleCharsetName = System.getProperty("sun.stdout.encoding");
                final Charset consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);

                if (action == Action.INTERPRET) {
                    interpreter.init(() -> -1, s -> {});
                    injectCommandlineArgs(interpreter, args);
                    final Object result = interpretStream(interpreter, new InputStreamReader(System.in, consoleCharset)::read, null, printResult, null);
                    if (finalResult && !printResult && result != null) {
                        System.out.println();
                        System.out.println("==> " + result);
                    }
                } else {
                    final SExpressionReader parser = interpreter.makeReader(new InputStreamReader(System.in, consoleCharset)::read, null);

                    switch (action) {
                    case TO_JAVA:
                        final boolean successJava = compileToJava(StandardCharsets.UTF_8, interpreter.symtab, interpreter.libDir, parser, clsName, outDir);
                        if (successJava) System.out.println("compiled stdin to " + (clsName == null ? "MurmelProgram" : clsName));
                        break;
                    case TO_JAR:
                        final String outFile = outDir != null ? outDir + "/a.jar" : "a.jar";
                        final boolean successJar = compileToJar(interpreter.symtab, libPath, parser, clsName, outFile);
                        if (successJar) System.out.println("compiled stdin to " + outFile);
                        break;
                    case COMPILE_AND_RUN:
                        //final ObjectWriter outWriter = makeWriter(System.out::print);
                        //interpreter.setReaderPrinter(parser, outWriter);
                        //interpreter.environment(null);
                        compileAndRunForms(parser, args, interpreter, false, finalResult);
                        break;
                    default: assert false : "can't happen";
                    }
                }
            }
        }
        catch (Exit e) {
            return e.rc;
        }
        return 0;
    }

    /** exit by throwing an {@link Exit} exception, doesn't return. The last form of the program will determine the exitlevel:
     *  nil will result in 0, a number will result in an exitlevel of number&127, any other non-nil value will result in an exitlevel of 1. */
    private static void exit(Object murmelResult) {
        if (murmelResult == null) return;
        if (numberp(murmelResult)) throw new Exit(((Number)murmelResult).intValue() & 0x7f);
        throw EXIT_PROGRAM_ERROR;
    }



    /// functions to interpret, compile and/ or run files or input streams
    private static Object interpretStream(final LambdaJ interpreter, ReadSupplier prog, Path fileName, final boolean printResult, List<Object> history) {
        try {
            final ObjectReader reader = interpreter.lispReader;
            reader.setInput(prog, fileName);
            final ObjectReader inReader = new SExpressionReader(interpreter.features, TraceLevel.TRC_NONE, null, interpreter.symtab, interpreter.featuresEnvEntry, System.in::read, null);
            final ObjectWriter outWriter = makeWriter(System.out::print);
            interpreter.setReaderPrinter(inReader, outWriter);
            final Object eof = "EOF";
            Object result = null;
            for (;;) {
                final Object form = reader.readObj(true, eof);
                if (form == eof) break;
                if (history != null) history.add(form);

                final long tStart = System.nanoTime();
                result = interpreter.expandAndEval(form, null);
                final long tEnd = System.nanoTime();
                interpreter.traceStats(tEnd - tStart);
                if (printResult) {
                    System.out.println();
                    System.out.print("==> "); outWriter.printObj(result, true); System.out.println();
                }
            }
            return result;
        } catch (LambdaJError e) {
            System.err.println();
            System.err.println(e);
            throw EXIT_RUNTIME_ERROR;
        }
    }

    private static boolean compileFiles(final List<String> files, boolean toJar, String clsName, Path libPath, String outDir) throws IOException {
        final SymbolTable symtab = new ListSymbolTable();
        final MurmelJavaCompiler c = new MurmelJavaCompiler(symtab, libPath, null);

        final ObjectReader program = parseFiles(files, c.intp, true);
        final String outFile;
        final boolean success;
        if (toJar) {
            outFile = outDir != null ? outDir + "/a.jar" : "a.jar";
            success = compileToJar(c, program, clsName, outFile);
        }
        else {
            success = compileToJava(StandardCharsets.UTF_8, c, program, clsName, outDir);
            if (clsName == null) clsName = "MurmelProgram";
            if (outDir == null) outDir = ".";
            outFile = outDir + '/' + clsName + ".java";
        }
        if (success) System.out.println("compiled " + files.size() + " file(s) to " + outFile);
        return success;
    }

    private static Object compileAndRunFiles(List<String> files, LambdaJ interpreter, String[] args, boolean verbose, boolean finalResult) throws IOException {
        final ObjectReader program = parseFiles(files, interpreter, verbose);
        //interpreter.init(System.in::read, System.out::print);
        return compileAndRunForms(program, args, interpreter, false, finalResult);
    }

    /** compile history to a class and run compiled class */
    private static Object compileAndRunForms(ObjectReader history, String[] cmdlineArgs, LambdaJ interpreter, boolean repl, boolean finalResult) {
        final Path tmpDir;
        try { tmpDir = getTmpDir(); }
        catch (IOException e) { 
            System.out.println("NOT compiled to .jar - cannot get/ create tmp directory: " + e.getMessage());
            if (!repl) throw EXIT_IO_ERROR;
            return null;
        }

        MurmelProgram prg = null;
        try {
            final MurmelJavaCompiler c = new MurmelJavaCompiler(interpreter.symtab, interpreter.libDir, tmpDir);
            final Class<MurmelProgram> murmelClass = c.formsToJavaClass("MurmelProgram", history, null);
            prg = murmelClass.getDeclaredConstructor().newInstance();
            injectCommandlineArgs(prg, cmdlineArgs);
            final long tStart = System.nanoTime();
            final Object result = prg.body();
            final long tEnd = System.nanoTime();
            interpreter.traceJavaStats(tEnd - tStart);
            if (repl || finalResult && result != null) {
                System.out.println();
                System.out.print("==> ");  prg.getLispPrinter().printObj(result, true);  System.out.println();
            }

            return result;
        }
        catch (LambdaJError e) {
            final String msg = (prg != null ? "runtime error" : "error") + location(prg) + ": " + e.getMessage();
            if (repl) {
                System.out.println("history NOT run as Java - " + msg);
            } else System.err.println(msg);
        }
        catch (Throwable t) {
            final String loc = location(prg);
            if (repl) {
                System.out.println("history NOT run as Java - " + (prg != null ? "runtime error" : "error") + loc + ":");
                t.printStackTrace(System.out);
            }
            else System.err.println("Caught Throwable" + loc + ": " + t);
        }
        if (!repl) throw EXIT_RUNTIME_ERROR;
        return null;
    }

    private static String location(MurmelProgram prg) {
        return prg instanceof MurmelJavaProgram ? " at " + ((MurmelJavaProgram) prg).loc : "";
    }

    private static boolean compileToJava(Charset charset, SymbolTable st, Path libDir, ObjectReader history, Object className, Object filename) {
        return compileToJava(charset, new MurmelJavaCompiler(st, libDir, null), history, className, filename);
    }

    /** compile history to Java source and print or write to a file.
     *  <ul>
     *  <li>if className is null "MurmelProgram" will be the class' name.
     *  <li>if filename is t the compiled Java code will be printed to the screen.
     *  <li>if filename is null the filename will be derived from the className
     *  <li>if filename not null then filename is interpreted as a base directory and the classname (with packages) will be appended
     *  </ul> */
    private static boolean compileToJava(Charset charset, MurmelJavaCompiler c, ObjectReader history, Object className, Object filename) {
        final String clsName = className == null ? "MurmelProgram" : className.toString();
        if (filename == c.intp.sT) {
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
        try (OutputStream os = Files.newOutputStream(p);
             WrappingWriter writer = new WrappingWriter(new BufferedWriter(new OutputStreamWriter(os, encoder)))) {
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

    private static boolean compileToJar(SymbolTable st, Path libDir, ObjectReader history, Object className, Object jarFile) {
        final Path tmpDir;
        try { tmpDir = getTmpDir(); }
        catch (IOException e) { System.out.println("NOT compiled to .jar - cannot get/ create tmp directory: " + e.getMessage()); return false; }

        return compileToJar(new MurmelJavaCompiler(st, libDir, tmpDir), history, className, jarFile);
    }

    private static boolean compileToJar(MurmelJavaCompiler c, ObjectReader history, Object className, Object jarFile) {
        try {
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



    /// repl and helpers
    /** Enter REPL, doesn't return */
    private static void repl(final LambdaJ interpreter, boolean isInit, final boolean istty, boolean echo, List<Object> prevHistory, String[] args) {
        final LambdaJSymbol cmdQuit   = interpreter.intern(":q");
        final LambdaJSymbol cmdHelp   = interpreter.intern(":h");
        final LambdaJSymbol cmdEcho   = interpreter.intern(":echo");
        final LambdaJSymbol cmdNoEcho = interpreter.intern(":noecho");
        final LambdaJSymbol cmdEnv    = interpreter.intern(":env");
        final LambdaJSymbol cmdMacros = interpreter.intern(":macros");
        final LambdaJSymbol cmdRes    = interpreter.intern(":res");
        final LambdaJSymbol cmdList   = interpreter.intern(":l");
        final LambdaJSymbol cmdWrite  = interpreter.intern(":w");
        final LambdaJSymbol cmdJava   = interpreter.intern(":java");
        final LambdaJSymbol cmdRun    = interpreter.intern(":r");
        final LambdaJSymbol cmdJar    = interpreter.intern(":jar");

        if (!echo) {
            System.out.println("Enter a Murmel form or :command (or enter :h for command help or :q to exit):");
            System.out.println();
        }

        final String consoleCharsetName = System.getProperty("sun.stdout.encoding");
        final Charset  consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);

        final Object eof = "EOF";
        final List<Object> history = prevHistory == null ? new ArrayList<>() : prevHistory;
        SExpressionReader parser = null;
        ObjectWriter outWriter = null;
        final Reader consoleReader = new InputStreamReader(System.in, consoleCharset);
        final ReadSupplier echoingSupplier = () -> { final int c = consoleReader.read(); if (c != -1) System.out.print((char)c); return c; };
        if (isInit) {
            interpreter.nCells = 0; interpreter.maxEnvLen = 0;
            parser = new SExpressionReader(interpreter.features, interpreter.trace, interpreter.tracer, interpreter.symtab, interpreter.featuresEnvEntry,
                                           echo ? echoingSupplier : consoleReader::read, null);
            outWriter = interpreter.lispPrinter;
        }
        for (;;) {
            if (!isInit) {
                interpreter.nCells = 0; interpreter.maxEnvLen = 0;
                parser = new SExpressionReader(interpreter.features, interpreter.trace, interpreter.tracer, interpreter.symtab, interpreter.featuresEnvEntry,
                                               echo ? echoingSupplier : consoleReader::read, null);
                outWriter = makeWriter(System.out::print);
                interpreter.lispReader = parser; interpreter.lispPrinter = outWriter;
                interpreter.environment(null);
                interpreter.clearMacros();
                interpreter.modules.clear();
                injectCommandlineArgs(interpreter, args);
                interpreter.featuresEnvEntry.rplacd(interpreter.makeFeatureList());
                isInit = true;
            }

            if (!echo) {
                System.out.print("JMurmel> ");
                System.out.flush();
            }

            try {
                if (istty) parser.resetPos();
                final Object exp = parser.readObj(true, eof);

                if (exp != null) {
                    if (exp == eof
                        || exp == cmdQuit) { System.out.println("bye."); System.out.println();  throw EXIT_SUCCESS; }
                    if (exp == cmdHelp)   { showHelp();  continue; }
                    if (exp == cmdEcho)   { echo = true; parser.setInput(echoingSupplier, null);continue; }
                    if (exp == cmdNoEcho) { echo = false; parser.setInput(consoleReader::read, null); continue; }
                    if (exp == cmdRes)    { isInit = false; history.clear();  continue; }
                    if (exp == cmdList)   { listHistory(history); continue; }
                    if (exp == cmdWrite)  { writeHistory(history, parser.readObj(false)); continue; }
                    if (exp == cmdJava)   { compileToJava(consoleCharset, interpreter.symtab, interpreter.libDir, makeReader(history), parser.readObj(false), parser.readObj(false)); continue; }
                    if (exp == cmdRun)    { compileAndRunForms(makeReader(history), null, interpreter, true, false); continue; }
                    if (exp == cmdJar)    { compileToJar(interpreter.symtab, interpreter.libDir, makeReader(history), parser.readObj(false), parser.readObj(false)); continue; }
                    //if (":peek".equals(exp.toString())) { System.out.println("gensymcounter: " + interpreter.gensymCounter); continue; }
                    if (exp == cmdEnv)    {
                        if (interpreter.topEnv != null) for (Object entry: interpreter.topEnv) System.out.println(entry);
                        System.out.println("env length: " + length(interpreter.topEnv));  System.out.println(); continue; }
                    if (exp == cmdMacros) {
                        final ArrayList<LambdaJSymbol> names = new ArrayList<>();
                        for (Object entry: ((ListSymbolTable)interpreter.symtab).symbols) {
                            if (entry == null) continue;
                            final LambdaJSymbol sym = (LambdaJSymbol)entry;
                            if (sym.macro != null) names.add(sym);
                        }
                        names.sort(Comparator.comparing(Object::toString));
                        for (LambdaJSymbol name: names) System.out.println(name + ": " + name.macro);
                        System.out.println("number of macros: " + names.size());
                        System.out.println(); continue;
                    }
                }
                history.add(exp);

                interpreter.values = NO_VALUES;
                final long tStart = System.nanoTime();
                final Object result = interpreter.expandAndEval(exp, null);
                final long tEnd = System.nanoTime();
                interpreter.traceStats(tEnd - tStart);
                System.out.println();
                if (interpreter.values == NO_VALUES) {
                    System.out.print("==> "); outWriter.printObj(result, true); System.out.println();
                } else {
                    if (interpreter.values != null) for (Object value: interpreter.values) {
                        System.out.print(" -> "); outWriter.printObj(value, true); System.out.println();
                    }
                }
            } catch (LambdaJError e) {
                if (istty) {
                    System.out.println();
                    System.out.println(e);
                    System.out.println();
                } else {
                    System.err.println();
                    System.err.println(e);
                    throw EXIT_RUNTIME_ERROR;
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



    /// helpers for commandline argument processing

    /** whether to print a non-nil result of the final form after exit. Must be called before {@link #handleScript}. Default is false when --script is used, true when --script is not used.
     *  --final-result turns on printing of a non-nil result of the last form, --no-final-result turns it off.
     *  If both are given then the last one wins. */
    private static boolean finalResult(String[] args) {
        boolean ret = !hasFlag("--script", args, false);

        for (int i = 0; i < args.length; i++) {
            final String arg = args[i];
            if ("--".equals(arg)) return ret;
            if ("--final-result".equals(arg))    { args[i] = null; ret = true; }
            if ("--no-final-result".equals(arg)) { args[i] = null; ret = false; }
        }
        return ret;
    }

    /** process --script, return true for error, false for ok */
    private static boolean handleScript(String[] args) {
        for (int i = 0; i < args.length; i++) {
            final String arg = args[i];
            if ("--".equals(arg)) return false;
            if ("--script".equals(arg)) {
                if (args.length <= i+1) {
                    System.err.println("LambdaJ: commandline argument --script requires one filename");
                    args[i] = null; // consume the arg
                    return true;
                }
                args[i] = args[i+1];
                args[i+1] = "--";
                return false;
            }
        }
        return false;
    }

    private static void misc(String[] args) {
        if (hasFlag("--version", args)) {
            showVersion();
            throw EXIT_SUCCESS;
        }

        if (hasFlag("--help", args) || hasFlag("--usage", args)) {
            showVersion();
            System.out.println();
            showUsage();
            throw EXIT_SUCCESS;
        }

        if (hasFlag("--help-features", args)) {
            showVersion();
            System.out.println();
            showFeatureUsage();
            throw EXIT_SUCCESS;
        }
    }

    private static Action action(String[] args) {
        final boolean toJava      = hasFlag("--java", args);
        final boolean toJar       = hasFlag("--jar", args);
        final boolean run         = hasFlag("--run", args);
        
        if (toJar) return Action.TO_JAR;
        if (toJava) return Action.TO_JAVA;
        if (run) return Action.COMPILE_AND_RUN;
        return Action.INTERPRET;
    }

    private static TraceLevel trace(String[] args) {
        TraceLevel trace = TraceLevel.TRC_NONE;
        if (hasFlag("--trace=stats", args))    trace = TraceLevel.TRC_STATS;
        if (hasFlag("--trace=envstats", args)) trace = TraceLevel.TRC_ENVSTATS;
        if (hasFlag("--trace=eval", args))     trace = TraceLevel.TRC_EVAL;
        if (hasFlag("--trace=func", args))     trace = TraceLevel.TRC_FUNC;
        if (hasFlag("--trace=env", args))      trace = TraceLevel.TRC_ENV;
        if (hasFlag("--trace", args))          trace = TraceLevel.TRC_LEX;
        return trace;
    }

    private static int features(String[] args) {
        int features = Features.HAVE_ALL_LEXC.bits();
        if (hasFlag("--XX-dyn", args))      features =  Features.HAVE_ALL_DYN.bits();

        if (hasFlag("--min+", args))        features =  Features.HAVE_MINPLUS.bits();
        if (hasFlag("--min", args))         features =  Features.HAVE_MIN.bits();
        if (hasFlag("--lambda+", args))     features =  Features.HAVE_LAMBDAPLUS.bits();
        if (hasFlag("--lambda", args))      features =  Features.HAVE_LAMBDA.bits();

        if (hasFlag("--no-nil", args))      features &= ~Features.HAVE_NIL.bits();
        if (hasFlag("--no-t", args))        features &= ~Features.HAVE_T.bits();
        if (hasFlag("--no-extra", args))    features &= ~Features.HAVE_XTRA.bits();
        if (hasFlag("--no-ffi", args))      features &= ~Features.HAVE_FFI.bits();
        if (hasFlag("--no-number", args))   features &= ~(Features.HAVE_NUMBERS.bits() | Features.HAVE_DOUBLE.bits() | Features.HAVE_LONG.bits());
        if (hasFlag("--no-string", args))   features &= ~Features.HAVE_STRING.bits();
        if (hasFlag("--no-io", args))       features &= ~Features.HAVE_IO.bits();
        if (hasFlag("--no-gui", args))      features &= ~Features.HAVE_GUI.bits();
        if (hasFlag("--no-util", args))     features &= ~Features.HAVE_UTIL.bits();

        if (hasFlag("--no-labels", args))   features &= ~Features.HAVE_LABELS.bits();
        if (hasFlag("--no-cons", args))     features &= ~Features.HAVE_CONS.bits();
        if (hasFlag("--no-cond", args))     features &= ~Features.HAVE_COND.bits();
        if (hasFlag("--no-apply", args))    features &= ~Features.HAVE_APPLY.bits();

        if (hasFlag("--no-atom", args))     features &= ~Features.HAVE_ATOM.bits();
        if (hasFlag("--no-eq", args))       features &= ~Features.HAVE_EQ.bits();
        if (hasFlag("--no-quote", args))    features &= ~Features.HAVE_QUOTE.bits();

        return features;
    }

    private static boolean hasFlag(String flag, String[] args) {
        return hasFlag(flag, args, true);
    }

    private static boolean hasFlag(String flag, String[] args, boolean erase) {
        for (int i = 0; i < args.length; i++) {
            final String arg = args[i];
            if ("--".equals(arg)) return false;
            if (flag.equals(arg)) {
                if (erase) args[i] = null; // consume the arg
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
        if (intp.topEnv == null) return; // empty environment probably because of commandline argument --lambda

        int n = 0;
        for (String arg: args) {
            n++;
            if ("--".equals(arg)) break;
        }

        intp.insertFront(intp.topEnv, intp.intern("*command-line-argument-list*"), arraySlice(args, n));
    }

    private static void injectCommandlineArgs(MurmelProgram prg, String[] args) {
        int n = 0;
        if (args != null) for (String arg: args) {
            n++;
            if ("--".equals(arg)) break;
        }

        prg.setCommandlineArgumentList(arraySlice(args, n));
    }



    /// functions that print info to the screen, used in REPL as well as from main()
    private static void showVersion() {
        System.out.println(ENGINE_VERSION);
    }

    private static void showHelp() {
        System.out.println("Available commands:\n"
        + "  :h ............................. this help screen\n"
        + "  :echo .......................... print forms to screen before eval'ing\n"
        + "  :noecho ........................ don't print forms\n"
        + "  :env ........................... list current global environment\n"
        + "  :macros ........................ list currently defined macros\n"
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
                           + "java -jar jmurmel.jar <commandline flags>... <source files>...\n"
                           + "java -jar jmurmel.jar <commandline flags>... <source files>... '--' args-for-program\n"
                           + "java -jar jmurmel.jar <commandline flags>... <source files>... '--script' source-file args-for-program\n"
                           + "\n"
                           + "In order to pass commandline arguments to the Murmel program either \"--\" or \"--script <murmelfile>\"\n"
                           + "must be used to indicate the end of JMurmel commandline arguments and the start of program\n"
                           + "commandline arguments.\n"
                           + "\n"
                           + "Commandline flags are:\n"
                           + "\n"
                           + "Misc flags:\n"
                           + "\n"
                           + "-- ...............  Can be used to indicate:\n"
                           + "                    commandline arguments after this will be passed\n"
                           + "                    to the program\n"
                           + "--script <file> ..  Can be used to indicate:\n"
                           + "                    process the file following '--script' and pass any remaining\n"
                           + "                    commandline arguments to the Murmel program.\n"
                           + "                    The last form in the last file will determine the exitlevel\n"
                           + "                    to the OS:\n"
                           + "                    nil -> 0\n"
                           + "                    number -> number & 127\n"
                           + "                    other non-nil -> 1\n"
                           + "--no-final-result\n"
                           + "--final-result ...  Whether or not to print the result of the last form after exit.\n"
                           + "                    Default is to print unless --script is used.\n"
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
                           + "--result .........  Print the results of each toplevel form when interpreting\n"
                           + "                    files or stdin.\n"
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
                           + "--trace=func .....  Print internal interpreter info re: function and macro calls\n"
                           + "--trace=env ......  Print more internal interpreter info executing programs\n"
                           + "--trace ..........  Print lots of internal interpreter info during\n"
                           + "                    reading/ parsing/ executing programs");
    }

    private static void showFeatureUsage() {
        System.out.println("Feature flags:\n"
                + "\n"
                + "--no-ffi ......  no functions 'jmethod' or 'jproxy'\n" 
                + "--no-gui ......  no turtle or bitmap graphics\n"
                + "--no-extra ....  no special forms if, define, defun, defmacro,\n"
                + "                 let, let*, letrec, progn, setq,\n"
                + "                 multiple-value-call, multiple-value-bind,\n"
                + "                 load, require, provide, declaim\n"
                + "                 no primitive functions eval, rplaca, rplacd, trace, untrace,\n"
                + "                 values, macroexpand-1\n"
                + "--no-number ...  no number support\n"
                + "--no-string ...  no string support\n"
                + "--no-io .......  no primitive functions read, write, writeln, lnwrite,\n"
                + "--no-util .....  no primitive functions consp, symbolp, listp, null,\n"
                + "                 append, assoc, assq, list, list*, format, format-locale\n"
                + "                 no time related primitives\n"
                + "\n"
                + "--min+ ........  turn off all above features, leaving a Lisp\n"
                + "                 with 10 special forms and primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special forms quote, lambda, cond, labels\n"
                + "                   the primitive functions atom, eq, cons, car, cdr, apply\n"
                + "                   the symbols nil, t\n"
                + "\n"
                + "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n"
                + "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n"
                + "--no-apply ....  no function 'apply'\n"
                + "--no-labels ...  no special form 'labels' (hint: use Y-combinator instead)\n"
                + "\n"
                + "--min .........  turn off all above features, leaving a Lisp with\n"
                + "                 8 special forms and primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special forms quote, lambda, cond\n"
                + "                   the primitive functions atom, eq, cons, car, cdr\n"
                + "\n"
                + "--no-cons .....  no primitive functions cons/ car/ cdr\n"
                + "--no-cond .....  no special form 'cond'\n"
                + "\n"
                + "--lambda+ .....  turn off pretty much everything except Lambda calculus,\n"
                + "                 leaving a Lisp with 4 special forms and primitives:\n"
                + "                   S-expressions\n"
                + "                   symbols and cons-cells (i.e. lists)\n"
                + "                   function application\n"
                + "                   the special form quote, lambda\n"
                + "                   the primitive functions atom, eq\n"
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
                + "                   the special form lambda\n"
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



    /// infrastructure utilities
    private static Path getLibPath(String libDir) {
        if (libDir == null) return null;
        try {
            final Path libPath = Paths.get(libDir).toAbsolutePath();
            if (!Files.isDirectory(libPath)) {
                System.err.println("LambdaJ: invalid value for --libdir: " + libDir + " is not a directory");
                throw EXIT_CMDLINE_ERROR;
            }
            if (!Files.isReadable(libPath)) {
                System.err.println("LambdaJ: invalid value for --libdir: " + libDir + " is not readable");
                throw EXIT_CMDLINE_ERROR;
            }
            return libPath;
        }
        catch (Exception e) {
            System.err.println("LambdaJ: cannot process --libdir: " + libDir + ": " + e.getMessage());
            throw EXIT_CMDLINE_ERROR;
        }
    }

    private static Path getTmpDir() throws IOException {
        final Path tmpDir = Files.createTempDirectory("JMurmel");
        tmpDir.toFile().deleteOnExit();
        return tmpDir;
    }
    
    private static ObjectReader makeReader(List<Object> forms) {
        final Iterator<Object> i = forms.iterator();
        return (eof) -> i.hasNext() ? i.next() : eof;
    }

    private static class MultiFileReadSupplier implements ReadSupplier {
        private final boolean verbose;
        private final ObjectReader delegate;
        private final Iterator<Path> paths;
        private Reader reader;

        public MultiFileReadSupplier(List<Path> paths, ObjectReader delegate, boolean verbose) {
            this.paths = paths.iterator();
            this.delegate = delegate;
            this.verbose = verbose;
        }

        private void next() throws IOException {
            final Reader old = reader;
            reader = null;
            if (old != null) old.close();
            final Path p = paths.next();
            if (verbose) System.out.println("parsing " + p.toString() + "...");
            reader = Files.newBufferedReader(p);
            delegate.setInput(this, p);
        }

        @Override
        public int read() throws IOException {
            if (reader == null && paths.hasNext()) next();
            try {
                final int ret = reader.read();
                if (ret != -1) return ret;
                if (paths.hasNext()) next();
                else return -1;
            }
            catch (IOException e) {
                final Reader old = reader;
                reader = null;
                try { if (old != null) old.close(); }
                catch (IOException e2) { e.addSuppressed(e2); }
                throw e;
            }
            return read();
        }
    }

    private static ObjectReader parseFiles(List<String> files, LambdaJ interpreter, boolean verbose) {
        final List<Path> paths = new ArrayList<>(files.size());
        for (String fileName : files) {
            if ("--".equals(fileName)) break;
            paths.add(Paths.get(fileName));
        }
        final ObjectReader reader = interpreter.makeReader(() -> -1, null);
        reader.setInput(new MultiFileReadSupplier(paths, reader, verbose), paths.get(0));
        return reader;
    }



    ///
    /// ## class MurmelJavaProgram
    /// class MurmelJavaProgram - base class for compiled Murmel programs

    /** Base class for compiled Murmel programs, contains Murmel runtime as well as embed API support for compiled Murmel programs. */
    public abstract static class MurmelJavaProgram implements MurmelProgram {

        public static class CompilerGlobal {
            private Object value;
            private ConsCell dynamicStack;

            public CompilerGlobal(Object value) { this.value = value; }

            public Object get() { return value; }
            public Object set(Object value) { return this.value = value; }
            
            public void push() { dynamicStack = cons(value, dynamicStack); }
            public void push(Object value) { dynamicStack = cons(this.value, dynamicStack); this.value = value; }
            public void pop() { value = car(dynamicStack); dynamicStack = (ConsCell)cdr(dynamicStack); }
        }

        public static final CompilerGlobal UNASSIGNED_GLOBAL = new CompilerGlobal(null) { public Object get() { throw new LambdaJError(false, "unassigned value"); } };
        public static final Object UNASSIGNED_LOCAL = "#<value is not assigned>";

        public static final Object[] NOARGS = new Object[0];

        public interface CompilerPrimitive extends Writeable {
            Object applyCompilerPrimitive(Object... args);
            @Override default void printSEx(WriteConsumer out, boolean ignored) { out.print("#<compiler primitive>"); }
        }

        private static final class Tailcall {
            MurmelFunction fn;
            MurmelFunction cleanup;
            Object[] args;
        }

        private final LambdaJ intp = new LambdaJ();

        protected MurmelJavaProgram() {
            intp.init(() -> -1, System.out::print);
            intp.setReaderPrinter(intp.makeReader(System.in::read, null), intp.getLispPrinter());
            _t = intern("t");
            _dynamic = intern("dynamic");
            features = (ConsCell)cdr(intp.featuresEnvEntry); // todo wenn kompilierter code *features* ändert, bekommt das der reader des interpreters nicht mit: eval '(read), und umgekehrt: eval '(push 'bla *features*)
                                                             // vor/nach eval features hintri/firi kopieren, values auch
        }



        /// JMurmel native embed API - Java calls compiled Murmel
        @Override public final ObjectReader getLispReader()  { return intp.getLispReader(); }
        @Override public final ObjectWriter getLispPrinter() { return intp.getLispPrinter(); }
        @Override public final void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) { intp.setReaderPrinter(lispStdin, lispStdout); }

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

        protected abstract Object runbody() throws Exception;
        @Override public Object body() {
            try {
                return runbody();
            }
            catch (UnsupportedOperationException e) {
                throw new LambdaJError(e.getMessage() + "\nUnsupported operation occured in " + loc);
            }
            catch (Exception e) {
                return rterror(e);
            }
        }

        public final Object rterror(Exception e) {
            throw new LambdaJError(e.getMessage() + "\nError occured in " + loc);
        }



        /// predefined global variables
        public final Object _t;
        public final Object _pi = Math.PI;
        public final Object _dynamic;

        /// predefined aliased global variables
        // internal-time-units-per-second: itups doesn't have a leading _ because it is avaliable under an alias name
        public final Object itups = 1e9;
        // *COMMAND-LINE-ARGUMENT-LIST*: will be assigned/ accessed from generated code
        public ConsCell commandlineArgumentList;

        public ConsCell features;

        /// predefined primitives
        
        // Predefined primitives sind vom typ CompilerPrimitive. Benutzt werden sie im generierten code so:
        //
        //     (CompilerPrimitive)rt()::add
        //
        // muessen public sein, weil sonst gibt z.B. "(let* () (format t "hallo"))" unter Java 8u262 einen Laufzeitfehler:
        //
        //     Exception in thread "main" java.lang.BootstrapMethodError: java.lang.IllegalAccessError:
        //     tried to access method io.github.jmurmel.LambdaJ$MurmelJavaProgram.format([Ljava/lang/Object;)Ljava/lang/Object; from class MurmelProgram$1
        //
        // Unter Java 17 gibts den Laufzeitfehler nicht, koennte ein Java 8 bug sein. Oder Java 17 hat den Bug, weil der Zugriff nicht erlaubt sein sollte.
        // Gilt nicht fuer methoden, die "normal" aufgerufen werden wie z.B. "cons(Object,Object)", die koennen protected sein (gibt dann halt unmengen synthetische $access$ methoden).
        //
        // Wenn statt "(CompilerPrimitive)rt()::add" -> "(CompilerPrimitive)((MurmelJavaProgram)rt())::add" generiert wird,
        // gibts unter Java 8, 17 und 19 einen Compilefehler.
        public final Object   _car     (Object... args) { oneArg("car",     args.length); return car(args[0]); }
        public static Object car (Object l)  { return LambdaJ.car(l); } // also used by generated code
        public static Object car (ConsCell l)  { return LambdaJ.car(l); }

        public final Object   _cdr     (Object... args) { oneArg("cdr",     args.length); return cdr(args[0]); }
        public static Object cdr (Object l)  { return LambdaJ.cdr(l); } // also used by generated code
        public static Object cdr (ConsCell l)  { return LambdaJ.cdr(l); }

        public final ConsCell _cons    (Object... args) { twoArgs("cons",   args.length); return cons(args[0], args[1]); }
        public static ConsCell cons(Object car, Object cdr)  { return ConsCell.cons(car, cdr); } // also used by generated code

        public final ConsCell _rplaca  (Object... args) { twoArgs("rplaca", args.length);  return rplaca(args[0], args[1]); }
        public static ConsCell rplaca(Object l, Object newCar) { return requireList(l).rplaca(newCar); }
        public static ConsCell rplaca(ConsCell l, Object newCar) { return l.rplaca(newCar); }

        public final ConsCell _rplacd  (Object... args) { twoArgs("rplacd", args.length);  return rplacd(args[0], args[1]); }
        public static ConsCell rplacd(Object l, Object newCdr) { return requireList(l).rplacd(newCdr); }
        public static ConsCell rplacd(ConsCell l, Object newCdr) { return l.rplacd(newCdr); }

        public final Object _apply (Object... args) {
            twoArgs("apply", args.length);
            Object fn = args[0];
            if (symbolp(fn)) fn = getValue(fn.toString());
            return applyTailcallHelper(fn, args[1]);
        }
        public final Object apply(Object... args) {
            Object fn = args[0];
            if (symbolp(fn)) fn = getValue(fn.toString());
            return applyTailcallHelper(fn, args[1]);
        }
        public final Object _eval      (Object... args) { varargs1_2("eval",     args.length); return intp.expandAndEval(args[0], args.length == 2 ? requireList(args[1]) : null); }
        public final Object _eq        (Object... args) { twoArgs("eq",          args.length); return args[0] == args[1] ? _t : null; }
        public final Object _eql       (Object... args) { twoArgs("eql",         args.length); return LambdaJ.eql(args[0], args[1]) ? _t : null; }
        public final Object eql     (Object o1, Object o2) { return LambdaJ.eql(o1, o2) ? _t : null; }
        public final Object _null      (Object... args) { oneArg("null",         args.length); return args[0] == null ? _t : null; }

        public final Object _read      (Object... args) { noArgs("read",         args.length); if (intp.getLispReader() == null) throw new LambdaJError(true, "%s: lispStdin is nil", "read");
                                                                                               return intp.getLispReader().readObj(null); } // todo eof parameter
        public final Object _write     (Object... args) { varargs1_2("write",    args.length); intp.write(args[0], args.length < 2 || args[1] != null); return _t; }
        public final Object _writeln   (Object... args) { varargs0_2("writeln",  args.length); intp.writeln(arraySlice(args), args.length < 2 || args[1] != null); return _t; }
        public final Object _lnwrite   (Object... args) { varargs0_2("lnwrite",  args.length); intp.lnwrite(arraySlice(args), args.length < 2 || args[1] != null); return _t; }

        public final Object _atom      (Object... args) { oneArg("atom",         args.length); return atom      (args[0]) ? _t : null; }
        public final Object _consp     (Object... args) { oneArg("consp",        args.length); return consp     (args[0]) ? _t : null; }
        public final Object _listp     (Object... args) { oneArg("listp",        args.length); return listp     (args[0]) ? _t : null; }
        public final Object _symbolp   (Object... args) { oneArg("symbolp",      args.length); return symbolp   (args[0]) ? _t : null; }
        public final Object _numberp   (Object... args) { oneArg("numberp",      args.length); return numberp   (args[0]) ? _t : null; }
        public final Object _stringp   (Object... args) { oneArg("stringp",      args.length); return stringp   (args[0]) ? _t : null; }
        public final Object _characterp(Object... args) { oneArg("characterp",   args.length); return characterp(args[0]) ? _t : null; }
        public final Object _integerp  (Object... args) { oneArg("integerp",     args.length); return integerp  (args[0]) ? _t : null; }
        public final Object _floatp    (Object... args) { oneArg("floatp",       args.length); return floatp    (args[0]) ? _t : null; }

        public final ConsCell _assoc   (Object... args) { twoArgs("assoc",       args.length); return assoc(args[0], args[1]); }
        public final ConsCell _assq    (Object... args) { twoArgs("assq",        args.length); return assq(args[0], args[1]); }
        public final ConsCell _list    (Object... args) {
            return ListBuilder.list(args);
        }
        public final Object listStar   (Object... args) {
            varargs1("list*", args.length);
            if (args.length == 1) return args[0];
            if (args.length == 2) return cons(args[0], args[1]);
            final ListBuilder b = new ListBuilder();
            int i = 0;
            for (; i < args.length-1; i++) {
                b.append(nth(i, args));
            }
            b.appendLast(nth(i, args));
            return b.first();
        }
        public final Object   _append  (Object... args) {
            final int nArgs;
            if (args == null || (nArgs = args.length) == 0) return null;
            if (nArgs == 1) return args[0];
            if (!listp(args[0])) throw new LambdaJError(true, "append: first argument %s is not a list", args[0]);

            int first = 0;
            while (first < nArgs-1 && args[first] == null) first++; // skip leading nil args if any

            ListBuilder lb = null;
            for (int i = first; i < nArgs - 1; i++) {
                final Object o = args[i];
                if (o == null) continue;
                if (!consp(o)) throw new LambdaJError(true, "append: argument %d is not a list: %s", i+1, printSEx(o));
                if (lb == null) lb = new ListBuilder();
                for (Object obj: (ConsCell)o) lb.append(obj);
            }
            if (lb == null) return args[first];
            lb.appendLast(args[nArgs-1]);
            return lb.first();
        }

        public final double   _fround   (Object... args) { varargs1_2("fround",   args.length); return cl_round   (quot12(args)); }
        public final double   _ffloor   (Object... args) { varargs1_2("ffloor",   args.length); return Math.floor (quot12(args)); }
        public final double   _fceiling (Object... args) { varargs1_2("fceiling", args.length); return Math.ceil  (quot12(args)); }
        public final double   _ftruncate(Object... args) { varargs1_2("ftruncate",args.length); return cl_truncate(quot12(args)); }

        public final long     _round   (Object... args) { varargs1_2("round",     args.length); return checkedToLong(cl_round   (quot12(args))); }
        public final long     _floor   (Object... args) { varargs1_2("floor",     args.length); return checkedToLong(Math.floor (quot12(args))); }
        public final long     _ceiling (Object... args) { varargs1_2("ceiling",   args.length); return checkedToLong(Math.ceil  (quot12(args))); }
        public final long     _truncate(Object... args) { varargs1_2("truncate",  args.length); return checkedToLong(cl_truncate(quot12(args))); }

        public static double cl_round(double d) { return Math.rint(d); }
        public static double cl_truncate(double d) { return LambdaJ.cl_truncate(d); }
        public static long checkedToLong(double d) { return LambdaJ.checkedToLong(d); }
        private static double quot12(Object[] args) { return args.length == 2 ? toDouble(args[0]) / toDouble(args[1]) : toDouble(args[0]); }

        public final Object   charInt  (Object... args) { oneArg("char-code",     args.length); return (long) requireChar(args[0]); }
        public final Object   intChar  (Object... args) { oneArg("code-char",     args.length); return (char) toInt(args[0]); }
        public final Object   stringeq (Object... args) { twoArgs("string=",      args.length); return Objects.equals(requireStringOrNull(args[0]), requireStringOrNull(args[1])) ? _t : null; }
        public final Object   stringToList (Object... args) {
            oneArg("string->list", args.length);
            final String s = LambdaJ.requireStringOrNull("string->list", args[0]);
            if (s == null) return null;
            final ListBuilder ret = new ListBuilder();
            final int len = s.length();
            for (int i = 0; i < len; i++) ret.append(s.charAt(i));
            return ret.first();
        }
        public final Object   listToString (Object... args) { oneArg("list->string", args.length); return LambdaJ.listToStringImpl(LambdaJ.requireList("list->string", args[0])); }

        public final double   _sqrt    (Object... args) { oneArg("sqrt",          args.length); return Math.sqrt (toDouble(args[0])); }
        public final double   _log     (Object... args) { oneArg("log",           args.length); return Math.log  (toDouble(args[0])); }
        public final double   _log10   (Object... args) { oneArg("log10",         args.length); return Math.log10(toDouble(args[0])); }
        public final double   _exp     (Object... args) { oneArg("exp",           args.length); return Math.exp  (toDouble(args[0])); }
        public final Number   _signum  (Object... args) { oneArg("signum",        args.length); return cl_signum (args[0]); }
        public final double   _expt    (Object... args) { twoArgs("expt",         args.length); return Math.pow  (toDouble(args[0]), toDouble(args[1])); }
        public final double   _mod     (Object... args) { twoArgs("mod",          args.length); return cl_mod(toDouble(args[0]), toDouble(args[1])); }
        public static double cl_mod(double lhs, double rhs) { return LambdaJ.cl_mod(lhs, rhs); }
        public final double   _rem     (Object... args) { twoArgs("rem",          args.length); return toDouble(args[0]) % toDouble(args[1]); }

        // predefined aliased primitives
        // the following don't have a leading _ because they are avaliable (in the environment) under alias names
        public final Number   inc      (Object... args) { oneArg("1+",         args.length); return LambdaJ.inc(args[0]); }
        public static Number  inc1     (Object arg)     { return LambdaJ.inc(arg); }
        public final Number   dec      (Object... args) { oneArg("1-",         args.length); return LambdaJ.dec(args[0]); }
        public static Number  dec1     (Object arg)     { return LambdaJ.dec(arg); }

        public final double add     (Object... args) { if (args.length > 0) { double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret += toDouble(args[i]); return ret; } return 0.0; }
        public final double mul     (Object... args) { if (args.length > 0) { double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret *= toDouble(args[i]); return ret; } return 1.0; }

        public final double sub     (Object... args) { varargs1("-", args.length);
                                                       if (args.length == 1) return 0.0 - toDouble(args[0]);
                                                       double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret -= toDouble(args[i]); return ret; }
        public final double quot    (Object... args) { varargs1("/", args.length);
                                                       if (args.length == 1) return 1.0 / toDouble(args[0]);
                                                       double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret /= toDouble(args[i]); return ret; }

        public final Object numbereq(Object... args) { return compare("=",  args, (d1, d2) -> d1 == d2); }
        public final Object lt      (Object... args) { return compare("<",  args, (d1, d2) -> d1 <  d2); }
        public final Object le      (Object... args) { return compare("<=", args, (d1, d2) -> d1 <= d2); }
        public final Object ge      (Object... args) { return compare(">=", args, (d1, d2) -> d1 >= d2); }
        public final Object gt      (Object... args) { return compare(">",  args, (d1, d2) -> d1 >  d2); }
        public final Object ne      (Object... args) { return compare("/=", args, (d1, d2) -> d1 != d2); }
        private Object compare(String op, Object[] args, DoubleBiPred pred) {
            final int length = args.length;
            varargs1(op, length);
            double prev = toDouble(args[0]);
            for (int i = 1; i < length; i++) {
                final double next = toDouble(args[i]);
                if (!pred.test(prev, next)) return null;
                prev = next;
            }
            return _t;
        }

        public final Object format             (Object... args) { return intp.format(arraySlice(args)); }
        public final Object formatLocale       (Object... args) { return intp.formatLocale(arraySlice(args)); }
        //public final Object macroexpand1       (Object... args) { return intp.macroexpand1(arraySlice(args)); }
        public final Object _gensym (Object... args) { return intp.gensym(arraySlice(args)); }

        public final Object getInternalRealTime(Object... args) { return LambdaJ.getInternalRealTime(arraySlice(args)); }
        public final Object getInternalRunTime (Object... args) { return LambdaJ.getInternalRunTime(arraySlice(args)); }
        public final Object getInternalCpuTime (Object... args) { return LambdaJ.getInternalCpuTime(arraySlice(args)); }
        public final Object sleep              (Object... args) { return LambdaJ.sleep(arraySlice(args)); }
        public final Object getUniversalTime   (Object... args) { return LambdaJ.getUniversalTime(arraySlice(args)); }
        public final Object getDecodedTime     (Object... args) { return intp.getDecodedTime(arraySlice(args)); }

        public final Object jmethod              (Object... args) { twoArgs("jmethod", args.length); return findMethod(args[0], args[0], arraySlice(args, 2)); }
        public static Primitive findMethod(Object className, Object methodName, ConsCell paramClasses) {
            return LambdaJ.findMethod(requireString(className), requireString(methodName), paramClasses);
        }
        public static Primitive findMethod(Object className, Object methodName, Object... paramClasses) {
            return LambdaJ.findMethod(requireString(className), requireString(methodName), arraySlice(paramClasses));
        }

        public final Object _jproxy            (Object... args) { return intp.makeProxy(arraySlice(args)); }

        public final Object _trace             (Object... args) { return null; }
        public final Object _untrace           (Object... args) { return null; }

        public final Object makeFrame          (Object... args) {
            varargsMinMax("make-frame", args.length, 1, 4);
            final String title = requireStringOrNull(args[0]);
            final TurtleFrame ret = new TurtleFrame(title, requireNumberOrNull(nth(1, args)), requireNumberOrNull(nth(2, args)), requireNumberOrNull(nth(3, args)));
            intp.current_frame = ret;
            return ret;
        }

        public final Object openFrame          (Object... args) { varargsMinMax("open-frame",    args.length, 0, 1); return requireFrame("open-frame",     nth(0, args)).open();    }
        public final Object closeFrame         (Object... args) { varargsMinMax("close-frame",   args.length, 0, 1); return requireFrame("close-frame",    nth(0, args)).close();   }
        public final Object resetFrame         (Object... args) { varargsMinMax("reset-frame",   args.length, 0, 1); return requireFrame("reset-frame",    nth(0, args)).reset();   }
        public final Object clearFrame         (Object... args) { varargsMinMax("clear-frame",   args.length, 0, 1); return requireFrame("clear-frame",    nth(0, args)).clear();   }
        public final Object repaintFrame       (Object... args) { varargsMinMax("repaint-frame", args.length, 0, 1); return requireFrame("repaint-frame",  nth(0, args)).repaint(); }
        public final Object flushFrame         (Object... args) { varargsMinMax("flush-frame",   args.length, 0, 1); return requireFrame("flush-frame",    nth(0, args)).flush();   }

        // set new current frame, return previous frame
        public final Object currentFrame       (Object... args) { varargsMinMax("current-frame", args.length, 0, 1); final Object prev = intp.current_frame; if (args.length > 0 && args[0] != null) intp.current_frame = requireFrame("current-frame", args[0]); return prev; }

        public final Object pushPos            (Object... args) { varargsMinMax("push-pos",      args.length, 0, 1); return requireFrame("push-pos",       nth(0, args)).pushPos(); }
        public final Object popPos             (Object... args) { varargsMinMax("pop-pos",       args.length, 0, 1); return requireFrame("pop-pos",        nth(0, args)).popPos();  }

        public final Object penUp              (Object... args) { varargsMinMax("pen-up",        args.length, 0, 1); return requireFrame("pen-up",         nth(0, args)).penUp();   }
        public final Object penDown            (Object... args) { varargsMinMax("pen-down",      args.length, 0, 1); return requireFrame("pen-down",       nth(0, args)).penDown(); }

        public final Object color              (Object... args) { varargsMinMax("color",         args.length, 1, 2); return requireFrame("color",          nth(1, args)).color  (toInt(nth(0, args))); }
        public final Object bgColor            (Object... args) { varargsMinMax("bgcolor",       args.length, 1, 2); return requireFrame("bgcolor",        nth(1, args)).bgColor(toInt(nth(0, args))); }

        public final Object text               (Object... args) { varargsMinMax("text",          args.length, 1, 2); return requireFrame("text",           nth(1, args)).text   (args[0].toString()); }

        public final Object right              (Object... args) { varargsMinMax("right",         args.length, 1, 2); return requireFrame("right",          nth(1, args)).right  (toDouble(args[0])); }
        public final Object left               (Object... args) { varargsMinMax("left",          args.length, 1, 2); return requireFrame("left",           nth(1, args)).left   (toDouble(args[0])); }
        public final Object forward            (Object... args) { varargsMinMax("forward",       args.length, 1, 2); return requireFrame("forward",        nth(1, args)).forward(toDouble(args[0])); }

        public final Object moveTo             (Object... args) { varargsMinMax("move-to",       args.length, 2, 3); return requireFrame("move-to",        nth(2, args)).moveTo(toDouble(args[0]), toDouble(args[1]));  }
        public final Object lineTo             (Object... args) { varargsMinMax("line-to",       args.length, 2, 3); return requireFrame("line-to",        nth(2, args)).lineTo(toDouble(args[0]), toDouble(args[1]));  }
        public final Object moveRel            (Object... args) { varargsMinMax("move-rel",      args.length, 2, 3); return requireFrame("move-rel",       nth(2, args)).moveRel(toDouble(args[0]), toDouble(args[1])); }
        public final Object lineRel            (Object... args) { varargsMinMax("line-rel",      args.length, 2, 3); return requireFrame("line-rel",       nth(2, args)).lineRel(toDouble(args[0]), toDouble(args[1])); }

        public final Object makeBitmap         (Object... args) { varargsMinMax("make-bitmap",   args.length, 2, 3); return requireFrame("make-bitmap",    nth(2, args)).makeBitmap(toInt(args[0]), toInt(args[1]));  }
        public final Object discardBitmap      (Object... args) { varargsMinMax("discard-bitmap",args.length, 0, 1); return requireFrame("discard-bitmap", nth(0, args)).discardBitmap();   }

        public final Object setPixel           (Object... args) { varargsMinMax("set-pixel",     args.length, 3, 4); return requireFrame("set-pixel",      nth(3, args)).setRGB(toInt(args[0]), toInt(args[1]), toInt(args[2]));  }
        public final Object rgbToPixel         (Object... args) { threeArgs("rgb-to-pixel", args.length);
                                                                  final int r = toInt(args[0]);
                                                                  final int g = toInt(args[1]);
                                                                  final int b = toInt(args[2]);
                                                                  return (long)(int)((r << 16) | (g << 8) | b); }
        public final Object hsbToPixel         (Object... args) { threeArgs("hsb-to-pixel", args.length);
                                                                  final float hue = toFloat(args[0]);
                                                                  final float sat = toFloat(args[1]);
                                                                  final float bri = toFloat(args[2]);
                                                                  return (long)Color.HSBtoRGB(hue, sat, bri); }
        public final Object _fatal             (Object... args) { oneArg("fatal", args.length); throw new RuntimeException(String.valueOf(args[0])); }

        Object[] values;
        public final Object _values            (Object... args) { values = args; return args.length == 0 ? null : args[0]; }

        /// Helpers that the Java code compiled from Murmel will use, i.e. compiler intrinsics
        public final LambdaJSymbol intern(String symName) { return intp.intern(symName); }

        public static Object arrayToList(Object[] args, int start) {
            if (start >= args.length) return null;
            if (args.length-start == 1) return cons(args[start], null);
            final ListBuilder ret = new ListBuilder();
            for (int i = start; i < args.length; i++) ret.append(args[i]);
            return ret.first();
        }

        public static ConsCell arraySlice(Object[] o, int offset) { return LambdaJ.arraySlice(o, offset); }
        public static ConsCell arraySlice(Object[] o) { return arraySlice(o, 0); }

        /** convert null, an array or a list to a (possibly empty) Object[] */
        public static Object[] toArray(Object o) {
            if (o == null)
                return NOARGS;
            if (o instanceof Object[])
                return (Object[])o;
            return listToArray(o);
        }

        public static ConsCell requireList(Object lst) {
            if (lst == null) return null;
            if (!consp(lst)) errorNotAList(lst);
            return (ConsCell)lst;
        }

        public static double toDouble(Object n) {
            // the redundant checks are faster than instanceof Number and will succeed most of the time
            if (n instanceof Long)    return ((Long)n).doubleValue();
            if (n instanceof Double)  return (Double) n;
            return LambdaJ.toDouble("?", n);
        }
        public static double toDouble(Double n) { if (n != null) return n;  throw errorNotANumber(null); }
        public static double toDouble(double n) { return n; }
        public static double toDouble(Long n)   { if (n != null) return n;  throw errorNotANumber(null); }
        public static double toDouble(long n)   { return n; }

        public static long  toLong(Object n)  {
            // the redundant checks are faster than instanceof Number and will succeed most of the time
            if (n instanceof Long)    return (Long) n;
            if (n instanceof Double)  return requireIntegralNumber("toLong", n, Long.MIN_VALUE, Long.MAX_VALUE).longValue();
            if (n instanceof Byte)    return ((Byte)n).longValue();
            if (n instanceof Short)   return ((Short)n).longValue();
            if (n instanceof Integer) return ((Integer)n).longValue();
            if (n instanceof Float)   return requireIntegralNumber("toLong", n, Long.MIN_VALUE, Long.MAX_VALUE).longValue();
            if (n instanceof Number)  return requireIntegralNumber("toLong", n, Long.MIN_VALUE, Long.MAX_VALUE).longValue();
            throw errorNotANumber(n);
        }
        public static long  toLong(Long n) { if (n != null) return n;  throw errorNotANumber(null); }
        public static long  toLong(long n) { return n; }

        public static int   toInt(Object n)   { return requireIntegralNumber("toInt", n, Integer.MIN_VALUE, Integer.MAX_VALUE).intValue(); }
        public static float toFloat(Object o) {
            final Number n = requireNumber("toFloat", o);
            final double d = n.doubleValue();
            if (d >= Float.MIN_VALUE && d <= Float.MAX_VALUE) return n.floatValue();
            throw errorOverflow("toFloat", "java.lang.Float", o);
        }
        public static boolean toBoolean(Object n)  { return n != null; }
        public static byte toByte(Object n)  { return requireIntegralNumber("toByte", n, Byte.MIN_VALUE, Byte.MAX_VALUE).byteValue(); }
        public static short toShort(Object n) { return requireIntegralNumber("toShort", n, Short.MIN_VALUE, Short.MAX_VALUE).shortValue(); }

        public static Character requireChar(Object o) {
            if (!characterp(o)) errorNotACharacter(o);
            return (Character)o;
        }

        public static String requireStringOrNull(Object o) {
            if (o == null) return null;
            return requireString(o);
        }

        public static String requireString(Object o) {
            if (!stringp(o)) errorNotAString(o);
            return o.toString();
        }

        public static Number requireNumberOrNull(Object o) {
            if (o == null) return null;
            return requireNumber("?", o);
        }

        private TurtleFrame requireFrame(String s, Object o) {
            final TurtleFrame ret;
            if (o == null && (ret = intp.current_frame) != null) return ret;
            if (o instanceof TurtleFrame) return (TurtleFrame)o;
            throw errorNotAFrame(s, o);
        }

        public static Object[] unassigned(int length) { final Object[] ret = new Object[length]; Arrays.fill(ret, UNASSIGNED_LOCAL); return ret; }

        public static void argCheck(String expr, int paramCount, int argCount) { if (paramCount != argCount) errorArgCount(expr, paramCount, paramCount, argCount); }
        public static void argCheckVarargs(String expr, int paramCount, int argCount) { if (argCount < paramCount - 1) errorArgCount(expr, paramCount - 1, Integer.MAX_VALUE, argCount); }



        /** Primitives are in the environment as (CompilerPrimitive)... . Compiled code that calls primitives will
         *  actually call this overload and not funcall(Object, Object...) that contains the TCO thunking code. */
        public static Object funcall(CompilerPrimitive fn, Object... args) { return fn.applyCompilerPrimitive(args); }

        public static Object tailcall(CompilerPrimitive fn, Object... args) { return funcall(fn, args); }

        /** used for (apply sym form) */
        public static Object applyHelper(CompilerPrimitive fn, Object argList) { return funcall(fn, toArray(argList)); }

        /** used for (apply sym form) */
        public static Object applyTailcallHelper(CompilerPrimitive fn, Object argList) { return funcall(fn, toArray(argList)); }



        /** TCO trampoline, used for function calls, and also for let, labels, progn */
        public static Object funcall(MurmelFunction fn, Object... args) {
            ConsCell cleanups = null;
            LambdaJError ex = null;
            Object r = null;
            try {
                r = fn.apply(args);
                while (r instanceof Tailcall) {
                    final Tailcall functionCall = (Tailcall) r;
                    r = functionCall.fn.apply(functionCall.args);
                    if (functionCall.cleanup != null) cleanups = cons(functionCall.cleanup, cleanups);
                    if (Thread.interrupted()) throw new InterruptedException("got interrupted");
                }
            }
            catch (LambdaJError e) { ex = e; }
            catch (Exception e) { ex = new LambdaJError(e, e.getMessage()); }
            finally {
                if (cleanups != null) for (Object cl: cleanups) {
                    try { ((MurmelFunction)cl).apply((Object[])null); }
                    catch (LambdaJError e) { if (ex == null) ex = e; else ex.addSuppressed(e); }
                    catch (Exception e)    { if (ex == null) ex = new LambdaJError(e, e.getMessage()); }
                }
                if (ex != null) throw ex;
            }
            return r;
        }

        public final Object funcall(Object fn, Object... args) {
            if (fn instanceof MurmelFunction)    return funcall((MurmelFunction)fn, args);
            if (fn instanceof CompilerPrimitive) return funcall((CompilerPrimitive)fn, args);
            if (fn instanceof Primitive)         return ((Primitive)fn).applyPrimitive(arraySlice(args));
            if (fn instanceof ClosureConsCell)   return interpret(fn, args);

            throw errorNotAFunction(fn);
        }

        private Object interpret(Object fn, Object[] args) {
            return intp.eval(cons(intp.intern("apply"),
                                  cons(fn,
                                       cons(cons(intp.sQuote,
                                                 cons(arraySlice(args),
                                                      null)),
                                            null))),
                             null);
        }

        private final Tailcall tailcall = new Tailcall();
        /** used for function calls */
        public final Tailcall tailcall(MurmelFunction fn, Object... args) {
            return tailcallWithCleanup(fn, null, args);
        }

        public final Tailcall tailcallWithCleanup(MurmelFunction fn, MurmelFunction cleanup, Object... args) {
            final Tailcall tailcall = this.tailcall;
            tailcall.fn = fn;
            tailcall.cleanup = cleanup;
            tailcall.args = args;
            return tailcall;
        }

        public final Object tailcall(Object fn, Object... args) {
            return tailcallWithCleanup(fn, null, args);
        }

        public final Object tailcallWithCleanup(Object fn, MurmelFunction cleanup, Object... args) {
            if (fn instanceof MurmelFunction)    {
                return tailcallWithCleanup((MurmelFunction)fn, cleanup, args);
            }
            assert cleanup == null : "unexpected: cleanup != null, fn is a " + fn.getClass().getSimpleName();
            if (fn instanceof CompilerPrimitive) return funcall((CompilerPrimitive)fn, args);
            if (fn instanceof Primitive)         return ((Primitive)fn).applyPrimitive(arraySlice(args));
            if (fn instanceof ClosureConsCell)   return interpret(fn, args);
            throw errorNotAFunction(fn);
        }

        /** used for (apply sym form) */
        public final Object applyHelper(Object fn, Object argList) { return funcall(fn, toArray(argList)); }

        /** used for (apply sym form) */
        public final Object applyTailcallHelper(Object fn, Object argList) { return tailcall(fn, toArray(argList)); }



        private static Object nth(int n, Object[] args) { return args.length > n ? args[n] : null; }

        private static void noArgs(String expr, int argCount)      { if (0 != argCount)               errorArgCount(expr, 0, 0, argCount); }
        private static void oneArg(String expr, int argCount)      { if (1 != argCount)               errorArgCount(expr, 1, 1, argCount); }
        private static void twoArgs(String expr, int argCount)     { if (2 != argCount)               errorArgCount(expr, 2, 2, argCount); }
        private static void threeArgs(String expr, int argCount)   { if (3 != argCount)               errorArgCount(expr, 3, 3, argCount); }

        /** 0..2 args */
        private static void varargs0_2(String expr, int argCount) { if (argCount > 2)                 errorArgCount(expr, 0, 2, argCount); }
        /** 1..2 args */
        private static void varargs1_2(String expr, int argCount) { if (argCount < 1 || argCount > 2) errorArgCount(expr, 1, 2, argCount); }
        /** one or more arguments */
        private static void varargs1(String expr, int argCount)   { if (argCount == 0)                errorArgCount(expr, 1, 1, 0); }

        private static void varargsMinMax(String expr, int argCount, int min, int max) {
            if (argCount < min || argCount > max)
                errorArgCount(expr, min, max, argCount);
        }

        private static void errorArgCount(String expr, int expectedMin, int expectedMax, int actual) {
            if (actual < expectedMin) throw new LambdaJError(true, "%s: not enough arguments", expr);
            if (actual > expectedMax) throw new LambdaJError(true, "%s: too many arguments", expr);
        }

        private static RuntimeException errorNotANumber(Object n) { throw new LambdaJError(true, "not a number: %s", printSEx(n)); }
        private static void errorNotAList(Object s)   { throw new LambdaJError(true, "not a cons/list: %s", printSEx(s)); }
        private static void errorNotACharacter(Object s) { throw new LambdaJError(true, "not a character: %s", printSEx(s)); }
        private static void errorNotAString(Object s) { throw new LambdaJError(true, "not a string: %s", printSEx(s)); }
        private static RuntimeException errorNotAFrame(String s, Object o) {
            if (o != null) throw new LambdaJError(true, "%s: not a frame: %s", s, printSEx(o));
            throw new LambdaJError(true, "%s: no frame argument and no current frame", s);
        }
        private static RuntimeException errorNotAFunction(Object fn) { throw new LambdaJError(true, "not a function: %s", fn); }



        @SuppressWarnings("unused") // used by multiple-value-call
        public class ValuesBuilder {
            private final ArrayList<Object> allValues = new ArrayList<>();
            
            public ValuesBuilder() { values = null; }

            public ValuesBuilder add(Object primary) {
                if (values == null) {
                    allValues.add(primary);
                } else if (values.length > 0) {
                    allValues.addAll(Arrays.asList(values));
                }
                values = null;
                return this;
            }

            public Object[] build() { return allValues.toArray(); }

            // return an array of length n, filling with nil or truncating as needed
            public Object[] build(int n, boolean truncate) {
                for (int i = allValues.size(); i < n; i++) allValues.add(null);
                if (truncate) return allValues.subList(0, n).toArray();
                else return allValues.toArray();
            }
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
                System.err.println("Runtime error at " + program.loc + ": " + e.getMessage());
                System.exit(1);
            } catch (Throwable t) {
                System.err.println("Caught Throwable at " + program.loc + ": " + t);
                System.exit(1);
            }
        }

        @Override public void setCommandlineArgumentList(ConsCell args) {
            commandlineArgumentList = args;
            intp.insertFront(intp.topEnv, intern("*command-line-argument-list*"), args);
        }

        @Override public Object getValue(String symbol) {
            switch (symbol) {
            case "nil": return null;
            case "t": return _t;
            case "pi": return _pi;
            case "internal-time-units-per-second": return itups;
            case "*command-line-argument-list*": return commandlineArgumentList; // this will be assigned by genereted code at runtime
            case "*features*": return features;
            case "car": return (CompilerPrimitive)this::_car;
            case "cdr": return (CompilerPrimitive)this::_cdr;
            case "cons": return (CompilerPrimitive)this::_cons;
            case "rplaca": return (CompilerPrimitive)this::_rplaca;
            case "rplacd": return (CompilerPrimitive)this::_rplacd;
            case "apply": return (CompilerPrimitive)this::_apply;
            case "eval": return (CompilerPrimitive)this::_eval;
            case "eq": return (CompilerPrimitive)this::_eq;
            case "eql": return (CompilerPrimitive)this::_eql;
            case "null": return (CompilerPrimitive)this::_null;
            case "read": return (CompilerPrimitive)this::_read;
            case "write": return (CompilerPrimitive)this::_write;
            case "writeln": return (CompilerPrimitive)this::_writeln;
            case "lnwrite": return (CompilerPrimitive)this::_lnwrite;
            case "atom": return (CompilerPrimitive)this::_atom;
            case "consp": return (CompilerPrimitive)this::_consp;
            case "listp": return (CompilerPrimitive)this::_listp;
            case "symbolp": return (CompilerPrimitive)this::_symbolp;
            case "numberp": return (CompilerPrimitive)this::_numberp;
            case "stringp": return (CompilerPrimitive)this::_stringp;
            case "characterp": return (CompilerPrimitive)this::_characterp;
            case "integerp": return (CompilerPrimitive)this::_integerp;
            case "floatp": return (CompilerPrimitive)this::_floatp;
            case "assoc": return (CompilerPrimitive)this::_assoc;
            case "assq": return (CompilerPrimitive)this::_assq;
            case "list": return (CompilerPrimitive)this::_list;
            case "append": return (CompilerPrimitive)this::_append;
            case "round": return (CompilerPrimitive)this::_round;
            case "floor": return (CompilerPrimitive)this::_floor;
            case "ceiling": return (CompilerPrimitive)this::_ceiling;
            case "truncate": return (CompilerPrimitive)this::_truncate;
            case "fround": return (CompilerPrimitive)this::_fround;
            case "ffloor": return (CompilerPrimitive)this::_ffloor;
            case "fceiling": return (CompilerPrimitive)this::_fceiling;
            case "ftruncate": return (CompilerPrimitive)this::_ftruncate;
            case "sqrt": return (CompilerPrimitive)this::_sqrt;
            case "log": return (CompilerPrimitive)this::_log;
            case "log10": return (CompilerPrimitive)this::_log10;
            case "exp": return (CompilerPrimitive)this::_exp;
            case "expt": return (CompilerPrimitive)this::_expt;
            case "mod": return (CompilerPrimitive)this::_mod;
            case "rem": return (CompilerPrimitive)this::_rem;
            case "signum": return (CompilerPrimitive)this::_signum;
            case "gensym": return (CompilerPrimitive)this::_gensym;
            case "trace": return (CompilerPrimitive)this::_trace;
            case "untrace": return (CompilerPrimitive)this::_untrace;
            case "fatal": return (CompilerPrimitive)this::_fatal;
            case "+": return (CompilerPrimitive)this::add;
            case "*": return (CompilerPrimitive)this::mul;
            case "-": return (CompilerPrimitive)this::sub;
            case "/": return (CompilerPrimitive)this::quot;
            case "=": return (CompilerPrimitive)this::numbereq;
            case "<=": return (CompilerPrimitive)this::le;
            case "<": return (CompilerPrimitive)this::lt;
            case ">=": return (CompilerPrimitive)this::ge;
            case ">": return (CompilerPrimitive)this::gt;
            case "/=": return (CompilerPrimitive)this::ne;
            case "1+": return (CompilerPrimitive)this::inc;
            case "1-": return (CompilerPrimitive)this::dec;
            case "format": return (CompilerPrimitive)this::format;
            case "format-locale": return (CompilerPrimitive)this::formatLocale;
            case "char-code": return (CompilerPrimitive)this::charInt;
            case "code-char": return (CompilerPrimitive)this::intChar;
            case "string=": return (CompilerPrimitive)this::stringeq;
            case "string->list": return (CompilerPrimitive)this::stringToList;
            case "list->string": return (CompilerPrimitive)this::listToString;
            case "list*": return (CompilerPrimitive)this::listStar;
            case "get-internal-real-time": return (CompilerPrimitive)this::getInternalRealTime;
            case "get-internal-run-time": return (CompilerPrimitive)this::getInternalRunTime;
            case "get-internal-cpu-time": return (CompilerPrimitive)this::getInternalCpuTime;
            case "sleep": return (CompilerPrimitive)this::sleep;
            case "get-universal-time": return (CompilerPrimitive)this::getUniversalTime;
            case "get-decoded-time": return (CompilerPrimitive)this::getDecodedTime;
            case "jmethod": return (CompilerPrimitive)this::jmethod;
            case "jproxy": return (CompilerPrimitive)this::_jproxy;
            case "make-frame": return (CompilerPrimitive)this::makeFrame;
            case "open-frame": return (CompilerPrimitive)this::openFrame;
            case "close-frame": return (CompilerPrimitive)this::closeFrame;
            case "reset-frame": return (CompilerPrimitive)this::resetFrame;
            case "clear-frame": return (CompilerPrimitive)this::clearFrame;
            case "repaint-frame": return (CompilerPrimitive)this::repaintFrame;
            case "flush-frame": return (CompilerPrimitive)this::flushFrame;
            case "current-frame": return (CompilerPrimitive)this::currentFrame;
            case "push-pos": return (CompilerPrimitive)this::pushPos;
            case "pop-pos": return (CompilerPrimitive)this::popPos;
            case "pen-up": return (CompilerPrimitive)this::penUp;
            case "pen-down": return (CompilerPrimitive)this::penDown;
            case "color": return (CompilerPrimitive)this::color;
            case "bgcolor": return (CompilerPrimitive)this::bgColor;
            case "text": return (CompilerPrimitive)this::text;
            case "right": return (CompilerPrimitive)this::right;
            case "left": return (CompilerPrimitive)this::left;
            case "forward": return (CompilerPrimitive)this::forward;
            case "move-to": return (CompilerPrimitive)this::moveTo;
            case "line-to": return (CompilerPrimitive)this::lineTo;
            case "move-rel": return (CompilerPrimitive)this::moveRel;
            case "line-rel": return (CompilerPrimitive)this::lineRel;
            case "make-bitmap": return (CompilerPrimitive)this::makeBitmap;
            case "discard-bitmap": return (CompilerPrimitive)this::discardBitmap;
            case "set-pixel": return (CompilerPrimitive)this::setPixel;
            case "rgb-to-pixel": return (CompilerPrimitive)this::rgbToPixel;
            case "hsb-to-pixel": return (CompilerPrimitive)this::hsbToPixel;
            case "values": return (CompilerPrimitive)this::_values;
            default: throw new LambdaJError(true, "%s: '%s' not bound", "getValue", symbol);
            }
        }
    }



    ///
    /// ## class MurmelJavaCompiler
    /// class MurmelJavaCompiler - compile Murmel to Java or to a in-memory Class-object and optionally to a .jar file
    ///
    public static class MurmelJavaCompiler {
        private final JavaCompilerHelper javaCompiler;
        final LambdaJ intp;

        public MurmelJavaCompiler(SymbolTable st, Path libDir, Path outPath) {
            final LambdaJ intp = new LambdaJ(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null, st, libDir);
            intp.init(() -> -1, System.out::print);
            this.intp = intp;

            this.javaCompiler = new JavaCompilerHelper(outPath);
        }

        public SymbolTable getSymbolTable() { return intp.getSymbolTable(); }


        /// symbols and name mangling
        private LambdaJSymbol intern(String symname) {
            if (symname == null) return intp.sNil;
            return intp.intern(symname);
        }

        /** return true if lhs is the same symbol as interned rhs */
        private boolean symbolEq(Object lhs, String rhs) {
            return lhs == intern(rhs);
        }

        /** replace chars that are not letters */
        private static String mangle(String symname, int sfx) {
            final int len = symname.length();
            final StringBuilder mangled = new StringBuilder(Math.max(len+10, 16));
            mangled.append('_');
            for (int i = 0; i < len; i++) {
                final char c = symname.charAt(i);
                if (c == '_' || c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') mangled.append(c);
                else mangled.append('_').append((int)c).append('_');
            }
            if (sfx != 0) mangled.append(sfx);
            return mangled.toString();
        }



        /// environment
        /** extend the environment by putting (symbol mangledsymname) in front of {@code prev},
         *  symbols that are reserved words throw an error. */
        private ConsCell extenv(String func, Object symbol, int sfx, ConsCell prev) {
            final LambdaJSymbol sym = intp.symbolOrMalformed(func, symbol);
            return extenvIntern(sym, mangle(symbol.toString(), sfx), prev);
        }

        /** extend environment w/o reserved word check */
        private static ConsCell extenvIntern(LambdaJSymbol sym, String javaName, ConsCell env) {
            return cons(cons(sym, javaName), env);
        }

        private ConsCell extenvprim(String symname, String javaName, ConsCell env) {
            final LambdaJSymbol sym = intern(symname);
            return extenvIntern(sym, "((CompilerPrimitive)rt()::" + javaName + ')', env);
        }



        private boolean passTwo;
        private Set<String> implicitDecl;
        /** return {@code form} as a Java expression */
        private String javasym(Object form, ConsCell env) {
            if (form == null || form == intp.sNil) return "(Object)null";
            final ConsCell symentry = assq(form, env);
            if (symentry == null) {
                if (passTwo) errorMalformedFmt("compilation unit", "undefined symbol %s", form);
                System.err.println("implicit declaration of " + form); // todo lineinfo of containing form
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
            final ConsCell prevEntry = assq(sym, env);
            if (prevEntry != null) {
                intp.notReserved(func, (LambdaJSymbol)car(prevEntry));
                errorMalformedFmt(func, "can't redefine symbol %s", sym);
            }
        }

        private void defined(String func, Object sym, ConsCell env) {
            if (sym == null) sym = intern("nil");
            final ConsCell symentry = assq(sym, env);
            if (symentry == null) errorMalformedFmt(func, "undefined symbol %s", sym.toString());
        }



        private static void notAPrimitive(String func, Object symbol, String javaName) {
            if (javaName.startsWith("((CompilerPrimitive")) errorNotImplemented("%s: assigning primitives is not implemented: %s", func, symbol.toString());
        }



        /// Environment for compiled Murmel:
        /// * nil, t, pi
        /// * *command-line-argument-list*, internal-time-units-per-second
        /// * car, cdr, cons, rplaca, rplacd
        /// * apply, eval, eq, eql, null, read, write, writeln, lnwrite
        /// * atom, consp, listp, symbolp, numberp, stringp, characterp, integerp, floatp
        /// * assoc, assq, list, list*, append
        /// * +, *, -, /, =, <, <=, >=, >
        /// * round, floor, ceiling, truncate
        /// * fround, ffloor, fceiling, ftruncate
        /// * sqrt, log, log10, exp, expt, mod, rem, signum
        /// * get-internal-real-time, get-internal-run-time, get-internal-cpu-time, sleep, get-universal-time, get-decoded-time
        /// * format, format-locale, char-code, code-char, string=, string->list, list->string
        /// * gensym, trace, untrace, (macroexpand-1), jmethod, jproxy
        /// * turtle-functions
        ///
        private static final String[] globalvars = { "nil", "t", "pi", "dynamic" };
        private static final String[][] aliasedGlobals = {
            { "internal-time-units-per-second", "itups" },
            { "*command-line-argument-list*", "commandlineArgumentList" },
            { "*features*", "features" },
        };
        private static final String[] primitives = {
                "car", "cdr", "cons", "rplaca", "rplacd",
                /*"apply",*/ "eval", "eq", "eql", "null", "read", "write", "writeln", "lnwrite",
                "atom", "consp", "listp", "symbolp", "numberp", "stringp", "characterp", "integerp", "floatp",
                "assoc", "assq", "list", "append", "values",
                "round", "floor", "ceiling", "truncate",
                "fround", "ffloor", "fceiling", "ftruncate",
                "sqrt", "log", "log10", "exp", "expt", "mod", "rem", "signum",
                "gensym", "trace", "untrace",
                "fatal", "jmethod", "jproxy",
        };
        private static final String[][] aliasedPrimitives = {
            {"+", "add"}, {"*", "mul"}, {"-", "sub"}, {"/", "quot"},
            {"=", "numbereq"}, {"<=", "le"}, {"<", "lt"}, {">=", "ge"}, {">", "gt"}, { "/=", "ne" },
            {"1+", "inc"}, {"1-", "dec"},
            {"format", "format"}, {"format-locale", "formatLocale" }, {"char-code", "charInt"}, {"code-char", "intChar"}, 
            {"string=", "stringeq"}, {"string->list", "stringToList"}, {"list->string", "listToString"},
            {"list*", "listStar"},
            //{ "macroexpand-1", "macroexpand1" },
            {"get-internal-real-time", "getInternalRealTime" }, {"get-internal-run-time", "getInternalRunTime" }, {"get-internal-cpu-time", "getInternalCpuTime" },
            {"sleep", "sleep" }, {"get-universal-time", "getUniversalTime" }, {"get-decoded-time", "getDecodedTime" },

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

        /** Compile the Murmel compilation unit {@code forms} to a Java class for a standalone application with a "public static void main()" */
        public Class <MurmelProgram> formsToJavaClass(String unitName, ObjectReader forms, String jarFileName) throws Exception {
            final StringWriter w = new StringWriter();
            formsToJavaSource(w, unitName, forms);
            return javaCompiler.javaToClass(unitName, w.toString(), jarFileName);
        }



        /// Wrappers to compile Murmel to Java source

        /** Compile the Murmel compilation unit to Java source for a standalone application class {@code unitName}
         *  with a "public static void main()" */
        public void formsToJavaSource(Writer w, String unitName, ObjectReader forms) {
            quotedForms.clear();  qCounter = 0;
            ConsCell predefinedEnv = null;
            for (String   global: globalvars)        predefinedEnv = extenvIntern(intern(global),   '_' + global,   predefinedEnv);
            for (String[] alias:  aliasedGlobals)    predefinedEnv = extenvIntern(intern(alias[0]), alias[1], predefinedEnv);
            for (String   prim:   primitives)        predefinedEnv = extenvprim(prim, mangle(prim, 0), predefinedEnv);
            for (String[] alias:  aliasedPrimitives) predefinedEnv = extenvprim(alias[0], alias[1], predefinedEnv);

            // _apply needs to be of type MurmelFunction so that it will be processed by the TCO trampoline
            predefinedEnv = extenvIntern(intp.intern("apply"), "((MurmelFunction)rt()::_apply)", predefinedEnv);

            final WrappingWriter ret = new WrappingWriter(w);

            final String clsName;
            final int dotpos = unitName.lastIndexOf('.');
            if (dotpos == -1) {
                clsName = unitName;
            } else {
                ret.append("package ").append(unitName.substring(0, dotpos)).append(";\n\n");
                clsName = unitName.substring(dotpos+1);
            }
            ret.append("import java.util.function.Function;\n"
                     + "import java.util.function.Supplier;\n"
                     + "import io.github.jmurmel.LambdaJ.*;\n\n"
                     + "public class ").append(clsName).append(" extends MurmelJavaProgram {\n"
                     + "    protected ").append(clsName).append(" rt() { return this; }\n\n"
                     + "    public static void main(String[] args) {\n"
                     + "        final ").append(clsName).append(" program = new ").append(clsName).append("();\n"
                     + "        program.commandlineArgumentList = arraySlice(args);\n"
                     + "        main(program);\n"
                     + "    }\n\n");

            final ArrayList<Object> bodyForms = new ArrayList<>();
            final StringBuilder globals = new StringBuilder();

            /// first pass: emit toplevel define/ defun forms
            final int prevSpeed = intp.speed;
            passTwo = false;
            implicitDecl = new HashSet<>();
            ConsCell globalEnv = predefinedEnv;
            final Object eof = "EOF";
            Object form;
            while (eof != (form = forms.readObj(true, eof))) {
                try {
                    globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, intp.expandForm(form));
                }
                catch (LambdaJError e) {
                    throw new LambdaJError(false, e.getMessage(), form);
                }
                catch (Exception e) {
                    throw errorInternal(e, "formToJava: caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
                }
            }

            if (!implicitDecl.isEmpty()) {
                errorMalformedFmt("compilation unit", "undefined symbols: %s", implicitDecl);
            }
            implicitDecl = null;

            intp.clearMacros(); // on pass2 macros will be re-interpreted at the right place so that illegal macro forward-refences are caught

            // emit getValue() for embed API
            ret.append("    @Override public Object getValue(String symbol) {\n");
            if (globals.length() > 0) ret.append("        switch (symbol) {\n").append(globals).append("        }\n");

            // ret.append("        switch (symbol) {\n");
            // for (String   global: globalvars)        ret.append("        case \"").append(global)  .append("\": return _").append(global).append(";\n");
            // for (String[] alias:  aliasedGlobals)    ret.append("        case \"").append(alias[0]).append("\": return ") .append(alias[1]).append(";\n");
            // for (String   prim:   primitives)        ret.append("        case \"").append(prim)    .append("\": return (CompilerPrimitive)rt()::_").append(prim).append(";\n");
            // for (String[] alias:  aliasedPrimitives) ret.append("        case \"").append(alias[0]).append("\": return (CompilerPrimitive)rt()::").append(alias[1]).append(";\n");
            // ret.append("        default: throw new LambdaJError(true, \"%s: '%s' is undefined\", \"getValue\", symbol);\n"
            //          + "        }\n");
            ret.append("        return super.getValue(symbol);\n");

            ret.append("    }\n\n"
                     + "    // toplevel forms\n"
                     + "    protected Object runbody() throws Exception {\n");

            /// second pass: emit toplevel forms that are not define or defun as well as the actual assignments for define/ defun
            intp.speed = prevSpeed;
            passTwo = true;
            emitForms(ret, bodyForms, globalEnv, globalEnv, 0, true);

            ret.append("    }\n");

            emitConstantPool(ret);

            ret.append("}\n");
            ret.flush();
        }

        private ConsCell toplevelFormToJava(WrappingWriter ret, List<Object> bodyForms, StringBuilder globals, ConsCell globalEnv, Object form) {
            final LambdaJ intp = this.intp;
            if (consp(form)) {
                final ConsCell ccForm = (ConsCell)form;
                final Object op = car(ccForm);

                if (op == intp.sDefine) {
                    globalEnv = defineToJava(ret, ccForm, globalEnv);
                    intp.eval(ccForm, null);
                }

                else if (op == intp.sDefun) {
                    globalEnv = defunToJava(ret, ccForm, globalEnv);
                    intp.eval(ccForm, null);
                }

                else if (op == intp.sDefmacro) {
                    intp.symbolOrMalformed("defmacro", cadr(ccForm));
                    intp.eval(ccForm, null);
                    bodyForms.add(form);
                    return globalEnv;
                }

                else if (op == intp.sProgn) {
                    // toplevel progn will be replaced by the forms it contains
                    final Object body = cdr(ccForm);
                    if (consp(body)) {
                        for (Object prognForm: (ConsCell)body) {
                            globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, prognForm);
                        }
                        return globalEnv;
                    }
                }

                final ConsCell macroClosure;
                if (op != null && symbolp(op) && null != (macroClosure = ((LambdaJSymbol)op).macro)) {
                    final Object expansion = intp.evalMacro(op, macroClosure, (ConsCell)cdr(form), 0, 0, 0);
                    globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, expansion);
                }

                else if (op == intp.sLoad) {
                    final ConsCell ccArgs = listOrMalformed("load", cdr(form));
                    oneArg("load", ccArgs);
                    globalEnv = loadFile(true, "load", ret, car(ccArgs), null, globalEnv, -1, false, bodyForms, globals);
                }

                else if (op == intp.sRequire) {
                    final ConsCell ccArgs = listOrMalformed("require", cdr(form));
                    varargsMinMax("require", ccArgs, 1, 2);
                    if (!stringp(car(ccArgs))) errorMalformed("require", "a string argument", ccArgs);
                    final Object modName = car(ccArgs);
                    if (!intp.modules.contains(modName)) {
                        Object modFilePath = cadr(ccArgs);
                        if (modFilePath == null) modFilePath = modName;
                        globalEnv = loadFile(true, "require", ret, modFilePath, null, globalEnv, -1, false, bodyForms, globals);
                        if (!intp.modules.contains(modName)) errorMalformedFmt("require", "require'd file '%s' does not provide '%s'", modFilePath, modName);
                    }
                }

                else if (op == intp.sProvide) {
                    final ConsCell ccArgs = listOrMalformed("provide", cdr(form));
                    oneArg("provide", ccArgs);
                    if (!stringp(car(ccArgs))) errorMalformed("provide", "a string argument", ccArgs);
                    final Object modName = car(ccArgs);
                    intp.modules.add(modName);
                }

                else if (op == intp.sDeclaim) {
                    intp.evalDeclaim(1, (ConsCell)cdr(form)); // todo kann form eine dotted list sein und der cast schiefgehen?
                    bodyForms.add(form);
                }

                else bodyForms.add(form);

                if (op == intp.sDefine || op == intp.sDefun)
                    globals.append("        case \"").append(cadr(form)).append("\": return ").append(javasym(cadr(form), globalEnv)).append(";\n");

            } else bodyForms.add(form);

            return globalEnv;
        }


        /** Emit a member for {@code symbol} and a function that assigns {@code form} to {@code symbol}.
         *  @param form a list (define symbol form) */
        private ConsCell defineToJava(WrappingWriter sb, ConsCell form, ConsCell env) {
            varargs1_2("toplevel define", listOrMalformed("toplevel define", cdr(form)));
            final LambdaJSymbol symbol = intp.symbolOrMalformed("define", cadr(form));
            notDefined("define", symbol, env);

            final String javasym = mangle(symbol.toString(), 0);
            env = extenvIntern(symbol, javasym + ".get()", env); // ggf. die methode define_javasym OHNE javasym im environment generieren, d.h. extenvIntern erst am ende dieser methode

            sb.append("    // ").append(form.lineInfo()).append("(define ").append(symbol).append(" ...)\n"
                    + "    public CompilerGlobal ").append(javasym).append(" = UNASSIGNED_GLOBAL;\n");

            sb.append("    public Object define_").append(javasym).append("() {\n"
                    + "        loc = \"");  stringToJava(sb, form.lineInfo(), -1);  stringToJava(sb, printSEx(form), 40);  sb.append("\";\n"
                    + "        if (").append(javasym).append(" != UNASSIGNED_GLOBAL) rterror(new LambdaJError(\"duplicate define\"));\n"
                    + "        try { final Object value = "); emitForm(sb, caddr(form), env, env, 0, false); sb.append(";\n"
                    + "        ").append(javasym).append(" = new CompilerGlobal(value); }\n"
                    + "        catch (LambdaJError e) { rterror(e); }\n"
                    + "        return intern(\"").append(symbol).append("\");\n"
                    + "    }\n\n");
            return env;
        }

        /** @param form a list (defun symbol ((symbol...) forms...)) */
        private ConsCell defunToJava(WrappingWriter sb, ConsCell form, ConsCell env) {
            final LambdaJSymbol symbol = intp.symbolOrMalformed("defun", cadr(form));
            final Object params = caddr(form);
            final Object body = cdddr(form);
            notDefined("defun", symbol, env);

            final String javasym = mangle(symbol.toString(), 0);
            env = extenvIntern(symbol, javasym + ".get()", env);

            sb.append("    // ").append(form.lineInfo()).append("(defun ").append(symbol).append(' '); printSEx(sb::append, params); sb.append(" forms...)\n"
                    + "    private CompilerGlobal ").append(javasym).append(" = UNASSIGNED_GLOBAL;\n");

            sb.append("    public LambdaJSymbol defun_").append(javasym).append("() {\n"
                    + "        loc = \"");  stringToJava(sb, form.lineInfo(), -1);  stringToJava(sb, printSEx(form), 40);  sb.append("\";\n"
                    + "        if (").append(javasym).append(" != UNASSIGNED_GLOBAL) rterror(new LambdaJError(\"duplicate defun\"));\n"
                    + "        final MurmelFunction func = (args0) -> {\n");
            final ConsCell extenv = params("defun", sb, params, env, 0, javasym, true);
            emitForms(sb, (ConsCell)body, extenv, env, 0, false);
            sb.append("        };\n"
                    + "        ").append(javasym).append(" = new CompilerGlobal(func);\n"
                    + "        return intern(\"").append(symbol).append("\");\n"
                    + "    }\n\n");

            return env;
        }



        /// emitForms - compile a list of Murmel forms to Java source
        /** generate Java code for a list of forms. Each form but the last will be emitted as an assignment
         *  to the local variable "ignoredN" because some forms are emitted as ?: expressions which is not a valid statement by itself. */
        private void emitForms(WrappingWriter ret, Iterable<Object> forms, ConsCell env, ConsCell topEnv, int rsfx, boolean topLevel) {
            final Iterator<Object> it;
            if (forms == null || !(it = forms.iterator()).hasNext()) {
                // e.g. the body of an empty lambda or function
                ret.append("        return null;\n");
                return;
            }

            boolean ign = false;
            while (it.hasNext()) {
                final Object form = it.next();
                if (consp(form)) { ret.append("        loc = \""); stringToJava(ret, ((ConsCell)form).lineInfo(), -1); stringToJava(ret, printSEx(form), 100); ret.append("\";\n        "); }
                else ret.append("        ");
                if (it.hasNext()) {
                    if (!ign) {
                        ret.append("Object ");
                        ign = true;
                    }
                    ret.append("ignored").append(rsfx).append(" = ");
                }
                else ret.append("return ");
                emitForm(ret, form, env, topEnv, rsfx, !topLevel && !it.hasNext());
                ret.append(";\n");
            }
        }

        /// formToJava - compile a Murmel form to Java source. Note how this is somehow similar to eval:
        private void emitForm(WrappingWriter sb, Object form, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final LambdaJ intp = this.intp;
            rsfx++;
            try {

                /// * symbols
                if (symbolp(form)) {
                    sb.append(javasym(form, env));  return;
                }
                /// * atoms that are not symbols
                if (atom(form)) {
                    emitAtom(sb, form);  return;
                }

                if (consp(form)) {
                    final ConsCell ccForm = (ConsCell)form;
                    final Object op = car(ccForm);      // first element of the of the form should be a symbol or an expression that computes a symbol
                    final ConsCell ccArguments = listOrMalformed("emitForm", cdr(ccForm));   // list with remaining atoms/ expressions


                    /// * special forms:

                    ///     - quote
                    if (intp.sQuote == op) { emitQuotedForm(sb, car(ccArguments), true); return; }

                    ///     - if
                    if (intp.sIf == op) {
                        varargsMinMax("if", ccArguments, 2, 3);
                        if (consp(car(ccArguments)) && caar(ccArguments) == intp.sNull) {
                            // optimize "(if (null ...) trueform falseform)" to "(if ... falseform trueform)"
                            final ConsCell transformed = ListBuilder.list(intp.sIf, cadar(ccArguments), caddr(ccArguments), cadr(ccArguments)); 
                            emitForm(sb, transformed, env, topEnv, rsfx, isLast);
                            return;
                        }
                        sb.append("(");
                        emitTruthiness(sb, car(ccArguments), env, topEnv, rsfx);
                        sb.append("\n        ? ("); emitForm(sb, cadr(ccArguments), env, topEnv, rsfx, isLast);
                        if (caddr(ccArguments) != null) { sb.append(")\n        : ("); emitForm(sb, caddr(ccArguments), env, topEnv, rsfx, isLast); sb.append("))"); }
                        else sb.append(")\n        : (Object)null)");
                        return;
                    }

                    ///     - cond
                    if (intp.sCond == op) {
                        emitCond(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - lambda
                    if (intp.sLambda == op) {
                        emitLambda(sb, ccArguments, env, topEnv, rsfx, true);
                        return;
                    }

                    ///     - setq
                    if (intp.sSetQ == op) {
                        if (ccArguments == null) sb.append("(Object)null"); // must cast to Object in case it will be used as the only argument to a vararg function
                        else if (cddr(ccArguments) == null)
                            emitSetq(sb, ccArguments, env, topEnv, rsfx);
                        else {
                            sb.append("((Supplier<Object>)(() -> {\n");
                            String javaName = null;
                            for (Object pairs = ccArguments; pairs != null; pairs = cddr(pairs)) {
                                sb.append("        ");
                                javaName = emitSetq(sb, pairs, env, topEnv, rsfx-1);
                                sb.append(";\n");
                            }
                            sb.append("        return ").append(javaName).append(";})).get()");
                        }
                        return;
                    }

                    if (intp.sDefine == op) {
                        if (rsfx != 1) errorNotImplemented("define as non-toplevel form is not yet implemented");
                        defined("define", car(ccArguments), env);
                        final String javasym = mangle(car(ccArguments).toString(), 0);
                        sb.append("define_").append(javasym).append("()");
                        return;
                    }

                    if (intp.sDefun == op) {
                        if (rsfx != 1) errorNotImplemented("defun as non-toplevel form is not yet implemented");
                        defined("defun", car(ccArguments), env);
                        final String javasym = mangle(car(ccArguments).toString(), 0);
                        sb.append("defun_").append(javasym).append("()");
                        return;
                    }

                    if (intp.sDefmacro == op) {
                        if (rsfx != 1) errorNotImplemented("defmacro as non-toplevel form is not yet implemented");
                        intp.expandForm(form); // this will process the macro definition as a side effect
                        sb.append("intern(\"").append(car(ccArguments)).append("\")");
                        return;
                    }

                    ///     - progn
                    if (intp.sProgn == op) {
                        emitProgn(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - labels: (labels ((symbol (params...) forms...)...) forms...) -> object
                    // note how labels is similar to let: let binds values to symbols, labels binds functions to symbols
                    if (intp.sLabels == op) {
                        emitLabels(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let: (let ((sym form)...) forms...) -> object
                    ///     - named let: (let sym ((sym form)...) forms...) -> object
                    if (intp.sLet == op) {
                        if (car(ccArguments) == intp.sDynamic)
                            emitLetLetStarDynamic(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, false, isLast);
                        else
                            emitLet(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let*: (let* ((sym form)...) forms...) -> Object
                    ///     - named let*: (let sym ((sym form)...) forms...) -> Object
                    if (intp.sLetStar == op) {
                        if (car(ccArguments) == intp.sDynamic)
                            emitLetLetStarDynamic(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, true, isLast);
                        else
                            emitLetStarLetrec(sb, ccArguments, env, topEnv, rsfx, false, isLast);
                        return;
                    }

                    ///     - letrec:       (letrec ((sym form)...) forms) -> Object
                    ///     - named letrec: (letrec sym ((sym form)...) forms) -> Object
                    if (intp.sLetrec == op) {
                        emitLetStarLetrec(sb, ccArguments, env, topEnv, rsfx, true, isLast);
                        return;
                    }

                    if (intp.sMultipleValueCall == op) {
                        sb.append(isLast ? "tailcall(" : "funcall(");
                        emitForm(sb, car(ccArguments), env, topEnv, rsfx, false);
                        if (cdr(ccArguments) != null) {
                            sb.append(", rt().new ValuesBuilder()");
                            for (Object arg: listOrMalformed("multiple-value-call", cdr(ccArguments))) {
                                sb.append("\n        .add(");
                                emitForm(sb, arg, env, topEnv, rsfx, false);
                                sb.append(')');
                            }
                            sb.append("\n        .build()");
                        }
                        else sb.append(", NOARGS");
                        sb.append(')');
                        return;
                    }

                    ///     - multiple-value-bind: (multiple-value-bind (var*) value-form forms)
                    if (intp.sMultipleValueBind == op) {
                        varargsMin("multiple-value-bind", ccArguments, 2);
                        final ConsCell vars = listOrMalformed("multiple-value-bind", car(ccArguments));
                        final boolean varargs = dottedList(vars);
                        int length = length(vars);
                        if (varargs) length--;
                        sb.append(isLast ? "tailcall(" : "funcall(");
                        emitLambda(sb, cons(vars, cddr(ccArguments)), env, topEnv, rsfx, false);
                        if (cadr(ccArguments) != null) {
                            sb.append(", rt().new ValuesBuilder()\n        .add(");
                            emitForm(sb, cadr(ccArguments), env, topEnv, rsfx, false);
                            sb.append(")\n        .build(").append(length).append(',').append(String.valueOf(!varargs)).append(")");
                        }
                        else sb.append(", NOARGS");
                        sb.append(')');
                        return;
                    }

                    if (intp.sLoad == op) {
                        varargs1("load", ccArguments);
                        // todo aenderungen im environment gehen verschuett, d.h. define/defun funktioniert nur bei toplevel load, nicht hier
                        loadFile(false, "load", sb, car(ccArguments), env, topEnv, rsfx-1, isLast, null, null);
                        return;
                    }

                    if (intp.sRequire == op) {
                        // pass1 has replaced all toplevel (require)s with the file contents
                        errorNotImplemented("require as non-toplevel form is not implemented");
                    }

                    if (intp.sProvide == op) {
                        // pass 2 shouldn't see this
                        errorNotImplemented("provide as non-toplevel form is not implemented");
                    }

                    if (intp.sDeclaim == op) {
                        intp.evalDeclaim(rsfx, ccArguments);
                        sb.append("(Object)null");
                        return;
                    }

                    /// * macro expansion
                    final ConsCell macroClosure;
                    if (op != null && symbolp(op) && null != (macroClosure = ((LambdaJSymbol)op).macro)) {
                        final Object expansion = intp.evalMacro(op, macroClosure, ccArguments, 0, 0, 0);
                        emitForm(sb, expansion, env, topEnv, rsfx-1, isLast);
                        return;
                    }

                    /// * special case (hack) for calling macroexpand-1: only quoted forms are supported which can be performed a compile time
                    if (symbolEq(op, "macroexpand-1")) {
                        if (intp.sQuote != caar(ccArguments)) errorNotImplemented("general macroexpand-1 is not implemented, only quoted forms are: (macroexpand-1 '..."); 
                        sb.append("((Supplier<Object>)(() -> {\n"
                                  + "        final Object expansion").append(rsfx).append(" = ");
                        emitQuotedForm(sb, intp.macroexpand1((ConsCell)cdar(ccArguments)), true);
                        final String expanded = cadr(intp.values) == intp.sT ? "rt()._t" : "null";
                        sb.append("; return rt()._values(expansion").append(rsfx).append(", ").append(expanded).append(");\n        })).get()");
                        return;
                    }

                    /// * some functions and operators are opencoded:
                    if (intp.speed >= 1 && symbolp(op) && opencode(sb, (LambdaJSymbol)op, ccArguments, env, topEnv, rsfx, isLast)) return;

                    if (intp.speed >= 1 && consp(op) && symbolp(car(op)) && symbolEq(car(op), "jmethod")
                        && emitJmethod(sb, requireCons("jmethod application", cdr(op)), env, topEnv, rsfx, true, ccArguments)) {
                        return;
                    }

                    /// * function call
                    sb.append(isLast ? "tailcall(" : "funcall(");
                    emitForm(sb, op, env, topEnv, rsfx, false);
                    if (ccArguments != null) {
                        for (Object arg: ccArguments) {
                            sb.append("\n        , ");
                            emitForm(sb, arg, env, topEnv, rsfx, false);
                        }
                    }
                    else sb.append(", NOARGS");
                    sb.append(')');
                    return;
                }
                throw new LambdaJError("emitForm: form not implemented: " + printSEx(form));

            }
            catch (LambdaJError e) {
                throw new LambdaJError(false, e.getMessage(), form);
            }
            catch (Exception e) {
                //e.printStackTrace();
                throw errorInternal(e, "emitForm: caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
            }
        }

        private void emitTruthiness(WrappingWriter sb, Object form, ConsCell env, ConsCell topEnv, int rsfx) {
            if (form == null || form == intp.sNil) sb.append("false");
            else if (form == intp.sT) sb.append("true");
            else if (consp(form) && car(form) == intp.sNull) {
                // optimize "(null ..."
                sb.append("(!("); emitTruthiness(sb, cadr(form), env, topEnv, rsfx); sb.append("))");
            }
            else if (consp(form) || symbolp(form)) {
                sb.append('('); emitForm(sb, form, env, topEnv, rsfx, false); sb.append(") != null");
            }
            else sb.append("true"); // must be an atom other than nil or a symbol -> true
        }

        /** write atoms that are not symbols (and "nil" is acceptable, too) */
        private void emitAtom(WrappingWriter sb, Object form) {
            if (form == null || form == intp.sNil) sb.append("(Object)null");
            else if (form instanceof Long) sb.append(Long.toString((Long) form)).append('L');
            else if (form instanceof Double) sb.append(Double.toString((Double) form));
            else if (form instanceof Character) {
                final char c = (Character) form;
                switch (c) {
                case '\'': sb.append("'\\''"); break;
                case '\\': sb.append("'\\\\'"); break;
                case '\r': sb.append("'\\r'"); break;
                case '\n': sb.append("'\\n'"); break;
                case '\t': sb.append("'\\t'"); break;
                default:
                    if (c >= 32 && c < 127) sb.append('\'').append(c).append('\'');
                    else sb.append(String.format("'\\u%04X'", (int)c));
                }
            }
            //else if (form instanceof String) sb.append("new String(\"").append(form).append("\")"); // new Object so that (eql "a" "a") is nil (Common Lisp allows both nil and t). otherwise the reader must intern strings as well
            else if (form instanceof String) { sb.append('"'); stringToJava(sb, (String)form, -1); sb.append('"'); }
            else errorInternal("emitAtom: atom %s is not implemented", form.toString());
        }

        private static void stringToJava(WrappingWriter sb, String s, int maxlen) {
            if (s == null)   { sb.append("null"); return; }
            if (s.isEmpty()) { sb.append(""); return; }

            final int length = s.length();
            for (int i = 0; i < length; i++) {
                if (maxlen > 0 && i == maxlen) { sb.append("..."); return; }
                final char c = s.charAt(i);
                switch (c) {
                case '\"': sb.append("\\\""); break;
                case '\\': sb.append("\\\\");   break;
                case '\r': sb.append("\\r");  break;
                case '\n': sb.append("\\n");  break;
                case '\t': sb.append("\\t");  break;
                default:
                    if (c >= 32 && c < 127) sb.append(c);
                    else sb.append(String.format("\\u%04X", (int)c));
                }
            }
        }

        private void emitCond(WrappingWriter sb, ConsCell condForm, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (condForm == null) {
                sb.append("(Object)null");
            } else {
                sb.append("(");
                boolean first = true;
                for (final Iterator<Object> iterator = condForm.iterator(); iterator.hasNext(); ) {
                    final Object clause = iterator.next();
                    if (first) first = false;
                    else sb.append("\n        : ");
                    final Object condExpr = car(clause), condForms = cdr(clause);
                    if (condExpr == intp.sT) {
                        emitProgn(sb, condForms, env, topEnv, rsfx, isLast);  sb.append(')');
                        if (iterator.hasNext()) System.err.println(condForm.lineInfo() + "forms following default 't' form will be ignored");
                        return;
                    } else {
                        emitTruthiness(sb, condExpr, env, topEnv, rsfx);
                        sb.append("\n        ? (");
                        emitProgn(sb, condForms, env, topEnv, rsfx, isLast);
                        sb.append(')');
                    }
                }
                sb.append("\n        : (Object)null)");
            }
        }

        /** paramsAndForms = ((sym...) form...) */
        private void emitLambda(WrappingWriter sb, final ConsCell paramsAndForms, ConsCell env, ConsCell topEnv, int rsfx, boolean argCheck) {
            sb.append("(MurmelFunction)(args").append(rsfx).append(" -> {\n");
            final Object params = car(paramsAndForms);
            final String expr = "(lambda " + printSEx(params) + " ...)";
            env = params("lambda", sb, params, env, rsfx, expr, argCheck);
            emitForms(sb, (ConsCell)cdr(paramsAndForms), env, topEnv, rsfx, false);
            sb.append("        })");
        }

        private int ignoredCounter = 0;

        /** emit a list of forms as a single Java expression */
        private void emitProgn(WrappingWriter sb, Object forms, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (!listp(forms)) errorMalformed("progn", "a list of forms", forms);
            final ConsCell ccForms = (ConsCell)forms;
            if (cdr(ccForms) == null) emitForm(sb, car(ccForms), env, topEnv, rsfx, isLast);
            else {
                sb.append(isLast ? "tailcall(" : "funcall(").append("(MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n");
                emitForms(sb, ccForms, env, topEnv, rsfx, false);
                sb.append("        }, (Object[])null)");
            }
        }

        private String emitSetq(WrappingWriter sb, Object pairs, ConsCell env, ConsCell topEnv, int rsfx) {
            final LambdaJSymbol symbol = intp.symbolOrMalformed("setq", car(pairs));
            final String javaName = javasym(symbol, env);

            if (cdr(pairs) == null) errorMalformed("setq", "odd number of arguments");
            final Object valueForm = cadr(pairs);

            notAPrimitive("setq", symbol, javaName);
            if (assq(symbol, env) == assq(symbol, topEnv)) {
                final String symName = mangle(symbol.toString(), 0);
                sb.append(symName).append(".set("); emitForm(sb, valueForm, env, topEnv, rsfx, false); sb.append(')');
            } else {
                sb.append(javaName).append(" = ");
                emitForm(sb, valueForm, env, topEnv, rsfx, false);
            }
            return javaName;
        }

        /** args = (formsym (sym...) form...) */
        private void emitLabel(String func, WrappingWriter sb, final Object symbolParamsAndForms, ConsCell env, ConsCell topEnv, int rsfx) {
            final LambdaJSymbol symbol = intp.symbolOrMalformed(func, car(symbolParamsAndForms));
            env = extenv(func, symbol, rsfx, env);

            sb.append("new MurmelFunction() {\n");
            sb.append("        private final MurmelFunction ").append(javasym(symbol, env)).append(" = this;\n"); // "Object o = (MurmelFunction)this::apply" is the same as "final Object x = this"
            sb.append("        public Object apply(Object... args").append(rsfx).append(") {\n");
            env = params(func, sb, cadr(symbolParamsAndForms), env, rsfx, symbol.toString(), true);
            emitForms(sb, (ConsCell)cddr(symbolParamsAndForms), env, topEnv, rsfx, false);
            sb.append("        } }");
        }

        /** args = (((symbol (sym...) form...)...) form...) */
        private void emitLabels(WrappingWriter sb, final ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (args == null) errorMalformed("labels", "expected at least one argument");

            final Object localFuncs = car(args);
            if (localFuncs == null || cddr(args) == null && atom(cadr(args))) {
                // no local functions or body is one single atom (the latter can't use the functions so skip them
                emitProgn(sb, cdr(args), env, topEnv, rsfx, isLast); // todo warum nicht emitAtom?
                return;
            }

            sb.append(isLast ? "tailcall(" : "funcall(");
            sb.append("new MurmelFunction() {\n");

            final ConsCell params = paramList("labels", localFuncs, true);
            for (Object localFunc: params) {
                env = extenv("labels", localFunc, rsfx, env);
            }

            for (Object symbolParamsAndBody: (ConsCell) localFuncs) {
                sb.append("        private final MurmelFunction ").append(javasym(car(symbolParamsAndBody), env)).append(" = ");
                emitLabel("labels", sb, symbolParamsAndBody, env, topEnv, rsfx+1);
                sb.append(";\n");
            }

            sb.append("        @Override public Object apply(Object... ignored) {\n");
            emitForms(sb, (ConsCell)cdr(args), env, topEnv, rsfx, false); // todo isLast statt false? oder .apply() statt tailcall/funcall?
            sb.append("        } }, NOARGS)");
        }

        /** let and named let */
        private void emitLet(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final boolean named = car(args) instanceof LambdaJSymbol;
            final Object loopLabel, bindings, body;
            if (named) { loopLabel = car(args); args = (ConsCell)cdr(args); }
            else       { loopLabel = null; }
            bindings = car(args);  body = cdr(args);
            if (bindings == null && body == null) { sb.append("(Object)null"); return; }

            sb.append(isLast ? "tailcall(" : "funcall(");

            final String op = named ? "named let" : "let";
            final ConsCell ccBindings = (ConsCell)bindings;
            final ConsCell params = paramList(op, ccBindings, false);
            if (named) {
                // named let
                emitLabel(op, sb, cons(loopLabel, cons(params, body)), env, topEnv, rsfx+1);
            } else {
                // regular let
                emitLambda(sb, cons(params, body), env, topEnv, rsfx+1, false);
            }
            if (ccBindings != null) {
                for (Object binding : ccBindings) {
                    sb.append("\n        , ");
                    if (consp(binding)) {
                        if (cddr(binding) != null) errorMalformedFmt(op, "illegal variable specification %s", printSEx(binding));
                        emitForm(sb, cadr(binding), env, topEnv, rsfx, false);
                    }
                    else sb.append("(Object)null"); // simplified let: (let (aaa) aaa)
                }
            } else sb.append(", NOARGS");
            sb.append(')');
        }

        /**
         * let* and letrec
         * args = ([name] ((symbol form)...) forms...) */
        private void emitLetStarLetrec(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean letrec, boolean isLast) {
            final boolean named = car(args) instanceof LambdaJSymbol;
            final Object loopLabel, bindings, body;
            if (named) { loopLabel = car(args); args = (ConsCell)cdr(args); }
            else       { loopLabel = null; }
            bindings = car(args);  body = cdr(args);
            if (bindings == null && body == null) { sb.append("(Object)null"); return; }

            sb.append(isLast ? "tailcall(" : "funcall(");
            sb.append("new MurmelFunction() {\n");

            final String op;
            if (named) {
                // named letrec: (letrec sym ((sym form)...) forms...) -> Object
                if (loopLabel == intp.sDynamic) errorMalformed("letrec", "dynamic is only allowed with let and let*");
                op = letrec ? "named letrec" : "named let*";
                if (!listp(bindings)) errorMalformed(op, "a list of bindings", bindings);
            }
            else {
                op = letrec ? "letrec" : "let*";
            }

            if (loopLabel != null) {
                env = extenv(op, loopLabel, rsfx, env);
                sb.append("        private final Object ").append(javasym(loopLabel, env)).append(" = this;\n");
            }
            sb.append("        @Override public Object apply(Object... args").append(rsfx).append(") {\n");
            final int argCount = length(bindings);
            if (argCount != 0) {
                sb.append("        if (args").append(rsfx).append("[0] == UNASSIGNED_LOCAL) {\n");

                // letrec: ALL let-bindings are in the environment during binding of the initial values todo value should be undefined
                int current = 0;
                if (letrec) for (Object binding: (ConsCell)bindings) {
                    final LambdaJSymbol sym;
                    if (symbolp(binding)) { sym = (LambdaJSymbol)binding; }
                    else { sym = intp.symbolOrMalformed(op, car(binding)); }
                    final String symName = "args" + rsfx + '[' + current++ + ']';
                    env = extenvIntern(sym, symName, env);
                }

                // initial assignments. let*: after the assignment add the let-symbol to the environment so that subsequent bindings will see it
                current = 0;
                for (Object binding: (ConsCell)bindings) {
                    final LambdaJSymbol sym;
                    final Object form;
                    if (consp(binding) && cddr(binding) != null) errorMalformedFmt(op, "illegal variable specification %s", printSEx(binding));
                    if (symbolp(binding)) { sym = (LambdaJSymbol)binding; form = null; }
                    else { sym = intp.symbolOrMalformed(op, car(binding)); form = cadr(binding); }
                    final String symName = "args" + rsfx + '[' + current++ + ']';
                    sb.append("        { ").append(symName).append(" = ");
                    emitForm(sb, form, env, topEnv, rsfx, false);
                    if (!letrec) env = extenvIntern(sym, symName, env);
                    sb.append("; }\n");
                }

                sb.append("        }\n");
                sb.append("        else argCheck(loc, ").append(argCount).append(", args").append(rsfx).append(".length);\n");
            }
            emitForms(sb, (ConsCell)body, env, topEnv, rsfx, isLast);
            sb.append("        } }, unassigned(").append(argCount).append("))");
        }

        /** let dynamic and let* dynamic */
        private void emitLetLetStarDynamic(WrappingWriter sb, final ConsCell bindingsAndForms, ConsCell env, ConsCell topEnv, int rsfx, boolean letStar, boolean isLast) {
            if (car(bindingsAndForms) == null && cdr(bindingsAndForms) == null) { sb.append("(Object)null"); return; }

            final String op = letStar ? "let* dynamic" : "let dynamic";
            sb.append(isLast ? "tailcallWithCleanup(" : "funcall(");

            sb.append("(MurmelFunction)(args").append(rsfx+1).append(" -> {\n");

            final ArrayList<String> globals = new ArrayList<>();

            final Object bindings = car(bindingsAndForms);
            final ConsCell params = paramList(op, bindings, false);
            ConsCell _env = env;
            if (params != null) {
                if (letStar) {
                    int n = 0;
                    final HashSet<Object> seenSymbols = new HashSet<>();
                    final Iterator<Object> bi = ((ConsCell)bindings).iterator();
                    for (final Object sym: params) {
                        final boolean seen = !seenSymbols.add(sym);
                        final String javaName;
                        if (seen) javaName = javasym(sym, _env);
                        else javaName = "args" + (rsfx + 1) + "[" + n++ + "]";

                        final String globalName;
                        final ConsCell maybeGlobal = assq(sym, topEnv);
                        if (maybeGlobal != null) {
                            notAPrimitive("let* dynamic", sym, cdr(maybeGlobal).toString());
                            globalName = mangle(sym.toString(), 0);
                            if (!seen) {
                                globals.add(globalName);
                                sb.append("        ").append(globalName).append(".push();\n");
                            }
                        }
                        else globalName = null; // letXX dynamic can bind both global as well as new local variables

                        final Object binding = bi.next();
                        sb.append("        ").append(javaName).append(" = ");
                        emitForm(sb, cadr(binding), env, topEnv, rsfx, false);
                        sb.append(";\n");
                        if (!seen) _env = extenvIntern((LambdaJSymbol)sym, javaName, env);

                        if (maybeGlobal != null) sb.append("        ").append(globalName).append(".set(").append(javasym(sym, _env)).append(");\n");
                    }
                }
                else {
                    _env = params("let dynamic", sb, params, env, rsfx + 1, null, false);
                    for (final Object sym: params) {
                        final ConsCell maybeGlobal = assq(sym, topEnv);
                        if (maybeGlobal != null) {
                            notAPrimitive("let dynamic", sym, cdr(maybeGlobal).toString());
                            final String globalName = mangle(sym.toString(), 0);
                            globals.add(globalName);
                            sb.append("        ").append(globalName).append(".push(").append(javasym(sym, _env)).append(");\n");
                        }
                    }
                }
            }

            if (isLast) {
                emitForms(sb, (ConsCell)cdr(bindingsAndForms), _env, topEnv, rsfx + 1, false);
                sb.append("        })\n");
                if (globals.isEmpty()) {
                    sb.append("        , null");
                }
                else {
                    sb.append("        , (MurmelFunction)(args").append(rsfx+1).append(" -> {\n");
                    for (String globalName : globals) sb.append("        ").append(globalName).append(".pop();\n");
                    sb.append("        return null;\n        })");
                }
            }
            else {
                if (!globals.isEmpty()) sb.append("        try {\n");

                // set parameter "topLevel" to true to avoid TCO. TCO would effectively disable the finally clause
                emitForms(sb, (ConsCell)cdr(bindingsAndForms), _env, topEnv, rsfx + 1, params != null);

                if (!globals.isEmpty()) {
                    sb.append("        }\n        finally {\n");
                    for (String globalName : globals) sb.append("        ").append(globalName).append(".pop();\n");
                    sb.append("        }\n");
                }
                sb.append("        })");
            }

            if (bindings != null)
                for (Object binding: (ConsCell)bindings) {
                    sb.append("\n        , ");
                    if (consp(binding) && cddr(binding) != null) errorMalformedFmt(op, "illegal variable specification %s", printSEx(binding));
                    if (letStar) sb.append("(Object)null");
                    else emitForm(sb, cadr(binding), env, topEnv, rsfx, false);
                }
            else sb.append(", NOARGS");
            sb.append(')');
        }



        /** from a list of bindings extract a new list of symbols: ((symbol1 form1)|symbol...) -> (symbol1...) */
        private static ConsCell paramList(String func, Object bindings, boolean lists) {
            if (bindings == null) return null;
            ConsCell params = null, insertPos = null;
            for (Object binding: (ConsCell)bindings) {
                if (params == null) {
                    params = cons(null, null);
                    insertPos = params;
                }
                else {
                    insertPos.rplacd(cons(null, null));
                    insertPos = (ConsCell) insertPos.cdr();
                }
                if (!lists && symbolp(binding)) insertPos.rplaca(binding);
                else if (consp(binding)) insertPos.rplaca(car(binding));
                else errorMalformed(func, "a binding", binding);
            }
            return params;
        }

        /** optionally emit an arg count check, check that there are no duplicates
         *  and return an environment extended by accesses to the arg array */
        private ConsCell params(String func, WrappingWriter sb, Object paramList, ConsCell env, int rsfx, String expr, boolean check) {
            if (paramList == null) {
                if (check) sb.append("        argCheck(\"").append(expr).append("\", 0, args").append(rsfx).append(".length);\n");
                return env;
            }

            if (symbolp(paramList)) {
                // (lambda a forms...) - style varargs
            }
            else if (dottedList(paramList)) {
                if (check) sb.append("        argCheckVarargs(\"").append(expr).append("\", ").append(length(paramList)).append(", args").append(rsfx).append(".length);\n");
            }
            else if (check) sb.append("        argCheck(\"").append(expr).append("\", ").append(length(paramList)).append(", args").append(rsfx).append(".length);\n");

            final HashSet<Object> seen = new HashSet<>();
            int n = 0;
            for (Object params = paramList; params != null; ) {
                if (consp(params)) {
                    final LambdaJSymbol param = intp.symbolOrMalformed(func, car(params));
                    if (!seen.add(param)) errorMalformedFmt(func, "duplicate symbol %s", param);
                    env = extenvIntern(param, "args" + rsfx + "[" + n++ + "]", env);
                }

                else if (symbolp(params)) {
                    intp.notReserved(func, (LambdaJSymbol)params);
                    if (!seen.add(params)) errorMalformedFmt(func, "duplicate symbol %s", params);
                    env = extenv(func, params, rsfx, env);
                    sb.append("        final Object ").append(javasym(params, env)).append(" = arrayToList(args").append(rsfx).append(", ").append(n).append(");\n");
                    return env;
                }
                
                else errorMalformed(func, "a symbol or a list of symbols", params);

                params = cdr(params);
            }
            return env;
        }

        private ConsCell loadFile(boolean pass1, String func, WrappingWriter sb, Object argument, ConsCell _env, ConsCell topEnv, int rsfx, boolean isLast, List<Object> bodyForms, StringBuilder globals) {
            final LambdaJ intp = this.intp;
            final Path p = intp.findFile(func, intp.getLispReader(), argument);
            final Object eof = "EOF";
            try (Reader r = Files.newBufferedReader(p)) {
                final SExpressionReader parser = intp.makeReader(r::read, p);
                for (;;) {
                    final Object form = parser.readObj(true, eof);
                    if (form == eof) break;

                    if (pass1) topEnv = toplevelFormToJava(sb, bodyForms, globals, topEnv, intp.expandForm(form));
                    else emitForm(sb, form, _env, topEnv, rsfx, isLast);
                }
                return topEnv;
            } catch (IOException e) {
                throw new LambdaJError(true, "load: error reading file '%s': ", e.getMessage());
            }
        }

        // todo throw error on circular list
        private static boolean dottedList(Object l) {
            for (;;) {
                if (l == null) return false;
                if (!consp(l)) return true;
                l = cdr(l);
            }
        }

        /** opencode some primitives, avoid trampoline for other primitives and avoid some argcount checks */
        private boolean opencode(WrappingWriter sb, LambdaJSymbol op, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final LambdaJ intp = this.intp;
            final LambdaJSymbol sApply = intp.intern("apply");

            if (op == sApply) {
                twoArgs("apply", args);
                final Object applyOp = car(args);
                final Object applyArg = cadr(args);

                if (applyOp == intp.sList) { sb.append("requireList("); emitForm(sb, applyArg, env, topEnv, rsfx, false); sb.append(")"); return true; }

                if (applyOp != sApply) { // apply needs special treatment for TCO
                    for (String prim: primitives)          if (symbolEq(applyOp, prim))    { opencodeApplyHelper(sb, "_" + prim,  applyArg, env, topEnv, rsfx);  return true; }
                    for (String[] prim: aliasedPrimitives) if (symbolEq(applyOp, prim[0])) { opencodeApplyHelper(sb, prim[1],  applyArg, env, topEnv, rsfx);  return true; }
                }

                sb.append(isLast ? "tailcall" : "funcall").append("((MurmelFunction)rt()::apply, ");
                emitForm(sb, applyOp, env, topEnv, rsfx, false);  sb.append(", ");
                emitForm(sb, applyArg, env, topEnv, rsfx, false);
                sb.append(")");
                return true;
            }

            if (op == intp.sAdd) { emitAddDbl(sb, "+", 0.0, args, env, topEnv, rsfx); return true; }
            if (op == intp.sMul) { emitAddDbl(sb, "*", 1.0, args, env, topEnv, rsfx); return true; }
            if (op == intp.sSub) { emitSubDbl(sb, "-", 0.0, args, env, topEnv, rsfx); return true; }
            if (op == intp.sDiv) { emitSubDbl(sb, "/", 1.0, args, env, topEnv, rsfx); return true; }

            if (op == intp.sMod) { emitFuncall2Numbers(sb, "mod", "cl_mod", args, env, topEnv, rsfx); return true; }
            if (op == intp.sRem) {
                twoArgs("rem", args);
                sb.append("(");
                emitFormAsDouble(sb, "rem", car(args), env, topEnv, rsfx);  sb.append(" % ");  emitFormAsDouble(sb, "rem", cadr(args), env, topEnv, rsfx);
                sb.append(")");
                return true;
            }

            if (symbolEq(op, "round"))     { emitDivision(sb, args, env, topEnv, rsfx, "round",     "cl_round",    true);  return true; }
            if (symbolEq(op, "floor"))     { emitDivision(sb, args, env, topEnv, rsfx, "floor",     "Math.floor",  true);  return true; }
            if (symbolEq(op, "ceiling"))   { emitDivision(sb, args, env, topEnv, rsfx, "ceiling",   "Math.ceil",   true);  return true; }
            if (symbolEq(op, "truncate"))  { emitDivision(sb, args, env, topEnv, rsfx, "truncate",  "cl_truncate", true);  return true; }

            if (symbolEq(op, "fround"))    { emitDivision(sb, args, env, topEnv, rsfx, "fround",    "cl_round",    false); return true; }
            if (symbolEq(op, "ffloor"))    { emitDivision(sb, args, env, topEnv, rsfx, "ffloor",    "Math.floor",  false); return true; }
            if (symbolEq(op, "fceiling"))  { emitDivision(sb, args, env, topEnv, rsfx, "fceiling",  "Math.ceil",   false); return true; }
            if (symbolEq(op, "ftruncate")) { emitDivision(sb, args, env, topEnv, rsfx, "ftruncate", "cl_truncate", false); return true; }

            if (op == intp.sNeq) { if (emitBinOp(sb, "==", args, env, topEnv, rsfx)) return true;
                                   emitFuncallVarargs(sb, "=",  "numbereq", 1, args, env, topEnv, rsfx); return true; }
            if (op == intp.sNe)  { if (emitBinOp(sb, "!=", args, env, topEnv, rsfx)) return true;
                                   emitFuncallVarargs(sb, "/=", "ne",       1, args, env, topEnv, rsfx); return true; }
            if (op == intp.sLt)  { if (emitBinOp(sb, "<", args, env, topEnv, rsfx)) return true;
                                   emitFuncallVarargs(sb, "<",  "lt",       1, args, env, topEnv, rsfx); return true; }
            if (op == intp.sLe)  { if (emitBinOp(sb, "<=", args, env, topEnv, rsfx)) return true;
                                   emitFuncallVarargs(sb, "<=", "le",       1, args, env, topEnv, rsfx); return true; }
            if (op == intp.sGe)  { if (emitBinOp(sb, ">=", args, env, topEnv, rsfx)) return true;
                                   emitFuncallVarargs(sb, ">=", "ge",       1, args, env, topEnv, rsfx); return true; }
            if (op == intp.sGt)  { if (emitBinOp(sb, ">", args, env, topEnv, rsfx)) return true;
                                   emitFuncallVarargs(sb, ">",  "gt",       1, args, env, topEnv, rsfx); return true; }

            if (op == intp.sCar)        { emitFuncall1(sb, "car",    "car",    args, env, topEnv, rsfx); return true; }
            if (op == intp.sCdr)        { emitFuncall1(sb, "cdr",    "cdr",    args, env, topEnv, rsfx); return true; }
            if (op == intp.sCons)       { emitFuncall2(sb, "cons",   "cons",   args, env, topEnv, rsfx); return true; }
            if (symbolEq(op, "rplaca")) { emitFuncall2(sb, "rplaca", "rplaca", args, env, topEnv, rsfx); return true; }
            if (symbolEq(op, "rplacd")) { emitFuncall2(sb, "rplacd", "rplacd", args, env, topEnv, rsfx); return true; }

            if (op == intp.sEq)   { twoArgs("eq", args);  emitEq(sb, car(args), cadr(args), env, topEnv, rsfx); return true; }
            if (op == intp.sNull) { oneArg("null", args); emitEq(sb, car(args), null, env, topEnv, rsfx); return true; }
            if (op == intp.sEql)  { emitFuncall2(sb, "eql", "eql", args, env, topEnv, rsfx); return true; }
            if (op == intp.sInc)  { emitFuncall1(sb, "1+", "inc1", args, env, topEnv, rsfx); return true; }
            if (op == intp.sDec)  { emitFuncall1(sb, "1-", "dec1", args, env, topEnv, rsfx); return true; }

            if (op == intp.sAppend) {
                if (args == null) { // no args
                    sb.append("(Object)null");  return true;
                }
                if (cdr(args) == null) { emitForm(sb, car(args), env, topEnv, rsfx, false); return true; }
                emitFuncallVarargs(sb, "append", "_append", 0, args, env, topEnv, rsfx); return true;
            }

            if (op == intp.sList) {
                if (args == null) { // no args
                    sb.append("(Object)null");  return true;
                }
                if (cdr(args) == null) { // one arg
                    sb.append("cons(");  emitForm(sb, car(args), env, topEnv, rsfx, false);  sb.append(", null)");  return true;
                }
                sb.append("new ListBuilder()");
                for (; args != null; args = (ConsCell)cdr(args)) {
                    sb.append(".append(");
                    emitForm(sb, car(args), env, topEnv, rsfx, false);
                    sb.append(")\n        ");
                }
                sb.append(".first()");
                return true;
            }

            if (op == intp.sListStar) {
                varargs1("list*", args);
                if (cdr(args) == null) { emitForm(sb, car(args), env, topEnv, rsfx, false); return true; }
                if (cddr(args) == null) {
                    sb.append("cons(");
                    emitForm(sb, car(args), env, topEnv, rsfx, false);
                    sb.append(", ");
                    emitForm(sb, cadr(args), env, topEnv, rsfx, false);
                    sb.append(')');
                    return true;
                }
                sb.append("new ListBuilder()");
                for (; cdr(args) != null; args = (ConsCell)cdr(args)) {
                    sb.append(".append(");
                    emitForm(sb, car(args), env, topEnv, rsfx, false);
                    sb.append(")\n        ");
                }
                sb.append(".appendLast("); emitForm(sb, car(args), env, topEnv, rsfx, false); sb.append(").first()");
                return true;
            }

            if (symbolEq(op, "jmethod")) {
                if (emitJmethod(sb, args, null, null, -1, false, null)) return true;
                emitFuncallVarargs(sb, "jmethod", "findMethod", 2, args, env, topEnv, rsfx);
                return true;
            }

            for (String prim: primitives)          if (symbolEq(op, prim))    { emitCallPrimitive(sb, "_" + prim, args, env, topEnv, rsfx, null);  return true; }
            for (String[] prim: aliasedPrimitives) if (symbolEq(op, prim[0])) { emitCallPrimitive(sb, prim[1], args, env, topEnv, rsfx, null);  return true; }

            return false;
        }

        private void opencodeApplyHelper(WrappingWriter sb, String func, Object args, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append(func).append("(toArray(");
            emitForm(sb, args, env, topEnv, rsfx, false);
            sb.append("))");
        }

        /** 2 args: divide 2 numbers and apply {@code javaOp} to the result,
         *  1 arg: apply {@code javaOp} to the number,
         *  in both cases if {@code asLong == true} then the result is converted to {@code long}
         */
        private void emitDivision(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, String murmel, String javaOp, boolean asLong) {
            varargsMinMax(murmel, args, 1, 2);
            if (asLong) sb.append("checkedToLong(");
            sb.append(javaOp).append("(toDouble(");
            if (cdr(args) == null) {
                emitForm(sb, car(args), env, topEnv, rsfx, false);
            }
            else {
                emitForm(sb, car(args), env, topEnv, rsfx, false);
                sb.append(") / toDouble(");
                emitForm(sb, cadr(args), env, topEnv, rsfx, false);
            }
            sb.append("))");
            if (asLong) sb.append(')');
        }

        /** emit "==" operator */
        private void emitEq(WrappingWriter sb, Object lhs, Object rhs, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append("(((Object)");
            emitForm(sb, lhs, env, topEnv, rsfx, false);
            sb.append(" == (Object)");
            if (rhs == null) sb.append("null"); else emitForm(sb, rhs, env, topEnv, rsfx, false);
            sb.append(") ? _t : null)");
        }

        /** emit double operator for zero or more number args */
        private void emitAddDbl(WrappingWriter sb, String op, double start, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append('(');
            if (args == null) sb.append(start);
            else {
                boolean first = true;
                for (Object arg: args) {
                    if (first) first = false;
                    else sb.append(' ').append(op).append(' ');
                    emitFormAsDouble(sb, op, arg, env, topEnv, rsfx);
                }
            }
            sb.append(')');
        }

        /** emit double operator for one or more number args */
        private void emitSubDbl(WrappingWriter sb, String op, double start, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            varargs1(op,  args);
            sb.append('(');
            if (cdr(args) == null) { sb.append(start).append(' ').append(op).append(' '); emitFormAsDouble(sb, op, car(args), env, topEnv, rsfx); }
            else {
                emitFormAsDouble(sb, op, car(args), env, topEnv, rsfx);
                for (Object arg: (ConsCell)cdr(args)) { sb.append(' ').append(op).append(' '); emitFormAsDouble(sb, op, arg, env, topEnv, rsfx); }
            }
            sb.append(')');
        }

        private void emitFuncallVarargs(WrappingWriter sb, String murmel, String func, int minArgs, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            if (minArgs > 0) varargsMin(murmel,  args, minArgs);
            emitCallPrimitive(sb, func, args, env, topEnv, rsfx, null);
        }

        private void emitFuncall1(WrappingWriter sb, String murmel, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            oneArg(murmel, args);
            emitCallPrimitive(sb, func, args, env, topEnv, rsfx, null);
        }

        private void emitFuncall2(WrappingWriter sb, String murmel, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            twoArgs(murmel, args);
            emitCallPrimitive(sb, func, args, env, topEnv, rsfx, null);
        }

        private void emitFuncall2Numbers(WrappingWriter sb, String murmel, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            twoArgs(murmel, args);
            emitCallPrimitive(sb, func, args, env, topEnv, rsfx, "toDouble");
        }

        /** emit a call to the primitive {@code func} without going through the trampoline,
         *  if {@code wrapper} is non-null then it will be applied to each function argument  */
        private void emitCallPrimitive(WrappingWriter sb, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, String wrapper) {
            sb.append(func).append("(");
            if (args != null) {
                if (wrapper != null) sb.append(wrapper).append('(');
                emitForm(sb, car(args), env, topEnv, rsfx, false);
                if (wrapper != null) sb.append(')');
                if (cdr(args) != null) for (Object arg: (ConsCell)cdr(args)) {
                    sb.append(", ");
                    if (wrapper != null) sb.append(wrapper).append('(');
                    emitForm(sb, arg, env, topEnv, rsfx, false);
                    if (wrapper != null) sb.append(')');
                }
            }
            else sb.append("NOARGS");
            sb.append(')');
        }

        /** if args has two arguments then emit a binary operator (double, double) -> boolean */
        private boolean emitBinOp(WrappingWriter sb, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            if (cdr(args) == null || cddr(args) != null) return false;
            sb.append("(");
            emitFormAsDouble(sb, func, car(args), env, topEnv, rsfx);
            sb.append(" ").append(func).append(" ");
            emitFormAsDouble(sb, func, cadr(args), env, topEnv, rsfx);
            sb.append(" ? _t : null)");
            return true;
        }

        /** eval form and change to double */
        private void emitFormAsDouble(WrappingWriter sb, String func, Object form, ConsCell env, ConsCell topEnv, int rsfx) {
            if (form == null || form instanceof Character || form instanceof String) errorNotANumber(func, form);
            if (form instanceof Long) sb.append(form.toString()).append(".0");
            else if (form instanceof Double) sb.append(form.toString());
            else { sb.append("toDouble("); emitForm(sb, form, env, topEnv, rsfx, false); sb.append(')'); }
        }

        /** argCount is number of arguments at compiletime if known or -1 for check at runtime */
        private boolean emitJmethod(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean emitCall, ConsCell ccArguments) {
            varargsMin("jmethod", args, 2);
            final Object strClazz = car(args), strMethod = cadr(args);
            // if class and method are stringliterals then we can do this at compiletime.
            // else jmethod() will check the runtime type at runtime
            if (!stringp(strClazz) || !stringp(strMethod)) return false;

            // check if the class exists in the current (the compiler's) VM. If it can't be loaded then don't opencode,
            // let jmethod handle things at runtime, the class may be available then.
            final Class<?> clazz;
            try {
                clazz = findClass(((String) strClazz).replace('$', '.'));
            }
            catch (ClassNotFoundException e) {
                // todo warn re: performance
                return false;
            }

            // all parameter classes (if any) must be one of the classes that we know how to do Murmel->Java conversion else "return false"
            final ArrayList<Class<?>> paramTypes = new ArrayList<>();
            final ArrayList<String> paramTypeNames = new ArrayList<>();
            if (cddr(args) != null) for (Object arg: (ConsCell)cddr(args)) {
                final String paramType = (String)arg;
                paramTypeNames.add(paramType);

                final Object[] typeDesc = classByName.get(paramType);
                if (typeDesc == null) return false; // todo warn re: performance
                final Class<?> paramClass = (Class<?>) typeDesc[0];
                paramTypes.add(paramClass);
            }

            // at last check if the method/ constructor with the specified parameter types/ classes exists
            final Class<?>[] params = paramTypes.isEmpty() ? null : paramTypes.toArray(new Class[0]);
            final Method m;
            final int startArg;
            final boolean voidMethod;
            try {
                if ("new".equals(strMethod)) { m = null; clazz.getDeclaredConstructor(params);  startArg = 0; voidMethod = false; }
                else                         { m = clazz.getMethod((String)strMethod, params);  startArg = Modifier.isStatic(m.getModifiers()) ? 0 : 1; voidMethod = m.getReturnType() == void.class; }
            }
            catch (Exception e) { throw new LambdaJError(true, "jmethod: exception finding method: %s", e.getMessage()); }

            final int paramCount = paramTypes.size() + startArg;
            if (emitCall) {
                // emit new clazz(args...)/ clazz.method(args...)/ firstarg.method(restargs...)
                final int argCount = length(ccArguments);
                if (argCount != paramCount) errorArgCount((String) strMethod, paramCount, paramCount, argCount, null);

                if ("new".equalsIgnoreCase((String) strMethod)) sb.append("new ").append(strClazz);
                else {
                    if (voidMethod) sb.append("((Supplier<Object>)(() -> { ");
                    if (Modifier.isStatic(m.getModifiers())) sb.append(strClazz).append('.').append(strMethod);
                    else {
                        // instance method, first arg is the object
                        sb.append("((").append(strClazz).append(')');
                        emitForm(sb, car(ccArguments), env, topEnv, rsfx, false);
                        sb.append(").").append(strMethod);
                        ccArguments = listOrMalformed((String)strMethod, cdr(ccArguments));
                    }
                }

                sb.append("(");
                boolean first = true;
                if (ccArguments != null) {
                    int i = startArg;
                    for (Object arg : ccArguments) {
                        if (first) first = false;
                        else sb.append("\n        , ");
                        final String conv = (String) classByName.get(paramTypeNames.get(i-startArg))[1];
                        if (conv == null) emitForm(sb, arg, env, topEnv, rsfx, false);
                        else { sb.append(conv).append('(');  emitForm(sb, arg, env, topEnv, rsfx, false);  sb.append(')'); }
                        i++;
                    }
                }
                sb.append(')');
                if (voidMethod) sb.append("; return null; })).get()");
            } else {
                // emit a lambda that contains an argcount check
                sb.append("((MurmelFunction)(args -> { "); // (MurmelJavaProgram.CompilerPrimitive) works too but is half as fast?!?
                sb.append("argCheck(loc, ").append(paramCount).append(", args.length);  ");
                if (!voidMethod) sb.append("return ");

                if ("new".equalsIgnoreCase((String) strMethod)) sb.append("new ").append(strClazz);
                else if (Modifier.isStatic(m.getModifiers())) sb.append(strClazz).append('.').append(strMethod);
                else sb.append("((").append(strClazz).append(')').append("args[0]").append(").").append(strMethod);

                sb.append("(");
                boolean first = true;
                if (params != null) for (int i = startArg; i < params.length + startArg; i++) {
                    if (first) first = false;
                    else sb.append("\n        , ");
                    final String conv = (String) classByName.get(paramTypeNames.get(i - startArg))[1];
                    if (conv == null) sb.append("args[").append(i).append(']');
                    else sb.append(conv).append("(args[").append(i).append("])");
                }
                sb.append("); ");
                if (voidMethod) sb.append("return null; ");
                sb.append("}))");
            }
            return true;
        }


        /** <p>emit a quoted form.
         * 
         *  <p>Nil, t and atoms that are not symbols are emitted as is.
         *  
         *  <p>For symbols or lists a Java expression is emitted that re-creates the
         *  quoted form at runtime.
         *  
         *  <p>If pool is true then above Java expression is added as an entry to the constant pool
         *  and a reference to the new or already existing identical constant pool entry is emitted. */
        private void emitQuotedForm(WrappingWriter sb, Object form, boolean pool) {
            if (form == null || intp.sNil == form) sb.append("(Object)null");

            else if (symbolp(form)) {
                if (form == intp.sT) sb.append("_t");
                else {
                    final String s = "intern(\"" + escapeString(form.toString()) + "\")";
                    if (pool) emitReference(sb, s);
                    else sb.append(s);
                }
            }
            else if (atom(form))    { emitAtom(sb, form); }

            else if (consp(form)) {
                final StringWriter b = new StringWriter();
                final WrappingWriter qsb = new WrappingWriter(b);
                if (atom(cdr(form))) {
                    // fast path for dotted pairs and 1 element lists
                    qsb.append("cons("); emitQuotedForm(qsb, car(form), false);
                    qsb.append(", ");    emitQuotedForm(qsb, cdr(form), false);
                    qsb.append(")");
                }
                else if (atom(cddr(form))) {
                    // fast path for 2 element lists or dotted 3 element lists
                    qsb.append("cons(");   emitQuotedForm(qsb, car(form),  false);
                    qsb.append(", cons("); emitQuotedForm(qsb, cadr(form), false);
                    qsb.append(", ");      emitQuotedForm(qsb, cddr(form), false);
                    qsb.append("))");
                }
                else {
                    // todo vielleicht statt .append() kette .appendElements() verwenden?
                    qsb.append("new ListBuilder()");
                    for (Object o = form; ; o = cdr(o)) {
                        qsb.append("\n        .append(");
                        emitQuotedForm(qsb, car(o), false);
                        qsb.append(')');
                        if (cdr(o) == null) break;
                        if (!consp(cdr(o))) {
                            qsb.append("\n        .appendLast(");
                            emitQuotedForm(qsb, cdr(o), false);
                            qsb.append(')');
                            break;
                        }
                    }
                    qsb.append("\n        .first()");
                }
                final String init = b.toString();

                if (true) {
                    // deduplicate quoted lists (list constants), modifying list constants will lead to unexpected behaviour
                    if (pool) emitReference(sb, init);
                    else sb.append(init);
                }
                else {
                    // don't deduplicate quoted lists
                    if (pool) {
                        sb.append("q").append(qCounter++);
                        quotedForms.add(init);
                    } else sb.append(init);
                }
            }

            else throw errorInternal("quote: unexpected form", form);
        }

        private int qCounter;
        private final List<String> quotedForms = new ArrayList<>();

        /** emit a reference to an existing identical constant in the constant pool
         *  or add a new one to the pool and emit a reference to that */
        private void emitReference(WrappingWriter sb, String s) {
            final int prev = quotedForms.indexOf(s);
            if (prev == -1) {
                sb.append("q").append(qCounter++);
                quotedForms.add(s);
            }
            else sb.append("q").append(prev);
        }

        private void emitConstantPool(WrappingWriter ret) {
            int ctr = 0;
            for (String quotedForm: quotedForms) {
                ret.append("    private final Object q").append(ctr).append(" = ").append(quotedForm).append(";\n");
                ctr++;
            }
        }



        private static ConsCell cons(Object car, Object cdr) {
            return ConsCell.cons(car, cdr);
        }
    }



    @SuppressWarnings("unused")
    public static class JFRHelper {
        private static final AtomicInteger counter = new AtomicInteger(0);

        private JFRHelper() {}

        @jdk.jfr.Relational
        @Target({ ElementType.FIELD })
        @Retention(RetentionPolicy.RUNTIME)
        @interface ParentId {
        }

        @jdk.jfr.Category({"JMurmel", "User Events"})
        @jdk.jfr.StackTrace(false)
        public abstract static class BaseEvent extends jdk.jfr.Event {

            @jdk.jfr.Description("Parent Event Id")
            @jdk.jfr.Label("Parent") @ParentId final int parent;

            @jdk.jfr.Description("Event Id")
            @jdk.jfr.Label("Id") final int id;

            @jdk.jfr.Description("Event Name")
            @jdk.jfr.Label("Name") String name;

            @jdk.jfr.Description("Event Information")
            @jdk.jfr.Label("Information") String info;

            BaseEvent(BaseEvent parent) {
                id = counter.getAndIncrement();
                if (parent != null) this.parent = parent.id;
                else this.parent = -counter.getAndIncrement();
            }
        }

        @jdk.jfr.Description("Generic Events submitted by User Code")
        @jdk.jfr.Label("Events")
        @jdk.jfr.Name("io.github.jmurmel.MurmelEvent")
        public static class JFREvent extends BaseEvent {

            JFREvent(BaseEvent parent) {
                super(parent);
            }
        }

        @jdk.jfr.Description("Murmel Function Calls")
        @jdk.jfr.Label("Function Calls")
        @jdk.jfr.Name("io.github.jmurmel.MurmelFunctionCall")
        public static class JFRFunctionCall extends BaseEvent {
            Object args;

            @jdk.jfr.Description("Function Call Arguments")
            @jdk.jfr.Label("Arguments") String strArgs;

            @jdk.jfr.Description("Function Call Return Value")
            @jdk.jfr.Label("Return Value") String ret;

            JFRFunctionCall(BaseEvent parent) {
                super(parent);
            }
        }


        public static void event(BaseEvent parent, Object name, Object info) {
            final JFREvent event = new JFREvent(parent);
            if (!event.shouldCommit()) return;

            event.name = String.valueOf(name);
            event.info = String.valueOf(info);
            event.commit();
        }



        public static JFREvent beginEvent(BaseEvent parent, Object name) {
            final JFREvent ret = new JFREvent(parent);
            if (!ret.isEnabled()) return ret;

            ret.name = name.toString();
            ret.begin();
            return ret;
        }

        public static void endEvent(JFREvent event, Object info) {
            event.end();
            if (!event.shouldCommit()) return;

            event.info = info.toString();
            event.commit();
        }



        public static JFRFunctionCall beginFunction(BaseEvent parent, Object name, Object args) {
            final JFRFunctionCall ret = new JFRFunctionCall(parent);
            if (!ret.isEnabled()) return ret;

            ret.name = name.toString();
            ret.args = args;
            ret.strArgs = LambdaJ.printSEx(args, false);
            ret.begin();
            return ret;
        }

        public static Object endFunction(JFRFunctionCall call, Object ret) {
            call.end();
            if (!call.shouldCommit()) return ret;

            final String strRet = LambdaJ.printSEx(ret, false);
            call.info = LambdaJ.printSEx(ConsCell.cons(call.name, call.args), false) + " -> " + strRet;
            call.ret = strRet;
            call.commit();
            return ret;
        }
    }
}


/// ## class JavaCompilerHelper
/// class JavaCompilerHelper - a helper class that wraps the Java system compiler in tools.jar,
/// used by MurmelJavaCompiler to compile the generated Java to an in-memory class and optionally a .jar file.
class JavaCompilerHelper {
    private static final Map<String, String> ENV = Collections.singletonMap("create", "true");
    private final MurmelClassLoader murmelClassLoader;

    JavaCompilerHelper(Path outPath) {
        murmelClassLoader = new MurmelClassLoader(outPath);
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
        mf.getMainAttributes().put(Attributes.Name.CLASS_PATH, new File(LambdaJ.class.getProtectionDomain().getCodeSource().getLocation().getPath()).getName());

        /*
        try (JarOutputStream jar = new JarOutputStream(new FileOutputStream(jarFileName), mf)) {
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

        try (FileSystem zipfs = FileSystems.newFileSystem(uri, ENV)) {
            Files.createDirectory(zipfs.getPath("META-INF/"));

            try (OutputStream out = Files.newOutputStream(zipfs.getPath("META-INF/MANIFEST.MF"))) {
                mf.write(out);
            }
            copyFolder(murmelClassLoader.getOutPath(), zipfs.getPath("/"));
        }
        cleanup();

        return program;
    }

    void cleanup() throws IOException {
        //System.out.println("cleanup " + murmelClassLoader.getOutPath().toString());
        try (Stream<Path> files = Files.walk(murmelClassLoader.getOutPath())) {
            // delete directory including files and sub-folders
            files.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    //.peek(f -> System.out.println("delete " + f.toString()))
                    .forEach(File::deleteOnExit);
        }
    }

    private static void copyFolder(Path src, Path dest) throws IOException {
        try (Stream<Path> stream = Files.walk(src)) {
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
        try (StandardJavaFileManager fm = comp.getStandardFileManager(null, null, null)) {
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
        super(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE);
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

        int index = inputValue.indexOf('\r');
        if (index == -1) return inputValue;

        final int len = inputValue.length();

        final StringBuilder stringBuilder = new StringBuilder(len);

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
/*class AnyToUnixEol implements LambdaJ.ReadSupplier {
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
            prev = '\r';
            if (echo) System.out.print(System.lineSeparator());
            return '\n';
        }
        if (c == '\n' && prev == '\r') {
            prev = '\n';
            return read(echo);
        }
        if (c == '\n') {
            prev = '\n';
            if (echo) System.out.print(System.lineSeparator());
            return '\n';
        }
        prev = c;
        if (echo && c != -1) System.out.print((char)c);
        return c;
    }
}*/

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
            || s.charAt(0) != '\n' && s.charAt(s.length() - 1) != '\n' && s.indexOf('\n') == -1) {
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

    @Override public WrappingWriter append(CharSequence c) {
        try { wrapped.append(c); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
        return this;
    }
    @Override public WrappingWriter append(char c) {
        try { wrapped.write(c); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
        return this;
    }

    public           WrappingWriter append(String s)       { write(s); return this; }
    public           WrappingWriter append(int n)          { write(String.valueOf(n)); return this; }
    public           WrappingWriter append(long l)         { write(String.valueOf(l)); return this; }
    public           WrappingWriter append(double d)       { write(String.valueOf(d)); return this; }
    public           WrappingWriter append(Object o)       { write(String.valueOf(o)); return this; }

    @Override
    public void write(String s) {
        try { wrapped.write(s); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

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



    private static int trX(double fac, double xoff, double x) {
        return (int)((x + xoff) * fac);
    }

    private static int trY(double fac, double yoff, double y) {
        return (int)((y + yoff) * fac);
    }

    private double fact(final int w, final int h) {
        final double xfac = ((double)w-2*padding) / (xmax - xmin);
        final double yfac = ((double)h-2*padding) / (ymax - ymin);

        return Math.min(xfac, yfac);
    }

    private double factBitmap(final int w, final int h) {
        final double xfac = ((double)w-2*padding) / bitmap.getWidth();
        final double yfac = ((double)h-2*padding) / bitmap.getHeight();

        return Math.min(xfac, yfac);
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
