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

    public static final String LANGUAGE_VERSION = "1.3";
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

    /** largest positive long that can be represented as a double w/o any loss */
    public static final long MOST_POSITIVE_FIXNUM = (1L << 53) - 1;

    /** largest negative long that can be represented as a double w/o any loss */
    public static final long MOST_NEGATIVE_FIXNUM = -(1L << 53);

    /** Copied from java.util.ArrayList which says:
     * The maximum size of array to allocate.
     * Some VMs reserve some header words in an array.
     * Attempts to allocate larger arrays may result in
     * OutOfMemoryError: Requested array size exceeds VM limit
     */
    public static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

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

        /** return a string with "line x:y..xx:yy: " if {@code form} is an {@link SExpConsCell} that contains line info */
        String lineInfo() { return ""; }
    }

    /** A murmel symbol name */
    public static final class LambdaJSymbol implements Serializable, Writeable {
        private static final long serialVersionUID = 1L;
        final String name;
        final WellknownSymbol wellknownSymbol;
        Closure macro;

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

        /** if {@code recordPos == true} then it would be desirable to record file/line positions inside the objects */
        default Object readObj(boolean recordPos, Object eof) { return readObj(eof); }
        default void setInput(ReadSupplier input, Path filePath) { throw new UnsupportedOperationException("this ObjectReader does not support changing input"); }
    }

    public interface ObjectWriter {
        void printObj(Object o, boolean printEscape);
        default void printObj(Object o) { printObj(o, true); }
        default void printString(String s) { printObj(s, false); }
        void printEol();
    }

    /** if an atom implements this interface then {@link Writeable#printSEx(LambdaJ.WriteConsumer, boolean)} will be used by the Murmel primitive {@code write} */
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

    /** for nonlocal returns */
    static class ReturnException extends LambdaJError {
        final Object tag, result;
        final Object[] values;

        ReturnException(Object tag, Object result, Object[] values) {
            super("#<returnexception tag=" + tag + ", result=" + result + '>');
            this.tag = tag;
            this.result = result;
            this.values = values;
        }

        ReturnException(Object tag, Object result, ConsCell values) {
            this(tag, result, values == NO_VALUES ? null : listToArray(values));
        }

        ConsCell valuesAsList() {
            if (values == null) return NO_VALUES;
            return arraySlice(values, 0);
        }
    }

    private abstract static class AbstractListBuilder<T extends AbstractListBuilder<T>> {
        private Object first;
        private Object last;

        /** add an item at the end of the list */
        @SuppressWarnings("unchecked")
        public T append(Object elem) {
            final ConsCell newCell = newCell(elem);
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

        abstract ConsCell newCell(Object car);
    }

    /** Builder class for constructing lists, also used by compiled Murmel */
    public static final class ListBuilder extends AbstractListBuilder<ListBuilder> {
        @Override ConsCell newCell(Object car) { return new ListConsCell(car, null); }

        public static ConsCell list(Object... elems) {
            if (elems == null || elems.length == 0) return null;
            if (elems.length == 1) return new ListConsCell(elems[0], null);
            return (ConsCell)new ListBuilder().appendElements(elems).first();
        }
    }

    private final class CountingListBuilder extends AbstractListBuilder<CountingListBuilder> {
        @Override ConsCell newCell(Object car) { return LambdaJ.this.cons(car, null); }
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
        @Override String lineInfo() { return (path == null ? "line " : path.toString() + ':') + startLineNo + ':' + startCharNo + ".." + lineNo + ':' + charNo + ':' + ' '; }
    }

    private static final class Closure implements Serializable, Writeable {
        private static final long serialVersionUID = 1L;
        final Object params;
        final ConsCell body, closure; // todo es sollten nur macros serialisiert werden. beim serialisieren sollte fuer closure!=topEnv ein fehler geworfen werden, beim einlesen sollte closure=topEnv gesetzt werden

        private Closure(Object params, ConsCell body, ConsCell closure)    { this.params = params; this.body = body; this.closure = closure; }

        @Override
        public void printSEx(WriteConsumer out, boolean escapeAtoms) {
            out.print("#<interpreted closure>");
        }
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

        public Object elt(long idx) {
            if (idx < 0) throw new LambdaJError("elt: index must be >= 0");
            if (idx >= length()) throw new LambdaJError(true, "elt: index %d is too large for a list of length %d", idx, length());
            return arry[(int)idx];
        }

        public Object eltset(Object newValue, long idx) {
            if (idx < 0) throw new LambdaJError("eltset: index must be >= 0");
            if (idx >= length()) throw new LambdaJError(true, "eltset: index %d is too large for a list of length %d", idx, length());
            arry[(int)idx] = newValue;
            return newValue;
        }

        private int length() { return arry == null ? 0 : arry.length - offset; }

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
                    else _printSEx(append, arry, obj, escapeAtoms);
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

        boolean[] listToBooleanArray() {
            if (offset >= arry.length) return new boolean[0];
            final int size = arry.length - offset;
            final boolean[] ret = new boolean[size];
            final Long zero = 0L, one = 1L;
            for (int i = 0, j = offset; i < size; i++, j++) {
                final Object o = arry[j];
                if (zero.equals(o)) ret[i] = false;
                else if (one.equals(o)) ret[i] = true;
                else throw new LambdaJError(true, "not a valid value for bitvector: %s", o);
            }
            return ret;
        }
    }



    /// ## Infrastructure
    static final int EOF = -1;
    static final Object[] EMPTY_ARRAY = new Object[0];

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
    
    Path currentSource;

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

        HAVE_VECTOR,         // vector, svref

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

        HAVE_OLDLAMBDA,      // lists whose car is 'lambda' are functions, too

        /** untyped lambda calculus with dynamic environments, S-expressions, that's all */
        HAVE_LAMBDA     { @Override public int bits() { return HAVE_LEXC.bits(); } },
        HAVE_LAMBDAPLUS { @Override public int bits() { return HAVE_LAMBDA.bits() | HAVE_QUOTE.bits() | HAVE_ATOM.bits() | HAVE_EQ.bits(); } },
        HAVE_MIN        { @Override public int bits() { return HAVE_LAMBDAPLUS.bits() | HAVE_CONS.bits() | HAVE_COND.bits(); } },
        HAVE_MINPLUS    { @Override public int bits() { return HAVE_MIN.bits() | HAVE_APPLY.bits() | HAVE_LABELS.bits() | HAVE_NIL.bits() | HAVE_T.bits(); } },
        HAVE_ALL_DYN    { @Override public int bits() { return (HAVE_MINPLUS.bits() | HAVE_XTRA.bits() | HAVE_FFI.bits()
                                                                | HAVE_NUMBERS.bits()| HAVE_DOUBLE.bits() | HAVE_LONG.bits() | HAVE_VECTOR.bits()
                                                                | HAVE_STRING.bits() | HAVE_IO.bits() | HAVE_GUI.bits() | HAVE_UTIL.bits())
                                                               & ~HAVE_LEXC.bits(); } },
        HAVE_ALL_LEXC   { @Override public int bits() { return HAVE_ALL_DYN.bits() | HAVE_LEXC.bits(); } }
        ;

        public int bits() { return 1 << ordinal(); }
    }

    private final int features;

    private boolean haveLabels()    { return (features & Features.HAVE_LABELS.bits())    != 0; }
    private boolean haveNil()       { return (features & Features.HAVE_NIL.bits())       != 0; }
    private boolean haveT()         { return (features & Features.HAVE_T.bits())         != 0; }
    private boolean haveXtra()      { return (features & Features.HAVE_XTRA.bits())      != 0; }
    private boolean haveFFI()       { return (features & Features.HAVE_FFI.bits())       != 0; }
    private boolean haveNumbers()   { return (features & Features.HAVE_NUMBERS.bits())   != 0; }
    private boolean haveString()    { return (features & Features.HAVE_STRING.bits())    != 0; }
    private boolean haveIO()        { return (features & Features.HAVE_IO.bits())        != 0; }
    private boolean haveGui()       { return (features & Features.HAVE_GUI.bits())       != 0; }
    private boolean haveUtil()      { return (features & Features.HAVE_UTIL.bits())      != 0; }
    private boolean haveApply()     { return (features & Features.HAVE_APPLY.bits())     != 0; }
    private boolean haveCons()      { return (features & Features.HAVE_CONS.bits())      != 0; }
    private boolean haveVector()    { return (features & Features.HAVE_VECTOR.bits())    != 0; }
    private boolean haveCond()      { return (features & Features.HAVE_COND.bits())      != 0; }
    private boolean haveAtom()      { return (features & Features.HAVE_ATOM.bits())      != 0; }
    private boolean haveEq()        { return (features & Features.HAVE_EQ.bits())        != 0; }
    private boolean haveQuote()     { return (features & Features.HAVE_QUOTE.bits())     != 0; }
    private boolean haveLexC()      { return (features & Features.HAVE_LEXC.bits())      != 0; }
    private boolean haveOldLambda() { return (features & Features.HAVE_OLDLAMBDA.bits()) != 0; }

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
        if (symtab == null) symtab = new ListSymbolTable();
        this.symtab = symtab;
        if (libDir != null) this.libDir = libDir;
        else this.libDir = murmelDir;
        if (features != Features.HAVE_ALL_LEXC.bits()) speed = 0;

        this.featuresEnvEntry = cons(intern("*features*"), makeFeatureList());

        if (haveT()) symtab.intern(sT);
        if (haveNil()) symtab.intern(sNil);
        symtab.intern(sLambda);

        if (haveQuote())  { internWellknown("quote"); }
        if (haveCond())   { internWellknown("cond"); }
        if (haveLabels()) { internWellknown("labels"); }

        if (haveXtra())   {
            sDynamic = intern("dynamic");

            symtab.intern(sDefine);
            internWellknown("defun");
            internWellknown("defmacro");
            internWellknown("if");
            internWellknown("let");
            internWellknown("let*");
            internWellknown("letrec");

            internWellknown("multiple-value-bind");
            internWellknown("multiple-value-call");

            internWellknown("unwind-protect");
            internWellknown("catch");
            internWellknown("throw");

            internWellknown("setq");

            symtab.intern(sProgn);

            internWellknown("load");
            internWellknown("require");
            internWellknown("provide");

            internWellknown("declaim");
        }
        else sDynamic = null;

        if (haveVector()) {
            sBit = intern("bit");
            sCharacter = intern("character");
        }
        else sBit = sCharacter = null;

        WellknownSymbol.forAllPrimitives(features, w -> internWellknown(w.sym));

        // Lookup only once on first use. The supplier below will do a lookup on first use and then replace itself
        // by another supplier that simply returns the cached value.
        expTrue = () -> { final Object s = makeExpTrue(); expTrue = () -> s; return s; };
    }

    private static final String[] FEATURES = { "murmel", "murmel-" + LANGUAGE_VERSION, "jvm", "ieee-floating-point" };
    private ConsCell makeFeatureList() {
        ConsCell l = null;
        for (String feat: FEATURES) l = new ListConsCell(intern(feat), l);
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

        SExpressionWriter(WriteConsumer out) { this.out = out; }

        @Override public void printObj(Object o, boolean printEscape) { printSEx(out, o, printEscape); }
        @Override public void printEol() { out.print("\n"); }
        @Override public void printString(String s) { out.print(s); }
    }



    /// ## Scanner, symboltable and S-expression reader

    static class ListSymbolTable implements SymbolTable {
        /// A symbol table implemented with a list just because. could easily replaced by a HashMap for better performance.
        ConsCell symbols;

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

    private static final Pattern LONG_PATTERN = Pattern.compile("[-+]?([0-9]|[0-9]+)");
    static boolean isLong(String s) {
        if (s == null || s.isEmpty()) return false;
        return LONG_PATTERN.matcher(s).matches(); // todo handgeschnitzer check statt regex?
    }

    private static final Pattern CL_DECIMAL_LONG_PATTERN = Pattern.compile("[-+]?([0-9]+\\.)");
    static boolean isCLDecimalLong(String s) {
        if (s == null || s.isEmpty()) return false;
        return CL_DECIMAL_LONG_PATTERN.matcher(s).matches(); // todo handgeschnitzer check statt regex?
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
        private Path filePath;
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

            sQuasiquote     = "quasiquote";
            sUnquote        = "unquote";
            sUnquote_splice = "unquote-splice";

            sNot          = intern("not");
            sAnd          = intern("and");
            sOr           = intern("or");

            sQuote          = intern("quote");
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
                final String charOrCharactername = readerMacroToken();
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
                return parseLong(readerMacroToken(), 2);

            case 'o':
            case 'O':
                skipWs();
                return parseLong(readerMacroToken(), 8);

            case 'x':
            case 'X':
                skipWs();
                return parseLong(readerMacroToken(), 16);

            case '(':
                final Object eof = new Object();
                return listToArray(readList(lineNo, charNo, eof));

            case '*':
                final String bv = readerMacroToken();
                boolean[] ret = new boolean[32];
                int i = 0;
                for (char c: bv.toCharArray()) {
                    if (i == ret.length) ret = Arrays.copyOf(ret, ret.length * 2);
                    switch (c) {
                    case '0': break;
                    case '1': ret[i] = true; break;
                    default: throw new LambdaJError(true, "not a valid value for bitvector: %c", c);
                    }
                    i++;
                }
                return Arrays.copyOf(ret, i);

            default:
                look = getchar();
                throw new ParseError("no dispatch function defined for %s", printChar(sub_char));
            }
        }

        private String readerMacroToken() {
            int index = 0;
            while (look != EOF && !isSpace(look) && !isSyntax(look)) {
                if (index < TOKEN_MAX) token[index++] = (char)look;
                look = getchar(false);
            }
            return tokenToString(token, 0, Math.min(index, SYMBOL_MAX));
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
            if (x == null)
                return null;
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

            if (cdr(xCons) == null)
                return qq_expand_list(op);

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
            if (x == null)
                return list(sList, sNil);
            if (atom(x))
                return list(sList, quote(x));

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return list(sList, cadr(xCons));

            if (op == sUnquote_splice)
                return cadr(xCons);

            if (op == sQuasiquote)
                return qq_expand_list(qq_expand(cadr(xCons)));

            if (cdr(xCons) == null)
                return list(sList, qq_expand_list(op));

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
    static void notReserved(final String op, final LambdaJSymbol sym) {
        if (reserved(sym)) errorReserved(op, sym);
    }

    static boolean reserved(LambdaJSymbol sym) {
        return sym == null || sym.wellknownSymbol == WellknownSymbol.sNil || sym.wellknownSymbol == WellknownSymbol.sT || sym.specialForm();
    }


    /// Symboltable
    private final SymbolTable symtab;
    public SymbolTable getSymbolTable() { return symtab; }

    private static final Object UNASSIGNED = "#<value is not assigned>";          // only relevant in letrec
    static final ConsCell NO_VALUES = new ListConsCell("no multiple values", null);
    private static final Object PSEUDO_SYMBOL = "non existant pseudo symbol"; // to avoid matches on pseudo env entries

    /** well known symbols for the reserved symbols t, nil and dynamic, and for some special operators.
     *  Depending on the features given to {@link LambdaJ#LambdaJ} these may be interned into the symbol table. */
    static final LambdaJSymbol sT = new LambdaJSymbol("t", true), sNil = new LambdaJSymbol("nil", true),
                               sLambda = new LambdaJSymbol("lambda", true), sDefine = new LambdaJSymbol("define", true), sProgn = new LambdaJSymbol("progn", true);

    /** some more well known symbols. These symbols are not reserved, the LambdaJSymbol objects could be used to store a macro closure, so the symbols must be instance members of LambdaJ. */
    final LambdaJSymbol sDynamic, sBit, sCharacter;

    enum WellknownSymbolKind { SF, PRIM, OC_PRIM, SYMBOL}
    enum WellknownSymbol {
        none("", null),
        // basic special forms
        sQuote("quote", WellknownSymbolKind.SF), sLambda("lambda", WellknownSymbolKind.SF),

        // additional special forms
        sCond("cond", WellknownSymbolKind.SF), sLabels("labels", WellknownSymbolKind.SF), sIf("if", WellknownSymbolKind.SF),
        sLet("let", WellknownSymbolKind.SF), sLetStar("let*", WellknownSymbolKind.SF), sLetrec("letrec", WellknownSymbolKind.SF),
        sSetQ("setq", WellknownSymbolKind.SF), sProgn("progn", WellknownSymbolKind.SF),
        sDefine("define", WellknownSymbolKind.SF), sDefun("defun", WellknownSymbolKind.SF), sDefmacro("defmacro", WellknownSymbolKind.SF),
        sMultipleValueBind("multiple-value-bind", WellknownSymbolKind.SF), sMultipleValueCall("multiple-value-call", WellknownSymbolKind.SF),
        sUnwindProtect("unwind-protect", WellknownSymbolKind.SF), sCatch("catch", WellknownSymbolKind.SF), sThrow("throw", WellknownSymbolKind.SF),
        sLoad("load", WellknownSymbolKind.SF), sRequire("require", WellknownSymbolKind.SF), sProvide("provide", WellknownSymbolKind.SF),
        sDeclaim("declaim", WellknownSymbolKind.SF),

        // predefined global variables
        sNil("nil", WellknownSymbolKind.SYMBOL), sT("t", WellknownSymbolKind.SYMBOL),

        // logic, predicates
        sEq("eq", Features.HAVE_EQ, 2)                                    { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(car(args) == cadr(args)); } },
        sEql("eql", Features.HAVE_UTIL, 2)                                { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(eql(car(args), cadr(args))); } },

        sConsp("consp", Features.HAVE_UTIL, 1)                            { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(consp(car(args))); } },
        sAtom("atom", Features.HAVE_ATOM, 1)                              { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(atom(car(args))); } },
        sSymbolp("symbolp", Features.HAVE_UTIL, 1)                        { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(symbolp(car(args))); } },
        sNull("null", Features.HAVE_UTIL, 1)                              { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(car(args) == null); } },
        sNumberp("numberp", Features.HAVE_NUMBERS, 1)                     { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(numberp(car(args))); } },
        sFloatp("floatp", Features.HAVE_NUMBERS, 1)                       { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(floatp(car(args))); } },
        sIntegerp("integerp", Features.HAVE_NUMBERS, 1)                   { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(integerp(car(args))); } },
        sCharacterp("characterp", Features.HAVE_STRING, 1)                { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(characterp(car(args))); } },

        sVectorp("vectorp", Features.HAVE_VECTOR, 1)                      { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(vectorp(car(args))); } },
        sSimpleVectorP("simple-vector-p", Features.HAVE_VECTOR, 1)        { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(svectorp(car(args))); } },

        sStringp("stringp", Features.HAVE_STRING, 1)                      { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(stringp(car(args))); } },
        sSimpleStringP("simple-string-p", Features.HAVE_STRING, 1)        { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(sstringp(car(args))); } },

        sBitVectorP("bit-vector-p", Features.HAVE_VECTOR, 1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(bitvectorp(car(args))); } },
        sSimpleBitVectorP("simple-bit-vector-p", Features.HAVE_VECTOR, 1) { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(sbitvectorp(car(args))); } },

        sFunctionp("functionp", Features.HAVE_UTIL, 1)                    { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(intp.functionp(car(args))); } },
        sListp("listp", Features.HAVE_UTIL, 1)                            { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(listp(car(args))); } },
        sAdjArrayp("adjustable-array-p", Features.HAVE_VECTOR, 1)         { Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(adjustableArrayP(car(args))); } },

        // conses and lists
        sCar("car", Features.HAVE_CONS, 1)            { Object   apply(LambdaJ intp, ConsCell args) { return caar(args); } }, 
        sCdr("cdr", Features.HAVE_CONS, 1)            { Object   apply(LambdaJ intp, ConsCell args) { return cdar(args); } }, 
        sCons("cons", Features.HAVE_CONS, 2)          { Object   apply(LambdaJ intp, ConsCell args) { return intp.cons(car(args), cadr(args)); } },
        sRplaca("rplaca", Features.HAVE_XTRA, 2)      { Object   apply(LambdaJ intp, ConsCell args) { return requireCons("rplaca", car(args)).rplaca(cadr(args)); } },
        sRplacd("rplacd", Features.HAVE_XTRA, 2)      { Object   apply(LambdaJ intp, ConsCell args) { return requireCons("rplacd", car(args)).rplacd(cadr(args)); } },

        sList("list", Features.HAVE_UTIL, -1)         { Object   apply(LambdaJ intp, ConsCell args) { return args; } },
        sListStar("list*", Features.HAVE_UTIL, 1, -1) { Object   apply(LambdaJ intp, ConsCell args) { return intp.listStar(args); } },
        sAppend("append", Features.HAVE_UTIL, -1)     { Object   apply(LambdaJ intp, ConsCell args) { return intp.append(args); } },
        sAssq("assq", Features.HAVE_UTIL, 2)          { Object   apply(LambdaJ intp, ConsCell args) { return assq(car(args), cadr(args)); } },
        sAssoc("assoc", Features.HAVE_UTIL, 2)        { Object   apply(LambdaJ intp, ConsCell args) { return assoc(car(args), cadr(args)); } },

        // numbers, characters
        sAdd("+", Features.HAVE_NUMBERS, -1)                 { Object apply(LambdaJ intp, ConsCell args) { return addOp(args, "+", 0.0, (lhs, rhs) -> lhs + rhs); } },
        sMul("*", Features.HAVE_NUMBERS, -1)                 { Object apply(LambdaJ intp, ConsCell args) { return addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs); } },
        sSub("-", Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs); } },
        sDiv("/", Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs); } },

        sNeq("=", Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.compare(args, "=",  (d1, d2) -> d1 == d2); } },
        sNe("/=", Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.compare(args, "/=", (d1, d2) -> d1 != d2); } },
        sLt("<",  Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.compare(args, "<",  (d1, d2) -> d1 <  d2); } },
        sLe("<=", Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.compare(args, "<=", (d1, d2) -> d1 <= d2); } },
        sGe(">=", Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.compare(args, ">=", (d1, d2) -> d1 >= d2); } },
        sGt(">",  Features.HAVE_NUMBERS, 1, -1)              { Object apply(LambdaJ intp, ConsCell args) { return intp.compare(args, ">",  (d1, d2) -> d1 >  d2); } },

        sInc("1+", Features.HAVE_NUMBERS, 1)                 { Object apply(LambdaJ intp, ConsCell args) { return inc(car(args)); } },
        sDec("1-", Features.HAVE_NUMBERS, 1)                 { Object apply(LambdaJ intp, ConsCell args) { return dec(car(args)); } },

        sSignum("signum", Features.HAVE_NUMBERS, 1)          { Object apply(LambdaJ intp, ConsCell args) { return cl_signum(car(args));} },

        sRound("round", Features.HAVE_NUMBERS, 1, 2)         { Object apply(LambdaJ intp, ConsCell args) { return toFixnum(Math.rint  (quot12("round", args))); } },
        sFloor("floor", Features.HAVE_NUMBERS, 1, 2)         { Object apply(LambdaJ intp, ConsCell args) { return toFixnum(Math.floor (quot12("floor", args))); } },
        sCeiling("ceiling", Features.HAVE_NUMBERS, 1, 2)     { Object apply(LambdaJ intp, ConsCell args) { return toFixnum(Math.ceil  (quot12("ceiling", args))); } },
        sTruncate("truncate", Features.HAVE_NUMBERS, 1, 2)   { Object apply(LambdaJ intp, ConsCell args) { return toFixnum(cl_truncate(quot12("truncate", args))); } },

        sFRound("fround", Features.HAVE_NUMBERS, 1, 2)       { Object apply(LambdaJ intp, ConsCell args) { return Math.rint  (quot12("round", args)); } },
        sFFloor("ffloor", Features.HAVE_NUMBERS, 1, 2)       { Object apply(LambdaJ intp, ConsCell args) { return Math.floor (quot12("floor", args)); } },
        sFCeiling("fceiling", Features.HAVE_NUMBERS, 1, 2)   { Object apply(LambdaJ intp, ConsCell args) { return Math.ceil  (quot12("ceiling", args)); } },
        sFTruncate("ftruncate", Features.HAVE_NUMBERS, 1, 2) { Object apply(LambdaJ intp, ConsCell args) { return cl_truncate(quot12("truncate", args)); } },

        sSqrt("sqrt", Features.HAVE_NUMBERS, 1)              { Object apply(LambdaJ intp, ConsCell args) { return Math.sqrt (toDouble("sqrt",  car(args))); } },
        sLog("log", Features.HAVE_NUMBERS, 1)                { Object apply(LambdaJ intp, ConsCell args) { return Math.log  (toDouble("log",   car(args))); } },
        sLog10("log10", Features.HAVE_NUMBERS, 1)            { Object apply(LambdaJ intp, ConsCell args) { return Math.log10(toDouble("log10", car(args))); } },
        sExp("exp", Features.HAVE_NUMBERS, 1)                { Object apply(LambdaJ intp, ConsCell args) { return Math.exp  (toDouble("exp",   car(args))); } },
        sExpt("expt", Features.HAVE_NUMBERS, 2)              { Object apply(LambdaJ intp, ConsCell args) { return Math.pow  (toDouble("expt",  car(args)), toDouble("expt", cadr(args))); } },

        sMod("mod", Features.HAVE_NUMBERS, 2)                { Object apply(LambdaJ intp, ConsCell args) { return cl_mod(toDouble("mod", car(args)), toDouble("mod", cadr(args))); } },
        sRem("rem", Features.HAVE_NUMBERS, 2)                { Object apply(LambdaJ intp, ConsCell args) { return toDouble("rem", car(args)) % toDouble("rem", cadr(args)); } },

        // vectors, sequences
        sMakeArray("make-array", Features.HAVE_VECTOR, 1, 3)           { Object    apply(LambdaJ intp, ConsCell args) { return intp.makeArray(args); } },
        sVectorPE("vector-push-extend", Features.HAVE_VECTOR, 2)       { Object    apply(LambdaJ intp, ConsCell args) { return vectorPushExtend(car(args), cadr(args)); } },
        sVectorCopy("vector-copy", Features.HAVE_VECTOR, 1)            { Object    apply(LambdaJ intp, ConsCell args) { return vectorCopy(car(args)); } },
        sVectorFill("vector-fill", Features.HAVE_VECTOR, 2, 4)         { Object    apply(LambdaJ intp, ConsCell args) { return vectorFill(car(args), cadr(args), caddr(args), cadddr(args)); } },

        sVectorLength("vector-length", Features.HAVE_VECTOR, 1)        { Object    apply(LambdaJ intp, ConsCell args) { return vectorLength(car(args)); } },
        sVectorToList("vector->list", Features.HAVE_VECTOR, 1)         { Object    apply(LambdaJ intp, ConsCell args) { return intp.vectorToList(car(args)); } },

        sSvLength("svlength", Features.HAVE_VECTOR, 1)                 { Object    apply(LambdaJ intp, ConsCell args) { return svlength(car(args)); } },
        sSvRef("svref", Features.HAVE_VECTOR, 2)                       { Object    apply(LambdaJ intp, ConsCell args) { return svref(car(args), toNonnegInt("svref", cadr(args))); } },
        sSvSet("svset", Features.HAVE_VECTOR, 3)                       { Object    apply(LambdaJ intp, ConsCell args) { return svset(car(args), cadr(args), toNonnegInt("svset", caddr(args))); } },
        sSVectorToList("simple-vector->list", Features.HAVE_VECTOR, 1) { Object    apply(LambdaJ intp, ConsCell args) { return intp.simpleVectorToList(car(args)); } },
        sListToSVector("list->simple-vector", Features.HAVE_VECTOR, 1) { Object    apply(LambdaJ intp, ConsCell args) { return listToArray(car(args)); } },
        sVector("vector", Features.HAVE_VECTOR, -1)                    { Object    apply(LambdaJ intp, ConsCell args) { return listToArray(args); } },

        sSLength("slength", Features.HAVE_STRING, 1)                   { Object    apply(LambdaJ intp, ConsCell args) { return slength(car(args)); } },
        sSRef("sref", Features.HAVE_STRING, 2)                         { Object    apply(LambdaJ intp, ConsCell args) { return sref(car(args), toNonnegInt("sref", cadr(args))); } },
        sSSet("sset", Features.HAVE_STRING, 3)                         { Object    apply(LambdaJ intp, ConsCell args) { return sset(requireChar("sset", car(args)), cadr(args), toNonnegInt("sset", caddr(args))); } },
        sSEq("string=", Features.HAVE_STRING, 2)                       { Object    apply(LambdaJ intp, ConsCell args) { return intp.boolResult(Objects.equals(requireStringOrCharOrSymbol("string=", car(args)), requireStringOrCharOrSymbol("string=", cadr(args)))); } },
        sStringToList("string->list", Features.HAVE_STRING, 1)         { Object    apply(LambdaJ intp, ConsCell args) { return intp.stringToList(car(args)); } },
        sListToString("list->string", Features.HAVE_STRING, 1)         { Object    apply(LambdaJ intp, ConsCell args) { return listToString(car(args)); } },

        sCharCode("char-code", Features.HAVE_STRING, 1)                { Object    apply(LambdaJ intp, ConsCell args) { return (long) requireChar("char-code", car(args)); } },
        sCodeChar("code-char", Features.HAVE_STRING, 1)                { Object    apply(LambdaJ intp, ConsCell args) { return (char) toInt("code-char", car(args)); } },

        sBvEq("bv=", Features.HAVE_VECTOR, 2)                          { Object    apply(LambdaJ intp, ConsCell args) { return intp.boolResult(bvEq(car(args), cadr(args))); } },

        sSBvLength("sbvlength", Features.HAVE_VECTOR, 1)               { Object    apply(LambdaJ intp, ConsCell args) { return sbvlength(car(args)); } },
        sSBvRef("sbvref", Features.HAVE_VECTOR, 2)                     { Object    apply(LambdaJ intp, ConsCell args) { return sbvref(car(args), toNonnegInt("sbvref", cadr(args))); } },
        sSBvSet("sbvset", Features.HAVE_VECTOR, 3)                     { Object    apply(LambdaJ intp, ConsCell args) { return sbvset(requireIntegralNumber("sbvset", car(args), 0, 1).longValue(), cadr(args), toNonnegInt("sbvset", caddr(args))); } },
        sSBvEq("sbv=", Features.HAVE_VECTOR, 2)                        { Object    apply(LambdaJ intp, ConsCell args) { return intp.boolResult(sbvEq(car(args), cadr(args))); } },
        sSBvToList("simple-bit-vector->list", Features.HAVE_VECTOR, 1) { Object    apply(LambdaJ intp, ConsCell args) { return intp.simpleBitVectorToList(car(args)); } },
        sListToSBv("list->simple-bit-vector", Features.HAVE_VECTOR, 1) { Object    apply(LambdaJ intp, ConsCell args) { return listToBooleanArray(car(args)); } },

        sSeqRef("seqref", Features.HAVE_VECTOR, 2)                     { Object    apply(LambdaJ intp, ConsCell args) { return seqref(car(args), toNonnegInt("seqref", cadr(args))); } }, // todo nicht auf int begrenzen wg. list
        sSeqSet("seqset", Features.HAVE_VECTOR, 3)                     { Object    apply(LambdaJ intp, ConsCell args) { return seqset(car(args), cadr(args), toNonnegInt("seqset", caddr(args))); } }, // todo nicht auf int begrenzen wg. list

        // I/O
        sRead("read", Features.HAVE_IO, 0, 1)                   { Object apply(LambdaJ intp, ConsCell args) { return intp.read(args); } },
        sWrite("write", Features.HAVE_IO, 1, 2)                 { Object apply(LambdaJ intp, ConsCell args) { return intp.write  (car(args), cdr(args) == null || cadr(args) != null); } },
        sWriteln("writeln", Features.HAVE_IO, 0, 2)             { Object apply(LambdaJ intp, ConsCell args) { return intp.writeln(args,      cdr(args) == null || cadr(args) != null); } },
        sLnwrite("lnwrite", Features.HAVE_IO, 0, 2)             { Object apply(LambdaJ intp, ConsCell args) { return intp.lnwrite(args,      cdr(args) == null || cadr(args) != null); } },
        sFormat("format", Features.HAVE_UTIL, 2, -1)            { Object apply(LambdaJ intp, ConsCell args) { return intp.format(args); } },
        sFormatLocale("format-locale", Features.HAVE_UTIL,3,-1) { Object apply(LambdaJ intp, ConsCell args) { return intp.formatLocale(args); } },

        // misc
        sValues("values", Features.HAVE_XTRA, -1)               { Object apply(LambdaJ intp, ConsCell args) { intp.values = args; return car(args); } },
        sGensym("gensym", Features.HAVE_XTRA, 0)                { Object apply(LambdaJ intp, ConsCell args) { return intp.gensym(); } },
        sTrace("trace", Features.HAVE_XTRA, -1)                 { Object apply(LambdaJ intp, ConsCell args) { return intp.trace(args); } },
        sUntrace("untrace", Features.HAVE_XTRA, -1)             { Object apply(LambdaJ intp, ConsCell args) { return intp.untrace(args); } },
        sMacroexpand1("macroexpand-1", Features.HAVE_XTRA, 1)   { Object apply(LambdaJ intp, ConsCell args) { return intp.macroexpand1(args); } },
        sFatal("fatal", Features.HAVE_UTIL, 1)                  { Object apply(LambdaJ intp, ConsCell args) { throw new RuntimeException(String.valueOf(car(args))); } },

        // time
        sRealtime("get-internal-real-time", Features.HAVE_UTIL, 0)  { Object   apply(LambdaJ intp, ConsCell args) { return getInternalRealTime(); } },
        sRuntime("get-internal-run-time", Features.HAVE_UTIL, 0)    { Object   apply(LambdaJ intp, ConsCell args) { return getInternalRunTime(); } }, // user
        sCputime("get-internal-cpu-time", Features.HAVE_UTIL, 0)    { Object   apply(LambdaJ intp, ConsCell args) { return getInternalCpuTime(); } }, // user + system
        sUniversalTime("get-universal-time", Features.HAVE_UTIL, 0) { Object   apply(LambdaJ intp, ConsCell args) { return getUniversalTime(); } },   // seconds since 1.1.1900
        sSleep("sleep", Features.HAVE_UTIL, 1)                      { Object   apply(LambdaJ intp, ConsCell args) { return sleep(car(args)); } },
        sDecodedTime("get-decoded-time", Features.HAVE_UTIL, 0)     { Object   apply(LambdaJ intp, ConsCell args) { return intp.getDecodedTime(); } },

        // Java FFI
        sJmethod("jmethod", Features.HAVE_FFI, 2, -1)           { Object apply(LambdaJ intp, ConsCell args) { return findMethod(requireString("jmethod", car(args)), requireString("jmethod", cadr(args)), requireList("jmethod", cddr(args))); } },
        sJproxy("jproxy",   Features.HAVE_FFI, 1, -1)           { Object apply(LambdaJ intp, ConsCell args) { return intp.makeProxy(args); } },
        ;

        final WellknownSymbolKind kind;
        final String sym;

        private final int min, max;
        private final Features feature;

        WellknownSymbol(String sym, WellknownSymbolKind kind) {
            assert kind != WellknownSymbolKind.PRIM;
            this.sym = sym; this.kind = kind; min = max = -2;
            feature = null;
        }

        WellknownSymbol(String sym, Features feature, int nArgs) {
            assert nArgs >= -1 && nArgs <= 3;
            this.sym = sym; this.kind = WellknownSymbolKind.PRIM; min = max = nArgs;
            this.feature = feature;
        }

        WellknownSymbol(String sym, Features feature, int minArgs, int maxArgs) {
            assert minArgs >= 0;
            this.sym = sym; this.kind = WellknownSymbolKind.PRIM; min = minArgs; max = maxArgs;
            this.feature = feature;
        }

        Object apply(LambdaJ intp, ConsCell args) { throw errorInternal("apply is not implemented for %s", sym); }
        Object applyPrimitive(LambdaJ intp, ConsCell args) { argCheck(args); return apply(intp, args); }

        void argCheck(ConsCell args) {
            assert kind == WellknownSymbolKind.PRIM;
            final String sym = this.sym;
            if (min == max) {
                switch (min) {
                case -1: return;
                case 0: noArgs(sym, args); return;
                case 1: oneArg(sym, args); return;
                case 2: twoArgs(sym, args); return;
                case 3: threeArgs(sym, args); return;
                }
            }
            if (max == -1) { varargsMin(sym, args, min); return; }
            varargsMinMax(sym, args, min, max);
        }

        /** invoke {@code proc} for all primitives that are avaliable with the given {@code features} */
        static void forAllPrimitives(int features, Consumer<WellknownSymbol> proc) {
            for (WellknownSymbol s: values()) {
                if (s.kind == WellknownSymbolKind.PRIM && (s.feature.bits() & features) != 0) {
                    proc.accept(s);
                }
            }
        }

        static WellknownSymbol of(String name) {
            for (WellknownSymbol s: values()) {
                if (s.sym.equalsIgnoreCase(name)) return s;
            }
            return null;
        }
    }

    /** return true if {@code op} is a symbol and is the given wellknown symbol {@code wellknownOp} */
    static boolean isOperator(Object op, WellknownSymbol wellknownOp) {
        if (op == null) return wellknownOp == WellknownSymbol.sNil;
        if (!symbolp(op)) return false;
        return ((LambdaJSymbol)op).wellknownSymbol == wellknownOp;
    }

    private Supplier<Object> expTrue;

    private Object makeExpTrue() {
        if (haveT()) return sT; // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (haveQuote()) return cons(intern("quote"), cons(sT, null));
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
    private static final ConsCell DYNAMIC_ENV = new ListConsCell(null, null);

    /** Build environment, setup Lisp reader and writer.
     *  Needs to be called once before {@link #eval(Object, ConsCell, int, int, int)} */
    ObjectReader init(ReadSupplier in, WriteConsumer out) {
        final SExpressionReader parser = new SExpressionReader(features, trace, tracer, symtab, featuresEnvEntry, in, null);
        final ObjectWriter outWriter = makeWriter(out);
        return init(parser, outWriter, null);
    }

    private ObjectReader init(ObjectReader inReader, ObjectWriter outWriter, ConsCell customEnv) {
        setReaderPrinter(inReader, outWriter);
        topEnv = customEnv;
        environment();
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
        final boolean doOpencode = speed >= 1;
        Object func = null;
        Object result = null;  /* should be assigned even if followed by a "return" because it will be used in the "finally" clause*/
        Deque<Object> traceStack = null;
        ConsCell restore = null;
        ConsCell localCatchTags = null;
        boolean isTc = false;
        try {
            stack++;

            tailcall:
            while (true) {
                /// eval - lookup symbols in the current environment
                if (symbolp(form)) return result = evalSymbol(form, env);

                /// eval - atoms that are not symbols eval to themselves
                if (atom(form)) return result = form;

                /// eval - form is not an atom - must be a cons (nonempty list) containing either a special form or a function application
                final ConsCell ccForm = (ConsCell)form;

                if (Thread.interrupted()) throw new InterruptedException("got interrupted");

                level++;
                if (traceOn) dbgEvalStart(isTc ? "eval TC" : "eval", form, env, stack, level);

                final Object operator = car(ccForm);      // first element of the of the form should be a symbol or an expression that computes a symbol
                if (operator == null) throw new LambdaJError(true, "function application: not a primitive or lambda: nil");

                final ConsCell ccArguments = (ConsCell)cdr(ccForm);   // list with remaining atoms/ expressions

                final boolean funcall;
                ConsCell ccForms = null;

                ConsCell argList = null;

                final LambdaJSymbol symOperator; // will be the car of the form as a LambdaJSymbol if it is a symbol, null otherwise
                if (symbolp(operator)) switch ((symOperator = (LambdaJSymbol)operator).wellknownSymbol) {
                /// eval - special forms

                /// eval - (quote exp) -> exp
                case sQuote:
                case sDefmacro: {
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

                case sDeclaim: {
                    return result = evalDeclaim(level, ccArguments);
                }


                /// eval - special forms that change the global environment

                /// eval - (define symbol exp) -> symbol with a side of global environment extension
                case sDefine: {
                    final Object symbol = car(ccArguments);
                    final Object value = eval(cadr(ccArguments), env, stack, level, traceLvl);

                    final ConsCell prevEnvEntry = fastassq(symbol, topEnv);
                    if (prevEnvEntry == null) insertFrontTopEnv(symbol, value);
                    else prevEnvEntry.rplacd(value);

                    return result = symbol;
                }

                /// eval - (defun symbol (params...) forms...) -> symbol with a side of global environment extension
                // shortcut for (define symbol (lambda (params...) forms...))
                case sDefun: {
                    final Object symbol = car(ccArguments);
                    final ConsCell selfEnvEntry = cons(symbol, null);
                    final Object closure = makeClosure(cadr(ccArguments), (ConsCell)cddr(ccArguments), cons(selfEnvEntry, env));
                    selfEnvEntry.rplacd(closure);

                    final ConsCell prevEnvEntry = fastassq(symbol, topEnv);
                    if (prevEnvEntry == null) insertFrontTopEnv(symbol, closure);
                    else prevEnvEntry.rplacd(closure);

                    return result = symbol;
                }


                /// eval - special forms that run expressions

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

                /// eval - (catch tagform forms...) -> object
                case sCatch: {
                    final Object tag = eval(car(ccArguments), env, stack, level, traceLvl);
                    localCatchTags = cons(tag, localCatchTags);
                    ccForms = (ConsCell)cdr(ccArguments);
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (throw tagform resultform) -> |
                case sThrow: {
                    final Object throwTag = eval(car(ccArguments), env, stack, level, traceLvl);
                    final Object throwResult = eval(cadr(ccArguments), env, stack, level, traceLvl);
                    // todo checken obs tag gibt, sonst (error 'control-error)
                    throw new ReturnException(throwTag, throwResult, values);
                }

                /// eval - (unwind-protect protected-form cleanup-forms...) -> object
                case sUnwindProtect: {
                    restore = cons(cons(sProgn, cdr(ccArguments)), restore);
                    ccForms = cons(car(ccArguments), null);
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (cond (condform forms...)... ) -> object
                case sCond: {
                    for (Object c: ccArguments) {
                        if (eval(car(c), env, stack, level, traceLvl) != null) {
                            ccForms = (ConsCell) cdr(c);
                            break;
                        }
                    }
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (if condform form optionalform) -> object
                case sIf: {
                    if (eval(car(ccArguments), env, stack, level, traceLvl) != null) {
                        form = cadr(ccArguments);
                    } else {
                        form = caddr(ccArguments);
                    }
                    if (form == null) return result = null;
                    isTc = true; continue tailcall;
                }


                /// eval - (labels ((symbol (params...) forms...)...) forms...) -> object
                case sLabels: {
                    final ConsCell localFunctions = (ConsCell)car(ccArguments);
                    env = localFunctions == null ? env : evalLabels(localFunctions, env);
                    ccForms = (ConsCell)cdr(ccArguments);
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

                /// eval - multiple-value-call
                /// eval - (multiple-value-call function-form values-form*) -> object
                case sMultipleValueCall: {
                    final Object funcOrSymbol = car(ccArguments);
                    func = eval(funcOrSymbol, env, stack, level, traceLvl); // could add the same performance cheat as in function call below
                    ConsCell allArgs = null;
                    final Object valueForms = cdr(ccArguments);
                    if (valueForms != null) for (Object valueForm : listOrMalformed("multiple-value-call", valueForms)) {
                        values = NO_VALUES;
                        final Object prim = eval(valueForm, env, stack, level, traceLvl);
                        final ConsCell newValues = values == NO_VALUES ? cons(prim, null) : values;
                        if (newValues != null) allArgs = listOrMalformed("multiple-value-call", append2(allArgs, newValues));
                    }
                    argList = allArgs;
                    if (doOpencode && funcOrSymbol instanceof LambdaJSymbol && ((LambdaJSymbol)funcOrSymbol).primitive()) {
                        return result = ((LambdaJSymbol)funcOrSymbol).wellknownSymbol.apply(this, argList);
                    }
                    funcall = true;
                    break; // fall through to "actually perform..."
                }

                /// eval - function call
                /// eval - (operatorform argforms...) -> object
                default:
                    // check if we forgot to handle a special form. All special forms should be handled in the cases above.
                    assert !symOperator.specialForm() : ccForm.lineInfo() + "unexpected special form " + symOperator;

                    // check if expandForm() has expanded all macros and make sure that expandForm() is used prior to any eval() call with a form that may contain macro calls
                    // macros can be unexpanded if the macro was defined after the defun
                    if (symOperator.macro != null) throw new LambdaJError(true, "function application: not a primitive or lambda: %s is a macro not a function", symOperator, form);

                    if (doOpencode && symOperator.primitive()) {
                        return result = symOperator.wellknownSymbol.apply(this, evlis(ccArguments, env, stack, level, traceLvl));
                    }
                    else {
                        func = evalSymbol(symOperator, env);
                        argList = evlis(ccArguments, env, stack, level, traceLvl);
                    }

                    funcall = true;
                    // fall through to "actually perform..."
                }
                else {
                    /// eval - apply a function to an argument list
                    /// eval - (apply form argform) -> object
                    if (operator == ocApply) {
                        twoArgs("apply", ccArguments);
                        final Object funcOrSymbol = car(ccArguments);
                        func = symbolp(funcOrSymbol) ? evalSymbol(funcOrSymbol, env) : funcOrSymbol; // could add the same performance cheat as in function call above
                        argList = listOrMalformed("apply", cadr(ccArguments));
                        if (doOpencode && funcOrSymbol instanceof LambdaJSymbol && ((LambdaJSymbol)funcOrSymbol).primitive()) {
                            return result = ((LambdaJSymbol)funcOrSymbol).wellknownSymbol.apply(this, argList);
                        }
                        // fall through to "actually perform..."
                    }

                    /// eval - (eval form) -> object
                    else if (operator == ocEval) {
                        varargs1_2("eval", ccArguments);
                        form = expandForm(car(ccArguments));
                        if (cdr(ccArguments) == null) env = topEnv; // todo unterschied zu kompiliertem eval: topEnv sind ALLE globals, kompiliertes eval sieht nur predefined globals
                        else {
                            final Object additionalEnv = cadr(ccArguments);
                            if (!listp(additionalEnv)) errorMalformed("eval", "'env' to be a list", additionalEnv);
                            env = append2((ConsCell)additionalEnv, topEnv);
                        }
                        isTc = true; continue tailcall;
                    }

                    else {
                        func = eval(operator, env, stack, level, traceLvl);
                        argList = evlis(ccArguments, env, stack, level, traceLvl);
                        // fall through to "actually perform..."
                    }

                    funcall = true;
                }



                if (funcall) {

                    /// eval - actually perform the function call that was set up by "apply" or "multiple-value-call" or "function call" above. "set up" means: func and argList were assigned
                    if (traced != null) traceLvl = traceEnter(func, argList, traceLvl);
                    values = NO_VALUES;
                    if (func instanceof OpenCodedPrimitive) {
                        form = cons(func, argList);
                        if (traced != null) traceStack = push(func, traceStack);
                        func = null;
                        isTc = true; continue tailcall;
                    }

                    else if (func instanceof Primitive) {
                        return result = applyPrimitive((Primitive) func, argList, stack, level);
                    }

                    else if (func instanceof Closure) {
                        final Closure ccFunc = (Closure)func;
                        final ConsCell closure = ccFunc.closure;
                        env = zip(ccFunc.params, argList, closure == DYNAMIC_ENV ? env : closure, true);

                        if (traceFunc)  tracer.println(pfx(stack, level) + " #<lambda " + ccFunc.params + "> " + printSEx(argList));
                        ccForms = ccFunc.body;
                        // fall through to "eval a list of forms"
                    }

                    else if (func instanceof MurmelJavaProgram.CompilerPrimitive) {
                        // compiler runtime func
                        return result = applyCompilerPrimitive((MurmelJavaProgram.CompilerPrimitive) func, argList, stack, level);
                    }

                    else if (func instanceof MurmelFunction) {
                        // compiled function
                        return result = applyCompiledFunction((MurmelFunction) func, argList, stack, level);
                    }

                    /* something like
                         (define l '(lambda () 'hello))
                         (l)
                       would end up here. That was legal in CLtL1 and was made illegal in Common Lisp, and wouldn't work in compiled Murmel,
                       nor would something similar work in Common Lisp (see "Issue FUNCTION-TYPE Writeup" http://www.lispworks.com/documentation/lw71/CLHS/Issues/iss175_w.htm).
                     */
                    else if (haveOldLambda() && consp(func) && car(func) == sLambda) {
                        final Object paramsAndBody = cdr(func);
                        env = zip(car(paramsAndBody), argList, env, true);

                        if (traceFunc)  tracer.println(pfx(stack, level) + " #<list lambda " + paramsAndBody + "> " + printSEx(argList));
                        ccForms = (ConsCell) cdr(paramsAndBody);
                        // fall through to "eval a list of forms"
                    }

                    else {
                        throw new LambdaJError(true, "function application: not a primitive or lambda: %s", printSEx(func));
                    }
                }

                /// eval - eval a list of forms
                if (ccForms != null) {
                    for (; cdr(ccForms) != null; ccForms = listOrMalformed("lambda application", cdr(ccForms))) { // must use listOrMalformed() to avoid CCE on e.g. (apply f '(1 . 2))
                        eval(car(ccForms), env, stack, level, traceLvl);
                    }
                    if (traced != null) traceStack = push(operator, traceStack);
                    form = car(ccForms); func = null; values = NO_VALUES; isTc = true; continue tailcall;
                }

                return result = null; // lambda/ progn/ labels/... w/o body
            }
        }

        catch (ReturnException re) {
            final Object thrownTag = re.tag;
            if (localCatchTags != null) for (ConsCell i = localCatchTags; i != null; i = (ConsCell)cdr(i)) {
                if (car(i) == thrownTag) { values = re.valuesAsList(); return result = re.result; }
            }
            throw re;
        }
        catch (LambdaJError | InterruptedException e) {
            throw new LambdaJError(false, e.getMessage(), form); // todo bei InterruptedException Thread.interrupt()?
        }
        catch (Exception e) {
            //e.printStackTrace();
            throw errorInternal(e, "eval: caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
        }
        finally {
            if (traceOn) dbgEvalDone(isTc ? "eval TC" : "eval", form, env, stack, level);
            if (traced != null && func != null) traceLvl = traceExit(func, result, traceLvl);
            if (traceStack != null) {
                Object s;
                while ((s = traceStack.pollLast()) != null) traceLvl = traceExit(s, result, traceLvl);
            }
            LambdaJError e = null;
            for (ConsCell c = restore; c != null; c = (ConsCell) cdr(c)) {
                final Object o = car(c);
                if (o instanceof RestoreDynamic) ((RestoreDynamic)o).restore();
                else {
                    try { eval(o, env, stack, level, traceLvl); }
                    catch (LambdaJError le) { e = le; }
                }
            }
            if (e != null) {
                if (e instanceof ReturnException) {
                    final ReturnException re = (ReturnException)e;
                    final Object thrownTag = re.tag;
                    if (localCatchTags != null) for (ConsCell i = localCatchTags; i != null; i = (ConsCell)cdr(i)) {
                        if (car(i) == thrownTag) { values = re.valuesAsList(); return re.result; }
                    }
                }
                throw e;
            }
        }
    }

    final Object eval(Object form, ConsCell env) {
        return eval(form, env != null ? append2(env, topEnv) : topEnv, 0, 0, 0);
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
        try {
            if (atom(form)) return form;
            final ConsCell ccForm = ((ConsCell)form).copy();
            final Object op = car(ccForm);
            if (op == null) throw new LambdaJError(true, "function application: not a primitive or lambda: nil");
            final ConsCell ccArgs = cdrShallowCopyList("eval", ccForm);

            if (symbolp(op)) {
                final LambdaJSymbol symOp = (LambdaJSymbol)op;
                if (symOp.specialForm()) switch (symOp.wellknownSymbol) {
                case sQuote:
                    oneArg("quote", ccArgs);
                    return form;

                case sLambda:
                    if (car(ccArgs) == sDynamic) {
                        varargsMin("lambda dynamic", ccArgs, 2);
                        checkLambdaList("lambda dynamic", cadr(ccArgs));
                        expandForms("lambda dynamic", cddrShallowCopyList("lambda dynamic", ccArgs));
                    }
                    else {
                        varargsMin("lambda", ccArgs, 1);
                        checkLambdaList("lambda", car(ccArgs));
                        expandForms("lambda", cdrShallowCopyList("lambda", ccArgs));
                    }
                    return ccForm;

                case sIf:
                    varargsMinMax("if", ccArgs, 2, 3);
                    expandForms("if", ccArgs);
                    return ccForm;

                case sCond:
                    if (ccArgs == null) return null;
                    for (ConsCell i = ccArgs; i != null; i = cdrShallowCopyList("cond", i)) {
                        if (!consp(car(i))) errorMalformed("cond", "a list (condexpr forms...)", car(i));
                        expandForms("cond", carShallowCopyList("cond", i));
                    }
                    return ccForm;

                case sProgn:
                    if (ccArgs == null) return null;
                    if (cdr(ccArgs) == null) return expandForm(car(ccArgs));
                    expandForms("progn", ccArgs);
                    return ccForm;

                case sLabels:
                    varargs1("labels", ccArgs);
                    for (ConsCell i = carShallowCopyList("labels", ccArgs); i != null; i = cdrShallowCopyList("labels", i)) {
                        if (!consp(car(i))) errorMalformed("labels", "a list (symbol (params...) forms...)", i);
                        final ConsCell localFunc = carShallowCopyList("labels", i);
                        varargsMin("labels", localFunc, 2);
                        final LambdaJSymbol funcSymbol = symbolOrMalformed("labels", car(localFunc));
                        if (funcSymbol.macro != null) throw new LambdaJError(true, "local function %s is also a macro which would shadow the local function", funcSymbol, localFunc);
                        checkLambdaList(printSEx(funcSymbol), cadr(localFunc));
                        if (cddr(localFunc) != null) {
                            final ConsCell body = cddrShallowCopyList("labels", localFunc);
                            expandForms("labels", body);
                        }
                    }
                    if (cdr(ccArgs) != null) expandForms("labels", cdrShallowCopyList("labels", ccArgs));
                    return ccForm;

                case sDefine:
                    varargs1_2("define", ccArgs);
                    symbolOrMalformed("define", car(ccArgs));
                    if (cdr(ccArgs) != null) {
                        final ConsCell valueForm = cdrShallowCopyList("define", ccArgs);
                        valueForm.rplaca(expandForm(car(valueForm)));
                    }
                    return ccForm;

                case sDefun:
                    varargsMin("defun", ccArgs, 2);
                    checkLambdaList("defun", cadr(ccArgs));
                    if (cddr(ccArgs) != null) expandForms("defun", cddrShallowCopyList("defun", ccArgs));
                    return ccForm;

                case sDefmacro:
                    varargs1("defmacro", ccArgs);
                    final LambdaJSymbol sym1 = symbolOrMalformed("defmacro", car(ccArgs));
                    if (cdr(ccArgs) == null) sym1.macro = null;
                    else {
                        if (cddr(ccArgs) != null) expandForms("defmacro", cddrShallowCopyList("defmacro", ccArgs));
                        final Object params = cadr(ccArgs);
                        checkLambdaList("defmacro", params);
                        sym1.macro = makeClosure(params, (ConsCell)cddr(ccArgs), topEnv);
                    }
                    return ccForm;

                case sLet:
                case sLetStar:
                case sLetrec:
                    final String sfName = symOp.toString();
                    final boolean letDynamic, namedLet;
                    final Object tag;
                    final ConsCell bindingsAndBody;
                    if (car(ccArgs) != null && symbolp(car(ccArgs))) {
                        tag = car(ccArgs);
                        if (tag == sDynamic) {
                            letDynamic = true;
                            namedLet = false;
                        }
                        else {
                            notReserved(sfName, (LambdaJSymbol)tag);
                            letDynamic = false;
                            namedLet = true;
                        }
                        if (letDynamic && symOp.wellknownSymbol == WellknownSymbol.sLetrec) throw errorMalformed(sfName, "dynamic is not allowed with letrec");
                        bindingsAndBody = cdrShallowCopyList(sfName, ccArgs);
                    }
                    else {
                        letDynamic = false;
                        namedLet = false;
                        tag = null;
                        bindingsAndBody = ccArgs;
                    }
                    if (car(bindingsAndBody) != null) {
                        if (!listp(car(bindingsAndBody))) throw errorMalformed(getOp(sfName, letDynamic, namedLet), "a list of bindings", car(bindingsAndBody));
                        final ConsCell bindings = carShallowCopyList(sfName, bindingsAndBody);

                        // check for duplicate variable names for let and letrec with more than one binding
                        final boolean useLookup = symOp.wellknownSymbol != WellknownSymbol.sLetStar && cdr(bindings) != null;
                        final ArrayList<Object> seen = useLookup ? new ArrayList<>() : null;

                        for (ConsCell i = bindings; i != null; i = cdrShallowCopyList(sfName, i)) {
                            final Object binding = car(i);
                            if (consp(binding)) {
                                if (cddr(binding) != null) throw errorMalformedFmt(getOp(sfName, letDynamic, namedLet), "illegal variable specification %s", printSEx(binding));
                                if (consp(cadr(binding))) {
                                    final ConsCell ccBinding = carShallowCopyList(sfName, i);
                                    final ConsCell valueFormList = cdrShallowCopyList(sfName, ccBinding);
                                    valueFormList.rplaca(expandForm(car(valueFormList)));
                                }
                            }
                            else if (symbolp(binding)) i.rplaca(cons(binding, null)); // change (let (a) ...) -> (let ((a)) ...)
                            else throw errorMalformed(getOp(sfName, letDynamic, namedLet), "bindings to contain lists and/or symbols", binding);
                            final LambdaJSymbol sym = symbolOrMalformed(sfName, caar(i));

                            // don't use notReserved(), this way getOp() only allocates space for string concatenation if needed to actually display an error message
                            if (reserved(sym)) errorReserved(getOp(sfName, letDynamic, namedLet), sym);
                            if (sym == tag) errorMalformedFmt(getOp(sfName, letDynamic, namedLet), "can't use loop symbol %s as a variable", sym);

                            if (seen != null) {
                                if (seen.contains(sym)) throw errorMalformedFmt(getOp(sfName, letDynamic, namedLet), "duplicate symbol %s", sym);
                                seen.add(sym);
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
                    expandForms("multiple-value-bind", cdrShallowCopyList("multiple-value-bind", ccArgs));
                    return ccForm;

                case sCatch:
                    varargs1("catch", ccArgs);
                    if (cdr(ccArgs) == null) return null;
                    expandForms("catch", cdrShallowCopyList("catch", ccArgs));
                    return ccForm;

                case sThrow:
                    twoArgs("throw", ccArgs);
                    expandForms("throw", ccArgs);
                    return ccForm;

                case sUnwindProtect:
                    varargs1("unwind-protect", ccArgs);
                    if (cdr(ccArgs) == null) return expandForm(car(ccArgs));
                    expandForms("unwind-protect", ccArgs);
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
                    return form; // no macroexpansion in declaim, load, require, provide forms

                case sMultipleValueCall:
                    varargs1("multiple-value-call", ccArgs);
                    expandForms("multiple-value-call", ccArgs);
                    return ccForm;

                default:
                    assert false : ccForm.lineInfo() + "special form " + symOp + " is not implemented";
                }

                // not a special form, must be a function or macro application
                if (symOp.macro != null) {
                    final Object expansion = macroexpandImpl(ccForm);
                    assert cadr(values) != null : ccForm.lineInfo() + "macro " + symOp + " was not expanded - secondary value is nil, form was " + form;
                    assert expansion != ccForm : ccForm.lineInfo() + "macro " + symOp + " was not expanded - expansion == ccForm, form was " + form;
                    values = NO_VALUES;
                    return expandForm(expansion);
                }
                if (symOp.primitive())
                    symOp.wellknownSymbol.argCheck(ccArgs);
            }
            expandForms("function application", ccForm);
            return ccForm;
        }
        catch (LambdaJError e) {
            throw new LambdaJError(false, e.getMessage(), form);
        }
    }

    private static ConsCell carShallowCopyList(String sfName, ConsCell cons) {
        if (car(cons) == null) return null;
        final ConsCell carCopy = listOrMalformed(sfName, car(cons)).copy();
        cons.rplaca(carCopy);
        return carCopy;
    }

    private static ConsCell cdrShallowCopyList(String sfName, ConsCell cons) {
        return listOrMalformed(sfName, cons.shallowCopyCdr());
    }

    private static ConsCell cddrShallowCopyList(String sfName, ConsCell cons) {
        return listOrMalformed(sfName, listOrMalformed(sfName, cons.shallowCopyCdr()).shallowCopyCdr());
    }

    /** expand all elements in the list ccForms. The first conscell is modified in place, subsequent conscells are copied.  */
    private void expandForms(String func, ConsCell forms) {
        for (; forms != null; forms = cdrShallowCopyList(func, forms)) {
            forms.rplaca(expandForm(car(forms)));
        }
    }

    static ConsCell listOrMalformed(String op, Object maybeList) {
        if (!listp(maybeList)) errorMalformed(op, "a list", maybeList);
        return (ConsCell)maybeList;
    }

    static LambdaJSymbol symbolOrMalformed(String op, Object maybeSymbol) {
        if (!symbolp(maybeSymbol)) errorMalformed(op, "a symbol", maybeSymbol);
        final LambdaJSymbol symbol = (LambdaJSymbol)maybeSymbol;
        notReserved(op, symbol);
        return symbol;
    }

    private static Object evalSymbol(Object form, ConsCell env) {
        final Object value;
        if (form == null || form == sNil) value = null;
        else if (form == sT) value = sT;
        else {
            final ConsCell envEntry = fastassq(form, env);
            if (envEntry != null) {
                value = cdr(envEntry);
                if (value == UNASSIGNED) throw new LambdaJError(true, "%s: '%s' is bound but has no assigned value", "eval", printSEx(form));
            }
            else throw new LambdaJError(true, "%s: '%s' is not bound", "eval", printSEx(form));
        }
        return value;
    }

    private Object evalSetq(ConsCell pairs, ConsCell env, int stack, int level, int traceLvl) {
        Object res = null;
        while (pairs != null) {
            final LambdaJSymbol symbol = (LambdaJSymbol)car(pairs);
            final ConsCell envEntry = fastassq(symbol, env);

            pairs = (ConsCell) cdr(pairs);
            final Object value = eval(car(pairs), env, stack, level, traceLvl);
            if (envEntry == null) insertFront(env, symbol, value);
            else envEntry.rplacd(value);
            res = value;
            pairs = (ConsCell) cdr(pairs);
        }
        return res;
    }

    private Object evalRequire(ConsCell arguments) {
        varargs1_2("require", arguments);
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
            final Object speedCons = assq(intern("speed"), rest);
            if (speedCons != null) {
                final Object speed = cadr(speedCons);
                if (!numberp(speed)) throw new LambdaJError(true, "declaim: argument to optimize must be a number, found %s", speed);
                this.speed = ((Number)speed).intValue();
            }
        }
        return null;
    }

    private ConsCell evalLabels(ConsCell localFunctions, ConsCell env) {
        final ListConsCell extEnv = acons(PSEUDO_SYMBOL, UNASSIGNED, env);
        for (Object localFunction : localFunctions) {
            final ConsCell currentFunc = (ConsCell)localFunction;
            final LambdaJSymbol currentSymbol = (LambdaJSymbol)car(currentFunc);
            final ConsCell ccParamsAndForms = (ConsCell)cdr(currentFunc);
            insertFront(extEnv, currentSymbol, makeClosure(car(ccParamsAndForms), (ConsCell)cdr(ccParamsAndForms), extEnv));
        }
        return extEnv;
    }

    private ConsCell[] evalLet(Object operator, final ConsCell arguments, ConsCell env, ConsCell restore, int stack, int level, int traceLvl) {
        final boolean letStar  = isOperator(operator, WellknownSymbol.sLetStar);
        final boolean letRec   = isOperator(operator, WellknownSymbol.sLetrec);
        final Object maybeLoopSymbol = car(arguments);
        final boolean letDynamic = maybeLoopSymbol == sDynamic;
        final boolean namedLet = !letDynamic && maybeLoopSymbol != null && symbolp(maybeLoopSymbol); // ohne "maybeLoopSymbol != null" wuerde die leere liste in "(let () 1)" als loop-symbol nil erkannt
        final LambdaJSymbol loopSymbol = namedLet ? (LambdaJSymbol)maybeLoopSymbol : null;

        final ConsCell bindingsAndBodyForms = namedLet || letDynamic ? (ConsCell)cdr(arguments) : arguments;
        final ConsCell ccBindings = (ConsCell)car(bindingsAndBodyForms);

        ConsCell params = null;
        ConsCell extenv = env;
        if (ccBindings != null) {
            final boolean useLookup = letDynamic && cdr(ccBindings) != null;
            final ArrayList<Object> seen = useLookup ? new ArrayList<>() : null;
            ConsCell newValues = null; // used for let dynamic
            ConsCell insertPos = null; // used for named let
            if (letRec) extenv = acons(PSEUDO_SYMBOL, UNASSIGNED, env);
            for (Object binding : ccBindings) {
                final LambdaJSymbol sym = (LambdaJSymbol)car(binding);
                final Object bindingForm = cadr(binding);

                ConsCell newBinding = null;
                if (letDynamic) newBinding = fastassq(sym, topEnv);
                else if (letRec) newBinding = insertFront(extenv, sym, UNASSIGNED);

                final Object val = bindingForm == null ? null : eval(bindingForm, letStar || letRec ? extenv : env, stack, level, traceLvl);
                if (letDynamic && newBinding != null) {
                    final boolean isNewSymbol;
                    if (seen != null) { isNewSymbol = !seen.contains(sym); if (isNewSymbol) seen.add(sym); }
                    else isNewSymbol = true; // ignored/ not needed for "let*", true for a single binding

                    if (isNewSymbol) restore = cons(new RestoreDynamic(newBinding, cdr(newBinding)), restore);
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
            final Object closure = makeClosure(params, bodyForms, extenv);
            ((ConsCell)extenv.car()).rplacd(closure);
        }
        return new ConsCell[] {bodyForms, extenv, restore};
    }

    private static String getOp(Object operator, boolean letDynamic, boolean namedLet) {
        return letDynamic ? operator + " dynamic" : (namedLet ? "named " : "") + operator;
    }

    private static class RestoreDynamic {
        final ConsCell entry;
        final Object oldValue;

        RestoreDynamic(ConsCell entry, Object oldValue) {
            this.entry = entry;
            this.oldValue = oldValue;
        }
        void restore() { entry.rplacd(oldValue); }
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

    Object evalMacro(Object operator, final Closure macroClosure, final ConsCell arguments) {
        if (traceFunc)  tracer.println(pfx(0, 0) + " #<macro " + operator + "> " + printSEx(arguments));

        final ConsCell menv = zip(macroClosure.params, arguments, topEnv, true);
        Object expansion = null;
        if (macroClosure.body != null) for (Object macroform: macroClosure.body) // loop over macro body so that e.g. "(defmacro m (a b) (write 'hallo) `(+ ,a ,b))" will work
            expansion = eval(macroform, menv, 0, 0, 0);
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

    void insertFrontTopEnv(Object symbol, Object value) {
        insertFront(topEnv, symbol, value);
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
                args = null; break;
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
    private Closure makeClosureFromForm(final ConsCell form, ConsCell env) {
        Object paramsAndForms = cdr(form);
        if (car(paramsAndForms) == sDynamic) {
            paramsAndForms = cdr(paramsAndForms);
            env = DYNAMIC_ENV;
        }
        final ConsCell ccParamsAndForms = (ConsCell)paramsAndForms;
        return makeClosure(car(ccParamsAndForms), listOrMalformed("lambda", cdr(ccParamsAndForms)), env);
    }

    /** check that 'a' is a symbol or a proper or dotted list of only symbols (empty list is fine, too).
     *  Also 'a' must not contain reserved symbols or duplicate symbols. */
    private static void checkLambdaList(String func, Object a) {
        if (a == null) return; // empty lambda list is fine
        if (symbolp(a)) { notReserved(func, (LambdaJSymbol)a); return; }
        if (atom(a)) errorMalformed(func, "a symbol or list of symbols", a);

        final HashSet<Object> seen = new HashSet<>();
        final ConsCell start = (ConsCell) a;
        for (;;) {
            if (consp(a) && cdr(a) == start) errorMalformed(func, "circular list of parameters is not allowed");
            final Object param = car(a);
            if (!symbolp(param)) errorMalformed(func, "a symbol or a list of symbols", a);
            notReserved(func, (LambdaJSymbol)param);
            if (!seen.add(param)) errorMalformedFmt(func, "duplicate symbol %s", param);

            a = cdr(a);
            if (a == null) return; // end of a proper list, everything a-ok, move along
            if (atom(a)) {
                if (!symbolp(a)) errorMalformed(func, "a symbol or a list of symbols", a);
                notReserved(func, (LambdaJSymbol)a);
                if (!seen.add(a)) errorMalformedFmt(func, "duplicate symbol %s", a);
                return; // that was the end of a dotted list, everything a-ok, move along
            }
        }
    }

    /** make a lexical closure (if enabled) or lambda */
    private Closure makeClosure(Object params, ConsCell body, ConsCell env) {
        nCells++;
        return new Closure(params, body, haveLexC() ? env : DYNAMIC_ENV);
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        try { return primfn.applyPrimitive(args); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<primitive> throws %s: %s", e.getClass().getSimpleName(), e.getMessage()); }
    }

    /** in case compiled code calls "(eval)" */
    private Object applyCompilerPrimitive(MurmelJavaProgram.CompilerPrimitive primfn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<compiler primitive> " + printSEx(args));
        try { return primfn.applyCompilerPrimitive(listToArray(args)); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<compiler primitive> throws %s: %s", e.getClass().getSimpleName(), e.getMessage()); }
    }

    MurmelJavaProgram compiledProgram = null;
    private Object applyCompiledFunction(MurmelFunction fn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<compiled function> " + printSEx(args));
        try {
            final Object ret = fn.apply(listToArray(args));
            assert compiledProgram != null;
            if (compiledProgram.values != null) {
                values = list(compiledProgram.values);
            }
            else {
                values = NO_VALUES;
            }
            return ret;
        }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(true, "#<compiled function> throws %s: %s", e.getClass().getSimpleName(), e.getMessage()); }
    }


    private Object loadFile(String func, Object argument) {
        final Path prev = currentSource;
        final Path p = findFile(func, argument);
        currentSource = p;
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
        finally {
            currentSource = prev;
        }
    }

    final Path findFile(String func, Object argument) {
        if (!stringp(argument)) errorMalformed(func, "a string argument", printSEx(argument));
        final String filename = (String)argument;
        final Path path;
        if (filename.toLowerCase().endsWith(".lisp")) path = Paths.get(filename);
        else path = Paths.get(filename + ".lisp");
        if (path.isAbsolute()) return path;

        Path current = currentSource;
        if (current == null) current = Paths.get("dummy");
        Path ret = current.resolveSibling(path);
        if (Files.isReadable(ret)) return ret;
        ret = libDir.resolve(path);
        return ret;
    }


    /// ### debug support - trace and untrace
    private Map<Object, LambdaJSymbol> traced;

    final Object trace(ConsCell symbols) {
        if (symbols == null) return traced == null ? null : new ArraySlice(traced.values().toArray(), 0);
        if (traced == null) traced = new HashMap<>();
        for (Object sym: symbols) {
            if (!symbolp(sym)) throw new LambdaJError(true, "trace: can't trace %s: not a symbol", printSEx(sym));
            if (((LambdaJSymbol)sym).specialForm()) {
                throw new LambdaJError(true, "trace: can't trace %s: it is a special form", printSEx(sym));
            }
            final ConsCell envEntry = fastassq(sym, topEnv);
            if (envEntry == null) throw new LambdaJError(true, "trace: can't trace %s: not bound", printSEx(sym));
            traced.put(cdr(envEntry), (LambdaJSymbol) sym);
        }
        return new ArraySlice(traced.values().toArray(), 0);
    }

    final Object untrace(ConsCell symbols) {
        if (symbols == null) { traced = null; return null; }
        ConsCell ret = null;
        if (traced != null) {
            for (Object sym: symbols) {
                if (symbolp(sym)) {
                    final ConsCell envEntry = fastassq(sym, topEnv);
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
            final ConsCell entry = fastassq(op, topEnv);
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
                    tracer.println(pfx + " -> env size:" + listLength(env) + " env:     " + printSEx(env));
                }
            }
        }
    }

    private void dbgEvalDone(String evFunc, Object exp, ConsCell env, int stack, int level) {
        if (trace.ge(TraceLevel.TRC_ENVSTATS)) {
            final int envLen = listLength(env);
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

    private ListConsCell acons(Object key, Object datum, ConsCell alist) { return cons(cons(key, datum), alist); }

    private static Object carCdrError(String func, Object o) { throw new LambdaJError(true, "%s: expected one list or string argument but got %s", func, printSEx(o)); }

    static Object   car(ConsCell c)    { return c == null ? null : c.car(); }
    static Object   car(Object o)      { return o == null ? null
                                                          : o instanceof ListConsCell ? ((ListConsCell)o).car()
                                                          : o instanceof ConsCell ? ((ConsCell)o).car()
                                                          : o instanceof String ? ((String)o).isEmpty() ? null : ((String)o).charAt(0)
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
                                                          : o instanceof String ? ((String)o).length() <= 1 ? null : ((String)o).substring(1)
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



    /* the following predicates more or less implement Murmel's type system */
    static boolean consp(Object o)      { return o instanceof ConsCell; }
    static boolean atom(Object o)       { return !consp(o); }

    static boolean symbolp(Object o)    { return o == null || o instanceof LambdaJSymbol; }

    static boolean numberp(Object o)    { return integerp(o) || floatp(o) || o instanceof Number; }
    static boolean floatp(Object o)     { return o instanceof Double || o instanceof Float || o instanceof BigDecimal; }
    static boolean integerp(Object o)   { return o instanceof Long || o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof BigInteger; }

    static boolean characterp(Object o) { return o instanceof Character; }

    static boolean vectorp(Object o)    { return stringp(o) || bitvectorp(o) || svectorp(o) || o instanceof List; }
    static boolean svectorp(Object o)   { return o != null && o.getClass().isArray() && !bitvectorp(o) && !stringp(o); }
    static boolean stringp(Object o)    { return sstringp(o) || o instanceof CharSequence; }
    static boolean sstringp(Object o)   { return o instanceof String || o instanceof char[]; }
    static boolean bitvectorp(Object o) { return sbitvectorp(o) || o instanceof Bitvector; }
    static boolean sbitvectorp(Object o) { return o instanceof boolean[]; }

    static boolean adjustableArrayP(Object o) {
        if (o instanceof Bitvector || o instanceof StringBuilder || o instanceof StringBuffer || o instanceof List) return true;
        //if (!vectorp(o)) throw errorNotAVector("adjustable-array-p", o);
        return false;
    }

    final boolean functionp(Object o)   { return o instanceof Primitive || o instanceof Closure
                                                 || o instanceof MurmelJavaProgram.CompilerPrimitive || o instanceof MurmelFunction
                                                 || (haveOldLambda() && consp(o) && car(o) == sLambda); }

    static boolean listp(Object o)      { return o == null || consp(o); }


    // these *should* have no usages as these checks would be superfluous.
    // The purpose of these functions is: if such extra checks were made then this would be discovered during testing.
    static boolean consp(ConsCell ignored)  { throw errorInternal("consp(ConsCell c) should NOT be called"); }
    static boolean listp(ConsCell ignored)  { throw errorInternal("listp(ConsCell c) should NOT be called"); }


    static ConsCell arraySlice(Object[] o, int offset) { return o == null || offset >= o.length ? null : new ArraySlice(o, offset); }
    static ConsCell arraySlice(Object... elems) {
        if (elems == null || elems.length == 0) return null;
        return new ArraySlice(elems, 0);
    }

    final Object boolResult(boolean b) { return b ? expTrue.get() : null; }

    private ConsCell list(Object... a) {
        if (a == null || a.length == 0) return null;
        ConsCell ret = null, insertPos = null;
        for (Object o: a) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                final ListConsCell cons = cons(o, null);
                insertPos.rplacd(cons);
                insertPos = cons;
            }
        }
        return ret;
    }

    static int listLength(ConsCell list) {
        if (list == null) return 0;
        int n = 0;
        for (Object ignored: list) n++;
        return n;
    }

    /** return the cons whose car is eq to {@code atom}.
     *  Note: searches using object identity (eq), will work for interned symbols, won't reliably work for e.g. numbers */
    static ConsCell assq(Object atom, Object lst) {
        if (lst == null) return null;
        final ConsCell ccList = requireList("assq", lst);

        for (Object entry: ccList) {
            if (entry != null) {
                if (atom == car(entry)) {
                    return (ConsCell)entry; // cast can't fail if car() succeeded
                }
            }
        }
        return null;
    }

    /** faster assq for internal use for environment lookup. ccList must be a proper list that only contains cons cells. */
    static ConsCell fastassq(Object atom, ConsCell ccList) {
        if (ccList == null) return null;
        //int n = 0;
        for (ConsCell rest = ccList; rest != null; rest = (ConsCell)rest.cdr()) {
            //n++;
            final ConsCell ccEntry = (ConsCell)rest.car();
            if (atom == ccEntry.car()) {
                //if (n >= 20) System.out.printf("assq: %s %d%n", atom, n);
                return ccEntry;
            }
        }
        return null;
    }

    /** Create a new list by copying lhs and appending rhs. Faster (?) 2 argument version of {@link #append(ConsCell)} for internal use. */
    private ConsCell append2(ConsCell lhs, ConsCell rhs) {
        if (lhs == null) return rhs;
        if (rhs == null) return lhs;
        ConsCell ret = null, insertPos = null;
        for (Object o: lhs) {
            if (ret == null) {
                ret = cons(o, null);
                insertPos = ret;
            }
            else {
                insertPos.rplacd(cons(o, null));
                insertPos = (ConsCell) insertPos.cdr();
            }
        }
        // insertPos cannot be null because lhs is either null or a nonempty list which means the loop body runs at least once
        insertPos.rplacd(rhs);
        return ret;
    }


    /// ###  Misc. helpers and printing of S-expressions

    /** convert a (possibly empty aka nil/ null) list to a (possibly empty) Object[] */
    static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return EMPTY_ARRAY;
        if (maybeList instanceof ArraySlice) return ((ArraySlice)maybeList).listToArray();
        if (!consp(maybeList)) throw new LambdaJError(true, "%s: expected argument to be a list but got %s", "listToArray", printSEx(maybeList));
        final List<Object> ret = new ArrayList<>();
        ((ConsCell) maybeList).forEach(ret::add); // todo forEach behandelt dotted und proper lists gleich -> im interpreter gibt (apply < '(1 2 3 4 . 5)) einen fehler, im compiler nicht
        //for (Object rest = maybeList; rest != null; rest = cdr(rest)) ret.add(car(rest));
        return ret.toArray();
    }

    static boolean[] listToBooleanArray(Object maybeList) {
        if (maybeList == null) return new boolean[0];
        if (maybeList instanceof ArraySlice) return ((ArraySlice)maybeList).listToBooleanArray();
        if (!consp(maybeList)) throw new LambdaJError(true, "%s: expected argument to be a list but got %s", "listToArray", printSEx(maybeList));
        boolean[] ret = new boolean[32];
        int i = 0;
        final Long zero = 0L, one = 1L;
        for (Object rest = maybeList; rest != null; rest = cdr(rest)) {
            if (i == ret.length) ret = Arrays.copyOf(ret, ret.length * 2);
            final Object o = car(rest);
            if (zero.equals(o)) ret[i] = false;
            else if (one.equals(o)) ret[i] = true;
            else throw new LambdaJError(true, "not a valid value for bitvector: %s", o);
            i++;
        }
        return Arrays.copyOf(ret, i);
    }

    /** transform {@code obj} into an S-expression, atoms are escaped */
    static String printSEx(Object obj) {
        return printSEx(obj, true);
    }

    static String printSEx(Object obj, boolean printEscape) {
        if (obj == null) return "nil";
        final StringBuilder sb = new StringBuilder();
        _printSEx(sb::append, obj, obj, printEscape);
        return sb.toString();
    }

    static void printSEx(WriteConsumer w, Object obj) {
        _printSEx(w, obj, obj, true);
    }

    static void printSEx(WriteConsumer w, Object obj, boolean printEscape) {
        _printSEx(w, obj, obj, printEscape);
    }

    static void _printSEx(WriteConsumer sb, Object list, Object obj, boolean escapeAtoms) {
        boolean headOfList = true;
        while (true) {
            if (obj == null) { sb.print("nil"); return; }
            else if (obj instanceof ArraySlice) { sb.print(((ArraySlice)obj).printSEx(headOfList, escapeAtoms)); return; }
            else if (consp(obj)) {
                if (headOfList) sb.print("(");
                final Object first = car(obj);
                if (first == list) { sb.print(headOfList ? "#<this cons>" : "#<this list>"); }
                else { _printSEx(sb, first, first, escapeAtoms); }
                final Object rest = cdr(obj);
                if (rest != null) {
                    if (consp(rest)) {
                        sb.print(" ");
                        if (list == rest) { sb.print("#<circular list>)"); return; }
                        else { obj = rest; headOfList = false; } // continue loop
                    }
                    else { sb.print(" . ");  printAtom(sb, rest, escapeAtoms);  sb.print(")");  return; }
                }
                else { sb.print(")"); return; }
            }
            else { printAtom(sb, obj, escapeAtoms); return; }
        }
    }

    private static void printAtom(WriteConsumer sb, Object atom, boolean escapeAtoms) {
        if (atom instanceof Writeable)            { ((Writeable)atom).printSEx(sb, escapeAtoms); }
        else if (escapeAtoms && characterp(atom)) { sb.print(printChar((int)(Character)atom)); }
        else if (vectorp(atom))                   { printVector(sb, atom, escapeAtoms); }
        else                                      { sb.print(atom.toString()); }
    }

    static String printChar(int c) {
        return "#\\"
               + (c < CTRL.length ? CTRL[c]
                                  : c < 127 ? String.valueOf((char)c)
                                            : String.valueOf(c));
    }

    /** prepend " and \ by a \ */
    static String escapeString(CharSequence s) {
        if (s == null) return null;
        if (s.length() == 0) return "";

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

    @SuppressWarnings("rawtypes")
    static void printVector(WriteConsumer sb, Object vector, boolean escapeAtoms) {
        if (vector instanceof boolean[]) {
            sb.print("#*");
            for (boolean b: (boolean[])vector) {
                sb.print(b ? "1" : "0");
            }
            return;
        }
        if (vector instanceof char[]) {
            if (escapeAtoms) sb.print("\"" + escapeString(new String(((char[])vector))) + "\"");
            else             sb.print(new String(((char[])vector)));
            return;
        }
        if (vector instanceof CharSequence) {
            if (escapeAtoms) sb.print("\"" + escapeString(((CharSequence)vector)) + "\"");
            else             sb.print(((CharSequence)vector).toString());
            return;
        }

        sb.print("#(");
        if (vector instanceof Object[]) {
            boolean first = true;
            for (Object o: (Object[])vector) {
                if (first) first = false;
                else sb.print(" ");
                _printSEx(sb, o, o, escapeAtoms);
            }
        }
        else if (vector instanceof List) {
            boolean first = true;
            for (Object o: (List)vector) {
                if (first) first = false;
                else sb.print(" ");
                _printSEx(sb, o, o, escapeAtoms);
            }
        }
        else throw errorNotImplemented("printing vectors of class %s is not implemented", vector.getClass().getSimpleName());
        sb.print(")");
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

    static RuntimeException errorNotABit(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a bit argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorNotAVector(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a vector argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorNotASimpleVector(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a simple vector argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorNotAString(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorNotABitVector(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a bitvector argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorNotASimpleBitVector(String func, Object n) {
        throw new LambdaJError(true, "%s: expected a simple bitvector argument but got %s", func, printSEx(n));
    }

    static RuntimeException errorOverflow(String func, String targetType, Object n) {
        throw new LambdaJError(true, "%s: value cannot be represented as a %s: %s", func, targetType, String.valueOf(n));
    }

    static RuntimeException errorIndexTooLarge(long idx, long actualLength) {
        throw new LambdaJError(true, "index %d is too large for a sequence of length %d", idx, actualLength);
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
    static void noArgs(String func, ConsCell a) {
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

    /** ecactly three arguments */
    static void threeArgs(String func, ConsCell a) {
        if (a == null) errorArgCount(func, 3, 3, 0, null);
        Object _a = cdr(a);
        if (_a == null) errorArgCount(func, 3, 3, 1, a);
        _a = cdr(_a);
        if (_a == null) errorArgCount(func, 3, 3, 2, a);
        _a = cdr(_a);
        if (_a != null) errorArgCount(func, 3, 3, 4, a);
    }

    /** varargs, 0 or 1 arg */
    static void varargs0_1(String func, ConsCell a) {
        if (cdr(a) != null) errorArgCount(func, 0, 1, listLength(a), a);
    }

    /** varargs, 0 to 2 args */
    static void varargs0_2(String func, ConsCell a) {
        if (cddr(a) != null) errorArgCount(func, 0, 2, listLength(a), a);
    }

    /** varargs, at least one arg */
    static void varargs1(String func, ConsCell a) {
        if (a == null) errorVarargsCount(func, 1, 0);
    }

    static void varargs1_2(String func, ConsCell a) {
        if (a == null || cddr(a) != null) errorArgCount(func, 1, 2, listLength(a), a);
    }

    /** varargs, at least {@code min} args */
    static void varargsMin(String func, ConsCell a, int min) {
        final Object x = nthcdr(min-1, a);
        if (x == null) errorVarargsCount(func, min, listLength(a));
    }

    /** varargs, between {@code min} and {@code max} args */
    static void varargsMinMax(String func, ConsCell a, int min, int max) {
        if (min == 0 && a == null) return;
        final Object x = nthcdr(min-1, a);
        final int n = min == 0 ? 0 : min-1;
        if (x == null || nthcdr(max-n, x) != null) errorArgCount(func, min, max, listLength(a), a);
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


    static Number requireNumberOrNull(String func, Object a) {
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
        if (n == null) errorNotAnIntegralNumber(func, null);
        if (n instanceof Long)    { return requireIntegralNumber(func, (Long) n, n, minIncl, maxIncl); }
        if (n instanceof Double)  { return requireIntegralNumber(func, (Double) n, n, minIncl, maxIncl); }
        if (n instanceof Byte)    { return requireIntegralNumber(func, (Byte) n, n, minIncl, maxIncl); }
        if (n instanceof Short)   { return requireIntegralNumber(func, (Short) n, n, minIncl, maxIncl); }
        if (n instanceof Integer) { return requireIntegralNumber(func, (Integer) n, n, minIncl, maxIncl); }
        if (n instanceof Float)   { return requireIntegralNumber(func, (double) (Float) n, n, minIncl, maxIncl); }
        if (n instanceof Number)  { return requireIntegralNumber(func, toDouble(func, n), n, minIncl, maxIncl); }
        throw errorNotAnIntegralNumber(func, n);
    }

    private static Number requireIntegralNumber(String func, double d, Object originalValue, long minIncl, long maxIncl) {
        // see https://stackoverflow.com/questions/9898512/how-to-test-if-a-double-is-an-integer
        if (Math.rint(d) == d && !Double.isInfinite(d) && d >= minIncl && d <= maxIncl) return d;
        throw errorNotAnIntegralNumber(func, originalValue);
    }

    private static Number requireIntegralNumber(String func, long l, Object originalValue, long minIncl, long maxIncl) {
        if (l >= minIncl && l <= maxIncl) return l;
        throw errorNotAnIntegralNumber(func, originalValue);
    }


    /** Return {@code c} as a Character, error if {@code c} is not a Character. */
    static Character requireChar(String func, Object c) {
        if (!(c instanceof Character)) throw new LambdaJError(true, "%s: expected a character argument but got %s", func, printSEx(c));
        return (Character)c;
    }

    private static boolean requireBit(String func, Object value) {
        return requireIntegralNumber(func, value, 0, 1).intValue() != 0;
    }

    static Object[] requireSimpleVector(String func, Object c) {
        if (!svectorp(c)) throw new LambdaJError(true, "%s: expected a simple vector argument but got %s", func, printSEx(c));
        return (Object[])c;
    }

    static boolean[] requireSimpleBitVector(String func, Object c) {
        if (!sbitvectorp(c)) throw new LambdaJError(true, "%s: expected a simple bit vector argument but got %s", func, printSEx(c));
        return (boolean[])c;
    }

    /** return {@code c} as a String, error if {@code c} is not a string, character or symbol */
    static String requireStringOrCharOrSymbol(String func, Object c) {
        if (c == null) return null;
        if (c instanceof Character || c instanceof LambdaJSymbol) return c.toString();
        return requireString(func, c);
    }

    static String requireString(String func, Object c) {
        if (!stringp(c)) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(c));
        if (c instanceof char[]) return String.valueOf((char[])c);
        return c.toString();
    }

    static CharSequence requireCharsequence(String func, Object c) {
        if (c instanceof char[]) return String.valueOf((char[])c);
        if (!(c instanceof CharSequence)) throw new LambdaJError(true, "%s: expected a string argument but got %s", func, printSEx(c));
        return (CharSequence)c;
    }

    /** Return {@code a} cast to a list, error if {@code a} is not a list or is nil. */
    static ConsCell requireCons(String func, Object a) {
        if (!consp(a)) throw new LambdaJError(true, "%s: expected a cons argument but got %s", func, printSEx(a));
        return (ConsCell)a;
    }

    /** Return {@code a} cast to a list, error if {@code a} is not a list or is nil. */
    static ConsCell requireList(String func, Object a) {
        if (a == null) return null;
        if (!consp(a)) throw new LambdaJError(true, "%s: expected a list argument but got %s", func, printSEx(a));
        return (ConsCell)a;
    }



    /// Number type conversions

    /** return the argument w/o decimal places as a long, exception if conversion is not possible */
    static long toFixnum(double d) {
        if (Double.isInfinite(d)) throw new LambdaJError("value is Infinite");
        if (Double.isNaN(d)) throw new LambdaJError("value is NaN");
        if (d < MOST_NEGATIVE_FIXNUM) throw new LambdaJError("underflow");
        if (d > MOST_POSITIVE_FIXNUM) throw new LambdaJError("overflow");
        return (long)d;
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

    /** convert {@code a} to an int, error if {@code a} is not a number. */
    static int toInt(String func, Object a) {
        return requireIntegralNumber(func, a, Integer.MIN_VALUE, Integer.MAX_VALUE).intValue();
    }

    static int toNonnegInt(String func, Object a) {
        return requireIntegralNumber(func, a, 0, Integer.MAX_VALUE).intValue();
    }



    /// Runtime for Lisp programs, i.e. an environment with primitives and predefined global symbols

    static boolean eql(Object o1, Object o2) {
        if (o1 == o2) return true;
        if (characterp(o1) && characterp(o2)) return Objects.equals(o1, o2);
        if (numberp(o1) && numberp(o2)) {
            if (Objects.equals(o1, o2)) return true;

            if (o1 instanceof BigInteger)
                return !(o2 instanceof BigInteger) && integerp(o2) && ((BigInteger)o1).compareTo(new BigInteger(String.valueOf(((Number)o2).longValue()))) == 0;
            if (o2 instanceof BigInteger)
                return integerp(o1) && ((BigInteger)o2).compareTo(new BigInteger(String.valueOf(((Number)o1).longValue()))) == 0;

            /*if (o1 instanceof BigDecimal)
                return !(o2 instanceof BigDecimal) && floatp(o2) && ((BigDecimal)o1).compareTo(new BigDecimal(String.valueOf(((Number)o2).doubleValue()))) == 0;
            if (o2 instanceof BigDecimal)
                return floatp(o1) && ((BigDecimal)o2).compareTo(new BigDecimal(String.valueOf(((Number)o1).doubleValue()))) == 0;*/

            if (integerp(o1) && integerp(o2))
                return ((Number)o1).longValue() == ((Number)o2).longValue();
            /*if (floatp(o1) && floatp(o2))
                return Double.compare(((Number)o1).doubleValue(), ((Number)o2).doubleValue()) == 0;*/
        }
        return false;
    }


    /// conses and lists

    final Object listStar(ConsCell args) {
        if (cdr(args) == null) return car(args);
        if (cddr(args) == null) return cons(car(args), cadr(args));
        final CountingListBuilder b = new CountingListBuilder();
        for (; cdr(args) != null; args = (ConsCell)cdr(args)) {
            b.append(car(args));
        }
        b.appendLast(car(args));
        return b.first();
    }

    /** append args non destructively, all args except the last are shallow copied (list structure is copied, contents is not),
     *  all args except the last must be a list */
    final Object append(ConsCell args) {
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

    /** return the cons whose car is eql to {@code atom}
     * @see #assq
     */
    static ConsCell assoc(Object atom, Object maybeList) {
        if (maybeList == null) return null;
        final ConsCell ccList = requireList("assoc", maybeList);
        for (Object entry: ccList) {
            if (entry != null) { // ignore null items
                if (eql(atom, car(entry))) return (ConsCell)entry;
            }
        }
        return null;
    }


    /// numbers

    interface DoubleBiPred {
        boolean test(double d1, double d2);
    }

    /** compare subsequent pairs of the given list of numbers with the given predicate */
    final Object compare(ConsCell args, String opName, DoubleBiPred pred) {
        Object prev = car(args);
        for (ConsCell rest = (ConsCell)cdr(args); rest != null; rest = (ConsCell)cdr(rest)) {
            final Object next = car(rest);
            if (!pred.test(toDouble(opName, prev), toDouble(opName, next))) return null;
            prev = next;
        }
        return expTrue.get();
    }

    /** operator for zero or more args */
    static double addOp(ConsCell _args, String opName, double startVal, DoubleBinaryOperator op) {
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
    static double subOp(ConsCell _args, String opName, double startVal, DoubleBinaryOperator op) {
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

    static double quot12(String func, ConsCell args) {
        final double lhs = toDouble(func, car(args));
        return cdr(args) == null ? lhs : lhs / toDouble(func, cadr(args));
    }

    static Number cl_signum(Object n) {
        if (n instanceof Double)     { return Math.signum((Double)n); }
        if (n instanceof Long)       { return (long)Long.signum((Long)n); }
        if (n instanceof Byte)       { return (long)Integer.signum((int) (Byte)n); }
        if (n instanceof Short)      { return (long)Integer.signum((int) (Short)n); }
        if (n instanceof Integer)    { return (long)Integer.signum((Integer)n); }
        if (n instanceof BigInteger) { return (long)((BigInteger)n).signum(); }
        if (n instanceof BigDecimal) { return (double)((BigDecimal)n).signum(); }

        return Math.signum(toDouble("signum", n));
    }

    /** produce a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer
     *  of the same sign as the mathematical quotient,
     *  and that has the greatest integral magnitude not greater than that of the mathematical quotient. */
    static double cl_truncate(double d) {
        return d < 0.0 ? Math.ceil(d) : Math.floor(d);
    }

    /** note that the Java modulo operator {@code %} works differently, see also https://en.wikipedia.org/wiki/Modulo_operation */
    static double cl_mod(double x, double y) {
        return x - Math.floor(x / y) * y;
    }

    static Number inc(Object n) {
        if (n instanceof Double) return ((Double)n) + 1;
        if (n instanceof Long) {
            final long l;
            if ((l = (Long) n) == MOST_POSITIVE_FIXNUM) throw new LambdaJError("1+: overflow, integer result does not fit in a fixnum");
            return l + 1;
        }
        if (n instanceof Byte) return ((Byte)n).longValue() + 1;
        if (n instanceof Short) return ((Short)n).longValue() + 1;
        if (n instanceof Integer) return ((Integer)n).longValue() + 1;
        if (n instanceof BigInteger) {
            try {
                final long l;
                if ((l = ((BigInteger)n).longValueExact()) == MOST_POSITIVE_FIXNUM) throw new LambdaJError("1+: overflow, integer result does not fit in a fixnum");
                return l + 1;
            }
            catch (ArithmeticException e) {
                throw new LambdaJError("1+: overflow, BigInteger argument does not fit in a fixnum");
            }
        }
        return toDouble("1+", n) + 1;
    }

    static Number dec(Object n) {
        if (n instanceof Double) return ((Double)n) - 1;
        if (n instanceof Long) {
            final long l;
            if ((l = (Long) n) == MOST_NEGATIVE_FIXNUM) throw new LambdaJError("1-: underflow, integer result does not fit in a fixnum");
            return l - 1;
        }
        if (n instanceof Byte) return ((Byte)n).longValue() - 1;
        if (n instanceof Short) return ((Short)n).longValue() - 1;
        if (n instanceof Integer) return ((Integer)n).longValue() - 1;
        if (n instanceof BigInteger) {
            try {
                final long l;
                if ((l = ((BigInteger)n).longValueExact()) == MOST_NEGATIVE_FIXNUM) throw new LambdaJError("1-: underflow, integer result does not fit in a fixnum");
                return l - 1;
            }
            catch (ArithmeticException e) {
                throw new LambdaJError("1-: underflow, BigInteger argument does not fit in a fixnum");
            }
        }
        return toDouble("1-", n) - 1;
    }


    /// vectors

    static final class Bitvector implements Serializable, Writeable {
        private static final long serialVersionUID = 1L;
        private final BitSet bitSet;
        private int size;

        Bitvector(int capacity, int size) {
            bitSet = new BitSet(capacity);
            this.size = size;
        }
        
        int size() { return size; }
        
        void fill(boolean value) {
            if (value) bitSet.set(0, size);
            else bitSet.clear();
        }

        boolean[] toBooleanArray() {
            final boolean[] ret = new boolean[size];
            if (size == 0) return ret;

            final BitSet bitSet = this.bitSet;
            final int limit = bitSet.length();
            for (int idx = 0; idx < limit; idx++) {
                ret[idx] = bitSet.get(idx);
            }
            return ret;
        }

        long get(int idx) { return bitSet.get(idx) ? 1L : 0L; }
        void set(int idx, boolean val) { bitSet.set(idx, val); }

        void add(boolean value) { if (value) bitSet.set(size); size++; }

        @Override
        public void printSEx(WriteConsumer sb, boolean escapeAtoms) {
            sb.print("#*");
            int idx = 0;
            for (; idx < bitSet.length(); idx++) sb.print(bitSet.get(idx) ? "1" : "0");
            for (; idx < size; idx++) sb.print("0");
        }
    }

    Object makeArray(ConsCell a) {
        final int size = toNonnegInt("make-array", car(a));
        final Object type = cadr(a);
        final Object cap = caddr(a);
        final boolean adjustable = cap != null;
        final int capacity;
        if (adjustable && cap != sT) capacity = requireIntegralNumber("make-array", cap, size, MAX_ARRAY_SIZE).intValue();
        else capacity = size;

        if (cdr(a) == null || type == sT) {
            if (adjustable) { final List<Object> ret = new ArrayList<>(capacity); for (int i = 0; i < size; i++) ret.add(null); return ret; }
            return new Object[size];
        }

        if (type == sBit) {
            if (adjustable) return new Bitvector(capacity, size);
            return new boolean[size];
        }

        if (type == sCharacter) {
            if (adjustable) { final StringBuilder ret = new StringBuilder(capacity); for (int i = 0; i < size; i++) ret.append('\0'); return ret; }
            return new char[size];
        }

        throw new LambdaJError(true, "make-array: unsupported or invalid type specification %s", printSEx(type));
    }

    static long vectorLength(Object maybeVector) {
        if (maybeVector instanceof Object[])     return ((Object[])maybeVector).length;
        if (maybeVector instanceof boolean[])    return ((boolean[])maybeVector).length;
        if (maybeVector instanceof Bitvector)    return ((Bitvector)maybeVector).size();
        if (maybeVector instanceof char[])       return ((char[])maybeVector).length;
        if (maybeVector instanceof CharSequence) return ((CharSequence)maybeVector).length();
        if (maybeVector instanceof List)         return ((List<?>)maybeVector).size();
        throw errorNotAVector("vector-length", maybeVector);
    }

    @SuppressWarnings("unchecked")
    static Object vectorFill(Object vector, Object value, Object _start, Object _end) {
        final int start, end;
        final int length = (int)vectorLength(vector);
        if (_start != null) {
            start = requireIntegralNumber("vector-fill", _start, 0, length).intValue();
            if (_end != null) {
                end = requireIntegralNumber("vector-fill", _end, start+1, length).intValue();
            }
            else end = length;
        }
        else { start = 0; end = length; }

        if (vector instanceof Object[])      { Arrays.fill(((Object[])vector), start, end, value); return vector; }
        if (vector instanceof boolean[])     { Arrays.fill(((boolean[])vector), start, end, requireBit("vector-fill", value)); return vector; }
        if (vector instanceof Bitvector)     { ((Bitvector)vector).fill(requireBit("vector-fill", value)); return vector; }
        if (vector instanceof char[])        { Arrays.fill(((char[])vector), start, end, requireChar("vector-fill", value)); return vector; }
        if (vector instanceof StringBuilder) { final StringBuilder sb = (StringBuilder)vector; final char c = requireChar("vector-fill", value); for (int i = start; i < end; i++) (sb).setCharAt(i, c); return vector; }
        if (vector instanceof StringBuffer)  { final StringBuffer sb = (StringBuffer)vector;   final char c = requireChar("vector-fill", value); for (int i = start; i < end; i++) (sb).setCharAt(i, c); return vector; }
        if (vector instanceof List)          { @SuppressWarnings("rawtypes") final List list = (List)vector; for (int i = start; i < end; i++) list.set(i, value); return vector; }
        throw errorNotAVector("vector-fill", vector);
    }

    static Object vectorCopy(Object vector) {
        final int length = (int)vectorLength(vector);
        if (vector instanceof Object[])     return Arrays.copyOf((Object[])vector, length);
        if (vector instanceof boolean[])    return Arrays.copyOf((boolean[])vector, length);
        if (vector instanceof Bitvector)    return ((Bitvector)vector).toBooleanArray();
        if (vector instanceof char[])       return Arrays.copyOf((char[])vector, length);
        if (vector instanceof CharSequence) return vector.toString().toCharArray();
        if (vector instanceof List<?>)      return ((List<?>)vector).toArray(new Object[0]);
        throw errorNotAVector("vector-copy", vector);
    }

    static long svlength(Object maybeVector) {
        if (maybeVector instanceof Object[]) return ((Object[])maybeVector).length;
        throw errorNotASimpleVector("svlength", maybeVector);
    }

    static Object svref(Object maybeVector, int idx) {
        if (maybeVector instanceof Object[]) return ((Object[])maybeVector)[idx];
        throw errorNotASimpleVector("svref", maybeVector);
    }

    static Object svset(Object newValue, Object maybeVector, int idx) {
        if (maybeVector instanceof Object[]) return ((Object[])maybeVector)[idx] = newValue;
        throw errorNotASimpleVector("svset", maybeVector);
    }

    static long slength(Object maybeVector) {
        if (maybeVector instanceof char[])       return ((char[])maybeVector).length;
        if (maybeVector instanceof CharSequence) return ((CharSequence)maybeVector).length();
        throw errorNotAString("slength", maybeVector);
    }

    static char sref(Object maybeString, int idx) {
        if (maybeString instanceof char[]) return ((char[])maybeString)[idx];
        return requireCharsequence("sref", maybeString).charAt(idx);
    }

    static char sset(char newValue, Object maybeString, int idx) {
        if (maybeString instanceof char[]) return ((char[])maybeString)[idx] = newValue;
        if (maybeString instanceof StringBuilder) { ((StringBuilder)maybeString).setCharAt(idx, newValue); return newValue; }
        if (maybeString instanceof StringBuffer) { ((StringBuffer)maybeString).setCharAt(idx, newValue); return newValue; }
        if (maybeString instanceof String) { throw new LambdaJError(true, "%s: cannot modify readonly string", "sset"); }
        throw new LambdaJError(true, "%s: expected a string argument but got %s", "sset", printSEx(maybeString));
    }

    static boolean bvEq(Object maybeVector1, Object maybeVector2) {
        if (sbitvectorp(maybeVector1) && sbitvectorp(maybeVector2)) return sbvEq(maybeVector1, maybeVector2);
        if (!bitvectorp(maybeVector1)) throw errorNotABitVector("bv=", maybeVector1);
        if (!bitvectorp(maybeVector2)) throw errorNotABitVector("bv=", maybeVector2);
        if (maybeVector1 == maybeVector2) return true;
        if (vectorLength(maybeVector1) != vectorLength(maybeVector2)) return false;
        for (int i = 0; i < vectorLength(maybeVector1); i++) {
            if (seqref(maybeVector1, i) != seqref(maybeVector2, i)) return false;
        }
        return true;
    }

    static long sbvref(Object bv, int idx) {
        if (bv instanceof boolean[]) return ((boolean[])bv)[idx] ? 1L : 0L;
        throw errorNotASimpleBitVector("sbvref", bv);
    }

    static long sbvset(long newValue, Object maybeVector, int idx) {
        if (maybeVector instanceof boolean[]) {
            final boolean b;
            if (newValue == 0) b = false;
            else if (newValue == 1) b = true;
            else throw new LambdaJError(true, "not a valid value for bitvector: %d", newValue);
            ((boolean[])maybeVector)[idx] = b;
            return newValue;
        }
        throw errorNotASimpleBitVector("sbvset", maybeVector);
    }

    static long sbvlength(Object maybeVector) {
        if (maybeVector instanceof boolean[]) return ((boolean[])maybeVector).length;
        throw errorNotASimpleBitVector("sbvlength", maybeVector);
    }

    static boolean sbvEq(Object maybeVector1, Object maybeVector2) {
        if (!(maybeVector1 instanceof boolean[])) throw errorNotASimpleBitVector("sbv=", maybeVector1);
        if (!(maybeVector2 instanceof boolean[])) throw errorNotASimpleBitVector("sbv=", maybeVector2);
        return Arrays.equals((boolean[])maybeVector1, (boolean[])maybeVector2);
    }

    final Object vectorToList(Object maybeVector) {
        if (svectorp(maybeVector)) return simpleVectorToList(maybeVector);
        if (stringp(maybeVector)) return stringToList(maybeVector);
        if (sbitvectorp(maybeVector)) return simpleBitVectorToList(maybeVector);

        if (maybeVector instanceof Bitvector) {
            final Bitvector bv = (Bitvector)maybeVector;
            if (bv.size() == 0) return null;
            final CountingListBuilder ret = new CountingListBuilder();
            for (int i = 0; i < bv.size(); i++) ret.append(bv.get(i));
            return ret.first();
        }

        if (maybeVector instanceof List) {
            @SuppressWarnings("rawtypes") final List l = (List)maybeVector;
            if (l.isEmpty()) return null;
            final CountingListBuilder ret = new CountingListBuilder();
            for (final Object o: l) ret.append(o);
            return ret.first();
        }
        throw errorNotAVector("vector->list", maybeVector);
    }

    final Object simpleVectorToList(Object maybeVector) {
        final Object[] s = requireSimpleVector("simple-vector->list", maybeVector);
        if (s.length == 0) return null;
        final CountingListBuilder ret = new CountingListBuilder();
        final int len = s.length;
        for (int i = 0; i < len; i++) ret.append(s[i]);
        return ret.first();
    }

    final Object simpleBitVectorToList(Object maybeVector) {
        final boolean[] s = requireSimpleBitVector("simple-bit-vector->list", maybeVector);
        final CountingListBuilder ret = new CountingListBuilder();
        final int len = s.length;
        for (int i = 0; i < len; i++) ret.append(s[i] ? 1L : 0L);
        return ret.first();
    }

    static char[] listToString(Object lst) {
        if (lst == null) return new char[0];
        final ConsCell l = requireList("list->string", lst);
        final StringBuilder ret = new StringBuilder();
        for (Object c: l) ret.append(requireChar("list->string", c)); // todo cyclecheck
        return ret.toString().toCharArray();
    }

    final Object stringToList(Object maybeString) {
        final CountingListBuilder ret = new CountingListBuilder();
        if (maybeString instanceof char[]) {
            final char[] carry = (char[])maybeString;
            final int len = carry.length;
            for (int i = 0; i < len; i++) ret.append(carry[i]);
            return ret.first();
        }
        final CharSequence s = requireCharsequence("string->list", maybeString);
        final int len = s.length();
        for (int i = 0; i < len; i++) ret.append(s.charAt(i));
        return ret.first();
    }

    @SuppressWarnings("unchecked")
    static long vectorPushExtend(Object newValue, Object maybeVector) {
        if (!adjustableArrayP(maybeVector)) throw new LambdaJError(true, "vector-push-extend: not an adjustable vector: %s", maybeVector);
        if (maybeVector instanceof StringBuilder) { final StringBuilder sb = (StringBuilder)maybeVector; sb.append(requireChar("vector-push-extend", newValue)); return sb.length() - 1; }
        if (maybeVector instanceof StringBuffer) { final StringBuffer sb = (StringBuffer)maybeVector; sb.append(requireChar("vector-push-extend", newValue)); return sb.length() - 1; }
        if (maybeVector instanceof Bitvector) { final Bitvector bv = (Bitvector)maybeVector; bv.add(requireBit("vector-push-extend", newValue)); return bv.size() - 1; }
        if (maybeVector instanceof List) { @SuppressWarnings("rawtypes") final List l = (List)maybeVector; l.add(newValue); return l.size() - 1; }
        throw errorInternal("vector-push-extend: unknown object type %s", maybeVector);
    }


    /// sequences

    static Object seqref(Object maybeSeq, long idx) {
        if (idx < 0) throw new LambdaJError("seqref: index must be >= 0");
        if (maybeSeq == null) errorIndexTooLarge(idx, 0);
        if (maybeSeq instanceof ArraySlice) return ((ArraySlice)maybeSeq).elt(idx);
        if (maybeSeq instanceof ConsCell) {
            long _idx = 0;
            for (Object o: (ConsCell)maybeSeq) {
                if (_idx == idx) return o;
                _idx++;
            }
            throw errorIndexTooLarge(idx, _idx);
        }
        if (maybeSeq instanceof Object[])     { final Object[]  arry = (Object[])maybeSeq;  if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx]; }
        if (maybeSeq instanceof char[])       { final char[]    arry = (char[])maybeSeq;    if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx]; }
        if (maybeSeq instanceof boolean[])    { final boolean[] arry = (boolean[])maybeSeq; if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx] ? 1L : 0L; }
        if (maybeSeq instanceof Bitvector)    { final Bitvector bv = (Bitvector)maybeSeq;   if (idx >= bv.size()) errorIndexTooLarge(idx, bv.size()); return bv.get((int)idx); }
        if (maybeSeq instanceof List)         { @SuppressWarnings("rawtypes") final List list = (List)maybeSeq; if (idx >= list.size()) errorIndexTooLarge(idx, list.size()); return list.get((int)idx); }
        if (maybeSeq instanceof CharSequence) { final CharSequence cs = (CharSequence)maybeSeq; if (idx >= cs.length()) errorIndexTooLarge(idx, cs.length()); return cs.charAt((int)idx); }
        throw errorInternal("seqref: unknown object type %s or not implemented", maybeSeq);
    }

    @SuppressWarnings("unchecked")
    static Object seqset(Object newValue, Object maybeSeq, long idx) {
        if (idx < 0) throw new LambdaJError("seqref: index must be >= 0");
        if (maybeSeq == null) errorIndexTooLarge(idx, 0);
        if (maybeSeq instanceof ArraySlice) return ((ArraySlice)maybeSeq).eltset(newValue, idx);
        if (maybeSeq instanceof ConsCell) {
            long _idx = 0;
            ConsCell lst = (ConsCell)maybeSeq;
            Object cdr = maybeSeq;
            for (; consp(cdr); cdr = cdr((ConsCell)cdr)) {
                lst = (ConsCell)cdr;
                if (_idx == idx) { lst.rplaca(newValue); return newValue; }
                _idx++;
            }
            if (_idx == idx && cdr != null) { lst.rplacd(newValue); return newValue; }
            throw errorIndexTooLarge(idx, cdr == null ? _idx : _idx + 1);
        }
        if (maybeSeq instanceof Object[])     { final Object[]  arry = (Object[])maybeSeq;  if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx] = newValue; }
        if (maybeSeq instanceof char[])       { final char[]    arry = (char[])maybeSeq;    if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx] = requireChar("seqset", newValue); }
        if (maybeSeq instanceof boolean[])    { final boolean[] arry = (boolean[])maybeSeq; if (idx >= arry.length) errorIndexTooLarge(idx, arry.length);
                                                final int newBit = requireIntegralNumber("seqset", newValue, 0, 1).intValue();
                                                if (newBit == 0) { arry[(int)idx] = false; return 0L; }
                                                if (newBit == 1) { arry[(int)idx] = true;  return 1L; }
                                                throw errorNotABit("seqset", newValue); }
        if (maybeSeq instanceof Bitvector)    { final Bitvector bv = (Bitvector)maybeSeq; if (idx >= bv.size()) errorIndexTooLarge(idx, bv.size()); bv.set((int)idx, requireBit("seqset", newValue));
                                                return newValue; }
        if (maybeSeq instanceof StringBuilder) { final StringBuilder sb = (StringBuilder)maybeSeq; if (idx >= sb.length()) errorIndexTooLarge(idx, sb.length());
                                                 final Character c = requireChar("seqset", newValue); sb.setCharAt((int)idx, c);
                                                 return newValue; }
        if (maybeSeq instanceof StringBuffer) { final StringBuffer sb = (StringBuffer)maybeSeq; if (idx >= sb.length()) errorIndexTooLarge(idx, sb.length());
                                                final Character c = requireChar("seqset", newValue); sb.setCharAt((int)idx, c);
                                                return newValue; }
        if (maybeSeq instanceof List)         { @SuppressWarnings("rawtypes") final List list = (List)maybeSeq; if (idx >= list.size()) errorIndexTooLarge(idx, list.size()); list.set((int)idx, newValue);
                                                return newValue; }
        throw errorInternal("seqset: unknown object type %s or not implemented", maybeSeq);
    }


    /// I/O

    final Object read(ConsCell a) {
        if (lispReader == null) throw new LambdaJError(true, "%s: lispStdin is nil", "read");
        if (a == null) {
            final Object eof = new Object();
            final Object ret = lispReader.readObj(eof);
            if (ret == eof) throw new LambdaJError("read: EOF");
            return ret;
        }
        else {
            return lispReader.readObj(car(a));
        }
    }

    final Object write(final Object arg, boolean printEscape) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "write");
        lispPrinter.printObj(arg, printEscape);
        return arg;
    }

    final Object writeln(final ConsCell arg, boolean printEscape) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "writeln");
        if (arg != null) {
            lispPrinter.printObj(car(arg), printEscape);
        }
        lispPrinter.printEol();
        return car(arg);
    }

    final Object lnwrite(final ConsCell arg, boolean printEscape) {
        if (lispPrinter == null) throw new LambdaJError(true, "%s: lispStdout is nil", "lnwrite");
        lispPrinter.printEol();
        if (arg == null) return null;
        final Object o;
        lispPrinter.printObj(o = car(arg), printEscape);
        lispPrinter.printString(" ");
        return o;
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


    /// misc

    static long getInternalRealTime() {
        return System.nanoTime();
    }

    static long getInternalRunTime() {
        return getThreadBean("get-internal-run-time").getCurrentThreadUserTime();
    }

    static long getInternalCpuTime() {
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

    static Object sleep(Object seconds) {
        
        try {
            final long millis = (long)(toDouble("sleep", seconds) * 1e3D);
            Thread.sleep(millis);
            return null;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new LambdaJError("sleep: got interrupted");
        }
    }

    static long getUniversalTime() {
        final ZoneId utc = ZoneId.of("UTC");
        final ZonedDateTime ld1900 = ZonedDateTime.of(1900, 1, 1, 0, 0, 0, 0, utc);
        return ld1900.until(ZonedDateTime.now(utc), ChronoUnit.SECONDS);
    }

    final ConsCell getDecodedTime() {
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
        if (!consp(maybeMacroCall)) {
            values = cons(maybeMacroCall, cons(null, null));
            return maybeMacroCall;
        }
        return macroexpandImpl((ConsCell) maybeMacroCall);
    }

    final Object macroexpandImpl(ConsCell form) {
        final Object maybeSymbol = car(form);
        if (maybeSymbol == null || !symbolp(maybeSymbol)) {
            values = cons(form, cons(null, null));
            return form;
        }
        final LambdaJSymbol macroSymbol = (LambdaJSymbol)maybeSymbol;
        final Closure macroClosure = macroSymbol.macro;
        if (macroClosure == null) {
            values = cons(form, cons(null, null));
            return form;
        }
        final ConsCell arguments = (ConsCell) cdr(form);
        final Object expansion = evalMacro(macroSymbol, macroClosure, arguments);
        values = cons(expansion, cons(sT, null));
        return expansion;
    }

    private int gensymCounter;
    final Object gensym() {
        return new LambdaJSymbol("#:g" + ++gensymCounter);
    }



    /// Murmel runtime support for Java FFI - Murmel calls Java
    private static class JavaConstructor implements Primitive, MurmelJavaProgram.CompilerPrimitive {
        private final Constructor<?> constructor;
        private final UnaryOperator<Object>[] argConv;

        @SuppressWarnings("unchecked")
        private JavaConstructor(Constructor<?> constructor, Iterable<?> paramClassNames) {
            this.constructor = constructor;
            this.argConv = makeArgConv(paramClassNames, constructor.getParameterCount(), 0);
        }

        @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
        @Override public String toString() { return "#<Java constructor: " + constructor.getName() + '>'; }

        @Override public Object applyPrimitive(ConsCell x) { return applyCompilerPrimitive(listToArray(x)); }

        @Override
        public Object applyCompilerPrimitive(Object... args) {
            final String name = "new " + constructor.getDeclaringClass().getName();
            javaCallArgCheck(name, constructor, argConv, args);
            try { return constructor.newInstance(args); }
            catch (InvocationTargetException ite) { throw new LambdaJError(true, "%s: %s", name, ite.getTargetException().toString()); }
            catch (Exception e)                   { throw new LambdaJError(true, "%s: %s", name, e.toString()); }
        }
    }

    static UnaryOperator<Object>[] makeArgConv(Iterable<?> paramClassNames, int paramCount, int skipThis) {
        final UnaryOperator<Object>[] argConv = new UnaryOperator[paramCount + skipThis];
        int i = 0;
        if (paramClassNames != null) for (Object paramClassName: paramClassNames) {
            final String strParamClassName = (String)paramClassName;
            final Object[] entry = classByName.get(strParamClassName);
            if (entry != null) argConv[i+skipThis] = (UnaryOperator<Object>) entry[2];
            i++;
        }
        return argConv;
    }

    private static class JavaMethod implements Primitive, MurmelJavaProgram.CompilerPrimitive {
        @FunctionalInterface private interface Invoker { Object invoke(Object... args) throws Throwable; }

        private final Method method;
        private final Invoker invoke;
        private final UnaryOperator<Object>[] argConv;

        @SuppressWarnings("unchecked")
        private JavaMethod(Method method, Iterable<?> paramClassNames) {
            this.method = method;
            int paramCount = method.getParameterCount();
            final boolean isStatic = Modifier.isStatic(method.getModifiers());
            if (!isStatic) paramCount++; // this + parameters

            this.argConv = makeArgConv(paramClassNames, method.getParameterCount(), isStatic ? 0 : 1);

            try {
                final MethodHandle mh = MethodHandles.publicLookup().unreflect(method);
                if (method.isVarArgs()) invoke = mh::invokeWithArguments;
                else switch (paramCount) {
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
            javaCallArgCheck(method.getName(), method, argConv, args);

            if (!Modifier.isStatic(method.getModifiers()) && !method.getDeclaringClass().isInstance(args[0]))
                throw new LambdaJError(true, "jmethod: %s is not an instance of class %s", args[0], method.getDeclaringClass().getName());

            try { return invoke.invoke(args); }
            catch (Throwable t) { throw new LambdaJError(true, "%s.%s: %s", method.getDeclaringClass().getName(), method.getName(), t.toString()); }
        }
    }

    /** check the number of args vs. number of parameters, and then convert argument types from Murmel to Java */
    static void javaCallArgCheck(String name, Executable method, UnaryOperator<Object>[] argConv, Object[] args) {
        final int paramCount = argConv.length;
        final int argCount = args == null ? 0 : args.length;
        if (method.isVarArgs()) { if (argCount < paramCount - 1) errorVarargsCount(name, paramCount-1, argCount); }
        else                    { if (paramCount != argCount) errorArgCount(name, paramCount, paramCount, argCount, null); }

        UnaryOperator<Object> conv = null;
        if (args != null) for (int i = 0; i < argCount; i++) {
            if (i < argConv.length) conv = argConv[i];
            if (conv != null) args[i] = conv.apply(args[i]);
        }
    }

    private static final Class<?>[] EMPTY_CLASS_ARRAY = new Class[0];
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
                   ? new JavaConstructor(clazz.getDeclaredConstructor(params), paramClassNames)
                   : new JavaMethod(clazz.getMethod(methodName, params), paramClassNames);
        }
        catch (LambdaJError le) { throw le; }
        catch (Exception e) { throw new LambdaJError(true, "jmethod: exception finding method %s.%s: %s", className, methodName, e.getMessage()); }
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

        classByName.put("boolean...",   new Object[] { boolean[].class,   "toBoolean",   (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });
        classByName.put("byte...",      new Object[] { byte[].class,      "toByte",      (UnaryOperator<Object>)(MurmelJavaProgram::toByte)});
        classByName.put("short...",     new Object[] { short[].class,     "toShort",     (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });
        classByName.put("int...",       new Object[] { int[].class,       "toInt",       (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });
        classByName.put("long...",      new Object[] { long[].class,      "toLong",      (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });
        classByName.put("float...",     new Object[] { float[].class,     "toFloat",     (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });
        classByName.put("double...",    new Object[] { double[].class,    "toDouble",    (UnaryOperator<Object>)(MurmelJavaProgram::toDouble)});

        classByName.put("char...",      new Object[] { char.class,      "requireChar", (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });


        classByName.put("Object",    new Object[] { Object.class,    "requireNotNull",      (UnaryOperator<Object>)(MurmelJavaProgram::requireNotNull) });      aliases("Object");
        classByName.put("Object?",   new Object[] { Object.class,    null,                  null });                                                            aliases("Object?");
        classByName.put("Number",    new Object[] { Number.class,    "requireNumber",       (UnaryOperator<Object>)(MurmelJavaProgram::requireNumber) });       aliases("Number");
        classByName.put("Number?",   new Object[] { Number.class,    "requireNumberOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireNumberOrNull) }); aliases("Number?");
        classByName.put("Boolean",   new Object[] { Boolean.class,   "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });           aliases("Boolean");
        classByName.put("Byte",      new Object[] { Byte.class,      "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte) });              aliases("Byte");
        classByName.put("Short",     new Object[] { Short.class,     "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });             aliases("Short");
        classByName.put("Integer",   new Object[] { Integer.class,   "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });               aliases("Integer");
        classByName.put("Long",      new Object[] { Long.class,      "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });              aliases("Long");
        classByName.put("Float",     new Object[] { Float.class,     "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });             aliases("Float");
        classByName.put("Double",    new Object[] { Double.class,    "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble) });            aliases("Double");

        classByName.put("Object...",    new Object[] { Object[].class,    "requireNotNull",      (UnaryOperator<Object>)(MurmelJavaProgram::requireNotNull) });      aliases("Object...");
        classByName.put("Object?...",   new Object[] { Object[].class,    null,                  null });                                                            aliases("Object?...");
        classByName.put("Number...",    new Object[] { Number[].class,    "requireNumber",       (UnaryOperator<Object>)(MurmelJavaProgram::requireNumber) });       aliases("Number...");
        classByName.put("Number?...",   new Object[] { Number[].class,    "requireNumberOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireNumberOrNull) }); aliases("Number?...");
        classByName.put("Boolean...",   new Object[] { Boolean[].class,   "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });           aliases("Boolean...");
        classByName.put("Byte...",      new Object[] { Byte[].class,      "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte) });              aliases("Byte...");
        classByName.put("Short...",     new Object[] { Short[].class,     "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });             aliases("Short...");
        classByName.put("Integer...",   new Object[] { Integer[].class,   "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });               aliases("Integer...");
        classByName.put("Long...",      new Object[] { Long[].class,      "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });              aliases("Long...");
        classByName.put("Float...",     new Object[] { Float[].class,     "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });             aliases("Float...");
        classByName.put("Double...",    new Object[] { Double[].class,    "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble) });            aliases("Double...");


        classByName.put("Character",       new Object[] { Character.class,      "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });         aliases("Character");
        classByName.put("CharSequence",    new Object[] { CharSequence.class,   "requireCharSequence", (UnaryOperator<Object>)(MurmelJavaProgram::requireCharSequence) }); aliases("CharSequence");
        classByName.put("String",          new Object[] { String.class,         "requireString",       (UnaryOperator<Object>)(MurmelJavaProgram::requireString) });       aliases("String");
        classByName.put("String?",         new Object[] { String.class,         "requireStringOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireStringOrNull) }); aliases("String?");

        classByName.put("Character...",    new Object[] { Character[].class,    "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });         aliases("Character...");
        classByName.put("CharSequence...", new Object[] { CharSequence[].class, "requireCharSequence", (UnaryOperator<Object>)(MurmelJavaProgram::requireCharSequence) }); aliases("CharSequence...");
        classByName.put("String...",       new Object[] { String[].class,       "requireString",       (UnaryOperator<Object>)(MurmelJavaProgram::requireString) });       aliases("String...");
        classByName.put("String?...",      new Object[] { String[].class,       "requireStringOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireStringOrNull) }); aliases("String?...");
    }

    private static void aliases(String existing) {
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

        DynamicProxy(Map<Method, MurmelFunction> methods) {
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
                                       a -> { final WriteConsumer out = (WriteConsumer) a[0]; out.print(asString); return null; });

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
    private void environment() {
        if (haveIO()) {
            WellknownSymbol.forAllPrimitives(Features.HAVE_IO.bits(), this::addBuiltin);
        }

        if (haveGui()) {
            final Primitive makeFrame = a -> {
                varargsMinMax("make-frame", a, 1, 4);
                final String title = requireString("make-frame", car(a));
                final TurtleFrame ret = new TurtleFrame(title, requireNumberOrNull("make-frame", cadr(a)), requireNumberOrNull("make-frame", caddr(a)), requireNumberOrNull("make-frame", cadddr(a)));
                current_frame = ret;
                return ret;
            };
            addBuiltin("make-frame",    makeFrame);
            addBuiltin("open-frame",    (Primitive) a -> { varargs0_1("open-frame",    a); return requireFrame("open-frame",    car(a)).open();    });
            addBuiltin("close-frame",   (Primitive) a -> { varargs0_1("close-frame",   a); return requireFrame("close-frame",   car(a)).close();   });
            addBuiltin("reset-frame",   (Primitive) a -> { varargs0_1("reset-frame",   a); return requireFrame("reset-frame",   car(a)).reset();   });
            addBuiltin("clear-frame",   (Primitive) a -> { varargs0_1("clear-frame",   a); return requireFrame("clear-frame",   car(a)).clear();   });
            addBuiltin("repaint-frame", (Primitive) a -> { varargs0_1("repaint-frame", a); return requireFrame("repaint-frame", car(a)).repaint(); });
            addBuiltin("flush-frame",   (Primitive) a -> { varargs0_1("flush-frame",   a); return requireFrame("flush-frame",   car(a)).flush();   });

            // set new current frame, return previous frame
            addBuiltin("current-frame", (Primitive) a -> { varargs0_1("current-frame", a); final Object prev = current_frame; if (car(a) != null) current_frame = requireFrame("current-frame", car(a)); return prev; });

            addBuiltin("push-pos",      (Primitive) a -> { varargs0_1("push-pos", a); return requireFrame("push-pos",car(a)).pushPos(); });
            addBuiltin("pop-pos",       (Primitive) a -> { varargs0_1("pop-pos",  a); return requireFrame("pop-pos", car(a)).popPos();  });

            addBuiltin("pen-up",        (Primitive) a -> { varargs0_1("pen-up",   a); return requireFrame("pen-up",   car(a)).penUp();   });
            addBuiltin("pen-down",      (Primitive) a -> { varargs0_1("pen-down", a); return requireFrame("pen-down", car(a)).penDown(); });

            addBuiltin("color",         (Primitive) a -> { varargs1_2("color",   a); return requireFrame("color",   cadr(a)).color  (toInt("color",   car(a))); });
            addBuiltin("bgcolor",       (Primitive) a -> { varargs1_2("bgcolor", a); return requireFrame("bgcolor", cadr(a)).bgColor(toInt("bgcolor", car(a))); });

            addBuiltin("text",          (Primitive) a -> { varargs1_2("text",    a); return requireFrame("text",    cadr(a)).text   (car(a).toString()); });

            addBuiltin("right",         (Primitive) a -> { varargs1_2("right",   a); return requireFrame("right",   cadr(a)).right  (toDouble("right",   car(a))); });
            addBuiltin("left",          (Primitive) a -> { varargs1_2("left",    a); return requireFrame("left",    cadr(a)).left   (toDouble("left",    car(a))); });
            addBuiltin("forward",       (Primitive) a -> { varargs1_2("forward", a); return requireFrame("forward", cadr(a)).forward(toDouble("forward", car(a))); });

            addBuiltin("move-to",       (Primitive) a -> { varargsMinMax("move-to", a, 2, 3);  return requireFrame("move-to",  caddr(a)).moveTo(toDouble("move-to",  car(a)), toDouble("move-to", cadr(a)));  });
            addBuiltin("line-to",       (Primitive) a -> { varargsMinMax("line-to", a, 2, 3);  return requireFrame("line-to",  caddr(a)).lineTo(toDouble("line-to",  car(a)), toDouble("line-to", cadr(a)));  });
            addBuiltin("move-rel",      (Primitive) a -> { varargsMinMax("move-rel", a, 2, 3); return requireFrame("move-rel", caddr(a)).moveRel(toDouble("move-rel", car(a)), toDouble("move-rel", cadr(a))); });
            addBuiltin("line-rel",      (Primitive) a -> { varargsMinMax("line-rel", a, 2, 3); return requireFrame("line-rel", caddr(a)).lineRel(toDouble("line-rel", car(a)), toDouble("line-rel", cadr(a))); });

            addBuiltin("make-bitmap",   (Primitive) a -> { varargsMinMax("make-bitmap",    a, 2, 3); return requireFrame("make-bitmap",    caddr(a)).makeBitmap(toInt("make-bitmap",  car(a)), toInt("make-bitmap", cadr(a))); });
            addBuiltin("discard-bitmap",(Primitive) a -> { varargs0_1("discard-bitmap",    a);       return requireFrame("discard-bitmap", car(a)).discardBitmap(); });
            addBuiltin("set-pixel",     (Primitive) a -> { varargsMinMax("set-pixel",      a, 3, 4); return requireFrame("set-pixel",      cadddr(a)).setRGB(toInt("set-pixel", car(a)), toInt("set-pixel", cadr(a)), toInt("set-pixel", caddr(a)));  });
            addBuiltin("rgb-to-pixel",  (Primitive) a -> { threeArgs("rgb-to-pixel",   a);
                                                           return (long)(int)(toInt("rgb-to-pixel", car(a)) << 16
                                                                              | toInt("rgb-to-pixel", cadr(a)) << 8
                                                                              | toInt("rgb-to-pixel", caddr(a))); });
            addBuiltin("hsb-to-pixel",  (Primitive) a -> { threeArgs("hsb-to-pixel",   a);
                                                           return (long)Color.HSBtoRGB(toFloat("hsb-to-pixel", car(a)),
                                                                                       toFloat("hsb-to-pixel", cadr(a)),
                                                                                       toFloat("hsb-to-pixel", caddr(a)));  });
        }

        if (haveString()) {
            WellknownSymbol.forAllPrimitives(Features.HAVE_STRING.bits(), this::addBuiltin);
        }

        if (haveApply()) {
            final LambdaJSymbol sApply = intern("apply");
            ocApply = new OpenCodedPrimitive(sApply);
            addBuiltin(sApply, ocApply);
        }

        if (haveXtra()) {
            addBuiltin(sDynamic, sDynamic);

            final LambdaJSymbol sEval = intern("eval");
            ocEval = new OpenCodedPrimitive(sEval);
            addBuiltin(sEval, ocEval);

            WellknownSymbol.forAllPrimitives(Features.HAVE_XTRA.bits(), this::addBuiltin);
        }

        if (haveT()) {
            addBuiltin(sT, sT);
        }

        if (haveNil()) {
            addBuiltin(sNil, null);
        }

        if (haveVector()) {
            addBuiltin("array-dimension-limit", MAX_ARRAY_SIZE);

            WellknownSymbol.forAllPrimitives(Features.HAVE_VECTOR.bits(), this::addBuiltin);
        }

        if (haveUtil()) {
            topEnv = cons(featuresEnvEntry, topEnv);
            addBuiltin("internal-time-units-per-second", (long)1e9);

            WellknownSymbol.forAllPrimitives(Features.HAVE_UTIL.bits(), this::addBuiltin);
        }

        if (haveFFI()) {
            WellknownSymbol.forAllPrimitives(Features.HAVE_FFI.bits(), this::addBuiltin);
        }

        if (haveNumbers()) {
            addBuiltin("pi", Math.PI);
            addBuiltin("most-positive-fixnum", MOST_POSITIVE_FIXNUM);
            addBuiltin("most-negative-fixnum", MOST_NEGATIVE_FIXNUM);

            WellknownSymbol.forAllPrimitives(Features.HAVE_NUMBERS.bits(), this::addBuiltin);
        }

        if (haveAtom()) {
            WellknownSymbol.forAllPrimitives(Features.HAVE_ATOM.bits(), this::addBuiltin);
        }

        if (haveEq()) {
            WellknownSymbol.forAllPrimitives(Features.HAVE_EQ.bits(), this::addBuiltin);
        }

        if (haveCons()) {
            WellknownSymbol.forAllPrimitives(Features.HAVE_CONS.bits(), this::addBuiltin);
        }
    }

    private void addBuiltin(final String sym, final Object value) {
        topEnv = acons(intern(sym), value, topEnv);
    }

    private void addBuiltin(final LambdaJSymbol sym, final Object value) {
        topEnv = acons(sym, value, topEnv);
    }
    
    private void addBuiltin(WellknownSymbol w) {
        topEnv = acons(intern(w.sym), (Primitive) a -> w.applyPrimitive(this, a), topEnv);
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
        final ConsCell envEntry = fastassq(intern(globalSymbol), topEnv);
        if (envEntry != null) return cdr(envEntry);
        throw new LambdaJError(true, "%s: '%s' is not bound", "getValue", globalSymbol);
    }

    private class CallLambda implements MurmelFunction {
        private final Closure lambda;
        private final ConsCell env;
        CallLambda(Closure lambda, ConsCell topEnv) { this.lambda = lambda; this.env = topEnv; }
        @Override public Object apply(Object... args) { return applyClosure(lambda, args, env); }
    }

    Object applyClosure(Closure closure, Object[] args, ConsCell originalTopEnv) {
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
        if (maybeFunction instanceof Closure)                           { return new CallLambda((Closure)maybeFunction, topEnv); }
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
        return new CallProgram(program, in, out);
    }

    private class CallProgram implements MurmelProgram {
        private final String program;
        private final ReadSupplier in;
        private final WriteConsumer out;

        public CallProgram(String program, ReadSupplier in, WriteConsumer out) {
            this.program = program;
            this.in = in;
            this.out = out;
        }

        @Override public Object getValue(String globalSymbol) { return LambdaJ.this.getValue(globalSymbol); }
        @Override public MurmelFunction getFunction(String funcName) { return LambdaJ.this.getFunction(funcName); }

        @Override public void setCommandlineArgumentList(ConsCell args) { insertFrontTopEnv(intern("*command-line-argument-list*"), args); }
        @Override public ObjectReader getLispReader() { return LambdaJ.this.getLispReader(); }
        @Override public ObjectWriter getLispPrinter() { return LambdaJ.this.getLispPrinter(); }
        @Override public void setReaderPrinter(ObjectReader reader, ObjectWriter writer) { LambdaJ.this.setReaderPrinter(reader, writer); }

        @Override public Object body() {
            return interpretExpressions(new StringReader(program)::read, in, out);
        }
    }


    /// JMurmel JSR-223 embed API - Java calls Murmel with JSR223 eval

    /** <p>evalScript is for JSR-223 support.
     *  <p>First call creates a new parser (parsers contain the symbol table) and inits the global environment
     *  <p>Subsequent calls will re-use the parser (including symbol table) and global environment. */
    public Object evalScript(Reader program, Reader in, Writer out) {
        if (topEnv == null) {
            lispReader = new SExpressionReader(features, trace, tracer, symtab, featuresEnvEntry, in::read, null);
            topEnv = null;
            environment();
        }
        final ObjectReader scriptParser = lispReader;
        scriptParser.setInput(program::read, null);
        currentSource = null;
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
        final Object result = expandAndEval(exp, null); // don't just use eval - maybe there are no macros to expand but expandAndEval also does syntax checks. Also they could pass a progn form containing macros.
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

    private enum Action { INTERPRET, TO_JAVA, TO_JAR, COMPILE_AND_RUN, }

    private static class Exit extends RuntimeException {
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
                            System.out.println("==> " + printSEx(result));
                        }
                        //printNotopencoded(interpreter.notOpencoded);
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
                        System.out.println("==> " + printSEx(result));
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

    //static void printNotopencoded(Map<LambdaJSymbol, Integer> notOpencoded) {
    //    for (Map.Entry<LambdaJSymbol, Integer> entry: notOpencoded.entrySet()) {
    //        System.out.printf("%d\t%s%n", entry.getValue(), entry.getKey());
    //    }
    //}

    /** exit by throwing an {@link Exit} exception, doesn't return. The last form of the program will determine the exitlevel:
     *  nil will result in 0, a number will result in an exitlevel of number&127, any other non-nil value will result in an exitlevel of 1. */
    private static void exit(Object murmelResult) {
        if (murmelResult == null) return; // todo throw new Exit(0) statt return?
        if (numberp(murmelResult)) throw new Exit(((Number)murmelResult).intValue() & 0x7f);
        throw EXIT_PROGRAM_ERROR;
    }



    /// functions to interpret, compile and/ or run files or input streams
    private static Object interpretStream(final LambdaJ interpreter, ReadSupplier prog, Path fileName, final boolean printResult, List<Object> history) {
        try {
            final ObjectReader reader = interpreter.lispReader;
            reader.setInput(prog, fileName);
            interpreter.currentSource = fileName;
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
        final MurmelJavaCompiler c = new MurmelJavaCompiler(symtab, libPath, getTmpDir());

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
        return compileAndRunForms(program, args, interpreter, false, finalResult);
    }

    /** compile history to a class and run compiled class */
    private static Object compileAndRunForms(ObjectReader history, String[] cmdlineArgs, LambdaJ interpreter, boolean repl, boolean finalResult) {
        final Path tmpDir;
        try { tmpDir = getTmpDir(); }
        catch (IOException e) {
            System.out.println("history NOT run as Java - cannot get/ create tmp directory: " + e.getMessage());
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

                if (repl && ((MurmelJavaProgram)prg).values != null) {
                    for (Object value : ((MurmelJavaProgram)prg).values) {
                        System.out.print(" -> ");
                        prg.getLispPrinter().printObj(value, true);
                        System.out.println();
                    }
                }
                else { System.out.print("==> ");  prg.getLispPrinter().printObj(result, true);  System.out.println(); }
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
        if (filename == sT) {
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
        final ReadSupplier nonechoingSupplier = consoleReader::read;
        if (isInit) {
            interpreter.nCells = 0; interpreter.maxEnvLen = 0;
            parser = new SExpressionReader(interpreter.features, interpreter.trace, interpreter.tracer, interpreter.symtab, interpreter.featuresEnvEntry,
                                           echo ? echoingSupplier : nonechoingSupplier, null);
            outWriter = interpreter.lispPrinter;
        }
        for (;;) {
            if (!isInit) {
                interpreter.nCells = 0; interpreter.maxEnvLen = 0;
                parser = new SExpressionReader(interpreter.features, interpreter.trace, interpreter.tracer, interpreter.symtab, interpreter.featuresEnvEntry,
                                               echo ? echoingSupplier : nonechoingSupplier, null);
                outWriter = makeWriter(System.out::print);
                interpreter.lispReader = parser; interpreter.lispPrinter = outWriter;
                interpreter.topEnv = null;
                interpreter.environment();
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
                    if (exp == cmdNoEcho) { echo = false; parser.setInput(nonechoingSupplier, null); continue; }
                    if (exp == cmdRes)    { isInit = false; history.clear();  continue; }
                    if (exp == cmdList)   { listHistory(history); continue; }
                    if (exp == cmdWrite)  { writeHistory(history, parser.readObj(false)); continue; }
                    if (exp == cmdJava)   { compileToJava(consoleCharset, interpreter.symtab, interpreter.libDir, makeReader(history), parser.readObj(false), parser.readObj(false)); continue; }
                    if (exp == cmdRun)    { compileAndRunForms(makeReader(history), null, interpreter, true, false); continue; }
                    if (exp == cmdJar)    { compileToJar(interpreter.symtab, interpreter.libDir, makeReader(history), parser.readObj(false), parser.readObj(false)); continue; }
                    //if (":peek".equals(exp.toString())) { System.out.println("gensymcounter: " + interpreter.gensymCounter); continue; }
                    if (exp == cmdEnv)    {
                        if (interpreter.topEnv != null) for (Object entry: interpreter.topEnv) System.out.println(entry);
                        System.out.println("env length: " + listLength(interpreter.topEnv));  System.out.println(); continue; }
                    if (exp == cmdMacros) {
                        final ArrayList<LambdaJSymbol> names = new ArrayList<>();
                        for (Object entry: ((ListSymbolTable)interpreter.symtab).symbols) {
                            if (entry == null) continue;
                            final LambdaJSymbol sym = (LambdaJSymbol)entry;
                            if (sym.macro != null) names.add(sym);
                        }
                        names.sort(Comparator.comparing(Object::toString));
                        for (LambdaJSymbol name: names) System.out.println(name + ": " + printSEx(ConsCell.cons(name.macro.params, name.macro.body)));
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
        if (hasFlag("--no-vector", args))   features &= ~Features.HAVE_VECTOR.bits();
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

        if (hasFlag("--XX-dyn", args))       features &= ~Features.HAVE_ALL_DYN.bits();
        if (hasFlag("--XX-oldlambda", args)) features |= Features.HAVE_OLDLAMBDA.bits();

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

        intp.insertFrontTopEnv(intp.intern("*command-line-argument-list*"), arraySlice(args, n));
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
                           + "--no-vector ...  no vector support\n"
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
                           + "--XX-oldlambda   Lists whose car is 'lambda' are (anonymous) functions, too.\n"
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
        private final Iterator<Path> paths;
        private final LambdaJ intp;
        private final ObjectReader delegate;

        private Reader reader;

        MultiFileReadSupplier(List<Path> paths, LambdaJ intp, ObjectReader delegate, boolean verbose) {
            this.paths = paths.iterator();
            this.intp = intp;
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
            intp.currentSource = p;
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
        reader.setInput(new MultiFileReadSupplier(paths, interpreter, reader, verbose), paths.get(0));
        interpreter.currentSource = paths.get(0);
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

        public static final CompilerGlobal UNASSIGNED_GLOBAL = new CompilerGlobal(null) { @Override public Object get() { throw new LambdaJError(false, "unassigned value"); } };
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
            intp.compiledProgram = this;
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
        public final Object _dynamic;

        public static final double _pi = Math.PI;
        public static final int arrayDimensionLimit = MAX_ARRAY_SIZE;
        public static final long mostPositiveFixnum = MOST_POSITIVE_FIXNUM;
        public static final long mostNegativeFixnum = MOST_NEGATIVE_FIXNUM;

        /// predefined aliased global variables
        // internal-time-units-per-second: itups doesn't have a leading _ because it is avaliable under an alias name
        public static final long itups = (long)1e9;
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

        // basic primitives
        public final Object _apply (Object... args) {
            twoArgs("apply", args.length);
            Object fn = args[0];
            if (symbolp(fn)) fn = getValue(fn.toString());
            return tailcall(fn, listToArray(args[1]));
        }
        public final Object apply(Object... args) {
            Object fn = args[0];
            if (symbolp(fn)) fn = getValue(fn.toString());
            return tailcall(fn, listToArray(args[1]));
        }
        public final Object _eval      (Object... args) { varargs1_2("eval",     args.length); return intp.expandAndEval(args[0], args.length == 2 ? LambdaJ.requireList("eval", args[1]) : null); }


        // logic, predicates
        private Object bool(boolean result) { return result ? _t : null; }

        public final Object _eq        (Object... args) { twoArgs("eq",          args.length);        return bool(args[0] == args[1]); }

        public final Object _eql       (Object... args) { twoArgs("eql",         args.length);        return bool(LambdaJ.eql(args[0], args[1])); }
        public final Object eql(Object o1, Object o2)   { return bool(LambdaJ.eql(o1, o2)); }

        public final Object _consp     (Object... args) { oneArg("consp",        args.length);        return bool(consp(args[0])); }
        public final Object _atom      (Object... args) { oneArg("atom",         args.length);        return bool(atom(args[0])); }
        public final Object _symbolp   (Object... args) { oneArg("symbolp",      args.length);        return bool(symbolp(args[0])); }
        public final Object _null      (Object... args) { oneArg("null",         args.length);        return bool(args[0] == null); }
        public final Object _numberp   (Object... args) { oneArg("numberp",      args.length);        return bool(numberp(args[0])); }
        public final Object _floatp    (Object... args) { oneArg("floatp",       args.length);        return bool(floatp(args[0])); }
        public final Object _integerp  (Object... args) { oneArg("integerp",     args.length);        return bool(integerp(args[0])); }
        public final Object _characterp(Object... args) { oneArg("characterp",   args.length);        return bool(characterp(args[0])); }

        public final Object _vectorp   (Object... args) { oneArg("vectorp",      args.length);        return bool(vectorp(args[0])); }
        public final Object svectorp   (Object... args) { oneArg("simple-vector-p", args.length);     return bool(LambdaJ.svectorp(args[0])); }
        public final Object _stringp   (Object... args) { oneArg("stringp",      args.length);        return bool(stringp(args[0])); }
        public final Object sstringp   (Object... args) { oneArg("simple-string-p", args.length);     return bool(LambdaJ.sstringp(args[0])); }
        public final Object bitvectorp (Object... args) { oneArg("bit-vector-p", args.length);        return bool(LambdaJ.bitvectorp(args[0])); }
        public final Object sbitvectorp(Object... args) { oneArg("simple-bit-vector-p", args.length); return bool(LambdaJ.sbitvectorp(args[0])); }


        public final Object _functionp (Object... args) { oneArg("functionp",    args.length);        return bool(intp.functionp(args[0])); }

        public final Object _listp     (Object... args) { oneArg("listp",        args.length);        return bool(listp(args[0])); }
        public final Object adjustableArrayP(Object... args) { oneArg("adjustable-array-p", args.length); return bool(LambdaJ.adjustableArrayP(args[0])); }


        // conses and lists
        public final  Object _car      (Object... args) { oneArg("car",     args.length); return car(args[0]); }
        public static Object car       (Object l)       { return LambdaJ.car(l); } // also used by generated code
        public static Object car       (ConsCell l)     { return LambdaJ.car(l); }

        public final  Object _cdr      (Object... args) { oneArg("cdr",     args.length); return cdr(args[0]); }
        public static Object cdr       (Object l)       { return LambdaJ.cdr(l); } // also used by generated code
        public static Object cdr       (ConsCell l)     { return LambdaJ.cdr(l); }

        public final  ConsCell _cons   (Object... args) { twoArgs("cons",   args.length); return cons(args[0], args[1]); }
        public static ConsCell cons(Object car, Object cdr)  { return ConsCell.cons(car, cdr); } // also used by generated code

        public final  ConsCell _rplaca (Object... args) { twoArgs("rplaca", args.length);  return rplaca(args[0], args[1]); }
        public static ConsCell rplaca(Object l, Object newCar) { return LambdaJ.requireList("rplaca", l).rplaca(newCar); }
        public static ConsCell rplaca(ConsCell l, Object newCar) { return l.rplaca(newCar); }

        public final  ConsCell _rplacd (Object... args) { twoArgs("rplacd", args.length);  return rplacd(args[0], args[1]); }
        public static ConsCell rplacd(Object l, Object newCdr) { return LambdaJ.requireList("rplacd", l).rplacd(newCdr); }
        public static ConsCell rplacd(ConsCell l, Object newCdr) { return l.rplacd(newCdr); }

        public final ConsCell _list    (Object... args) { return ListBuilder.list(args); }
        public final Object listStar   (Object... args) {
            int nArgs;
            varargs1("list*", nArgs = args.length);
            if (nArgs == 1) return args[0];
            if (nArgs == 2) return cons(args[0], args[1]);
            final ListBuilder b = new ListBuilder();
            int i = 0;
            nArgs--;
            for (; i < nArgs; i++) b.append(nth(i, args));
            b.appendLast(nth(i, args));
            return b.first();
        }
        public final Object   _append  (Object... args) {
            int nArgs;
            if (args == null || (nArgs = args.length) == 0) return null;
            if (nArgs == 1) return args[0];
            if (!listp(args[0])) throw new LambdaJError(true, "append: first argument %s is not a list", args[0]);

            nArgs--;
            int first = 0;
            while (first < nArgs && args[first] == null) first++; // skip leading nil args if any

            ListBuilder lb = null;
            for (int i = first; i < nArgs; i++) {
                final Object o = args[i];
                if (o == null) continue;
                if (!consp(o)) throw new LambdaJError(true, "append: argument %d is not a list: %s", i+1, printSEx(o));
                if (lb == null) lb = new ListBuilder();
                for (Object obj: (ConsCell)o) lb.append(obj);
            }
            if (lb == null) return args[first];
            lb.appendLast(args[nArgs]);
            return lb.first();
        }
        public final ConsCell _assq    (Object... args) { twoArgs("assq",        args.length); return assq(args[0], args[1]); }
        public final ConsCell _assoc   (Object... args) { twoArgs("assoc",       args.length); return assoc(args[0], args[1]); }


        // numbers, characters

        public final double add        (Object... args) { if (args.length > 0) { double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret += toDouble(args[i]); return ret; } return 0.0; }
        public final double mul        (Object... args) { if (args.length > 0) { double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret *= toDouble(args[i]); return ret; } return 1.0; }

        public final double sub        (Object... args) { varargs1("-", args.length);
                                                          if (args.length == 1) return 0.0 - toDouble(args[0]);
                                                          double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret -= toDouble(args[i]); return ret; }
        public final double quot       (Object... args) { varargs1("/", args.length);
                                                          if (args.length == 1) return 1.0 / toDouble(args[0]);
                                                          double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret /= toDouble(args[i]); return ret; }

        public final Object numbereq   (Object... args) { return compare("=",  args, (d1, d2) -> d1 == d2); }
        public final Object ne         (Object... args) { return compare("/=", args, (d1, d2) -> d1 != d2); }
        public final Object lt         (Object... args) { return compare("<",  args, (d1, d2) -> d1 <  d2); }
        public final Object le         (Object... args) { return compare("<=", args, (d1, d2) -> d1 <= d2); }
        public final Object ge         (Object... args) { return compare(">=", args, (d1, d2) -> d1 >= d2); }
        public final Object gt         (Object... args) { return compare(">",  args, (d1, d2) -> d1 >  d2); }
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

        public final Number   inc      (Object... args) { oneArg("1+",         args.length); return LambdaJ.inc(args[0]); }
        public static Number  inc1     (Object arg)     { return LambdaJ.inc(arg); }
        public final Number   dec      (Object... args) { oneArg("1-",         args.length); return LambdaJ.dec(args[0]); }
        public static Number  dec1     (Object arg)     { return LambdaJ.dec(arg); }

        public final Number   _signum  (Object... args) { oneArg("signum",        args.length); return cl_signum (args[0]); }

        public final long     _round   (Object... args) { varargs1_2("round",     args.length); return toFixnum(cl_round   (quot12(args))); }
        public final long     _floor   (Object... args) { varargs1_2("floor",     args.length); return toFixnum(Math.floor (quot12(args))); }
        public final long     _ceiling (Object... args) { varargs1_2("ceiling",   args.length); return toFixnum(Math.ceil  (quot12(args))); }
        public final long     _truncate(Object... args) { varargs1_2("truncate",  args.length); return toFixnum(cl_truncate(quot12(args))); }

        public final double   _fround   (Object... args) { varargs1_2("fround",   args.length); return cl_round   (quot12(args)); }
        public final double   _ffloor   (Object... args) { varargs1_2("ffloor",   args.length); return Math.floor (quot12(args)); }
        public final double   _fceiling (Object... args) { varargs1_2("fceiling", args.length); return Math.ceil  (quot12(args)); }
        public final double   _ftruncate(Object... args) { varargs1_2("ftruncate",args.length); return cl_truncate(quot12(args)); }

        public static double cl_round(double d) { return Math.rint(d); }
        public static double cl_truncate(double d) { return LambdaJ.cl_truncate(d); }
        public static long toFixnum(double d) { return LambdaJ.toFixnum(d); }
        private static double quot12(Object[] args) { return args.length == 2 ? toDouble(args[0]) / toDouble(args[1]) : toDouble(args[0]); }

        public final double   _sqrt    (Object... args) { oneArg("sqrt",          args.length); return Math.sqrt (toDouble(args[0])); }
        public final double   _log     (Object... args) { oneArg("log",           args.length); return Math.log  (toDouble(args[0])); }
        public final double   _log10   (Object... args) { oneArg("log10",         args.length); return Math.log10(toDouble(args[0])); }
        public final double   _exp     (Object... args) { oneArg("exp",           args.length); return Math.exp  (toDouble(args[0])); }
        public final double   _expt    (Object... args) { twoArgs("expt",         args.length); return Math.pow  (toDouble(args[0]), toDouble(args[1])); }
        public final double   _mod     (Object... args) { twoArgs("mod",          args.length); return cl_mod(toDouble(args[0]), toDouble(args[1])); }
        public static double cl_mod(double lhs, double rhs) { return LambdaJ.cl_mod(lhs, rhs); }
        public final double   _rem     (Object... args) { twoArgs("rem",          args.length); return toDouble(args[0]) % toDouble(args[1]); }


        // vectors, sequences

        public final Object   makeArray(Object... args) { varargsMinMax("make-array", args.length, 1, 3);
                                                          if (args.length == 1) return new Object[toArrayIndex(args[0])];
                                                          return intp.makeArray(arraySlice(args)); }
        public final long     vectorLength(Object... args) { oneArg("vector-length", args.length); return LambdaJ.vectorLength(args[0]); }
        public final Object   vectorCopy  (Object... args) { oneArg("vector-copy", args.length);   return LambdaJ.vectorCopy(args[0]); }
        public final Object   vectorFill  (Object... args) { varargsMinMax("vector-fill", args.length, 2, 4);
                                                             return LambdaJ.vectorFill(args[0], args[1], args.length <= 2 ? null : args[2], args.length <= 3 ? null : args[3]); }
        public final long     vectorPushExtend(Object... args) { twoArgs("vector-push-extend", args.length); return LambdaJ.vectorPushExtend(args[0], args[1]); }
        public final Object   vectorToList (Object... args) {
            oneArg("vector->list", args.length);
            final Object maybeVector = args[0];

            if (LambdaJ.svectorp(maybeVector))    return simpleVectorToList(args);
            if (stringp(maybeVector))             return stringToList(args);
            if (LambdaJ.sbitvectorp(maybeVector)) return simpleBitVectorToList(args);

            if (maybeVector instanceof Bitvector) {
                final Bitvector bv = (Bitvector)maybeVector;
                if (bv.size() == 0) return null;
                final ListBuilder ret = new ListBuilder();
                for (int i = 0; i < bv.size(); i++) ret.append(bv.get(i));
                return ret.first();
            }

            if (maybeVector instanceof List) {
                final List<?> l = (List<?>)maybeVector;
                if (l.isEmpty()) return null;
                final ListBuilder ret = new ListBuilder();
                for (final Object o: l) ret.append(o);
                return ret.first();
            }
            throw errorNotAVector("vector->list", maybeVector);
        }

        public final long     _svlength(Object... args) { oneArg("svlength", args.length); return svlength(args[0]); }
        public final Object   _svref   (Object... args) { twoArgs("svref",   args.length); return svref(args[0], args[1]); }
        public static Object svref(Object v, Object idx) { return LambdaJ.svref(v, toArrayIndex(idx)); }
        public final Object   _svset   (Object... args) { threeArgs("svref", args.length); return svset(args[0], args[1], args[2]); }
        public static Object svset(Object val, Object v, Object idx) { return LambdaJ.svset(val, v, toArrayIndex(idx)); }
        public final Object   simpleVectorToList (Object... args) {
            oneArg("simple-vector->list", args.length);
            final Object maybeVector = args[0];
            final Object[] s = LambdaJ.requireSimpleVector("simple-vector->list", maybeVector);
            final ListBuilder ret = new ListBuilder();
            final int len = s.length;
            for (int i = 0; i < len; i++) ret.append(s[i]);
            return ret.first();
        }
        public final Object listToSimpleVector(Object... args) { oneArg("list->simple-vector", args.length); return LambdaJ.listToArray(args[0]); }
        public final Object _vector  (Object... args) { return args; }

        public final long      _slength(Object... args) { oneArg("slength", args.length); return slength(args[0]); }
        public final char      _sref   (Object... args) { twoArgs("sref", args.length); return LambdaJ.sref(args[0], toArrayIndex(args[1])); }
        public final char      _sset   (Object... args) { threeArgs("sset", args.length); return LambdaJ.sset(LambdaJ.requireChar("sset", args[0]), args[1], toArrayIndex(args[2])); }
        public final Object   stringeq (Object... args) { twoArgs("string=", args.length); return bool(Objects.equals(LambdaJ.requireStringOrCharOrSymbol("string=", args[0]), LambdaJ.requireStringOrCharOrSymbol("string=", args[1]))); }
        public final Object   stringToList (Object... args) {
            oneArg("string->list", args.length);
            final Object maybeString = args[0];
            final ListBuilder ret = new ListBuilder();
            if (maybeString instanceof char[]) {
                final char[] carry = (char[])maybeString;
                final int len = carry.length;
                for (int i = 0; i < len; i++) ret.append(carry[i]);
                return ret.first();
            }
            final CharSequence s = LambdaJ.requireCharsequence("string->list", maybeString);
            final int len = s.length();
            for (int i = 0; i < len; i++) ret.append(s.charAt(i));
            return ret.first();
        }
        public final Object listToString(Object... args) { oneArg("list->string", args.length); return LambdaJ.listToString(args[0]); }

        public final long   charInt     (Object... args) { oneArg("char-code",     args.length); return (long) LambdaJ.requireChar("char-code", args[0]); }
        public final char   intChar     (Object... args) { oneArg("code-char",     args.length); return (char) toInt(args[0]); }

        public final Object bvEq        (Object... args) { twoArgs("bv=", args.length);          return bool(LambdaJ.bvEq(args[0], args[1])); }

        public final  long  _sbvlength  (Object... args) { oneArg("sbvlength", args.length);     return sbvlength(args[0]); }
        public final  long  _sbvref     (Object... args) { twoArgs("sbvref", args.length); return sbvref(args[0], args[1]); }
        public static long  sbvref(Object v, Object idx) { return LambdaJ.sbvref(v, toArrayIndex(idx)); }
        public static long  sbvref(Object v, long idx)   { return LambdaJ.sbvref(v, toArrayIndex(idx)); }
        public final  long  _sbvset     (Object... args) { threeArgs("sbvset", args.length);     return sbvset(args[0], args[1], args[2]); }
        public static long  sbvset(Object val, Object v, Object idx) { return LambdaJ.sbvset(toBit(val), v, toArrayIndex(idx)); }
        public static long  sbvset(long val, Object v, Object idx)   { return LambdaJ.sbvset(toBit(val), v, toArrayIndex(idx)); }
        public static long  sbvset(long val, Object v, long idx)     { return LambdaJ.sbvset(toBit(val), v, toArrayIndex(idx)); }
        public final Object sbvEq      (Object... args) { twoArgs("sbv=", args.length);         return bool(LambdaJ.sbvEq(args[0], args[1])); }
        public final Object simpleBitVectorToList(Object... args) {
            oneArg("simple-bit-vector->list", args.length);
            final Object maybeVector = args[0];
            final boolean[] s = LambdaJ.requireSimpleBitVector("simple-bit-vector->list", maybeVector);
            final ListBuilder ret = new ListBuilder();
            final int len = s.length;
            for (int i = 0; i < len; i++) ret.append(s[i] ? 1L : 0L);
            return ret.first();
        }
        public final Object   listToSimpleBitVector(Object... args) {
            oneArg("list->simple-bit-vector", args.length);
            return LambdaJ.listToBooleanArray(LambdaJ.requireList("list->simple-bit-vector", args[0]));
        }

        public final Object   _seqref  (Object... args) { twoArgs("seqref",   args.length); return LambdaJ.seqref(args[0], toArrayIndex(args[1])); }
        public final Object   _seqset  (Object... args) { threeArgs("seqset", args.length); return LambdaJ.seqset(args[0], args[1], toArrayIndex(args[2])); }


        // I/O
        public final Object _read      (Object... args) { varargs0_1("read",     args.length); return intp.read(arraySlice(args)); }
        public final Object _write     (Object... args) { varargs1_2("write",    args.length); return intp.write(args[0], args.length < 2 || args[1] != null); }
        public final Object _writeln   (Object... args) { varargs0_2("writeln",  args.length); return intp.writeln(arraySlice(args), args.length < 2 || args[1] != null); }
        public final Object _lnwrite   (Object... args) { varargs0_2("lnwrite",  args.length); return intp.lnwrite(arraySlice(args), args.length < 2 || args[1] != null); }

        public final Object format     (Object... args)  { varargs2("format", args.length); return intp.format(arraySlice(args)); }
        public final Object formatLocale(Object... args) { varargs3("format-locale", args.length); return intp.formatLocale(arraySlice(args)); }


        // misc
        Object[] values;
        public final Object _values    (Object... args) { values = args; return args.length == 0 ? null : args[0]; }
        public final Object _gensym    (Object... args) { noArgs("gensym", args.length); return intp.gensym(); }
        public final Object _trace     (Object... args) { return null; }
        public final Object _untrace   (Object... args) { return null; }
        public final Object _fatal     (Object... args) { oneArg("fatal", args.length); throw new RuntimeException(String.valueOf(args[0])); }


        // time
        public final long   getInternalRealTime(Object... args) { noArgs("get-internal-real-time", args.length); return LambdaJ.getInternalRealTime(); }
        public final long   getInternalRunTime (Object... args) { noArgs("get-internal-run-time", args.length); return LambdaJ.getInternalRunTime(); }
        public final long   getInternalCpuTime (Object... args) { noArgs("get-internal-cpu-time", args.length); return LambdaJ.getInternalCpuTime(); }
        public final Object sleep              (Object... args) { oneArg("sleep", args.length); return LambdaJ.sleep(args[0]); }
        public final long   getUniversalTime   (Object... args) { noArgs("get-universal-time", args.length); return LambdaJ.getUniversalTime(); }
        public final Object getDecodedTime     (Object... args) { noArgs("get-decoded-time", args.length); return intp.getDecodedTime(); }


        // Java FFI
        public final Object _jmethod   (Object... args) { varargs2("jmethod", args.length); return findMethod(args[0], args[1], arraySlice(args, 2)); }
        public static Primitive findMethod(Object className, Object methodName, ConsCell paramClasses) {
            return LambdaJ.findMethod(LambdaJ.requireString("jmethod", className), LambdaJ.requireString("jmethod", methodName), paramClasses);
        }
        public static Primitive findMethod(Object className, Object methodName, Object... paramClasses) {
            return LambdaJ.findMethod(LambdaJ.requireString("jmethod", className), LambdaJ.requireString("jmethod", methodName), arraySlice(paramClasses));
        }

        public final Object _jproxy    (Object... args) { varargs1("jproxy", args.length); return intp.makeProxy(arraySlice(args)); }


        // graphics
        public final Object makeFrame  (Object... args) {
            varargsMinMax("make-frame", args.length, 1, 4);
            final String title = LambdaJ.requireString("make-frame", args[0]);
            final TurtleFrame ret = new TurtleFrame(title, LambdaJ.requireNumberOrNull("make-frame", nth(1, args)), LambdaJ.requireNumberOrNull("make-frame", nth(2, args)), LambdaJ.requireNumberOrNull("make-frame", nth(3, args)));
            intp.current_frame = ret;
            return ret;
        }

        public final Object openFrame    (Object... args) { varargs0_1("open-frame",    args.length); return requireFrame("open-frame",     nth(0, args)).open();    }
        public final Object closeFrame   (Object... args) { varargs0_1("close-frame",   args.length); return requireFrame("close-frame",    nth(0, args)).close();   }
        public final Object resetFrame   (Object... args) { varargs0_1("reset-frame",   args.length); return requireFrame("reset-frame",    nth(0, args)).reset();   }
        public final Object clearFrame   (Object... args) { varargs0_1("clear-frame",   args.length); return requireFrame("clear-frame",    nth(0, args)).clear();   }
        public final Object repaintFrame (Object... args) { varargs0_1("repaint-frame", args.length); return requireFrame("repaint-frame",  nth(0, args)).repaint(); }
        public final Object flushFrame   (Object... args) { varargs0_1("flush-frame",   args.length); return requireFrame("flush-frame",    nth(0, args)).flush();   }

        // set new current frame, return previous frame
        public final Object currentFrame (Object... args) { varargs0_1("current-frame", args.length);
                                                            final Object prev = intp.current_frame;
                                                            if (args.length > 0 && args[0] != null) intp.current_frame = requireFrame("current-frame", args[0]);
                                                            return prev; }

        public final Object pushPos      (Object... args) { varargs0_1("push-pos",      args.length); return requireFrame("push-pos",       nth(0, args)).pushPos(); }
        public final Object popPos       (Object... args) { varargs0_1("pop-pos",       args.length); return requireFrame("pop-pos",        nth(0, args)).popPos();  }

        public final Object penUp        (Object... args) { varargs0_1("pen-up",        args.length); return requireFrame("pen-up",         nth(0, args)).penUp();   }
        public final Object penDown      (Object... args) { varargs0_1("pen-down",      args.length); return requireFrame("pen-down",       nth(0, args)).penDown(); }

        public final Object color        (Object... args) { varargs1_2("color",         args.length); return requireFrame("color",          nth(1, args)).color  (toInt(args[0])); }
        public final Object bgColor      (Object... args) { varargs1_2("bgcolor",       args.length); return requireFrame("bgcolor",        nth(1, args)).bgColor(toInt(args[0])); }

        public final Object text         (Object... args) { varargs1_2("text",          args.length); return requireFrame("text",           nth(1, args)).text   (args[0].toString()); }

        public final Object right        (Object... args) { varargs1_2("right",         args.length); return requireFrame("right",          nth(1, args)).right  (toDouble(args[0])); }
        public final Object left         (Object... args) { varargs1_2("left",          args.length); return requireFrame("left",           nth(1, args)).left   (toDouble(args[0])); }
        public final Object forward      (Object... args) { varargs1_2("forward",       args.length); return requireFrame("forward",        nth(1, args)).forward(toDouble(args[0])); }

        public final Object moveTo       (Object... args) { varargsMinMax("move-to",       args.length, 2, 3); return requireFrame("move-to",        nth(2, args)).moveTo(toDouble(args[0]), toDouble(args[1]));  }
        public final Object lineTo       (Object... args) { varargsMinMax("line-to",       args.length, 2, 3); return requireFrame("line-to",        nth(2, args)).lineTo(toDouble(args[0]), toDouble(args[1]));  }
        public final Object moveRel      (Object... args) { varargsMinMax("move-rel",      args.length, 2, 3); return requireFrame("move-rel",       nth(2, args)).moveRel(toDouble(args[0]), toDouble(args[1])); }
        public final Object lineRel      (Object... args) { varargsMinMax("line-rel",      args.length, 2, 3); return requireFrame("line-rel",       nth(2, args)).lineRel(toDouble(args[0]), toDouble(args[1])); }

        public final Object makeBitmap   (Object... args) { varargsMinMax("make-bitmap",   args.length, 2, 3); return requireFrame("make-bitmap",    nth(2, args)).makeBitmap(toInt(args[0]), toInt(args[1]));  }
        public final Object discardBitmap(Object... args) { varargs0_1("discard-bitmap",   args.length);       return requireFrame("discard-bitmap", nth(0, args)).discardBitmap();   }

        public final Object setPixel     (Object... args) { varargsMinMax("set-pixel",     args.length, 3, 4); return setPixel(toInt(args[0]), toInt(args[1]), toInt(args[2]), nth(3, args)); }
        public final Object setPixel     (Object x, Object y, Object rgb) { return setPixel(x, y, rgb, null);  }
        public final Object setPixel     (Object x, Object y, Object rgb, Object frame) { return requireFrame("set-pixel", frame).setRGB(toInt(x), toInt(y), toInt(rgb));  }

        public final  long rgbToPixel    (Object... args) { threeArgs("rgb-to-pixel", args.length); return rgbToPixel(args[0], args[1], args[2]); }
        public static long rgbToPixel    (Object red, Object green, Object blue) { return (int)((toInt(red) << 16) | (toInt(green) << 8) | toInt(blue)); }

        public final  long hsbToPixel    (Object... args) { threeArgs("hsb-to-pixel", args.length); return hsbToPixel(args[0], args[1], args[2]); }
        public static long hsbToPixel    (Object h, Object s, Object b) { return Color.HSBtoRGB(toFloat(h), toFloat(s), toFloat(b)); }


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

        private static int toArrayIndex(Object o) {
            if (o instanceof Long)   { final long l   = (Long)o;   final int i = Math.abs((int)l);       if (l == i)      return i; errorNotAnArrayIndex(o); }
            if (o instanceof Double) { final double d = (Double)o; final int i = Math.abs((int)d);       if (d == i)      return i; errorNotAnArrayIndex(o); }
            if (o instanceof Number) { final Number n = (Number)o; final int i = Math.abs(n.intValue()); if (n.equals(i)) return i; errorNotAnArrayIndex(o); }
            throw errorNotAnArrayIndex(o);
        }
        private static int toArrayIndex(long l) {
            final int i = Math.abs((int)l);
            if (l == i) return i;
            throw errorNotAnArrayIndex(l);
        }

        private static long toBit(Object o) {
            if (o instanceof Long)   { final long l   = (Long)o;   if (l == 0 || l == 1)      return l; errorNotABit(o); }
            if (o instanceof Double) { final double d = (Double)o; final long l = (int)d;       if (d == l && (l == 0 || l == 1))      return l; errorNotABit(o); }
            if (o instanceof Number) { final Number n = (Number)o; final long l = n.longValue(); if (n.equals(l) && (l == 0 || l == 1)) return l; errorNotABit(o); }
            throw errorNotABit(o);
        }
        private static long toBit(long l) {
            if (l == 0 || l == 1) return l;
            throw errorNotABit(l);
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

        public static int   toInt(Object n)       { return requireIntegralNumber("toInt", n, Integer.MIN_VALUE, Integer.MAX_VALUE).intValue(); }
        public static float toFloat(Object o) {
            final Number n = LambdaJ.requireNumber("toFloat", o);
            final double d = n.doubleValue();
            if (d >= Float.MIN_VALUE && d <= Float.MAX_VALUE) return n.floatValue();
            throw errorOverflow("toFloat", "java.lang.Float", o);
        }
        public static boolean toBoolean(Object n)  { return n != null; }
        public static byte toByte(Object n)  { return requireIntegralNumber("toByte", n, Byte.MIN_VALUE, Byte.MAX_VALUE).byteValue(); }
        public static short toShort(Object n) { return requireIntegralNumber("toShort", n, Short.MIN_VALUE, Short.MAX_VALUE).shortValue(); }


        /** used by generated Java code */
        public static Object requireNotNull(Object obj) {
            if (obj == null) { throw new LambdaJError(true, "object is nil"); }
            return obj;
        }

        /** used by generated Java code */
        public static ConsCell requireList(Object lst) {
            if (lst == null) return null;
            if (!consp(lst)) errorNotAList(lst);
            return (ConsCell)lst;
        }

        /** used by JFFI and generated inline JFFI */
        public static Character requireChar(Object o) {
            if (!characterp(o)) errorNotACharacter(o);
            return (Character)o;
        }

        /** used by JFFI and generated inline JFFI */
        public static CharSequence requireCharSequence(Object o) {
            if (o instanceof char[]) return String.valueOf((char[])o);
            if (!(o instanceof CharSequence)) errorNotAString(o);
            return (CharSequence)o;
        }

        /** used by JFFI and generated inline JFFI */
        public static String requireString(Object o) {
            if (!stringp(o)) errorNotAString(o);
            return o.toString();
        }

        /** used by JFFI and generated inline JFFI */
        public static String requireStringOrNull(Object o) {
            if (o == null) return null;
            if (!stringp(o)) errorNotAString(o);
            return o.toString();
        }

        /** used by JFFI and generated inline JFFI */
        public static Number requireNumber(Object o) {
            return LambdaJ.requireNumber("?", o);
        }

        /** used by JFFI and generated inline JFFI */
        public static Number requireNumberOrNull(Object o) {
            if (o == null) return null;
            return LambdaJ.requireNumber("?", o);
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

        public static <T> T[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, T[] resultArray) {
            if (transform == null) {
                for (int dst = 0, i = paramCount; i < args.length; ) { resultArray[dst++] = (T)args[i++]; }
            }
            else {
                for (int dst = 0, i = paramCount; i < args.length; ) { resultArray[dst++] = (T)transform.apply(args[i++]); }
            }
            return resultArray;
        }

        public static byte[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, byte[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (byte)transform.apply(args[i++]);
            return resultArray;
        }

        public static short[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, short[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (short)transform.apply(args[i++]);
            return resultArray;
        }

        public static int[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, int[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (int)transform.apply(args[i++]);
            return resultArray;
        }

        public static long[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, long[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (long)transform.apply(args[i++]);
            return resultArray;
        }

        public static float[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, float[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (float)transform.apply(args[i++]);
            return resultArray;
        }

        public static double[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, double[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (double)transform.apply(args[i++]);
            return resultArray;
        }

        public static char[] toVarargs(Object[] args, int paramCount, UnaryOperator<Object> transform, char[] resultArray) {
            assert transform != null;
            for (int dst = 0, i = paramCount; i < args.length; ) resultArray[dst++] = (char)transform.apply(args[i++]);
            return resultArray;
        }



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
                    if (functionCall.cleanup != null) cleanups = cons(functionCall.cleanup, cleanups);
                    r = functionCall.fn.apply(functionCall.args);
                    if (Thread.interrupted()) throw new InterruptedException("got interrupted");
                }
            }
            catch (LambdaJError e) { ex = e; }
            catch (Exception e) { ex = new LambdaJError(e, e.getMessage()); }
            finally {
                if (cleanups != null) for (Object cl: cleanups) {
                    try { ((MurmelFunction)cl).apply((Object[])null); }
                    //catch (LambdaJError e) { if (ex == null) ex = e; else ex.addSuppressed(e); }
                    //catch (Exception e)    { if (ex == null) ex = new LambdaJError(e, e.getMessage()); else ex.addSuppressed(e); }
                    catch (LambdaJError e) { ex = e; }
                    catch (Exception e)    { ex = new LambdaJError(e, e.getMessage()); }
                }
                if (ex != null) throw ex;
            }
            return r;
        }

        public final Object funcall(Object fn, Object... args) {
            if (fn instanceof MurmelFunction)    return funcall((MurmelFunction)fn, args);
            if (fn instanceof CompilerPrimitive) return funcall((CompilerPrimitive)fn, args);
            if (fn instanceof Primitive)         return ((Primitive)fn).applyPrimitive(arraySlice(args));
            if (fn instanceof Closure)           return interpret(fn, args);

            throw errorNotAFunction(fn);
        }

        private Object interpret(Object fn, Object[] args) {
            return intp.eval(cons(intp.intern("apply"),
                                  cons(fn,
                                       cons(cons(intp.intern("quote"),
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
            if (fn instanceof Closure)   return interpret(fn, args);
            throw errorNotAFunction(fn);
        }

        /** used for (apply sym form) */
        public final Object applyHelper(Object fn, Object argList) { return funcall(fn, toArray(argList)); }

        /** used for (apply sym form) */
        public final Object applyTailcallHelper(Object fn, Object argList) { return tailcall(fn, toArray(argList)); }

        public final Object doCatch(Object tag, MurmelFunction body) {
            try {
                return funcall(body, NOARGS);
            }
            catch (ReturnException re) {
                if (tag == re.tag) { values = re.values; return re.result; }
                throw re;
            }
            catch (LambdaJError le) { throw le; }
            catch (Exception e) { return rterror(e); }
        }

        public final Object doThrow(Object tag, Object primaryResult) {
            // todo checken obs tag gibt, sonst (error 'control-error)
            throw new ReturnException(tag, primaryResult, values);
        }



        private static Object nth(int n, Object[] args) { return args.length > n ? args[n] : null; }

        private static void noArgs(String expr, int argCount)      { if (0 != argCount)               errorArgCount(expr, 0, 0, argCount); }
        private static void oneArg(String expr, int argCount)      { if (1 != argCount)               errorArgCount(expr, 1, 1, argCount); }
        private static void twoArgs(String expr, int argCount)     { if (2 != argCount)               errorArgCount(expr, 2, 2, argCount); }
        private static void threeArgs(String expr, int argCount)   { if (3 != argCount)               errorArgCount(expr, 3, 3, argCount); }

        /** 0..1 args */
        private static void varargs0_1(String expr, int argCount) { if (argCount > 1)                 errorArgCount(expr, 0, 1, argCount); }
        /** 0..2 args */
        private static void varargs0_2(String expr, int argCount) { if (argCount > 2)                 errorArgCount(expr, 0, 2, argCount); }
        /** 1..2 args */
        private static void varargs1_2(String expr, int argCount) { if (argCount < 1 || argCount > 2) errorArgCount(expr, 1, 2, argCount); }
        /** one or more arguments */
        private static void varargs1(String expr, int argCount)   { if (argCount == 0)                errorArgCount(expr, 1, -1, 0); }
        /** two or more arguments */
        private static void varargs2(String expr, int argCount)   { if (argCount < 2)                 errorArgCount(expr, 2, -1, argCount); }
        private static void varargs3(String expr, int argCount)   { if (argCount < 3)                 errorArgCount(expr, 3, -1, argCount); }

        private static void varargsMinMax(String expr, int argCount, int min, int max) {
            if (argCount < min || argCount > max)
                errorArgCount(expr, min, max, argCount);
        }

        private static void errorArgCount(String expr, int expectedMin, int expectedMax, int actual) {
            if (actual < expectedMin) throw new LambdaJError(true, "%s: not enough arguments", expr);
            if (expectedMax != -1 && actual > expectedMax) throw new LambdaJError(true, "%s: too many arguments", expr);
        }

        private static RuntimeException errorNotANumber(Object n) { throw new LambdaJError(true, "not a number: %s", printSEx(n)); }
        private static RuntimeException errorNotABit(Object n) { throw new LambdaJError(true, "not a bit: %s", printSEx(n)); }
        private static RuntimeException errorNotAnArrayIndex(Object n) { throw new LambdaJError(true, "invalid array index/ size: %s", printSEx(n)); }
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
            intp.insertFrontTopEnv(intern("*command-line-argument-list*"), args);
        }

        @Override public Object getValue(String symbol) {
            switch (symbol) {

            // predefined global variables
            case "nil": return null;
            case "t": return _t;
            case "pi": return _pi;

            case "array-dimension-limit": return arrayDimensionLimit;
            case "most-positive-fixnum": return mostPositiveFixnum;
            case "most-negative-fixnum": return mostNegativeFixnum;
            case "internal-time-units-per-second": return itups;

            case "*command-line-argument-list*": return commandlineArgumentList; // this will be assigned by genereted code at runtime
            case "*features*": return features;

            // basic primitives
            case "apply": return (CompilerPrimitive)this::_apply;
            case "eval": return (CompilerPrimitive)this::_eval;

            // logic, predicates
            case "eq": return (CompilerPrimitive)this::_eq;
            case "eql": return (CompilerPrimitive)this::_eql;

            case "consp": return (CompilerPrimitive)this::_consp;
            case "atom": return (CompilerPrimitive)this::_atom;
            case "symbolp": return (CompilerPrimitive)this::_symbolp;
            case "null": return (CompilerPrimitive)this::_null;
            case "numberp": return (CompilerPrimitive)this::_numberp;
            case "floatp": return (CompilerPrimitive)this::_floatp;
            case "integerp": return (CompilerPrimitive)this::_integerp;
            case "characterp": return (CompilerPrimitive)this::_characterp;

            case "vectorp": return (CompilerPrimitive)this::_vectorp;
            case "simple-vector-p": return (CompilerPrimitive)this::svectorp;
            case "stringp": return (CompilerPrimitive)this::_stringp;
            case "simple-string-p": return (CompilerPrimitive)this::sstringp;
            case "bit-vector-p": return (CompilerPrimitive)this::bitvectorp;
            case "simple-bit-vector-p": return (CompilerPrimitive)this::sbitvectorp;

            case "functionp": return (CompilerPrimitive)this::_functionp;

            case "listp": return (CompilerPrimitive)this::_listp;
            case "adjustable-array-p": return (CompilerPrimitive)this::adjustableArrayP;

            // conses and lists
            case "car": return (CompilerPrimitive)this::_car;
            case "cdr": return (CompilerPrimitive)this::_cdr;
            case "cons": return (CompilerPrimitive)this::_cons;
            case "rplaca": return (CompilerPrimitive)this::_rplaca;
            case "rplacd": return (CompilerPrimitive)this::_rplacd;

            case "list": return (CompilerPrimitive)this::_list;
            case "list*": return (CompilerPrimitive)this::listStar;
            case "append": return (CompilerPrimitive)this::_append;
            case "assq": return (CompilerPrimitive)this::_assq;
            case "assoc": return (CompilerPrimitive)this::_assoc;

            // numbers, characters
            case "+": return (CompilerPrimitive)this::add;
            case "*": return (CompilerPrimitive)this::mul;
            case "-": return (CompilerPrimitive)this::sub;
            case "/": return (CompilerPrimitive)this::quot;

            case "=": return (CompilerPrimitive)this::numbereq;
            case "/=": return (CompilerPrimitive)this::ne;
            case "<": return (CompilerPrimitive)this::lt;
            case "<=": return (CompilerPrimitive)this::le;
            case ">=": return (CompilerPrimitive)this::ge;
            case ">": return (CompilerPrimitive)this::gt;

            case "1+": return (CompilerPrimitive)this::inc;
            case "1-": return (CompilerPrimitive)this::dec;

            case "signum": return (CompilerPrimitive)this::_signum;

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

            // vectors, sequences
            case "make-array": return (CompilerPrimitive)this::makeArray;

            case "vector-length": return (CompilerPrimitive)this::vectorLength;
            case "vector-copy": return (CompilerPrimitive)this::vectorCopy;
            case "vector-fill": return (CompilerPrimitive)this::vectorFill;
            case "vector-push-extend": return (CompilerPrimitive)this::vectorPushExtend;
            case "vector->list": return (CompilerPrimitive)this::vectorToList;

            case "svlength": return (CompilerPrimitive)this::_svlength;
            case "svref": return (CompilerPrimitive)this::_svref;
            case "svset": return (CompilerPrimitive)this::_svset;
            case "simple-vector->list": return (CompilerPrimitive)this::simpleVectorToList;
            case "list->simple-vector": return (CompilerPrimitive)this::listToSimpleVector;
            case "vector": return (CompilerPrimitive)this::_vector;

            case "slength": return (CompilerPrimitive)this::_slength;
            case "sref": return (CompilerPrimitive)this::_sref;
            case "sset": return (CompilerPrimitive)this::_sset;
            case "string=": return (CompilerPrimitive)this::stringeq;
            case "string->list": return (CompilerPrimitive)this::stringToList;
            case "list->string": return (CompilerPrimitive)this::listToString;

            case "char-code": return (CompilerPrimitive)this::charInt;
            case "code-char": return (CompilerPrimitive)this::intChar;

            case "bv=": return (CompilerPrimitive)this::bvEq;

            case "sbvlength": return (CompilerPrimitive)this::_sbvlength;
            case "sbvref": return (CompilerPrimitive)this::_sbvref;
            case "sbvset": return (CompilerPrimitive)this::_sbvset;
            case "sbv=": return (CompilerPrimitive)this::sbvEq;
            case "simple-bit-vector->list": return (CompilerPrimitive)this::simpleBitVectorToList;
            case "list->simple-bit-vector": return (CompilerPrimitive)this::listToSimpleBitVector;

            case "seqref": return (CompilerPrimitive)this::_seqref;
            case "seqset": return (CompilerPrimitive)this::_seqset;

            // I/O
            case "read": return (CompilerPrimitive)this::_read;
            case "write": return (CompilerPrimitive)this::_write;
            case "writeln": return (CompilerPrimitive)this::_writeln;
            case "lnwrite": return (CompilerPrimitive)this::_lnwrite;

            case "format": return (CompilerPrimitive)this::format;
            case "format-locale": return (CompilerPrimitive)this::formatLocale;

            // misc
            case "values": return (CompilerPrimitive)this::_values;
            case "gensym": return (CompilerPrimitive)this::_gensym;
            case "trace": return (CompilerPrimitive)this::_trace;
            case "untrace": return (CompilerPrimitive)this::_untrace;
            case "fatal": return (CompilerPrimitive)this::_fatal;

            // time
            case "get-internal-real-time": return (CompilerPrimitive)this::getInternalRealTime;
            case "get-internal-run-time": return (CompilerPrimitive)this::getInternalRunTime;
            case "get-internal-cpu-time": return (CompilerPrimitive)this::getInternalCpuTime;
            case "sleep": return (CompilerPrimitive)this::sleep;
            case "get-universal-time": return (CompilerPrimitive)this::getUniversalTime;
            case "get-decoded-time": return (CompilerPrimitive)this::getDecodedTime;

            // Java FFI
            case "jmethod": return (CompilerPrimitive)this::_jmethod;
            case "jproxy": return (CompilerPrimitive)this::_jproxy;

            // graphics
            case "make-frame": return (CompilerPrimitive)this::makeFrame;
            case "open-frame": return (CompilerPrimitive)this::openFrame;
            case "close-frame": return (CompilerPrimitive)this::closeFrame;
            case "reset-frame": return (CompilerPrimitive)this::resetFrame;
            case "clear-frame": return (CompilerPrimitive)this::clearFrame;
            case "repaint-frame": return (CompilerPrimitive)this::repaintFrame;
            case "flush-frame": return (CompilerPrimitive)this::flushFrame;
            case "current-frame": return (CompilerPrimitive)this::currentFrame;

            case "color": return (CompilerPrimitive)this::color;
            case "bgcolor": return (CompilerPrimitive)this::bgColor;

            case "right": return (CompilerPrimitive)this::right;
            case "left": return (CompilerPrimitive)this::left;
            case "forward": return (CompilerPrimitive)this::forward;
            case "move-to": return (CompilerPrimitive)this::moveTo;
            case "line-to": return (CompilerPrimitive)this::lineTo;
            case "move-rel": return (CompilerPrimitive)this::moveRel;
            case "line-rel": return (CompilerPrimitive)this::lineRel;

            case "push-pos": return (CompilerPrimitive)this::pushPos;
            case "pop-pos": return (CompilerPrimitive)this::popPos;
            case "pen-up": return (CompilerPrimitive)this::penUp;
            case "pen-down": return (CompilerPrimitive)this::penDown;

            case "text": return (CompilerPrimitive)this::text;

            case "make-bitmap": return (CompilerPrimitive)this::makeBitmap;
            case "discard-bitmap": return (CompilerPrimitive)this::discardBitmap;
            case "set-pixel": return (CompilerPrimitive)this::setPixel;
            case "rgb-to-pixel": return (CompilerPrimitive)this::rgbToPixel;
            case "hsb-to-pixel": return (CompilerPrimitive)this::hsbToPixel;

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
            if (symname == null) return sNil;
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
        private static ConsCell extenv(String func, Object symbol, int sfx, ConsCell prev) {
            final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(func, symbol);
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
        private Set<Object> implicitDecl;
        private Set<LambdaJSymbol> globalDecl;

        /** return {@code form} as a Java expression */
        private String javasym(Object form, ConsCell env) {
            if (form == null || form == sNil) return "(Object)null";
            final ConsCell symentry = fastassq(form, env);
            if (symentry == null) {
                if (passTwo) errorMalformedFmt("compilation unit", "undefined symbol %s", form);
                System.err.println("implicit declaration of " + form); // todo lineinfo of containing form
                implicitDecl.add(form);
                return mangle(form.toString(), 0) + ".get()"; // on pass 1 assume that undeclared variables are forward references to globals
            }
            else if (!passTwo && globalDecl.contains(form)) implicitDecl.remove(form);

            final String javasym;
            if (listp(cdr(symentry))) javasym = (String)cadr(symentry); // function: symentry is (sym . (javasym . (params...)))
            else javasym = (String)cdr(symentry);
            return javasym;
        }

        private static void notDefined(String func, Object sym, ConsCell env) {
            final ConsCell prevEntry = fastassq(sym, env);
            if (prevEntry != null) {
                LambdaJ.notReserved(func, (LambdaJSymbol)car(prevEntry));
                errorMalformedFmt(func, "can't redefine symbol %s", sym);
            }
        }

        private static void defined(String func, Object sym, ConsCell env) {
            if (sym == null) return; // nil is always defined
            final ConsCell symentry = fastassq(sym, env);
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
        { "array-dimension-limit", "arrayDimensionLimit" },
        { "most-positive-fixnum", "mostPositiveFixnum"}, { "most-negative-fixnum", "mostNegativeFixnum"},
        { "internal-time-units-per-second", "itups" },
        { "*command-line-argument-list*", "commandlineArgumentList" },
        { "*features*", "features" },
        };
        private static final String[] primitives = {
        "car", "cdr", "cons", "rplaca", "rplacd",
        /*"apply",*/ "eval", "eq", "eql", "null", "read", "write", "writeln", "lnwrite",
        "atom", "consp", "functionp", "listp", "symbolp", "numberp", "stringp", "characterp", "integerp", "floatp", "vectorp",
        "assoc", "assq", "list", "vector", "seqref", "seqset", "svref", "svset", "svlength", "slength", "sref", "sset", "sbvref", "sbvset", "sbvlength",
        "append", "values",
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
        {"adjustable-array-p", "adjustableArrayP"}, {"vector-push-extend", "vectorPushExtend"},
        {"vector->list", "vectorToList"}, {"simple-vector->list", "simpleVectorToList"}, {"list->simple-vector", "listToSimpleVector"},
        {"simple-bit-vector->list", "simpleBitVectorToList"}, {"list->simple-bit-vector", "listToSimpleBitVector"},
        {"vector-length", "vectorLength"}, {"vector-copy", "vectorCopy"}, {"vector-fill", "vectorFill"}, 
        {"simple-vector-p", "svectorp"}, {"simple-string-p", "sstringp"},
        {"bit-vector-p", "bitvectorp"}, {"bv=", "bvEq"}, {"simple-bit-vector-p", "sbitvectorp"}, {"sbv=", "sbvEq"}, {"make-array", "makeArray"},
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
            globalDecl = new HashSet<>();
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
            globalDecl = null;

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

                if (isOperator(op, WellknownSymbol.sDefine)) {
                    globalEnv = defineToJava(ret, ccForm, globalEnv);
                    intp.eval(ccForm, null);
                }

                else if (isOperator(op, WellknownSymbol.sDefun)) {
                    globalEnv = defunToJava(ret, ccForm, globalEnv);
                    intp.eval(ccForm, null);
                }

                else if (isOperator(op, WellknownSymbol.sDefmacro)) {
                    LambdaJ.symbolOrMalformed("defmacro", cadr(ccForm));
                    intp.eval(ccForm, null);
                    bodyForms.add(ccForm);
                    return globalEnv;
                }

                else if (isOperator(op, WellknownSymbol.sProgn)) {
                    // toplevel progn will be replaced by the forms it contains
                    final Object body = cdr(ccForm);
                    if (consp(body)) {
                        for (Object prognForm: (ConsCell)body) {
                            globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, prognForm);
                        }
                        return globalEnv;
                    }
                }

                final Closure macroClosure; // todo sollte eigentlich nicht mehr vorkommen ?!?
                if (op != null && symbolp(op) && null != (macroClosure = ((LambdaJSymbol)op).macro)) {
                    final Object expansion = intp.evalMacro(op, macroClosure, (ConsCell)cdr(ccForm));
                    globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, expansion);
                }

                else if (isOperator(op, WellknownSymbol.sLoad)) {
                    final ConsCell ccArgs = listOrMalformed("load", cdr(ccForm));
                    oneArg("load", ccArgs);
                    if (ccForm instanceof SExpConsCell) { final SExpConsCell sExpConsCell = (SExpConsCell)ccForm; intp.currentSource = sExpConsCell.path; } // todo unschoener hack 
                    globalEnv = loadFile("load", ret, car(ccArgs), null, globalEnv, -1, false, bodyForms, globals);
                }

                else if (isOperator(op, WellknownSymbol.sRequire)) {
                    final ConsCell ccArgs = listOrMalformed("require", cdr(ccForm));
                    varargs1_2("require", ccArgs);
                    if (!stringp(car(ccArgs))) errorMalformed("require", "a string argument", ccArgs);
                    final Object modName = car(ccArgs);
                    if (!intp.modules.contains(modName)) {
                        Object modFilePath = cadr(ccArgs);
                        if (modFilePath == null) modFilePath = modName;
                        if (ccForm instanceof SExpConsCell) { final SExpConsCell sExpConsCell = (SExpConsCell)ccForm; intp.currentSource = sExpConsCell.path; } // todo unschoener hack 
                        globalEnv = loadFile("require", ret, modFilePath, null, globalEnv, -1, false, bodyForms, globals);
                        if (!intp.modules.contains(modName)) errorMalformedFmt("require", "require'd file '%s' does not provide '%s'", modFilePath, modName);
                    }
                }

                else if (isOperator(op, WellknownSymbol.sProvide)) {
                    final ConsCell ccArgs = listOrMalformed("provide", cdr(ccForm));
                    oneArg("provide", ccArgs);
                    if (!stringp(car(ccArgs))) errorMalformed("provide", "a string argument", ccArgs);
                    final Object modName = car(ccArgs);
                    intp.modules.add(modName);
                }

                else if (isOperator(op, WellknownSymbol.sDeclaim)) {
                    intp.evalDeclaim(1, (ConsCell)cdr(ccForm)); // todo kann form eine dotted list sein und der cast schiefgehen?
                    bodyForms.add(ccForm);
                }

                else bodyForms.add(ccForm);

                if (isOperator(op, WellknownSymbol.sDefine) || isOperator(op, WellknownSymbol.sDefun))
                    globals.append("        case \"").append(cadr(ccForm)).append("\": return ").append(javasym(cadr(ccForm), globalEnv)).append(";\n");

            } else bodyForms.add(form);

            return globalEnv;
        }


        /** Emit a member for {@code symbol} and a function that assigns {@code form} to {@code symbol}.
         *  @param form a list (define symbol form) */
        private ConsCell defineToJava(WrappingWriter sb, ConsCell form, ConsCell env) {
            varargs1_2("toplevel define", listOrMalformed("toplevel define", cdr(form)));
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed("define", cadr(form));
            notDefined("define", symbol, env);
            globalDecl.add(symbol);

            final String javasym = mangle(symbol.toString(), 0);
            env = extenvIntern(symbol, javasym + ".get()", env); // ggf. die methode define_javasym OHNE javasym im environment generieren, d.h. extenvIntern erst am ende dieser methode

            sb.append("    // ").append(form.lineInfo()).append("(define ").append(symbol).append(" ...)\n"
                      + "    public CompilerGlobal ").append(javasym).append(" = UNASSIGNED_GLOBAL;\n");

            sb.append("    public Object define_").append(javasym).append("() {\n"
                      + "        loc = \"");  stringToJava(sb, form.lineInfo(), -1);  stringToJava(sb, printSEx(form), 40);  sb.append("\";\n"
                      + "        if (").append(javasym).append(" != UNASSIGNED_GLOBAL) rterror(new LambdaJError(\"duplicate define\"));\n"
                      + "        try { final Object value = "); emitForm(sb, caddr(form), env, env, 0, false); sb.append(";\n"
                      + "        ").append(javasym).append(" = new CompilerGlobal(value); }\n"
                      + "        catch (Exception e) { rterror(e); }\n"
                      + "        return intern(\"").append(symbol).append("\");\n"
                      + "    }\n\n");
            return env;
        }

        /** @param form a list (defun symbol ((symbol...) forms...)) */
        private ConsCell defunToJava(WrappingWriter sb, ConsCell form, ConsCell env) {
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed("defun", cadr(form));
            final Object params = caddr(form);
            final Object body = cdddr(form);
            notDefined("defun", symbol, env);
            globalDecl.add(symbol);

            final String javasym = mangle(symbol.toString(), 0);
            final ConsCell localEnv = extenvIntern(symbol, javasym, env);

            sb.append("    // ").append(form.lineInfo()).append("(defun ").append(symbol).append(' '); printSEx(sb::append, params); sb.append(" forms...)\n"
                      + "    private CompilerGlobal ").append(javasym).append(" = UNASSIGNED_GLOBAL;\n");

            sb.append("    public LambdaJSymbol defun_").append(javasym).append("() {\n"
                      + "        loc = \"");  stringToJava(sb, form.lineInfo(), -1);  stringToJava(sb, printSEx(form), 40);  sb.append("\";\n"
                      + "        if (").append(javasym).append(" != UNASSIGNED_GLOBAL) rterror(new LambdaJError(\"duplicate defun\"));\n"
                      + "        final MurmelFunction func = new MurmelFunction() {\n" 
                      + "        private final MurmelFunction " + javasym + " = this;\n"
                      + "        public Object apply(Object... args0) {\n");
            final ConsCell extenv = params("defun", sb, params, localEnv, 0, symbol.toString(), true);
            emitForms(sb, (ConsCell)body, extenv, localEnv, 0, false);
            sb.append("        }};\n"
                      + "        ").append(javasym).append(" = new CompilerGlobal(func);\n"
                      + "        return intern(\"").append(symbol).append("\");\n"
                      + "    }\n\n");

            return extenvIntern(symbol, javasym + ".get()", env);
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
                    if (isOperator(op, WellknownSymbol.sQuote)) { emitQuotedForm(sb, car(ccArguments), true); return; }

                    ///     - if
                    if (isOperator(op, WellknownSymbol.sIf)) {
                        varargsMinMax("if", ccArguments, 2, 3);
                        if (consp(car(ccArguments)) && caar(ccArguments) == intp.intern("null")) {
                            // optimize "(if (null ...) trueform falseform)" to "(if ... falseform trueform)"
                            final ConsCell transformed = ListBuilder.list(op, cadar(ccArguments), caddr(ccArguments), cadr(ccArguments));
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
                    if (isOperator(op, WellknownSymbol.sCond)) {
                        emitCond(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    /// eval - (catch tagform forms...) -> object
                    if (isOperator(op, WellknownSymbol.sCatch)) {
                        emitCatch(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    /// eval - (throw tagform resultform) -> |
                    if (isOperator(op, WellknownSymbol.sThrow)) {
                        emitThrow(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - lambda
                    if (isOperator(op, WellknownSymbol.sLambda)) {
                        emitLambda(sb, ccArguments, env, topEnv, rsfx, true);
                        return;
                    }

                    ///     - setq
                    if (isOperator(op, WellknownSymbol.sSetQ)) {
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

                    if (isOperator(op, WellknownSymbol.sDefine)) {
                        if (rsfx != 1) errorNotImplemented("define as non-toplevel form is not yet implemented");
                        defined("define", car(ccArguments), env);
                        final String javasym = mangle(car(ccArguments).toString(), 0);
                        sb.append("define_").append(javasym).append("()");
                        return;
                    }

                    if (isOperator(op, WellknownSymbol.sDefun)) {
                        if (rsfx != 1) errorNotImplemented("defun as non-toplevel form is not yet implemented");
                        defined("defun", car(ccArguments), env);
                        final String javasym = mangle(car(ccArguments).toString(), 0);
                        sb.append("defun_").append(javasym).append("()");
                        return;
                    }

                    if (isOperator(op, WellknownSymbol.sDefmacro)) {
                        if (rsfx != 1) errorNotImplemented("defmacro as non-toplevel form is not yet implemented");
                        intp.expandForm(form); // this will process the macro definition as a side effect
                        sb.append("intern(\"").append(car(ccArguments)).append("\")");
                        return;
                    }

                    ///     - progn
                    if (isOperator(op, WellknownSymbol.sProgn)) {
                        emitProgn(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - unwind-protect
                    if (isOperator(op, WellknownSymbol.sUnwindProtect)) {
                        emitUnwindProtect(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - labels: (labels ((symbol (params...) forms...)...) forms...) -> object
                    // note how labels is similar to let: let binds values to symbols, labels binds functions to symbols
                    if (isOperator(op, WellknownSymbol.sLabels)) {
                        emitLabels(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let: (let ((sym form)...) forms...) -> object
                    ///     - named let: (let sym ((sym form)...) forms...) -> object
                    if (isOperator(op, WellknownSymbol.sLet)) {
                        if (car(ccArguments) == intp.sDynamic)
                            emitLetLetStarDynamic(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, false, isLast);
                        else
                            emitLet(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let*: (let* ((sym form)...) forms...) -> Object
                    ///     - named let*: (let sym ((sym form)...) forms...) -> Object
                    if (isOperator(op, WellknownSymbol.sLetStar)) {
                        if (car(ccArguments) == intp.sDynamic)
                            emitLetLetStarDynamic(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, true, isLast);
                        else
                            emitLetStarLetrec(sb, ccArguments, env, topEnv, rsfx, false, isLast);
                        return;
                    }

                    ///     - letrec:       (letrec ((sym form)...) forms) -> Object
                    ///     - named letrec: (letrec sym ((sym form)...) forms) -> Object
                    if (isOperator(op, WellknownSymbol.sLetrec)) {
                        emitLetStarLetrec(sb, ccArguments, env, topEnv, rsfx, true, isLast);
                        return;
                    }

                    if (isOperator(op, WellknownSymbol.sMultipleValueCall)) {
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
                    if (isOperator(op, WellknownSymbol.sMultipleValueBind)) {
                        varargsMin("multiple-value-bind", ccArguments, 2);
                        final ConsCell vars = listOrMalformed("multiple-value-bind", car(ccArguments));
                        final boolean varargs = dottedList(vars);
                        int length = listLength(vars);
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

                    if (isOperator(op, WellknownSymbol.sLoad)) {
                        varargs1("load", ccArguments);
                        // todo aenderungen im environment gehen verschuett, d.h. define/defun funktioniert nur bei toplevel load, nicht hier
                        loadFile("load", sb, car(ccArguments), env, topEnv, rsfx-1, isLast, null, null);
                        return;
                    }

                    if (isOperator(op, WellknownSymbol.sRequire)) {
                        // pass1 has replaced all toplevel (require)s with the file contents
                        errorNotImplemented("require as non-toplevel form is not implemented");
                    }

                    if (isOperator(op, WellknownSymbol.sProvide)) {
                        // pass 2 shouldn't see this
                        errorNotImplemented("provide as non-toplevel form is not implemented");
                    }

                    if (isOperator(op, WellknownSymbol.sDeclaim)) {
                        intp.evalDeclaim(rsfx, ccArguments);
                        sb.append("(Object)null");
                        return;
                    }

                    /// * macro expansion todo das kann eigentlich nicht mehr passieren ?!?
                    final Closure macroClosure;
                    if (op != null && symbolp(op) && null != (macroClosure = ((LambdaJSymbol)op).macro)) {
                        final Object expansion = intp.evalMacro(op, macroClosure, ccArguments);
                        emitForm(sb, expansion, env, topEnv, rsfx-1, isLast);
                        return;
                    }

                    /// * special case (hack) for calling macroexpand-1: only quoted forms are supported which can be performed a compile time
                    if (symbolEq(op, "macroexpand-1")) {
                        if (!consp(car(ccArguments)) || !symbolEq(caar(ccArguments), "quote")) errorNotImplemented("general macroexpand-1 is not implemented, only quoted forms are: (macroexpand-1 '...");
                        sb.append("((Supplier<Object>)(() -> {\n"
                                  + "        final Object expansion").append(rsfx).append(" = ");
                        emitQuotedForm(sb, intp.macroexpand1((ConsCell)cdar(ccArguments)), true);
                        final String expanded = cadr(intp.values) == sT ? "rt()._t" : "null";
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
            if (form == null || form == sNil) sb.append("false");
            else if (form == sT) sb.append("true");
            else if (consp(form) && car(form) == intp.intern("null")) {
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
            if (form == null || form == sNil) sb.append("(Object)null");
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
            else if (vectorp(form)) emitVectorLiteral(sb, form);
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
                    if (condExpr == sT) {
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

        private void emitCatch(WrappingWriter sb, ConsCell tagAndForms, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            varargs1("catch", tagAndForms);
            final ConsCell body = cons(intern("lambda"), cons(null, cdr(tagAndForms)));
            final ConsCell args = cons(car(tagAndForms), cons(body, null));
            emitFuncall2(sb, "catch", "doCatch", args, env, topEnv, rsfx);
        }

        private void emitThrow(WrappingWriter sb, ConsCell tagAndResultForm, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            twoArgs("throw", tagAndResultForm);
            emitFuncall2(sb, "throw", "doThrow", tagAndResultForm, env, topEnv, rsfx);
        }

        private void emitUnwindProtect(WrappingWriter sb, Object forms, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (!listp(forms)) errorMalformed("unwind-protect", "a list of forms", forms);
            final ConsCell ccForms = (ConsCell)forms;
            final Object protectedForm = car(ccForms);
            final ConsCell cleanupForms = listOrMalformed("unwind-protect", cdr(ccForms));
            if (isLast) {
                sb.append("tailcallWithCleanup(").append("(MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> { return ");
                emitForm(sb, cons(sProgn, cons(protectedForm, null)), env, topEnv, rsfx, false);
                sb.append("; },\n");
                sb.append("        (MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n");
                emitForms(sb, cleanupForms, env, topEnv, rsfx, false);
                sb.append("        },\n");
                sb.append("        (Object[])null)");
            }
            else {
                sb.append("funcall(").append("(MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n        try { return ");
                emitForm(sb, cons(sProgn, cons(protectedForm, null)), env, topEnv, rsfx, true);
                sb.append("; }\n");
                sb.append("        finally {\n");
                final String tmp = "tmp" + rsfx;
                sb.append("        Object ").append(tmp).append(";\n");
                for (Object cleanup: cleanupForms) {
                    sb.append("        ").append(tmp).append(" = ");
                    emitForm(sb, cleanup, env, topEnv, rsfx, false);
                    sb.append(";\n");
                }
                sb.append("        } },\n");
                sb.append("        (Object[])null)");
            }
        }

        private String emitSetq(WrappingWriter sb, Object pairs, ConsCell env, ConsCell topEnv, int rsfx) {
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed("setq", car(pairs));
            final String javaName = javasym(symbol, env);

            if (cdr(pairs) == null) errorMalformed("setq", "odd number of arguments");
            final Object valueForm = cadr(pairs);

            notAPrimitive("setq", symbol, javaName);
            if (fastassq(symbol, env) == fastassq(symbol, topEnv)) {
                final String symName = mangle(symbol.toString(), 0);
                sb.append(symName).append(".set("); emitForm(sb, valueForm, env, topEnv, rsfx, false); sb.append(')');
            } else {
                sb.append(javaName).append(" = ");  emitForm(sb, valueForm, env, topEnv, rsfx, false);
            }
            return javaName;
        }

        /** args = (formsym (sym...) form...) */
        private void emitLabel(String func, WrappingWriter sb, final Object symbolParamsAndForms, ConsCell env, ConsCell topEnv, int rsfx) {
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed(func, car(symbolParamsAndForms));
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
                emitProgn(sb, cdr(args), env, topEnv, rsfx, isLast);
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
                    emitForm(sb, cadr(binding), env, topEnv, rsfx, false);
                }
            } else sb.append(", NOARGS");
            sb.append(')');
        }

        /** let* and letrec
         *  args = ([name] ((symbol form)...) forms...) */
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
            }
            else {
                op = letrec ? "letrec" : "let*";
            }

            if (loopLabel != null) {
                env = extenv(op, loopLabel, rsfx, env);
                sb.append("        private final Object ").append(javasym(loopLabel, env)).append(" = this;\n");
            }
            sb.append("        @Override public Object apply(Object... args").append(rsfx).append(") {\n");
            if (!listp(bindings)) errorMalformed(op, "a list of bindings", bindings);
            final ConsCell ccBindings = (ConsCell)bindings;
            final int argCount = listLength(ccBindings);
            if (argCount != 0) {
                sb.append("        if (args").append(rsfx).append("[0] == UNASSIGNED_LOCAL) {\n");

                // letrec: ALL let-bindings are in the environment during binding of the initial values todo value should be undefined
                int current = 0;
                if (letrec) for (Object binding: ccBindings) {
                    final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(op, car(binding));
                    final String symName = "args" + rsfx + '[' + current++ + ']';
                    env = extenvIntern(sym, symName, env);
                }

                // initial assignments. let*: after the assignment add the let-symbol to the environment so that subsequent bindings will see it
                current = 0;
                for (Object binding: ccBindings) {
                    final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(op, car(binding));
                    final Object form = cadr(binding);
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
                        final ConsCell maybeGlobal = fastassq(sym, topEnv);
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
                        final ConsCell maybeGlobal = fastassq(sym, topEnv);
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
                if (check) sb.append("        argCheckVarargs(\"").append(expr).append("\", ").append(listLength((ConsCell)paramList)).append(", args").append(rsfx).append(".length);\n");
            }
            else if (check) sb.append("        argCheck(\"").append(expr).append("\", ").append(listLength((ConsCell)paramList)).append(", args").append(rsfx).append(".length);\n");

            final HashSet<Object> seen = new HashSet<>();
            int n = 0;
            for (Object params = paramList; params != null; ) {
                if (consp(params)) {
                    final LambdaJSymbol param = LambdaJ.symbolOrMalformed(func, car(params));
                    if (!seen.add(param)) errorMalformedFmt(func, "duplicate symbol %s", param);
                    env = extenvIntern(param, "args" + rsfx + "[" + n++ + "]", env);
                }

                else if (symbolp(params)) {
                    LambdaJ.notReserved(func, (LambdaJSymbol)params);
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

        private ConsCell loadFile(String func, WrappingWriter sb, Object argument, ConsCell _env, ConsCell topEnv, int rsfx, boolean isLast, List<Object> bodyForms, StringBuilder globals) {
            final boolean pass1 = !passTwo;
            final LambdaJ intp = this.intp;
            final Path prev = intp.currentSource;
            final Path p = intp.findFile(func, argument);
            intp.currentSource = p;
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
            finally {
                intp.currentSource = prev;
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
            if (op == null) return false;

            final LambdaJ intp = this.intp;
            final LambdaJSymbol sApply = intp.intern("apply");

            if (op == sApply) {
                twoArgs("apply", args);
                final Object applyOp = car(args);
                final Object applyArg = cadr(args);

                if (applyOp == intp.intern("list")) { sb.append("requireList("); emitForm(sb, applyArg, env, topEnv, rsfx, false); sb.append(")"); return true; }

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

            final WellknownSymbol prim = op.wellknownSymbol;

            if (prim == WellknownSymbol.sAdd) { emitAddDbl(sb, "+", 0.0, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sMul) { emitAddDbl(sb, "*", 1.0, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sSub) { emitSubDbl(sb, "-", 0.0, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sDiv) { emitSubDbl(sb, "/", 1.0, args, env, topEnv, rsfx); return true; }

            if (prim == WellknownSymbol.sMod) { emitFuncall2Numbers(sb, "mod", "cl_mod", args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sRem) {
                twoArgs("rem", args);
                sb.append("(");
                emitFormAsDouble(sb, "rem", car(args), env, topEnv, rsfx);  sb.append(" % ");  emitFormAsDouble(sb, "rem", cadr(args), env, topEnv, rsfx);
                sb.append(")");
                return true;
            }

            if (prim == WellknownSymbol.sRound)     { emitDivision(sb, args, env, topEnv, rsfx, "round",     "cl_round",    true);  return true; }
            if (prim == WellknownSymbol.sFloor)     { emitDivision(sb, args, env, topEnv, rsfx, "floor",     "Math.floor",  true);  return true; }
            if (prim == WellknownSymbol.sCeiling)   { emitDivision(sb, args, env, topEnv, rsfx, "ceiling",   "Math.ceil",   true);  return true; }
            if (prim == WellknownSymbol.sTruncate)  { emitDivision(sb, args, env, topEnv, rsfx, "truncate",  "cl_truncate", true);  return true; }

            if (prim == WellknownSymbol.sFRound)    { emitDivision(sb, args, env, topEnv, rsfx, "fround",    "cl_round",    false); return true; }
            if (prim == WellknownSymbol.sFFloor)    { emitDivision(sb, args, env, topEnv, rsfx, "ffloor",    "Math.floor",  false); return true; }
            if (prim == WellknownSymbol.sFCeiling)  { emitDivision(sb, args, env, topEnv, rsfx, "fceiling",  "Math.ceil",   false); return true; }
            if (prim == WellknownSymbol.sFTruncate) { emitDivision(sb, args, env, topEnv, rsfx, "ftruncate", "cl_truncate", false); return true; }

            if (prim == WellknownSymbol.sNeq) { if (emitBinOp(sb, "==", args, env, topEnv, rsfx)) return true;
                                                emitFuncallVarargs(sb, "=",  "numbereq", 1, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sNe)  { if (emitBinOp(sb, "!=", args, env, topEnv, rsfx)) return true;
                                                emitFuncallVarargs(sb, "/=", "ne",       1, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sLt)  { if (emitBinOp(sb, "<", args, env, topEnv, rsfx)) return true;
                                                emitFuncallVarargs(sb, "<",  "lt",       1, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sLe)  { if (emitBinOp(sb, "<=", args, env, topEnv, rsfx)) return true;
                                                emitFuncallVarargs(sb, "<=", "le",       1, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sGe)  { if (emitBinOp(sb, ">=", args, env, topEnv, rsfx)) return true;
                                                emitFuncallVarargs(sb, ">=", "ge",       1, args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sGt)  { if (emitBinOp(sb, ">", args, env, topEnv, rsfx)) return true;
                                                emitFuncallVarargs(sb, ">",  "gt",       1, args, env, topEnv, rsfx); return true; }

            if (prim == WellknownSymbol.sCar)        { emitFuncall1(sb, "car",    "car",    args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sCdr)        { emitFuncall1(sb, "cdr",    "cdr",    args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sCons)       { emitFuncall2(sb, "cons",   "cons",   args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sRplaca)     { emitFuncall2(sb, "rplaca", "rplaca", args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sRplacd)     { emitFuncall2(sb, "rplacd", "rplacd", args, env, topEnv, rsfx); return true; }

            if (prim == WellknownSymbol.sSvRef)      { emitFuncall2(sb, "svref", "svref", args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sSvSet)      { emitFuncall3(sb, "svset", "svset", args, env, topEnv, rsfx); return true; }

            if (prim == WellknownSymbol.sSBvRef)     { emitFuncall2(sb, "sbvref", "sbvref", args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sSBvSet)     { emitFuncall3(sb, "sbvset", "sbvset", args, env, topEnv, rsfx); return true; }

            if (prim == WellknownSymbol.sEq)   { twoArgs("eq", args);  emitEq(sb, car(args), cadr(args), env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sNull) { oneArg("null", args); emitEq(sb, car(args), null, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sEql)  { emitFuncall2(sb, "eql", "eql", args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sInc)  { emitFuncall1(sb, "1+", "inc1", args, env, topEnv, rsfx); return true; }
            if (prim == WellknownSymbol.sDec)  { emitFuncall1(sb, "1-", "dec1", args, env, topEnv, rsfx); return true; }

            if (prim == WellknownSymbol.sAppend) {
                if (args == null) { // no args
                    sb.append("(Object)null");  return true;
                }
                if (cdr(args) == null) { emitForm(sb, car(args), env, topEnv, rsfx, false); return true; }
                emitFuncallVarargs(sb, "append", "_append", 0, args, env, topEnv, rsfx); return true;
            }

            if (prim == WellknownSymbol.sList) {
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

            if (prim == WellknownSymbol.sListStar) {
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

            for (String primitive: primitives)          if (symbolEq(op, primitive))    { emitCallPrimitive(sb, "_" + primitive, args, env, topEnv, rsfx, null);  return true; }
            for (String[] primitive: aliasedPrimitives) if (symbolEq(op, primitive[0])) { emitCallPrimitive(sb, primitive[1], args, env, topEnv, rsfx, null);  return true; }

            return false;
        }

        private void opencodeApplyHelper(WrappingWriter sb, String func, Object args, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append(func).append("(toArray(");
            emitForm(sb, args, env, topEnv, rsfx, false);
            sb.append("))");
        }

        /** 2 args: divide 2 numbers and apply {@code javaOp} to the result,
         *  1 arg: apply {@code javaOp} to the number,
         *  in both cases if {@code asLong == true} then the result is converted to a fixnum
         */
        private void emitDivision(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, String murmel, String javaOp, boolean asLong) {
            varargs1_2(murmel, args);
            checkNonNumber(murmel, car(args));
            if (asLong) sb.append("toFixnum(");
            sb.append(javaOp).append("(");
            if (cdr(args) == null) {
                emitFormAsDouble(sb, murmel, car(args), env, topEnv, rsfx);
            }
            else {
                checkNonNumber(murmel, cadr(args));
                emitFormAsDouble(sb, murmel, car(args), env, topEnv, rsfx);
                sb.append(" / ");
                emitFormAsDouble(sb, murmel, cadr(args), env, topEnv, rsfx);
            }
            sb.append(")");
            if (asLong) sb.append(')');
        }

        /** emit "==" operator */
        private void emitEq(WrappingWriter sb, Object lhs, Object rhs, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append("(((Object)(");
            emitForm(sb, lhs, env, topEnv, rsfx, false);
            sb.append(") == (Object)(");
            if (rhs == null) sb.append("null"); else emitForm(sb, rhs, env, topEnv, rsfx, false);
            sb.append(")) ? _t : null)");
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

        private void emitFuncall3(WrappingWriter sb, String murmel, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            threeArgs(murmel, args);
            emitCallPrimitive(sb, func, args, env, topEnv, rsfx, null);
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
            checkNonNumber(func, form);
            if (form instanceof Long) sb.append(form.toString()).append(".0");
            else if (form instanceof Double) sb.append(form.toString());
            else { sb.append("toDouble("); emitForm(sb, form, env, topEnv, rsfx, false); sb.append(')'); }
        }

        /** barf if form cannot eval to a number */
        private void checkNonNumber(String func, Object form) {
            if (form == null || form instanceof Character || vectorp(form) || (consp(form) && symbolEq(car(form), "quote"))) errorNotANumber(func, form);
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
            final Executable m;
            final int startArg;
            final boolean voidMethod;
            try {
                if ("new".equals(strMethod)) { m = clazz.getDeclaredConstructor(params);  startArg = 0; voidMethod = false; }
                else                         { m = clazz.getMethod((String)strMethod, params);  startArg = Modifier.isStatic(m.getModifiers()) ? 0 : 1; voidMethod = ((Method)m).getReturnType() == void.class; }
            }
            catch (Exception e) { throw new LambdaJError(true, "jmethod: exception finding method: %s", e.getMessage()); }

            final int paramCount = paramTypes.size() + startArg;
            if (emitCall) {
                // emit new clazz(args...)/ clazz.method(args...)/ firstarg.method(restargs...)
                final int argCount = listLength(ccArguments);
                if (m.isVarArgs()) { if (argCount < paramCount-1) errorVarargsCount((String)strMethod, paramCount-1, argCount); }
                else               { if (argCount != paramCount)  errorArgCount((String) strMethod, paramCount, paramCount, argCount, null); }

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
                    String conv = null;
                    for (Object arg : ccArguments) {
                        if (first) first = false;
                        else sb.append("\n        , ");
                        if (!m.isVarArgs() || i - startArg < paramTypeNames.size()) conv = (String) classByName.get(paramTypeNames.get(i-startArg))[1];
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
                if (m.isVarArgs()) { sb.append("argCheckVarargs(loc, ").append(paramCount-1).append(", args.length);  ");}
                else               { sb.append("argCheck(loc, ").append(paramCount).append(", args.length);  "); }
                if (!voidMethod) sb.append("return ");

                if ("new".equalsIgnoreCase((String) strMethod)) sb.append("new ").append(strClazz);
                else if (Modifier.isStatic(m.getModifiers())) sb.append(strClazz).append('.').append(strMethod);
                else sb.append("((").append(strClazz).append(')').append("args[0]").append(").").append(strMethod);

                sb.append("(");
                if (params != null) {
                    boolean first = true;
                    if (m.isVarArgs()) {
                        Object[] desc = null;
                        for (int i = startArg; i < params.length + startArg - 1; i++) {
                            if (first) first = false;
                            else sb.append("\n        , ");
                            desc = classByName.get(paramTypeNames.get(i - startArg));
                            if (desc == null) sb.append("args[").append(i).append(']');
                            else sb.append(desc[1]).append("(args[").append(i).append("])");
                        }

                        // handle last parameter which is vararg: pass an array of the appropriate type with the remaining args
                        desc = classByName.get(paramTypeNames.get(params.length-1));
                        final int varargPos = params.length + startArg - 1;
                        final String conv = "(java.util.function.UnaryOperator<Object>)(MurmelJavaProgram::" + desc[1] + ")";
                        sb.append("\n        , toVarargs(args, " + varargPos + ", " + conv + ", new " + ((Class<?>)desc[0]).getComponentType().getCanonicalName() + "[args.length - " + varargPos + "])");
                    }
                    else {
                        String conv = null;
                        for (int i = startArg; i < params.length + startArg; i++) {
                            if (first) first = false;
                            else sb.append("\n        , ");
                            conv = (String)classByName.get(paramTypeNames.get(i - startArg))[1];
                            if (conv == null) sb.append("args[").append(i).append(']');
                            else sb.append(conv).append("(args[").append(i).append("])");
                        }
                    }
                }
                sb.append("); ");
                if (voidMethod) sb.append("return null; ");
                sb.append("}))");
            }
            return true;
        }


        private void emitVectorLiteral(WrappingWriter sb, Object form) {
            if (form instanceof String) { emitStringLiteral(sb, (String)form); }
            else if (form instanceof Object[]) { emitSimpleVectorLiteral(sb, (Object[])form); }
            else if (form instanceof boolean[]) { emitSimpleBitVectorLiteral(sb, (boolean[])form); }
            else errorInternal("emitVectorLiteral: vector type %s is not implemented", form.toString());
        }

        private static void emitStringLiteral(WrappingWriter sb, String form) { sb.append('"'); stringToJava(sb, form, -1); sb.append('"'); }

        private void emitSimpleVectorLiteral(WrappingWriter sb, Object[] form) {
            final StringWriter b = new StringWriter();
            final WrappingWriter qsb = new WrappingWriter(b);

            qsb.append("new Object[] {");
            boolean first = true;
            for (Object elem: form) {
                if (first) first = false;
                else qsb.append(',');
                emitQuotedForm(qsb, elem, true);
            }
            qsb.append("}");

            emitReference(sb, b.toString());
        }

        private void emitSimpleBitVectorLiteral(WrappingWriter sb, boolean[] form) {
            final StringWriter b = new StringWriter();

            b.append("new boolean[] {");
            boolean first = true;
            for (boolean elem: form) {
                if (first) first = false;
                else b.append(',');
                b.append(String.valueOf(elem));
            }
            b.append("}");

            emitReference(sb, b.toString());
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
            if (form == null || form == sNil) sb.append("(Object)null");

            else if (symbolp(form)) {
                if (form == sT) sb.append("_t");
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

                // deduplicate quoted lists (list constants), modifying list constants will lead to unexpected behaviour
                if (pool) emitReference(sb, init);
                else sb.append(init);
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

        private JFRHelper() {}

        @jdk.jfr.Relational
        @Target({ ElementType.FIELD })
        @Retention(RetentionPolicy.RUNTIME)
        @interface ParentId {
        }

        @jdk.jfr.Category({"JMurmel", "User Events"})
        @jdk.jfr.StackTrace(false)
        public abstract static class BaseEvent extends jdk.jfr.Event {
            private static final AtomicInteger counter = new AtomicInteger(0);

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
final class JavaCompilerHelper {
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

final class JavaSourceFromString extends SimpleJavaFileObject {
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

final class MurmelClassLoader extends ClassLoader {
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

final class EolUtil {
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
        if (index + 1 < len && inputValue.charAt(index + 1) == '\n') {
            // this means we encountered a \r\n  ... move index forward one more character
            index++;
        }
        stringBuilder.append('\n');
        index++;
        while (index < len) {
            final char c = inputValue.charAt(index);
            if (c == '\r') {
                if (index + 1 < len && inputValue.charAt(index + 1) == '\n') {
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

/** A wrapping {@link LambdaJ.WriteConsumer} that translates '\n' to the given line separator {@code eol}. */
final class UnixToAnyEol implements LambdaJ.WriteConsumer {
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
final class WrappingWriter extends Writer {
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
final class TurtleFrame {
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
