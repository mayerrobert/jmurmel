/* LambdaJ is Copyright (C) 2020-2024 Robert Mayer.

This work is licensed under the terms of the MIT license.
For a copy, see https://opensource.org/licenses/MIT. */

package io.github.jmurmel;

import io.github.jmurmel.LambdaJ.NotNull;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.io.*;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
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
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.tools.ToolProvider;
import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;

import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.SimpleJavaFileObject;

import static io.github.jmurmel.LambdaJ.Names.*;
import static io.github.jmurmel.LambdaJ.Chk.*;
import static io.github.jmurmel.LambdaJ.Subr.*;

import static java.lang.annotation.ElementType.*;

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

    public static final String LANGUAGE_VERSION = "1.4";
    public static final String ENGINE_NAME = "JMurmel: Java based implementation of Murmel";
    public static final String ENGINE_VERSION;
    public static final String ENGINE_VERSION_NUM;

    static {
        String versionInfo, versionNum;
        final ClassLoader cl = LambdaJ.class.getClassLoader();
        final URL url = cl.getResource("META-INF/jmurmelversioninfo.properties");
        if (url == null) versionInfo = versionNum = "unknown";
        else {
            try (InputStream is = url.openStream()) {
                final Properties manifest = new Properties();
                manifest.load(is);
                versionInfo = manifest.getProperty("Engine-Version", "unknown");
                versionNum = versionInfo.substring("Version ".length(), versionInfo.indexOf('/'));
            } catch (Exception e) {
                versionInfo = versionNum = "error";
            }
        }
        ENGINE_VERSION = versionInfo;
        ENGINE_VERSION_NUM = versionNum;
    }

    /** largest positive long that can be represented as a double w/o any loss */
    public static final long MOST_POSITIVE_FIXNUM_VAL = (1L << 53) - 1;

    /** largest negative long that can be represented as a double w/o any loss */
    public static final long MOST_NEGATIVE_FIXNUM_VAL = -(1L << 53);

    /** Copied from java.util.ArrayList which says:
     * The maximum size of array to allocate.
     * Some VMs reserve some header words in an array.
     * Attempts to allocate larger arrays may result in
     * OutOfMemoryError: Requested array size exceeds VM limit
     */
    public static final int ARRAY_DIMENSION_LIMIT_VAL = Integer.MAX_VALUE - 8;

    /** Max length of symbols*/
    public static final int SYMBOL_MAX = 30;

    /** Max length of string literals */
    public static final int TOKEN_MAX = 2000;

    interface ConsIterator extends Iterator<Object> {
        boolean wasDotted();
    }

    /** Main building block for Lisp-lists */
    public abstract static class ConsCell implements Iterable<Object>, Serializable, Writeable {
        private static final long serialVersionUID = 1L;

        @Override public void printSEx(WriteConsumer out, boolean escapeAtoms) { LambdaJ.printSEx(out, this, escapeAtoms); }

        public static @NotNull ConsCell cons(Object car, Object cdr) { return new ListConsCell(car, cdr); }

        public @NotNull ConsCell copy() { throw new UnsupportedOperationException("copy not supported on " + getClass().getSimpleName()); }
        public @NotNull Object shallowCopyCdr() { throw new UnsupportedOperationException("shallowCopyCdr not supported on " + getClass().getSimpleName()); }

        public abstract Object car();
        public @NotNull ConsCell rplaca(Object car) { throw new UnsupportedOperationException(RPLACA + " not supported on " + getClass().getSimpleName()); }

        public abstract Object cdr();
        public @NotNull ConsCell rplacd(Object cdr) { throw new UnsupportedOperationException(RPLACD + " not supported on " + getClass().getSimpleName()); }

        public abstract Object elt(long idx);
        public abstract Object eltset(Object newVal, long idx);

        /** return a string with "line x:y..xx:yy: " if {@code form} is an {@link SExpConsCell} that contains line info */
        @NotNull String lineInfo() { return ""; }

        @Override public abstract ConsIterator iterator();

        @Override public boolean equals(Object other) { return other instanceof ConsCell && compare(this, other, CompareMode.EQUAL) == 0; }
        @Override public int hashCode() { return sxhashSigned(100); }
        abstract int sxhashSigned(int rec);

        final int compareToEqual(ConsCell c2) {
            final ConsIterator me = iterator(), other = c2.iterator();
            while (me.hasNext() && other.hasNext()) {
                final int compareCar = compare(me.next(), other.next(), CompareMode.EQUAL);
                if (compareCar != 0) return compareCar;
            }
            if (me.hasNext()) return 1;
            else if (other.hasNext()) return -1;
            else if (me.wasDotted() && !other.wasDotted()) return -1;
            else if (!me.wasDotted() && other.wasDotted()) return 1;
            else return 0;
        }

        /** return a list. See also {@link LambdaJ#list} */
        public static ConsCell list(Object... elems) {
            if (elems == null || elems.length == 0) return null;
            final ConsCell ret = cons(elems[0], null);
            if (elems.length == 1) return ret;
            ConsCell insertPos = ret;
            final int n = elems.length;
            for (int i = 1; i < n; i++) {
                final ConsCell cons = cons(elems[i], null);
                insertPos.rplacd(cons);
                insertPos = cons;
            }
            return ret;
        }

        public static Object listStar(Object... elems) {
            assert elems != null && elems.length != 0;
            if (elems.length == 1) return elems[0];
            if (elems.length == 2) return cons(elems[0], elems[1]);
            final int n = elems.length - 1;
            ConsCell insertPos;
            final ConsCell ret = insertPos = cons(elems[0], null);
            for (int i = 1; i < n; i++) {
                final ConsCell cons = cons(elems[i], null);
                insertPos.rplacd(cons);
                insertPos = cons;
            }
            insertPos.rplacd(elems[n]);
            return ret;
        }
    }

    /** A murmel symbol name */
    public static final class LambdaJSymbol implements Serializable, Writeable {
        private static final long serialVersionUID = 1L;
        final @NotNull String name;
        final @NotNull WellknownSymbol wellknownSymbol;
        Closure macro;

        public LambdaJSymbol(@NotNull String symbolName) {
            this(symbolName, WellknownSymbol.notInterned);
        }

        public LambdaJSymbol(boolean intern, @NotNull String symbolName) {
            this(symbolName, intern ? WellknownSymbol.interned : WellknownSymbol.notInterned);
        }

        LambdaJSymbol(@NotNull String symbolName, boolean wellknown) {
            this(symbolName, wellknown ? WellknownSymbol.of(symbolName) : WellknownSymbol.notInterned);
        }

        private LambdaJSymbol(@NotNull String symbolName, @NotNull WellknownSymbol ws) {
            name = Objects.requireNonNull(symbolName, "can't use null symbolname");
            wellknownSymbol = ws;
        }

        public boolean specialForm() { return wellknownSymbol.kind == WellknownSymbolKind.SF; }
        public boolean primitive() { return wellknownSymbol.kind == WellknownSymbolKind.PRIM; }

        @Override public void printSEx(WriteConsumer out, boolean escapeAtoms) {
            final String name = this.name;
            if (!escapeAtoms) { out.print(name); return; }

            if (wellknownSymbol == WellknownSymbol.notInterned) out.print("#:");

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
                if (isWhitespace(c)) return true;
                if ('\\' == c) return true;
                if (!(c >= 32 && c <= 126 || Character.isAlphabetic(c))) return true;
            }
            return false;
        }

        @Override public String toString() { return name; }

        private static CharSequence escapeSymbol(LambdaJSymbol s) {
            final String name = s.name;
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
            return ret;
        }
    }

    public interface SymbolTable extends Iterable<LambdaJSymbol> {
        @NotNull LambdaJSymbol intern(@NotNull LambdaJSymbol symbol);
        @Override @NotNull Iterator<LambdaJSymbol> iterator();
        default @NotNull LambdaJSymbol intern(@NotNull String symbolName) { return intern(new LambdaJSymbol(symbolName)); }
    }

    private static class StringReadSupplier implements ReadSupplier {
        private final String s;
        private final int length;
        private int pos;

        private StringReadSupplier(String s) { this.s = s;  length = s.length(); }
        @Override public int read() { return pos >= length ? EOF : s.charAt(pos++); }
    }

    @FunctionalInterface public interface ReadSupplier {
        int read() throws IOException;
        static ReadSupplier of(Path p) throws IOException { return new StringReadSupplier(JavaUtil.readString(p, StandardCharsets.UTF_8)); }
    }
    @FunctionalInterface public interface WriteConsumer { void print(CharSequence s); }
    @FunctionalInterface public interface TraceConsumer { void println(CharSequence msg); }

    @FunctionalInterface public interface ObjectReader {
        Object readObj(Object eof);

        /** if {@code recordPos == true} then it would be desirable to record file/line positions inside the objects */
        default Object readObj(boolean recordPos, Object eof) { return readObj(eof); }
        default void setInput(@NotNull ReadSupplier input, Path filePath) { throw new UnsupportedOperationException("this ObjectReader does not support changing input"); }
        default Path getInput() { return null; }
    }

    public interface ObjectWriter {
        void printObj(Object o, boolean printEscape);
        default void printObj(Object o) { printObj(o, true); }
        default void printString(CharSequence s) { printObj(s, false); }
        void printEol();
    }

    /** if an atom implements this interface then {@link Writeable#printSEx(LambdaJ.WriteConsumer, boolean)} will be used by the Murmel primitive {@code write} */
    @FunctionalInterface public interface Writeable extends Formattable {
        /** will be used by the Murmel primitive {@code write} */
        void printSEx(WriteConsumer out, boolean escapeAtoms);

        @Override
        default void formatTo(Formatter formatter, int flags, int width, int precision) {
            final boolean alternate = (flags & FormattableFlags.ALTERNATE) == FormattableFlags.ALTERNATE;

            StringBuilder sb = new StringBuilder();
            printSEx(sb::append, alternate); // todo Writeable#printSEx() koennte maxwidth unterstuetzen statt erst erzeugen und dann abschneiden, wuerde auch list cycles erledigen
            sb = EolUtil.unixToJavaEol(sb);

            // apply precision
            if (precision != -1 && sb.length() > precision) {
                sb.setLength(precision - 3); sb.append("...");
            }

            final String fmt = (flags & FormattableFlags.UPPERCASE) == FormattableFlags.UPPERCASE ? "%S" : "%s";

            // apply width and justification
            final int len = sb.length();
            if (len >= width) { formatter.format(fmt, sb); return; }

            final boolean leftJustified = (flags & FormattableFlags.LEFT_JUSTIFY) == FormattableFlags.LEFT_JUSTIFY;
            if (leftJustified) {
                for (int i = len; i < width; i++) sb.append(' ');
            }
            else {
                final StringBuilder blanks = new StringBuilder(width);
                for (int i = len; i < width; i++) blanks.append(' ');
                formatter.format(blanks.toString());
            }
            formatter.format(fmt, sb);
        }
    }

    @FunctionalInterface public interface Primitive extends Writeable {
        Object applyPrimitive(ConsCell x);
        default Object applyPrimitiveVarargs(Object... args) { return applyPrimitive(arraySlice(args, 0)); }
        @Override default void printSEx(WriteConsumer out, boolean ignored) { out.print("#<primitive>"); }
    }

    @FunctionalInterface public interface CustomEnvironmentSupplier {
        ConsCell customEnvironment(SymbolTable symtab);
    }

    public static class LambdaJError extends RuntimeException implements Writeable {
        public static final long serialVersionUID = 1L;
        
        private final @NotNull String location;

        public LambdaJError(String msg)                                                    { super(msg, null, false, false); location = ""; }
        public LambdaJError(boolean format, String msg, Object... params)                  { super((format ? fmt(msg, params) : msg), null, false, false); location = getErrorExp(params); }
        public LambdaJError(Throwable cause, boolean format, String msg, Object... params) { this(format ? fmt(msg, params) : msg, merge(getLocation(cause), getErrorExp(params)), getMurmelCause(cause)); }
        public LambdaJError(Throwable cause)                                               { this(cause.getMessage(), getLocation(cause), getMurmelCause(cause)); }
        public LambdaJError(Throwable cause, String msg)                                   { this(msg, getLocation(cause), getMurmelCause(cause)); }
        public LambdaJError(Throwable cause, Object errorForm)                             { this(cause.getMessage(), merge(getLocation(cause), getErrorExp(new Object[] { errorForm })), getMurmelCause(cause)); }
        public LambdaJError(Throwable cause, boolean fromCompiledCode, String errorLoc)    { this(cause.getMessage(), merge(getLocation(cause), getErrorExp(errorLoc)), getMurmelCause(cause)); }
        public LambdaJError(String msg, boolean fromCompiledCode, String errorLoc)         { this(msg, getErrorExp(errorLoc), null); }

        private LambdaJError(String msg, @NotNull String location, Throwable cause) {
            super(msg, cause);
            this.location = location;
        }

        @Override
        public void printSEx(WriteConsumer out, boolean escapeAtoms) {
            if (escapeAtoms) out.print(conditionName() + " - ");
            out.print(getMessage());
            if (escapeAtoms && !location.isEmpty()) {
                out.print(System.lineSeparator());
                out.print(location);
            }
        }

        public @NotNull String conditionName() {
            final Throwable cause = getCause();
            if (cause instanceof LambdaJError) return ((LambdaJError)cause).conditionName();

            if (cause != null) {
                if (cause instanceof ArithmeticException) return "arithmetic-error";
                if (cause instanceof SimpleTypeError) return "simple-type-error";
                if (cause instanceof IndexOutOfBoundsException) return "invalid-index-error";
                if (cause instanceof ClassCastException) return "type-error";
                if (cause instanceof InvalidPathException) return "file-error";
                if (cause instanceof EOFException) return "end-of-file";
                if (cause instanceof ReaderError) return "reader-error";
                if (cause instanceof IOException) return "stream-error";
                if (cause instanceof Exception) return "error";
            }
            return "condition";
        }


        @Override public @NotNull String toString() { return conditionName() + " - " + getMessage(); }

        public @NotNull String getLocation() { return location; }

        private static @NotNull String merge(@NotNull String cause, @NotNull String prev) {
            if (cause.isEmpty() && prev.isEmpty()) return "";
            if (cause.isEmpty()) return prev;
            if (prev.isEmpty()) return cause;
            return cause + System.lineSeparator() + prev;
        }

        private static @NotNull String getErrorExp(Object[] params) {
            final Object exp;
            if (params != null && params.length > 0 && (exp = params[params.length-1]) instanceof ConsCell)
                return "error occurred in " + ((ConsCell) exp).lineInfo() + LambdaJ.printSEx(exp);
            return "";
        }

        private static @NotNull String getErrorExp(String errorLoc) {
            if (errorLoc != null)
                return "error occurred in " + errorLoc;
            return "";
        }

        private static @NotNull String getLocation(Throwable cause) {
            if (cause instanceof LambdaJError) return ((LambdaJError)cause).location;
            return "";
        }

        private static Throwable getMurmelCause(Throwable t) {
            if (t instanceof LambdaJError && t.getCause() != null) return t.getCause();
            return t;
        }
    }

    public static class SimpleError extends LambdaJError    { public SimpleError(String msg, Object... params) { super(true, msg, params); }
                                                              public SimpleError(String msg) { super(msg); }
                                                              @Override public @NotNull String conditionName() { return "simple-error"; } }

    public static class CellError extends LambdaJError      { public CellError(String msg, Object... params) { super(true, msg, params); }
                                                              public CellError(String msg) { super(msg); }
                                                              @Override public @NotNull String conditionName() { return "cell-error"; } }
    public static class UnboundVariable extends CellError   { public UnboundVariable(String msg, Object... params) { super(msg, params); }
                                                              public UnboundVariable(String msg) { super(msg); }
                                                              @Override public @NotNull String conditionName() { return "unbound-variable"; } }
    public static class UndefinedFunction extends CellError { public UndefinedFunction(String msg, Object... params) { super(msg, params); }
                                                              public UndefinedFunction(String msg) { super(msg); }
                                                              @Override public @NotNull String conditionName() { return "undefined-function"; } }

    public static class ControlError extends LambdaJError   { public ControlError(String msg, Object... params) { super(true, msg, params); }
                                                              public ControlError(String msg) { super(msg); } }

    public static class ProgramError extends LambdaJError   { public ProgramError(String msg, Object... params) { super(true, msg, params); }
                                                              public ProgramError(String msg) { super(msg); }
                                                              @Override public @NotNull String conditionName() { return "program-error"; } }

    public static class ParseError extends LambdaJError     { public ParseError(String errorLoc, String msg, Object... args) { super(fmt(msg, args), true, errorLoc); }
                                                              public ParseError(String msg) { super(msg); } 
                                                              @Override public @NotNull String conditionName() { return "parse-error"; } }

    // artithmetic-error... java.lang.ArithmeticException
    // type-error...        java.lang.ClassCastException
    public static class SimpleTypeError extends ClassCastException { public SimpleTypeError(String msg, Object... params) { super(fmt(msg, params)); } }
    public static class InvalidIndexError extends IndexOutOfBoundsException { public InvalidIndexError(String msg, Object... params) { super(fmt(msg, params)); } }
    // file-error...        java.nio.file.InvalidPathException

    // stream-error...      java.io.IOException
    //     end-of-file...   java.io.EOFException
    public static class ReaderError extends IOException     { public ReaderError(String message) { super(message); }
                                                              public ReaderError(String msg, Object... params) { super(fmt(msg, params)); } }

    static String fmt(String msg, Object... params) {
        if (msg == null) return null;
        return String.format(msg, params);
    }

    /** possibly wrap {@code t} in a {@link LambdaJError} and throw, wrap doesn't return */
    static RuntimeException wrap(@NotNull Throwable t) {
        if (t instanceof RuntimeException) throw (RuntimeException)t;
        throw new LambdaJError(t, t.getMessage());
    }

    static RuntimeException wrap(@NotNull Throwable t, @NotNull String errorLoc) {
        if (t instanceof RuntimeException) throw (RuntimeException)t;
        throw new LambdaJError(t, true, errorLoc);
    }

    /** same as {@link #wrap} but returns void so that callsites won't need a bytecode to pop the returnvalue */
    static void wrap0(@NotNull Throwable t) {
        wrap(t);
    }

    static void wrap0(@NotNull Throwable t, String errorLoc) {
        wrap(t, errorLoc);
    }


    /// ## Data types used by interpreter program as well as interpreted programs

    /** for nonlocal returns */
    static final class ReturnException extends LambdaJError {
        final Object tag, result;
        final Object[] values;

        ReturnException(Object tag, Object result, Object[] values) {
            super((String)null);
            this.tag = tag;
            this.result = result;
            this.values = values;
        }

        ReturnException(Object tag, Object result, ConsCell values) {
            this(tag, result, values == NO_VALUES ? null : listToArray(values));
        }

        // this should only ever be used in case of an internal error in LambdaJ
        @Override public String getMessage() { return "#<returnexception tag=" + tag + ", result=" + result + '>'; }

        ConsCell valuesAsList() {
            if (values == null) return NO_VALUES;
            return arraySlice(values, 0);
        }
    }

    public abstract static class AbstractListBuilder<T extends AbstractListBuilder<T>> {
        private Object first;
        private Object last;

        /** add an item at the end of the list */
        @SuppressWarnings("unchecked")
        public @NotNull T append(Object elem) {
            final ConsCell newCell = newCell(elem);
            if (first == null) {
                last = first = newCell;
            }
            else if (last instanceof ConsCell) {
                ((ConsCell) last).rplacd(newCell);
                last = newCell;
            }
            else throw new LambdaJ.SimpleTypeError("can't append another element to dotted list");
            return (T)this;
        }

        @SuppressWarnings("unchecked")
        public @NotNull T appendElements(Object... elems) {
            final int length = elems.length;
            for (int i = 0; i < length; i++) append(elems[i]);
            return (T)this;
        }

        /** add an item at the end of the list to create a dotted list.
         *  Once {@code #appendLast(Object)} has been invoked subsequent invocations
         *  of {@code #appendLast(Object)} and/ or {@link #append(Object)} will result in an error as dotted lists can't be appended to. */
        @SuppressWarnings("unchecked")
        public @NotNull T appendLast(Object lastElem) {
            if (first == null) {
                last = first = lastElem;
            }
            else if (last instanceof ConsCell) {
                ((ConsCell) last).rplacd(lastElem);
                last = lastElem;
            }
            else throw new LambdaJ.SimpleTypeError("can't append another last element to dotted list");
            return (T)this;
        }

        /** return the constructed list so far */
        public Object first() { return first; }

        abstract @NotNull ConsCell newCell(Object car);
    }

    /** Builder class for constructing lists, also used by compiled Murmel */
    public static final class ListBuilder extends AbstractListBuilder<ListBuilder> {
        @Override @NotNull ConsCell newCell(Object car) { return ConsCell.cons(car, null); }
    }

    private final class CountingListBuilder extends AbstractListBuilder<CountingListBuilder> {
        @Override @NotNull ConsCell newCell(Object car) { return LambdaJ.this.cons(car, null); }
    }

    private abstract static class AbstractConsCell extends ConsCell {
        private static class ListConsCellIterator implements ConsIterator {
            private final @NotNull AbstractConsCell coll;
            private ConsIterator delegate;
            private Object cursor;
            private boolean wasDotted;

            private ListConsCellIterator(@NotNull AbstractConsCell coll) { this.coll = coll; cursor = coll; }
            @Override public boolean hasNext() {
                if (delegate != null) return delegate.hasNext();
                return cursor != null;
            }

            @Override public Object next() {
                if (delegate != null) return delegate.next();
                final Object _cursor;
                if ((_cursor = cursor) == null) throw new NoSuchElementException();
                if (_cursor instanceof ArraySlice) {
                    // a ListConsCell based list can contain an ArraySlice as the last cdr
                    // (i.e. a list starts as conses and is continued by an ArraySlice.
                    // An ArraySlice can not be continued by conses.
                    cursor = null;
                    delegate = ((ArraySlice)_cursor).iterator();
                    return delegate.next();
                }
                if (_cursor instanceof AbstractConsCell) {
                    final AbstractConsCell list = (AbstractConsCell)_cursor;
                    if (list.cdr() == coll) cursor = null; // circle detected, stop here
                    else { cursor = list.cdr(); wasDotted = cursor != null; }
                    return list.car();
                }
                cursor = null;
                return _cursor;  // last element of dotted list
            }

            @Override public boolean wasDotted() {
                assert !hasNext() : "wasDotted was called when not at end (hasNext() == true)";
                if (delegate != null) return delegate.wasDotted();
                return wasDotted;
            }
        }

        private static final long serialVersionUID = 1L;

        private Object car, cdr;

        AbstractConsCell(Object car, Object cdr)    { this.car = car; this.cdr = cdr; }
        @Override public @NotNull String toString() { return LambdaJ.printSEx(this, false).toString(); }
        @Override public @NotNull ConsIterator iterator() { return new ListConsCellIterator(this); }

        @Override public @NotNull Object shallowCopyCdr() { if (consp(cdr)) cdr = ((ConsCell)cdr).copy(); return cdr; }

        @Override public Object car() { return car; }
        @Override public @NotNull ConsCell rplaca(Object car) { this.car = car; return this; }

        @Override public Object cdr() { return cdr; }
        @Override public @NotNull ConsCell rplacd(Object cdr) { this.cdr = cdr; return this; }

        @Override public Object elt(long idx) {
            long _idx = 0;
            for (Object o: this) {
                if (_idx == idx) return o;
                _idx++;
            }
            throw errorIndexTooLarge(idx, _idx);
        }

        @Override public Object eltset(Object newValue, long idx) {
            long _idx = 0;
            ConsCell lst = this;
            Object cdr = this;
            for (; consp(cdr); cdr = lst.cdr()) {
                lst = (ConsCell)cdr;
                if (_idx == idx) { lst.rplaca(newValue); return newValue; }
                _idx++;
            }
            if (_idx == idx && cdr != null) { lst.rplacd(newValue); return newValue; }
            throw errorIndexTooLarge(idx, cdr == null ? _idx : _idx + 1);
        }

        void adjustEnd(int endLineNo, int endCharNo) { /* default is: no-op */ }
        void adjustEnd(SExpConsCell cell) { /* default is: no-op */ }

        /** avoid endless loop in case of circular lists or lists with embedded circles */
        @Override final int sxhashSigned(int rec) {
            int ret = 0;
            for (Object lst = this; lst != null && --rec > 0; lst = LambdaJ.cdr(lst)) {
                if (lst instanceof ArraySlice) return ret + ((ArraySlice)lst).sxhashSigned(rec+1);
                if (!(lst instanceof ConsCell)) return ret + 31 * LambdaJ.sxhashSigned(lst);
                final Object nextCar = LambdaJ.car(lst);
                ret += 31 * (nextCar instanceof ConsCell ? ((ConsCell)nextCar).sxhashSigned(rec) : LambdaJ.sxhashSigned(nextCar));
            }
            return ret;
        }
    }

    private static final class ListConsCell extends AbstractConsCell {
        private static final long serialVersionUID = 1L;

        ListConsCell(Object car, Object cdr) { super(car, cdr); }

        @Override public @NotNull ConsCell copy() { return cons(car(), cdr()); }
    }

    private static final class SExpConsCell extends AbstractConsCell {
        private static final long serialVersionUID = 1L;
        private final transient Path path;
        private final int startLineNo, startCharNo;
        private int lineNo, charNo;

        SExpConsCell(Path path, int startLine, int startChar, int line, int charNo, Object car, Object cdr)    {
            super(car, cdr);
            this.path = path; this.startLineNo = startLine; this.startCharNo = startChar; this.lineNo = line; this.charNo = charNo;
        }

        @Override public @NotNull ConsCell copy() { return new SExpConsCell(path, startLineNo, startCharNo, lineNo, charNo, car(), cdr()); }

        @Override void adjustEnd(int lineNo, int charNo) { this.lineNo = lineNo; this.charNo = charNo; }
        @Override void adjustEnd(SExpConsCell cell) { this.lineNo = cell.lineNo; this.charNo = cell.charNo; }
        @Override @NotNull String lineInfo() { return (path == null ? "line " : path.toString() + ':') + startLineNo + ':' + startCharNo + ".." + lineNo + ':' + charNo + ':' + ' '; }

        Path path() { return path; }
    }

    private static class Closure implements Serializable, Writeable {
        private static final long serialVersionUID = 1L;
        final Object params;
        final ConsCell body, closure; // todo es sollten nur macros serialisiert werden. beim serialisieren sollte fuer closure!=topEnv ein fehler geworfen werden, beim einlesen sollte closure=topEnv gesetzt werden

        private Closure(Object params, ConsCell body, ConsCell closure)    { this.params = params; this.body = body; this.closure = closure; }
        Object params() { return params; }
        ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) { return intp.zip("function application", params, args, closure, true); }
        @Override public void printSEx(WriteConsumer out, boolean escapeAtoms) { out.print("#<interpreted closure>"); }

        static @NotNull Closure of(Object params, ConsCell body, ConsCell closure) {
            if (params == null) return new Closure0(body, closure);
            if (symbolp(params)) return new ClosureVararg(params, body, closure);

            final ConsCell ccParams = (ConsCell)params;
            final Object cdrParams = cdr(ccParams);
            if (cdrParams == null) return new Closure1(ccParams, body, closure);
            if (symbolp(cdrParams)) return new Closure1Varargs(ccParams, body, closure);

            final Object cddrParams = cdr((ConsCell)cdrParams);
            if (cddrParams == null) return new Closure2(ccParams, body, closure);
            if (symbolp(cddrParams)) return new Closure2Varargs(ccParams, body, closure);

            if (cdr((ConsCell)cddrParams) == null) return new Closure3(ccParams, body, closure);

            return new Closure(ccParams, body, closure);
        }

        static void tooManyArgs(Object args)        { errorApplicationArgCount("%s: too many arguments. Remaining arguments: %s", "function application", args); }
        static void notEnoughArgs(Object paramList) { errorApplicationArgCount("%s: not enough arguments. Parameters w/o argument: %s", "function application", paramList); }
        static void notEnoughArgsLst(Object param)  { errorApplicationArgCount("%s: not enough arguments. Parameters w/o argument: (%s)", "function application", param); }
    }

    // no arguments
    private static final class Closure0 extends Closure {
        Closure0(ConsCell body, ConsCell closure) { super(null, body, closure); }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) {
            if (args != null) tooManyArgs(args);
            return closure;
        }
    }

    // one argument
    private static final class Closure1 extends Closure {
        Closure1(ConsCell params, ConsCell body, ConsCell closure) { super(car(params), body, closure); }

        @Override Object params() { return ConsCell.cons(params, null); }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) {
            if (args == null) notEnoughArgsLst(params);
            final Object cdrArgs = cdr(args);
            if (cdrArgs != null) tooManyArgs(cdrArgs);
            return intp.acons(params, car(args), closure);
        }
    }

    // two arguments
    private static final class Closure2 extends Closure {
        final Object p1, p2;
        Closure2(ConsCell params, ConsCell body, ConsCell closure) {
            super(params, body, closure);
            p1 = car(params); p2 = cadr(params);
        }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) {
            if (args == null) notEnoughArgs(params);
            final Object cdrArgs = cdr(args);
            if (cdrArgs == null) notEnoughArgsLst(p2);
            final Object cddrArgs = cdr(cdrArgs);
            if (cddrArgs != null) tooManyArgs(cddrArgs);
            return intp.acons(p1, car(args), intp.acons(p2, car(cdrArgs), closure));
        }
    }

    // three arguments
    private static final class Closure3 extends Closure {
        final Object p1, p2, p3;
        Closure3(ConsCell params, ConsCell body, ConsCell closure) {
            super(params, body, closure);
            p1 = car(params); p2 = cadr(params); p3 = caddr(params);
        }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) {
            if (args == null) notEnoughArgs(params);
            final Object cdrArgs = cdr(args);
            if (cdrArgs == null) notEnoughArgs(cdr(params));
            final Object cddrArgs = cdr(cdrArgs);
            if (cddrArgs == null) notEnoughArgs(cddr(params));
            final Object cdddrArgs = cdr(cddrArgs);
            if (cdddrArgs != null) tooManyArgs(cdddrArgs);
            return intp.acons(p1, car(args), intp.acons(p2, car(cdrArgs), intp.acons(p3, car(cddrArgs), closure)));
        }
    }

    // 0 or more arguments
    private static final class ClosureVararg extends Closure {
        ClosureVararg(Object param, ConsCell body, ConsCell closure) { super(param, body, closure); }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) { return intp.acons(params, args, closure); }
    }

    // one or more arguments
    private static final class Closure1Varargs extends Closure {
        final Object p, more;
        Closure1Varargs(ConsCell params, ConsCell body, ConsCell closure) {
            super(params, body, closure);
            p = car(params); more = cdr(params);
        }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) {
            if (args == null) notEnoughArgs(params);
            return intp.acons(p, car(args), intp.acons(more, cdr(args), closure));
        }
    }

    // two or more arguments
    private static final class Closure2Varargs extends Closure {
        final Object p1, p2, more;
        Closure2Varargs(ConsCell params, ConsCell body, ConsCell closure) {
            super(params, body, closure);
            p1 = car(params); p2 = cadr(params); more = cddr(params);
        }

        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) {
            if (args == null) notEnoughArgs(params);
            final Object cdrArgs = cdr(args);
            if (cdrArgs == null) notEnoughArgs(cdr(params));
            return intp.acons(p1, car(args), intp.acons(p2, car(cdrArgs), intp.acons(more, cdr(cdrArgs), closure)));
        }
    }

    private static final class DynamicLambda extends Closure {
        DynamicLambda(Object paramss, ConsCell body) { super(paramss, body, null); }
        @Override ConsCell zip(@NotNull LambdaJ intp, ConsCell args, ConsCell env) { return intp.zip("dynamic function application", params, args, env, true); }
        @Override public void printSEx(WriteConsumer out, boolean escapeAtoms) { out.print("#<interpreted " + LAMBDA_DYNAMIC + ">"); }
    }

    private static final class ArraySlice extends ConsCell {
        private static final class ArraySliceIterator implements ConsIterator {
            private final Object @NotNull [] arry;
            private int cursor;

            ArraySliceIterator(Object @NotNull [] arry, int offset) { this.arry = arry; this.cursor = offset; }
            @Override public boolean hasNext() { return cursor < arry.length; }

            @Override public Object next() {
                if (cursor >= arry.length) throw new NoSuchElementException();
                return arry[cursor++];
            }

            @Override public boolean wasDotted() { return false; }
        }

        private static final long serialVersionUID = 1L;

        private final Object @NotNull [] arry;
        private final int offset;

        /** {@link #arraySlice} should be preferred because it will return {@code null} instead of a "null" (empty) ArraySlice */
        ArraySlice(Object @NotNull [] arry, int offset) {
            assert /*arry != null &&*/ offset < arry.length;
            this.arry = arry;  this.offset = offset;
        }

        /** {@link #arraySlice} should be preferred because it will return {@code null} instead of an "null" ArraySlice */
        private ArraySlice(@NotNull ArraySlice slice) {
            assert /*slice.arry != null &&*/ slice.offset < slice.arry.length;
            this.arry = slice.arry;  offset = slice.offset + 1;
        }

        @Override public Object car() { return arry[offset]; }
        @Override public @NotNull ConsCell rplaca(Object car) { arry[offset] = car; return this; }

        @Override public Object cdr() { return arry.length <= offset+1 ? null : new ArraySlice(this); }

        @Override public Object elt(long idx) {
            checkSequenceBounds(idx);
            return arry[(int)idx];
        }

        @Override public Object eltset(Object newValue, long idx) {
            checkSequenceBounds(idx);
            arry[(int)idx] = newValue;
            return newValue;
        }

        private void checkSequenceBounds(long idx) {
            if (idx < 0) throw new InvalidIndexError("index must be >= 0");
            if (idx >= length()) errorIndexTooLarge(idx, length());
        }

        private int length() { return arry.length - offset; }

        @Override public @NotNull String toString() { return printSEx(true, false).toString(); }
        @Override public @NotNull ConsIterator iterator() { return new ArraySliceIterator(this.arry, this.offset); }

        @NotNull CharSequence printSEx(boolean headOfList, boolean escapeAtoms) {
            final Object[] arry = this.arry;
            final int alen = arry.length, offset = this.offset;

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
            return ret;
        }

        Object @NotNull [] listToArray() {
            if (offset == 0) return arry;
            if (offset >= arry.length) return EMPTY_ARRAY;
            return Arrays.copyOfRange(arry, offset, arry.length);
        }

        boolean @NotNull [] listToBooleanArray() {
            if (offset >= arry.length) return new boolean[0];
            final int size = arry.length - offset;
            final boolean[] ret = new boolean[size];
            final Long zero = 0L, one = 1L;
            for (int i = 0, j = offset; i < size; i++, j++) {
                final Object o = arry[j];
                if (zero.equals(o)) ret[i] = false;
                else if (one.equals(o)) ret[i] = true;
                else throw new SimpleTypeError("not a valid value for bitvector: %s", LambdaJ.printSEx(o));
            }
            return ret;
        }

        @Override int sxhashSigned(int rec) {
            int ret = 0;
            for (int i = offset; i < arry.length && --rec > 0; i++) {
                final Object nextCar = arry[i];
                ret += 31 * (nextCar instanceof ConsCell ? ((ConsCell)nextCar).sxhashSigned(rec) : LambdaJ.sxhashSigned(nextCar));
            }
            return ret;
        }
    }



    /// ## Infrastructure

    static final int EOF = -1;
    static final ReadSupplier NULL_READCHARS = () -> EOF;
    static final WriteConsumer NULL_WRITECHARS = c -> {};

    static final Object[] EMPTY_ARRAY = new Object[0];
    static final boolean[] EMPTY_BITVECTOR = new boolean[0];

    final @NotNull ConsCell featuresEnvEntry;
    final @Null ConsCell conditionHandlerEnvEntry, randomStateEnvEntry;

    static final String[] CTRL = {
    "Nul", "Soh", "Stx", "Etx", "Eot", "Enq", "Ack", "Bel", "Backspace", "Tab", "Newline",
    "Vt", "Page", "Return", "So", "Si", "Dle", "Dc1", "Dc2", "Dc3", "Dc4",
    "Nak", "Syn", "Etb", "Can", "Em", "Sub", "Esc", "Fs", "Gs", "Rs",
    "Us"
    };

    /** additional directory for load and require, default is installation directory, see {@link InstallDir#installDir} */
    final Path libDir;

    Path currentSource;

    public enum TraceLevel {
        TRC_NONE, TRC_STATS, TRC_ENVSTATS, TRC_EVAL, TRC_FUNC, TRC_ENV, TRC_PARSE, TRC_TOK, TRC_LEX;
        public boolean ge(TraceLevel l) { return ordinal() >= l.ordinal(); }
    }
    final @NotNull TraceLevel trace;
    private final boolean traceOn;      // false if trace == TRC_NONE 
    private final boolean traceFunc;    // false if trace < TRC_FUNC

    final @NotNull TraceConsumer tracer;

    public enum Features {
        HAVE_QUOTE,          // quote will allow to distinguish code and data. without quote use cons.
        HAVE_ATOM,
        HAVE_EQ,

        HAVE_CONS,           // cons, car, cdr

        HAVE_VECTOR,         // vector, svref

        HAVE_HASH,           // make-hash-table, ...

        HAVE_COND,

        HAVE_APPLY,          // McCarthy didn't list apply, he probably implied eval, tough
        HAVE_LABELS,         // without labels: use Z-combinator (imperative version of the Y-combinator)

        HAVE_NIL, HAVE_T,    // use () and (quote t) instead. printObj will print nil regardless

        HAVE_XTRA,           // extra special forms such as if
        HAVE_DEFINE,         // special form define

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
        HAVE_MINPLUS    { @Override public int bits() { return HAVE_MIN.bits() | HAVE_APPLY.bits() | HAVE_LABELS.bits() | HAVE_NIL.bits() | HAVE_T.bits() | HAVE_DEFINE.bits(); } },
        HAVE_ALL_DYN    { @Override public int bits() { return (HAVE_MINPLUS.bits() | HAVE_XTRA.bits() | HAVE_FFI.bits()
                                                                | HAVE_NUMBERS.bits()| HAVE_DOUBLE.bits() | HAVE_LONG.bits() | HAVE_VECTOR.bits() | HAVE_HASH.bits()
                                                                | HAVE_STRING.bits() | HAVE_IO.bits() | HAVE_GUI.bits() | HAVE_UTIL.bits())
                                                               & ~HAVE_LEXC.bits(); } },
        HAVE_ALL_LEXC   { @Override public int bits() { return HAVE_ALL_DYN.bits() | HAVE_LEXC.bits(); } }
        ;

        public int bits() { return 1 << ordinal(); }
    }

    final int features;

    final boolean have(Features feature) { return (features & feature.bits()) != 0; }

    /** constructor with all features, no tracing */
    public LambdaJ() {
        this(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null);
    }

    public LambdaJ(SymbolTable symtab) {
        this(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null, symtab, null, null, null, null);
    }

    /** constructor */
    public LambdaJ(int features, TraceLevel trace, TraceConsumer tracer) {
        this(features, trace, tracer, null, null, null, null, null);
    }

    /** constructor */
    LambdaJ(int features, @NotNull TraceLevel trace, TraceConsumer tracer, @Null SymbolTable symtab,
            ConsCell featuresEnvEntry, ConsCell conditionHandlerEnvEntry, ConsCell randomStateEnvEntry,
            Path libDir) {
        this.features = features;

        this.trace = trace;
        traceOn = trace != TraceLevel.TRC_NONE;
        traceFunc = trace.ge(TraceLevel.TRC_FUNC);

        this.tracer = tracer != null ? tracer : System.err::println;
        if (symtab == null) symtab = new ListSymbolTable();
        this.symtab = symtab;
        if (libDir != null) this.libDir = libDir;
        else this.libDir = InstallDir.installDir;
        if (features != Features.HAVE_ALL_LEXC.bits()) speed = 0;

        this.featuresEnvEntry = featuresEnvEntry != null ? featuresEnvEntry : cons(intern(FEATURES), makeFeatureList(symtab));

        if (have(Features.HAVE_T)) symtab.intern(sT);
        if (have(Features.HAVE_NIL)) symtab.intern(sNil);
        symtab.intern(sLambda);

        if (have(Features.HAVE_QUOTE))  { internWellknown(QUOTE); }
        if (have(Features.HAVE_COND))   { internWellknown(COND); }
        if (have(Features.HAVE_LABELS)) { internWellknown(LABELS); }

        if (have(Features.HAVE_DEFINE)) internWellknown(DEFINE);

        if (have(Features.HAVE_XTRA))   {
            sDynamic = intern(DYNAMIC);

            internWellknown(DEFUN);
            internWellknown(DEFMACRO);
            internWellknown(IF);
            internWellknown(LET);
            internWellknown(LETSTAR);
            internWellknown(LETREC);

            internWellknown(MULTIPLE_VALUE_BIND);
            internWellknown(MULTIPLE_VALUE_CALL);

            internWellknown(UNWIND_PROTECT);
            internWellknown(CATCH);
            internWellknown(THROW);
            internWellknown(TRY);
            sConditionHandler = intern(CONDITION_HANDLER);
            this.conditionHandlerEnvEntry = conditionHandlerEnvEntry != null ? conditionHandlerEnvEntry : cons(sConditionHandler, null);

            internWellknown(SETQ);

            symtab.intern(sProgn);

            internWellknown(LOAD);
            internWellknown(REQUIRE);
            internWellknown(PROVIDE);

            internWellknown(DECLAIM);
        }
        else {
            sDynamic = null;
            sConditionHandler = null;
            this.conditionHandlerEnvEntry = null;
        }

        if (have(Features.HAVE_VECTOR)) {
            sBit = intern("bit");
            sCharacter = intern("character");
        }
        else sBit = sCharacter = null;

        if (have(Features.HAVE_NUMBERS)) {
            sRandomState = intern(RANDOM_STATE);
            this.randomStateEnvEntry = randomStateEnvEntry != null ? randomStateEnvEntry : cons(sRandomState, null);
        }
        else {
            sRandomState = null;
            this.randomStateEnvEntry = null;
        }

        WellknownSymbol.forAllPrimitives(features, w -> internWellknown(w.sym));

        // Lookup only once on first use. The supplier below will do a lookup on first use and then replace itself
        // by another supplier that simply returns the cached value.
        expTrue = () -> { final Object s = makeExpTrue(); expTrue = () -> s; return s; };
    }

    private static final String[] DEFAULT_FEATURES = {"murmel", "murmel-" + LANGUAGE_VERSION, "jvm", "ieee-floating-point" };
    static ConsCell makeFeatureList(SymbolTable s) {
        ConsCell l = null;
        for (String feat: DEFAULT_FEATURES) l = new ListConsCell(s.intern(feat), l);
        return l;
    }


    /// ## Printer

    /** create an ObjectWriter that transforms \n to the platform default line separator */
    public static @NotNull ObjectWriter makeWriter(@NotNull WriteConsumer out) {
        return makeWriter(out, System.lineSeparator());
    }

    /** create an ObjectWriter that transforms \n to the given {@code lineSeparator} */
    public static @NotNull ObjectWriter makeWriter(@NotNull WriteConsumer out, String lineSeparator) {
        if ("\r\n".equals(lineSeparator)) return new SExpressionWriter(new UnixToAnyEol(out, "\r\n"));
        if ("\r"  .equals(lineSeparator)) return new SExpressionWriter(new UnixToAnyEol(out, "\r"));

        return new SExpressionWriter(out);
    }

    /** this class will write objects as S-expressions to the given {@link WriteConsumer} w/o any eol translation */
    private static final class SExpressionWriter implements ObjectWriter {
        private final @NotNull WriteConsumer out;

        SExpressionWriter(@NotNull WriteConsumer out) { //noinspection ConstantConditions
                                                        assert out != null; this.out = out; }

        @Override public void printObj(Object o, boolean printEscape) { printSEx(out, o, printEscape); }
        @Override public void printEol() { out.print("\n"); }
        @Override public void printString(CharSequence s) { out.print(s); }
    }



    /// ## Scanner, symboltable and S-expression reader

    static class ListSymbolTable implements SymbolTable {
        private final @NotNull Map<@NotNull String, @NotNull LambdaJSymbol> symbols = JavaUtil.newHashMap(WellknownSymbol.values().length + 10);

        @Override public @NotNull LambdaJSymbol intern(@NotNull LambdaJSymbol sym) {
            final String symNameLC = sym.name.toLowerCase();
            final LambdaJSymbol existing = symbols.get(symNameLC);
            if (existing != null) return existing;

            if (sym.wellknownSymbol == WellknownSymbol.notInterned) sym = new LambdaJSymbol(true, sym.name);
            symbols.put(symNameLC, sym);
            return sym;
        }

        @Override public @NotNull LambdaJSymbol intern(@NotNull String symName) {
            final String symNameLC = symName.toLowerCase();
            final LambdaJSymbol existing = symbols.get(symNameLC);
            if (existing != null) return existing;

            final LambdaJSymbol ret = new LambdaJSymbol(true, symName);
            symbols.put(symNameLC, ret);
            return ret;
        }

        @Override public @NotNull Iterator<LambdaJSymbol> iterator() { return symbols.values().iterator(); }
    }

    public static @NotNull ObjectReader makeReader(@NotNull ReadSupplier in, @NotNull SymbolTable symtab, ConsCell featuresEnvEntry) { return new SExpressionReader(in, symtab, featuresEnvEntry, null); }
    final @NotNull SExpressionReader makeReader(@NotNull ReadSupplier in, Path path) { return new SExpressionReader(in, symtab, featuresEnvEntry, path); }

    static boolean isDigit(int c) {
        return Character.isDigit(c);
        //return c >= '0' && c <= '9'; // only accept ASCII digits, reject other Unicode digits
    }

    static int digit(int c) {
        return Character.digit(c, 10);
        //return isDigit(c) ? c - '0' : -1;
    }

    static boolean isWhitespace(int x) {
        return Character.isWhitespace(x);
        //return Character.isSpace((char)x); // ignores non-ASCII whitspace characters
    }

    static boolean isSExSyntax(int x) { return x == '(' || x == ')' /*|| x == '.'*/ || x == '\'' || x == '`' || x == ','; }

    /** is {@code s} an optional sign followed by one or more digits? */
    static boolean isLong(@NotNull String s) {
        assert /*s != null &&*/ !s.isEmpty() : "tokens should not be null and should not be the empty string";

        return isLong(s, s.length());
    }

    private static boolean isLong(@NotNull String s, int len) {
        final char first = s.charAt(0);
        if (first == '+' || first == '-') {
            if (len == 1) return false;
        }
        else if (!isDigit(first)) return false;
        for (int i = 1; i < len; i++) if (!isDigit(s.charAt(i))) return false;
        return true;
    }

    /** is {@code s} an optional sign followed by one or more digits followed by a '.'? */
    static boolean isCLDecimalLong(@NotNull String s) {
        assert /*s != null &&*/ !s.isEmpty() : "tokens should not be null and should not be the empty string";

        final int lenMinus1 = s.length() - 1;
        if (s.charAt(lenMinus1) != '.') return false;
        if (lenMinus1 < 1) return false;
        return isLong(s, lenMinus1);
    }

    static boolean isDouble(@NotNull String s) {
        assert /*s != null &&*/ !s.isEmpty() : "tokens should not be null and should not be the empty string";

        final int len;
        if ((len = s.length()) < 2) return false;

        int idx = 1;
        char c = s.charAt(0);
        if (c == '+' || c == '-') {
            if (len < 3) return false;
            idx = 2;
            c = s.charAt(1);
        }

        if (c == '.') {
            // s starts with [+-]?\.
            // must be followed by at least one digit
            if (!isDigit(s.charAt(idx))) return false;

            // s starts with [+-]?\.\d
            // eat additional digits and then there must be [eE] or end-of-string
            while (++idx < len && isDigit(s.charAt(idx))) {
                /* nothing */
            }
            if (idx == len) return true;
        }
        else if (isDigit(c)) {
            // s starts with [+-]?[0-9]
            // eat additional digits and then there must be [.eE]
            while (idx < len && isDigit(s.charAt(idx))) {
                idx++;
            }
            if (idx == len) return false;

            if (s.charAt(idx) == '.') {
                if (++idx == len || !isDigit(s.charAt(idx))) return false;

                while (++idx < len && isDigit(s.charAt(idx))) {
                    /* nothing */
                }
                if (idx == len) return true;
            }
        }
        else return false;

        c = s.charAt(idx++);
        if (c != 'e' && c != 'E') return false;

        // s starts with [+-]?\.\d+[eE]
        // must be followed by [+-]?\d+
        c = s.charAt(idx++);
        if (c == '+' || c == '-') {
            if (idx == len) return false;
            c = s.charAt(idx++);
        }

        // s starts with [+-]?\.\d+[eE][+-]?
        // must be followed by one or more digits
        if (!isDigit(c)) return false;

        // s starts with [+-]?\.\d+[eE][+-]?\d
        // eat additional digits and then there must be end-of-string
        while (idx < len && isDigit(s.charAt(idx))) {
            idx++;
        }
        return idx == len;
    }

    /** This class will read and parse S-Expressions (while generating symbol table entries)
     *  from the given {@link ReadSupplier} */
    static final class SExpressionReader implements ObjectReader {
        private final boolean haveNil;
        private final boolean haveString;
        private final boolean haveDouble;
        private final boolean haveLong;

        private final boolean traceLex;
        private final boolean traceTok;
        private final boolean traceParse;
        private final TraceConsumer tracer;

        private final @NotNull SymbolTable st;
        private final @NotNull ConsCell featuresEnvEntry;

        private @NotNull ReadSupplier in;    // readObj() will read from this
        private Path filePath;
        private boolean pos;
        private int lineNo = 1, charNo;
        private int prevLineNo = 1, prevCharNo;
        private boolean escape; // is the lookahead escaped
        private boolean tokEscape;
        private int backquote;
        private int look = EOF;
        private final char[] token = new char[TOKEN_MAX];
        private Object tok;

        /** Create an S-expression parser (==reader) with all features, no tracing.
         *
         *  @param in a {@link ReadSupplier} that supplies characters,
         *            {@code InputStream::read} won't work because that supplies bytes but not (Unicode-) characters,
         *            {@code Reader::read} will work
         */
        SExpressionReader(@NotNull ReadSupplier in, @NotNull SymbolTable st, ConsCell featuresEnvEntry, Path filePath) {
            this(Features.HAVE_ALL_DYN.bits(), TraceLevel.TRC_NONE, null, st, featuresEnvEntry, in, filePath);
        }

        /** Create an S-expression parser (==reader).
         * @param in a {@link ReadSupplier} that supplies characters,
         *            {@code InputStream::read} won't work because that supplies bytes but not (Unicode-) characters,
         *            {@code Reader::read} will work
         *
         */
        SExpressionReader(int features, @NotNull TraceLevel trace, TraceConsumer tracer, @NotNull SymbolTable st, ConsCell featuresEnvEntry, @NotNull ReadSupplier in, Path filePath) {
            haveNil = have(features, Features.HAVE_NIL);
            haveString = have(features, Features.HAVE_STRING);
            haveDouble = have(features, Features.HAVE_DOUBLE);
            haveLong = have(features, Features.HAVE_LONG);

            assert trace == TraceLevel.TRC_NONE || tracer != null;
            traceLex = trace.ge(TraceLevel.TRC_LEX);
            traceTok = trace.ge(TraceLevel.TRC_TOK);
            traceParse = trace.ge(TraceLevel.TRC_PARSE);
            this.tracer = tracer;

            this.st = st;
            this.in = in;
            this.filePath = filePath;

            sNot      = st.intern("not");
            sAnd      = st.intern("and");
            sOr       = st.intern("or");

            sQuote    = st.intern(QUOTE);
            sAppend   = st.intern(APPEND);
            sList     = st.intern(LIST);
            sListStar = st.intern(LISTSTAR);
            sCons     = st.intern(CONS);
            sNil      = st.intern(NIL);

            sVect     = st.intern(VECT);
            sHash     = st.intern(HASH);
            sApply    = st.intern(APPLY);

            this.featuresEnvEntry = featuresEnvEntry != null ? featuresEnvEntry : ConsCell.cons(null, null);
        }

        // this is really only useful for the repl. If parser.charNo != 0 the next thing the parser reads is the lineseparator following the previous sexp that was not consumed.
        void resetPos() { lineNo = charNo == 0 ? 1 : 0;  charNo = 0; }

        private static boolean have(int features, Features feature) { return (features & feature.bits()) != 0; }

        @Override public void setInput(@NotNull ReadSupplier input, Path filePath) {
            //noinspection ConstantConditions
            assert input != null; in = input; this.filePath = filePath; lineNo = 1; charNo = 0;
        }
        @Override public Path getInput() { return filePath; }

        /// Scanner
        private boolean isSpace(int x)  { return !escape && isWhitespace(x); }
        private boolean isDQuote(int x) { return !escape && x == '"'; }
        private boolean isBar(int x)    { return !escape && x == '|'; }

        @SuppressWarnings("BooleanMethodIsAlwaysInverted")
        private boolean isSpaceOrSyntaxOrEof(int x) { return !escape && (isWhitespace(x) || isSExSyntax(x)) || x == EOF; }

        private boolean prevWasCR;
        private int readchar() throws IOException {
            final int c = in.read();
            if (c == '\r') {
                prevWasCR = true;
                lineNo++;
                charNo = 0;
                return '\n';
            }
            if (c == '\n') {
                if (prevWasCR) {
                    // current char is a \n, previous char was a \r which was returned as a \n.
                    // Therefore the current \n is silently dropped, return the next char.
                    prevWasCR = false;
                    return readchar();
                }
                lineNo++;
                charNo = 0;
                return '\n';
            }
            prevWasCR = false; prevLineNo = lineNo; prevCharNo = charNo;
            if (c != EOF) { charNo++; }
            return c;
        }

        private int getchar() { return getchar(true); }

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
                errorReaderErrorFmt(posInfo(), "characterset conversion error in SExpressionReader: %s", e.toString());
                return -2; // notreached
            } catch (Exception e) {
                errorReaderErrorFmt(posInfo(), "I/O error in SExpressionReader: %s", e.toString());
                return -2; // notreached
            }
        }

        private void skipWs() { while (isSpace(look)) { look = getchar(); } }

        private static final Object CONTINUE = new Object();

        private Map<Integer, Object> labelledObjects;

        /** if we get here then we have already read '#' and look contains the character after #subchar */
        private Object readerMacro(int sub_char, int arg) throws IOException {
            switch (sub_char) {
            // #:symbolname ... uninterned symbol
            case ':': return new LambdaJSymbol(isBar(look) ? readBarSymbol() : readerMacroToken());

            // #\ ... character literal
            case '\\':
                final String charOrCharactername = readerMacroToken();
                if (charOrCharactername.length() == 1) return charOrCharactername.charAt(0);
                if (isLong(charOrCharactername)) {
                    try {
                        return (char) Integer.parseInt(charOrCharactername);
                    } catch (NumberFormatException e) {
                        throw new ParseError(posInfo(), "'%s' following #\\ is not a valid number", charOrCharactername);
                    }
                }
                for (int i = 0; i < CTRL.length; i++) {
                    if (CTRL[i].equalsIgnoreCase(charOrCharactername)) return (char)i;
                }
                errorReaderErrorFmt(posInfo(), "unrecognized character name %s", charOrCharactername);
                //NOTREACHED

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
                throw new EOFException(fmt("line %d:%d: EOF in multiline comment", ln, cn));

            // #' ... function, ignore for CL compatibility
            case '\'':
                return CONTINUE;

            // #+... , #-... feature expressions
            case '+':
            case '-':
                final boolean hasFeature = featurep(readObjRec(null));
                final Object next = readObjRec(null);
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

            case 'r':
            case 'R':
                skipWs();
                if (arg < Character.MIN_RADIX || arg > Character.MAX_RADIX) errorReaderErrorFmt(posInfo(), "%s is not a valid radix for #R", arg);
                return parseLong(readerMacroToken(), arg);

            case 'x':
            case 'X':
                skipWs();
                return parseLong(readerMacroToken(), 16);

            case '(':
                final ConsCell vec = readList(lineNo, charNo);
                if (backquote > 0) return ConsCell.listStar(sBqVector, arg, vec);
                return arg >= 0 ? listToArray(vec, arg) : LambdaJ.listToArray(vec);

            case '*':
                final String bv = readerMacroToken();
                return stringToBitvector(bv, arg >= 0 ? arg : bv.length());

            case 'H':
                if (look != '(') errorReaderError(posInfo(), "expected '(' after '#H'");
                look = getchar();
                final ConsCell testAndPairs = readList(lineNo, charNo);
                if (backquote > 0) return ConsCell.cons(sBqHash, testAndPairs);
                return hash(st, testAndPairs);

            case '=':
                final Object obj = readObjRec(null);
                if (labelledObjects == null) labelledObjects = JavaUtil.newHashMap(10);
                if (labelledObjects.putIfAbsent(arg, obj) != null) errorReaderErrorFmt(posInfo(), "label #%d= was already defined", arg);
                return obj;

            case '#':
                final Object ref;
                if (labelledObjects != null && (ref = labelledObjects.get(arg)) != null) return ref;
                errorReaderErrorFmt(posInfo(), "reference to undefined label #%d#", arg);

            default:
                look = getchar();
                errorReaderErrorFmt(posInfo(), "no dispatch function defined for %s", printChar(sub_char));
                return null; // notreached
            }
        }

        private boolean[] stringToBitvector(String bv, int len) {
            if (len < bv.length()) errorReaderErrorFmt(posInfo(), "too many bits in \"%s\": expected %d or fewer", bv, len);
            if (bv.isEmpty()) {
                if (len == 0) return EMPTY_BITVECTOR;
                errorReaderErrorFmt(posInfo(), "#%d* requires at least 1 bit of input", len);
            }
            final boolean[] ret = new boolean[len];
            int i = 0;
            for (char c: bv.toCharArray()) {
                switch (digit(c)) {
                case 0: break;
                case 1: ret[i] = true; break;
                default: errorReaderErrorFmt(posInfo(), "not a valid value for bitvector: %c", c);
                }
                i++;
            }
            final boolean last = ret[i-1];
            if (last) Arrays.fill(ret, i, len, true);
            return ret;
        }

        private String readerMacroToken() {
            int index = 0;
            while (!isSpaceOrSyntaxOrEof(look)) {
                if (index < TOKEN_MAX) token[index++] = (char)look;
                look = getchar(false);
            }
            return tokenToString(0, Math.min(index, SYMBOL_MAX));
        }

        private final Object sNot, sAnd, sOr;

        private boolean featurep(Object form) {
            if (symbolp(form)) return some(x -> x == form, requireList(FEATURES, cdr(featuresEnvEntry)));
            else if (consp(form)) {
                final ConsCell ccForm = (ConsCell)form;
                final Object op = car(ccForm);  final ConsCell args = requireList("feature expression", cdr(ccForm));
                if (op == sAnd) return every(args);
                if (op == sOr) return some(this::featurep, args);
                if (op == sNot) {
                    if (args == null) throw new SimpleError("feature expression not: not enough subexpressions, got %s", printSEx(form));
                    if (cdr(args) != null) throw new SimpleError("feature expression not: too many subexpressions, got %s", printSEx(form));
                    return !featurep(car(args));
                }
            }
            throw new SimpleError("unsupported feature expressions, got %s", printSEx(form));
        }

        private boolean every(ConsCell lst) {
            if (lst == null) return true;
            for (Object o: lst) { if (!featurep(o)) return false; } // todo iterator laesst improper list zu
            return true;
        }

        private static boolean some(Function<Object, Boolean> pred, ConsCell lst) {
            if (lst == null) return false;
            for (Object o: lst) { if (pred.apply(o)) return true; } // todo iterator laesst improper list zu
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
        private void readToken() throws IOException {
            final int eof = LambdaJ.EOF;
            for (;;) {
                skipWs();
                tok = null;
                if (look == eof) {
                    if (traceLex) tracer.println("*** scan  EOF");
                    return;
                }

                if (!escape) {
                    switch (look) {
                    case '|':  { tok = st.intern(readBarSymbol()); break; }

                    case '(':  { tok = Token.LP;    look = getchar(); break; }
                    case ')':  { tok = Token.RP;    look = getchar(); break; }
                    case '\'': { tok = Token.SQ;    look = getchar(); break; }
                    case '`':  { tok = Token.BQ;    look = getchar(); break; }
                    case ',':  { tok = Token.COMMA; look = getchar(); break; }

                    case '"':
                        if (haveString) {
                            int index = 0;
                            do {
                                if (index < TOKEN_MAX) token[index++] = (char) look;
                                look = getchar(false);
                            } while (look != eof && !isDQuote(look));
                            if (look == eof)
                                throw new EOFException("string literal is missing closing \"");
                            look = getchar(); // consume trailing "
                            tok = tokenToString(1, index); // Strings may or may not be interned in evalForm()
                        }
                        break;

                    case '#': // todo readermacros ggf. per feature/ commandlineflag abdrehen? und/ oder processReaderMacros dem konstruktor geben?
                        look = getchar(false);
                        int arg = -1, digit;
                        final int subChar;
                        if (escape) subChar = '\\';
                        else {
                            if ((digit = digit(look)) != -1) {
                                arg = 0;
                                do {
                                    arg *= 10;  arg += digit;  look = getchar(false);
                                } while ((digit = digit(look)) != -1);
                            }
                            subChar = look;
                            look = getchar(false);
                        }
                        tok = readerMacro(subChar, arg);
                        break;
                    }
                }

                if (tok == null) {
                    int index = 0;
                    boolean escapeSeen = false;
                    while (!isSpaceOrSyntaxOrEof(look)) {
                        if (escape) escapeSeen = true;
                        if (index < TOKEN_MAX) token[index++] = (char) look;
                        look = getchar();
                    }
                    @NotNull String s = tokenToString(0, index);
                    //noinspection ConstantConditions
                    if (!tokEscape && ".".equals(s)) {
                        tok = Token.DOT;
                    } else if (!escapeSeen && haveDouble && isDouble(s)) {
                        tok = parseDouble(s);
                    } else if (!escapeSeen && haveLong && isLong(s)) {
                        tok = parseLong(s, 10);
                    } else if (!escapeSeen && haveDouble && isLong(s)) {
                        tok = parseDouble(s);
                    } else if (!escapeSeen && (haveDouble || haveLong) && isCLDecimalLong(s)) {
                        // reject CL-style 123. for "123 in radix 10" - Murmel doesn't support changing reader radix,
                        // and non-lispers may think digits followed by a dot are floating point numbers (as is the case in most programming languages)
                        throw new ParseError(posInfo(prevLineNo, prevCharNo), "digits followed by '.' to indicate 'integer in radix' 10 is not supported. Digits followed by '.' without decimal numbers to indicate 'floating point' also is not supported.");
                    } else {
                        if (s.length() > SYMBOL_MAX) s = s.substring(0, SYMBOL_MAX);
                        tok = st.intern(s);
                    }
                }

                if (tok != CONTINUE) {
                    if (traceLex) tracer.println("*** scan  token  |" + tok + '|');
                    return;
                }
            }
        }

        private String readBarSymbol() {
            int index = 0;
            while (true) {
                look = getchar(false);
                if (look == LambdaJ.EOF) wrap0(new EOFException("|-quoted symbol is missing closing |"), posInfo()); // wrap0() doesn't return
                if (isBar(look)) break;
                if (index < SYMBOL_MAX) token[index++] = (char) look;
            }
            look = getchar(); // consume trailing |
            return tokenToString(0, Math.min(index, SYMBOL_MAX));
        }

        // todo sollte wsl parseFixnum() solangs keine bignums gibt, sein sonst wird was gelesen was eig. ungueltig ist?!
        private Number parseLong(String s, int radix) {
            try {
                return Long.valueOf(s, radix);
            } catch (NumberFormatException e) {
                errorReaderErrorFmt(posInfo(), "'%s' is not a valid number", s);
                return null; // notreached
            }
        }

        private Number parseDouble(String s) {
            try {
                return Double.valueOf(s);
            } catch (NumberFormatException e) {
                errorReaderErrorFmt(posInfo(), "'%s' is not a valid number", s);
                return null; // notreached
            }
        }

        private @NotNull String tokenToString(int first, int end) { return new String(token, first, end - first); }


        /// S-expression parser
        /** Record line and char numbers in the conses */
        @Override public Object readObj(boolean recordPos, Object eof) {
            this.pos = recordPos;
            final Object ret = readObj(eof);
            this.pos = false;
            return ret;
        }

        @Override public Object readObj(Object eof) {
            if (look == EOF) {
                prevWasCR = false;
                lineNo = 1; charNo = 0;
                look = getchar();
            }
            try { return readObjRec(eof); }
            finally { if (labelledObjects != null) labelledObjects.clear(); }
        }

        private Object readObjRec(Object eof) {
            skipWs(); // das brauchts damit die zeilennummern stimmen. Dass readToken() mit skipWs() beginnt, nuetzt uns nix.
            final int startLine = lineNo, startChar = charNo;
            try {
                readToken();
                //return expand_backquote(readObject(startLine, startChar));
                return readObject(startLine, startChar, eof);
            }
            catch (LambdaJError le) { throw le; }
            catch (Exception pe) { throw new LambdaJError(pe, true, posInfo()); }
        }

        private static final Object sQuasiquote = "quasiquote", sUnquote = "unquote", sUnquote_splice = "unquote-splice", sBqVector = "bq-vector", sBqHash = "bq-hash";
        private final LambdaJSymbol sQuote, sAppend, sList, sListStar, sCons, sNil;
        private final LambdaJSymbol sApply, sVect, sHash;

        private Object readObject(int startLine, int startChar, Object eof) throws IOException {
            if (tok == null) {
                if (traceParse) tracer.println("*** parse EOF");
                return eof;
            }
            if (tok == sNil) {
                // can't handle this with "if (symbolp..." below: nil is not a regular symbol but has the type "null" which is a subtype of symbol
                if (traceTok) tracer.println("*** parse symbol " + NIL);
                if (haveNil) return null;
                else return tok;
            }
            if (symbolp(tok)) {
                if (traceTok) tracer.println("*** parse symbol " + printSEx(tok));
                return tok;
            }
            if (!tokEscape) {
                if (tok == Token.RP) errorReaderError(posInfo(), "unexpected ')'");
                if (tok == Token.LP) {
                    try {
                        final ConsCell list = readList(startLine, startChar);
                        if (!tokEscape && tok == Token.DOT) {
                            if (list == null) errorReaderError(posInfo(), "nothing appears before . in list");
                            skipWs();
                            final ConsCell cdr = readList(lineNo, charNo);
                            if (cdr == null) errorReaderError(posInfo(startLine, startChar), "illegal end of dotted list: nothing appears after . in list");
                            if (cdr(cdr) != null) errorReaderErrorFmt(posInfo(startLine, startChar), "illegal end of dotted list: %s", printSEx(cdr));
                            assert list != null;
                            nconc2(list, car(cdr));
                            if (traceParse) tracer.println("*** parse list   " + printSEx(list));
                            return list;
                        }
                        if (traceParse) tracer.println("*** parse list   " + printSEx(list));
                        return list;
                    }
                    catch (IOException e) { errorReaderError(posInfo(startLine, startChar), e.getMessage()); }
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
                    final Object o;
                    try {
                        backquote++;
                        readToken();
                        final Object exp = readObject(_startLine, _startChar, eof);
                        if (backquote == 1) o = qq_expand(exp);
                        else o = cons(startLine, startChar, sQuasiquote, cons(startLine, startChar, exp, null));
                    }
                    finally { backquote--; }
                    return o;
                }
                if (tok == Token.COMMA) {
                    if (backquote == 0) errorReaderError(posInfo(startLine, startChar), "comma is not inside a backquote");
                    skipWs();
                    final Object unquote;
                    if (look == '.') errorReaderError(posInfo(startLine, startChar), ",. is not supported");
                    if (look == '@') { unquote = sUnquote_splice; look = getchar(); }
                    else unquote = sUnquote;
                    final int _startLine = lineNo, _startChar = charNo;
                    final Object o;
                    try {
                        backquote--;
                        readToken();
                        o = cons(startLine, startChar, unquote, cons(startLine, startChar, readObject(_startLine, _startChar, eof), null));
                    }
                    finally { backquote++; }
                    return o;
                }
            }
            if (traceTok) tracer.println("*** parse value  " + tok);
            return tok;
        }

        private String posInfo(int startLine, int startChar) {
            return (filePath == null ? "line " : filePath.toString() + ':') + startLine + ':' + startChar + ".." + lineNo + ':' + charNo;
        }

        private String posInfo() {
            return (filePath == null ? "line " : filePath.toString() + ':') + lineNo + ':' + charNo;
        }

        private ConsCell readList(int listStartLine, int listStartChar) throws IOException {
            AbstractConsCell first = null, appendTo = null;
            skipWs();
            for (;;) {
                final int carStartLine = lineNo, carStartChar = charNo;
                readToken();
                if (tok == null) throw new EOFException("cannot read list. missing ')'?");
                if (!tokEscape && (tok == Token.RP || tok == Token.DOT)) {
                    if (first != null) first.adjustEnd(prevLineNo, prevCharNo);
                    return first;
                }
                final AbstractConsCell newCell = cons(listStartLine, listStartChar);
                if (first == null) first = newCell;
                if (appendTo != null) appendTo.rplacd(newCell);
                appendTo = newCell;

                newCell.rplaca(readObject(carStartLine, carStartChar, null));
                if (newCell.car() instanceof SExpConsCell) {
                    newCell.adjustEnd((SExpConsCell)newCell.car());
                }
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

        /** Append rest at the end of first, modifying first in the process.
         *  Returns a dotted list unless rest is a proper list. This works like a two arg nconc. */
        private static void nconc2(@NotNull ConsCell first, Object rest) {
            for (ConsCell last = first; ; last = (ConsCell) cdr(last)) {
                if (cdr(last) == first) errorReaderErrorFmt("%s: first argument is a circular list", "appendToList");
                if (cdr(last) == null) {
                    last.rplacd(rest);
                    return;
                }
            }
        }



        /*
         qq-expand and qq-expand-list are based on "Quasiquotation in Lisp (1999) by Alan Bawden"
         https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227, https://3e8.org/pub/scheme/doc/Quasiquotation%20in%20Lisp%20(Bawden).pdf

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
            if (x == sT || x == sNil || (atom(x) && !symbolp(x)))
                return x;
            if (atom(x))
                return quote(x);

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return cadr(xCons);

            if (op == sUnquote_splice)
                errorReaderError(posInfo(), "can't splice here"); // todo wsl falsch

            if (op == sQuasiquote)
                return qq_expand(qq_expand(cadr(xCons)));

            if (op == sBqVector)
                return qq_expandVector(qq_expand(cdr(xCons)));
            if (op == sBqHash)
                return qq_expandHash(qq_expand(cdr(xCons)));

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
                return list(sQuote, new ListConsCell(null, null));
            if (atom(x))
                return list(sQuote, new ListConsCell(x, null));

            final ConsCell xCons = (ConsCell)x;
            final Object op = car(xCons);

            if (op == sUnquote)
                return list(sList, cadr(xCons));

            if (op == sUnquote_splice)
                return cadr(xCons);

            if (op == sQuasiquote)
                return qq_expand_list(qq_expand(cadr(xCons)));

            if (op == sBqVector)
                return toList(qq_expandVector(qq_expand(cdr(xCons))));
            if (op == sBqHash)
                return toList(qq_expandHash(qq_expand(cdr(xCons))));

            if (cdr(xCons) == null)
                return list(sList, qq_expand_list(op));

            //return list(sList, list(sAppend, qq_expand_list(op), qq_expand(cdr(xCons))));
            final ConsCell combined = optimizedAppend(qq_expand_list(op), qq_expand(cdr(xCons)));
            if (car(combined) == sQuote) return list(sQuote, cdr(combined));
            return list(sList, combined);
        }

        private Object qq_expandVector(Object args) {
            ConsCell ccArgs = (ConsCell)args;
            if (car(ccArgs) == sQuote) {
                ccArgs = (ConsCell)cadr(ccArgs);
                final int arg = (Integer)car(ccArgs);
                return arg >= 0 ? listToArray((ConsCell)cdr(ccArgs), arg) : LambdaJ.listToArray(cdr(ccArgs));
            }
            if (car(ccArgs) == sList) return ConsCell.cons(sVect, cdr(ccArgs));
            return ConsCell.cons(sApply, ConsCell.list(sVect, ccArgs));
        }

        private Object qq_expandHash(Object args) {
            final ConsCell ccArgs = (ConsCell)args;
            if (car(ccArgs) == sQuote) return hash(st, (ConsCell)cadr(ccArgs));
            if (car(ccArgs) == sList) return ConsCell.cons(sHash, cdr(ccArgs));
            return ConsCell.cons(sApply, ConsCell.list(sHash, ccArgs));
        }

        private Object toList(Object expanded) {
            assert !symbolp(expanded);
            if (atom(expanded)) return list(sQuote, ConsCell.cons(expanded, null));
            return list(sList, expanded);
        }

        /** create a form that will append lhs and rhs: "(append lhs rhs)"
         * For some special case the form will be optimized:
         *
         * (append (quote lhsX) (quote rhsX...)) -> (quote lhsX rhsX...)
         * (append (quote lhsX) (list rhsX...))  -> (list (quote lhsX) rhsX...)   ; only quote if needed
         * (append (quote lhsX) (list* rhsX...)) -> (list* (quote lhsX) rhsX...)  ; only quote if needed
         * (append (quote lhsX) (cons rhsX...))  -> (list* (quote lhsX) rhsX...)  ; only quote if needed
         * (append (quote lhsX) rhs)             -> (cons (quote lhsX) rhs)       ; only quote if needed
         *
         * (append (list lhsX) (list rhsX...))   -> (list lhsX rhsX...)
         * (append (list lhsX) (list* rhsX...))  -> (list* lhsX rhsX...)
         * (append (list lhsX) (cons rhsX...))   -> (list* lhsX rhsX...)
         * (append (list lhsX) rhs)              -> (cons lhsX rhs)
         *
         * (append lhs (list rhsX))              -> (append lhs (cons rhsX nil))
         */
        private ConsCell optimizedAppend(Object lhs, Object rhs) {
            if (rhs == null) return (ConsCell)lhs;
            if (consp(lhs)) {
                if (car(lhs) == sQuote) {
                    assert cddr(lhs) == null : "expected a single argument quote call but got " + lhs;
                    assert cdr(cadr(lhs)) == null : "expected a quoted single element list but got " + lhs;

                    Object x = car(cadr(lhs));
                    if (!(x == sT || x == sNil || (atom(x) && !symbolp(x))))
                        x = quote(x);

                    if (consp(rhs)) {
                        final Object carRhs = car(rhs);
                        if (carRhs == sQuote) return new ListConsCell(sQuote, new ListConsCell(((ConsCell)cadr(lhs)).rplacd(cadr(rhs)), null));
                        if (carRhs == sList)  return new ListConsCell(sList, new ListConsCell(x, cdr(rhs)));
                        if (carRhs == sListStar
                            || carRhs == sCons) return new ListConsCell(sListStar, new ListConsCell(x, cdr(rhs)));
                    }

                    return list(sCons, x, rhs);
                }

                if (car(lhs) == sList) {
                    assert cddr(lhs) == null : "expected a single argument list call but got " + lhs;

                    if (consp(rhs)) {
                        final Object carRhs = car(rhs);
                        if (carRhs == sList)    return new ListConsCell(sList, new ListConsCell(cadr(lhs), cdr(rhs)));
                        if (carRhs == sListStar
                            || carRhs == sCons) return new ListConsCell(sListStar, new ListConsCell(cadr(lhs), cdr(rhs)));
                    }

                    return list(sCons, cadr(lhs), rhs);
                }
            }

            if (consp(rhs) && car(rhs) == sList && cddr(rhs) == null)
                return list(sAppend, lhs, list(sCons, cadr(rhs), null));

            if (consp(rhs) && car(rhs) == sAppend)
                return new ListConsCell(sAppend, new ListConsCell(lhs, cdr(rhs)));

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

    /// Murmel has a list of reserved words that may not be used as a symbol: t, nil and special forms

    /** Throw error if sym is a reserved symbol */
    static void notReserved(final String op, final LambdaJSymbol sym) {
        if (reserved(sym)) errorReserved(op, sym);
    }

    static boolean reserved(LambdaJSymbol sym) {
        if (sym == null) return true;
        final WellknownSymbolKind k = sym.wellknownSymbol.kind;
        return k == WellknownSymbolKind.SF          // special forms
               || k == WellknownSymbolKind.SYMBOL;  // nil and t
    }


    /// Symboltable
    private final @NotNull SymbolTable symtab;
    public SymbolTable getSymbolTable() { return symtab; }

    private static final Object UNASSIGNED = "#<value is not assigned>";          // only relevant in letrec
    static final ConsCell NO_VALUES = new ListConsCell("no multiple values", null);
    private static final Object PSEUDO_SYMBOL = "non existant pseudo symbol"; // to avoid matches on pseudo env entries

    /** print names of some wellknown symbols */
    public static final class Names {
        // basic special forms
        public static final String QUOTE = "quote", LAMBDA = "lambda", LAMBDA_DYNAMIC = "lambda dynamic";

        // additional special forms
        public static final String COND = "cond", LABELS = "labels", IF = "if";
        public static final String LET = "let", LETSTAR = "let*", LETREC = "letrec";
        public static final String SETQ = "setq", PROGN = "progn";
        public static final String DEFINE = "define", DEFUN = "defun", DEFMACRO = "defmacro";
        public static final String MULTIPLE_VALUE_BIND = "multiple-value-bind", MULTIPLE_VALUE_CALL = "multiple-value-call";
        public static final String UNWIND_PROTECT = "unwind-protect", CATCH = "catch", THROW = "throw", TRY = "try";

        // special forms for system construction
        public static final String LOAD = "load", REQUIRE = "require", PROVIDE = "provide";

        public static final String DECLAIM = "declaim";
        // parameters to declaim
        public static final String OPTIMIZE = "optimize", SPEED = "speed", DEBUG = "debug";

        // predefined global variables
        public static final String T = "t", NIL = "nil";
        public static final String PI = "pi", DYNAMIC = "dynamic";
        public static final String FEATURES = "*features*", CONDITION_HANDLER = "*condition-handler*", RANDOM_STATE = "*random-state*";
        public static final String MOST_POSITIVE_FIXNUM = "most-positive-fixnum", MOST_NEGATIVE_FIXNUM = "most-negative-fixnum", ARRAY_DIMENSION_LIMIT = "array-dimension-limit";
        public static final String INTERNAL_TIME_UNITS_PER_SECOND = "internal-time-units-per-second";
        public static final String COMMAND_LINE_ARGUMENT_LIST = "*command-line-argument-list*";

        // basic primitives
        public static final String APPLY = "apply";
        public static final String EVAL = "eval";

        // logic, predicates
        public static final String EQ = "eq", EQL = "eql", EQUAL = "equal";
        public static final String CONSP = "consp", ATOM = "atom", NULL = "null" /* null as a function and type */, SYMBOLP = "symbolp";
        public static final String NUMBERP = "numberp", FLOATP = "floatp", INTEGERP = "integerp", CHARACTERP = "characterp";
        public static final String RANDOM_STATE_P = "random-state-p";
        public static final String VECTORP = "vectorp", SIMPLE_VECTOR_P = "simple-vector-p";
        public static final String STRINGP = "stringp", SIMPLE_STRING_P = "simple-string-p";
        public static final String BIT_VECTOR_P = "bit-vector-p", SIMPLE_BIT_VECTOR_P = "simple-bit-vector-p";
        public static final String HASH_TABLE_P = "hash-table-p", FUNCTIONP = "functionp", LISTP = "listp";
        public static final String TYPEP = "typep";
        public static final String ADJUSTABLE_ARRAY_P = "adjustable-array-p";

        // conses and lists
        public static final String CAR = "car", CDR = "cdr", CONS = "cons", RPLACA = "rplaca", RPLACD = "rplacd";
        public static final String LIST = "list" /* list as a function NOT type */, LISTSTAR = "list*", APPEND = "append", ASSQ = "assq", ASSOC = "assoc";

        // vectors, sequences
        public static final String VECTOR = "vector" /* vector as a function and type */, VECT = "vect";
        public static final String MAKE_ARRAY = "make-array";

        // Hash tables
        public static final String HASH = "hash", MAKE_HASH_TABLE = "make-hash-table";

        // misc
        public static final String VALUES = "values";
        public static final String ERROR = "error";
        public static final String JMETHOD = "jmethod";

        private Names() {}
    }

    /** well known symbols for the reserved symbols t, nil and dynamic, and for some special operators.
     *  Depending on the features given to {@link LambdaJ#LambdaJ} these may be interned into the symbol table. */
    static final LambdaJSymbol sT = new LambdaJSymbol(T, true), sNil = new LambdaJSymbol(NIL, true),
                               sLambda = new LambdaJSymbol(LAMBDA, true), sLambdaDynamic = new LambdaJSymbol(LAMBDA_DYNAMIC, true), sProgn = new LambdaJSymbol(PROGN, true);

    /** some more well known symbols. These symbols are not reserved, the LambdaJSymbol objects could be used to store a macro closure, so the symbols must be instance members of LambdaJ. */
    final LambdaJSymbol sDynamic, sBit, sCharacter, sConditionHandler, sRandomState;

    enum WellknownSymbolKind { SF, PRIM, SYMBOL}
    enum WellknownSymbol {
        notInterned("", null), interned("", null),

        // basic special forms
        sQuote(QUOTE, WellknownSymbolKind.SF), sLambda(LAMBDA, WellknownSymbolKind.SF), sLambdaDynamic(LAMBDA_DYNAMIC, WellknownSymbolKind.SF),

        // additional special forms
        sCond(COND, WellknownSymbolKind.SF), sLabels(LABELS, WellknownSymbolKind.SF), sIf(IF, WellknownSymbolKind.SF),
        sLet(LET, WellknownSymbolKind.SF), sLetStar(LETSTAR, WellknownSymbolKind.SF), sLetrec(LETREC, WellknownSymbolKind.SF),
        sSetQ(SETQ, WellknownSymbolKind.SF), sProgn(PROGN, WellknownSymbolKind.SF),
        sDefine(DEFINE, WellknownSymbolKind.SF), sDefun(DEFUN, WellknownSymbolKind.SF), sDefmacro(DEFMACRO, WellknownSymbolKind.SF),
        sMultipleValueBind(MULTIPLE_VALUE_BIND, WellknownSymbolKind.SF), sMultipleValueCall(MULTIPLE_VALUE_CALL, WellknownSymbolKind.SF),
        sUnwindProtect(UNWIND_PROTECT, WellknownSymbolKind.SF), sCatch(CATCH, WellknownSymbolKind.SF), sThrow(THROW, WellknownSymbolKind.SF), sTry(TRY, WellknownSymbolKind.SF),
        sLoad(LOAD, WellknownSymbolKind.SF), sRequire(REQUIRE, WellknownSymbolKind.SF), sProvide(PROVIDE, WellknownSymbolKind.SF),
        sDeclaim(DECLAIM, WellknownSymbolKind.SF),

        // predefined global variables. Java usage is indirectly through WellknownSymbol.of().
        sNil(NIL, WellknownSymbolKind.SYMBOL), sT(T, WellknownSymbolKind.SYMBOL),

        // logic, predicates
        sEq(EQ, Features.HAVE_EQ, false, 2)                             { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(car(args) == cadr(args)); } },
        sEql(EQL, Features.HAVE_UTIL, 2)                                { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(eql(car(args), cadr(args))); } },
        sEqual(EQUAL, Features.HAVE_UTIL, 2)                            { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(equal(car(args), cadr(args))); } },

        sConsp(CONSP, Features.HAVE_UTIL, 1)                            { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(consp(car(args))); } },
        sAtom(ATOM, Features.HAVE_ATOM, 1)                              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(atom(car(args))); } },
        sSymbolp(SYMBOLP, Features.HAVE_UTIL, 1)                        { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(symbolp(car(args))); } },
        sNull(NULL, Features.HAVE_UTIL, false, 1)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(car(args) == null); } },
        sNumberp(NUMBERP, Features.HAVE_NUMBERS, true, 1)               { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(numberp(car(args))); } },
        sFloatp(FLOATP, Features.HAVE_NUMBERS, true, 1)                 { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(floatp(car(args))); } },
        sIntegerp(INTEGERP, Features.HAVE_NUMBERS, true, 1)             { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(integerp(car(args))); } },
        sCharacterp(CHARACTERP, Features.HAVE_STRING, 1)                { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(characterp(car(args))); } },
        sRandomstatep(RANDOM_STATE_P, Features.HAVE_NUMBERS, true, 1)   { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(randomstatep(car(args))); } },

        sVectorp(VECTORP, Features.HAVE_VECTOR, 1)                      { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(vectorp(car(args))); } },
        sSimpleVectorP(SIMPLE_VECTOR_P, Features.HAVE_VECTOR, 1)        { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(svectorp(car(args))); } },

        sStringp(STRINGP, Features.HAVE_STRING, 1)                      { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(stringp(car(args))); } },
        sSimpleStringP(SIMPLE_STRING_P, Features.HAVE_STRING, 1)        { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(sstringp(car(args))); } },

        sBitVectorP(BIT_VECTOR_P, Features.HAVE_VECTOR, 1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(bitvectorp(car(args))); } },
        sSimpleBitVectorP(SIMPLE_BIT_VECTOR_P, Features.HAVE_VECTOR, 1) { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(sbitvectorp(car(args))); } },

        sHashtableP(HASH_TABLE_P, Features.HAVE_HASH, 1)                { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(hashtablep(car(args))); } },
        sFunctionp(FUNCTIONP, Features.HAVE_UTIL, 1)                    { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(intp.functionp(car(args))); } },
        sListp(LISTP, Features.HAVE_UTIL, 1)                            { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(listp(car(args))); } },

        sTypep(TYPEP, Features.HAVE_UTIL, 2)                            { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(typep(intp.getSymbolTable(), intp, intp.typeSpecs(), car(args), cadr(args))); } },

        sAdjArrayp(ADJUSTABLE_ARRAY_P, Features.HAVE_VECTOR, 1)         { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(adjustableArrayP(car(args))); } },

        // conses and lists
        sCar(CAR, Features.HAVE_CONS, 1)                     { @Override Object apply(LambdaJ intp, ConsCell args) { return caar(args); } },
        sCdr(CDR, Features.HAVE_CONS, 1)                     { @Override Object apply(LambdaJ intp, ConsCell args) { return cdar(args); } },
        sCons(CONS, Features.HAVE_CONS, 2)                   { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.cons(car(args), cadr(args)); } },
        sRplaca(RPLACA, Features.HAVE_XTRA, 2)               { @Override Object apply(LambdaJ intp, ConsCell args) { return requireCons(RPLACA, car(args)).rplaca(cadr(args)); } },
        sRplacd(RPLACD, Features.HAVE_XTRA, 2)               { @Override Object apply(LambdaJ intp, ConsCell args) { return requireCons(RPLACD, car(args)).rplacd(cadr(args)); } },

        sList(LIST, Features.HAVE_UTIL, true, -1)            { @Override Object apply(LambdaJ intp, ConsCell args) { return args; } },
        sListStar(LISTSTAR, Features.HAVE_UTIL, false, 1,-1) { @Override Object apply(LambdaJ intp, ConsCell args) { return listStar(intp, args); } },
        sAppend(APPEND, Features.HAVE_UTIL, false, -1)       { @Override Object apply(LambdaJ intp, ConsCell args) { return append(intp, args); } },
        sAssq(ASSQ, Features.HAVE_UTIL, 2)                   { @Override Object apply(LambdaJ intp, ConsCell args) { return assq(car(args), cadr(args)); } },
        sAssoc(ASSOC, Features.HAVE_UTIL, 2)                 { @Override Object apply(LambdaJ intp, ConsCell args) { return assoc(car(args), cadr(args)); } },

        // numbers, characters
        sAdd("+", Features.HAVE_NUMBERS, -1)                 { @Override Object apply(LambdaJ intp, ConsCell args) { return addOp(args, "+", 0.0, Double::sum); } },
        sMul("*", Features.HAVE_NUMBERS, -1)                 { @Override Object apply(LambdaJ intp, ConsCell args) { return addOp(args, "*", 1.0, (lhs, rhs) -> lhs * rhs); } },
        sSub("-", Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return subOp(args, "-", 0.0, (lhs, rhs) -> lhs - rhs); } },
        sDiv("/", Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return subOp(args, "/", 1.0, (lhs, rhs) -> lhs / rhs); } },

        sNeq("=", Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(LambdaJ.Subr.compare(args, "=",  (d1, d2) -> d1 == d2)); } },
        sNe("/=", Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(LambdaJ.Subr.compare(args, "/=", (d1, d2) -> d1 != d2)); } },
        sLt("<",  Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(LambdaJ.Subr.compare(args, "<",  (d1, d2) -> d1 <  d2)); } },
        sLe("<=", Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(LambdaJ.Subr.compare(args, "<=", (d1, d2) -> d1 <= d2)); } },
        sGe(">=", Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(LambdaJ.Subr.compare(args, ">=", (d1, d2) -> d1 >= d2)); } },
        sGt(">",  Features.HAVE_NUMBERS, 1, -1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(LambdaJ.Subr.compare(args, ">",  (d1, d2) -> d1 >  d2)); } },

        sInc("1+", Features.HAVE_NUMBERS, true, 1)           { @Override Object apply(LambdaJ intp, ConsCell args) { return inc(car(args)); } },
        sDec("1-", Features.HAVE_NUMBERS, true, 1)           { @Override Object apply(LambdaJ intp, ConsCell args) { return dec(car(args)); } },

        sSignum("signum", Features.HAVE_NUMBERS, true, 1)    { @Override Object apply(LambdaJ intp, ConsCell args) { return cl_signum(car(args));} },

        sRound("round", Features.HAVE_NUMBERS, 1, 2)         { @Override Object apply(LambdaJ intp, ConsCell args) { return toFixnum(Math.rint  (quot12("round", args))); } },
        sFloor("floor", Features.HAVE_NUMBERS, 1, 2)         { @Override Object apply(LambdaJ intp, ConsCell args) { return toFixnum(Math.floor (quot12("floor", args))); } },
        sCeiling("ceiling", Features.HAVE_NUMBERS, 1, 2)     { @Override Object apply(LambdaJ intp, ConsCell args) { return toFixnum(Math.ceil  (quot12("ceiling", args))); } },
        sTruncate("truncate", Features.HAVE_NUMBERS, 1, 2)   { @Override Object apply(LambdaJ intp, ConsCell args) { return toFixnum(cl_truncate(quot12("truncate", args))); } },

        sFRound("fround", Features.HAVE_NUMBERS, 1, 2)       { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.rint  (quot12("round", args)); } },
        sFFloor("ffloor", Features.HAVE_NUMBERS, 1, 2)       { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.floor (quot12("floor", args)); } },
        sFCeiling("fceiling", Features.HAVE_NUMBERS, 1, 2)   { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.ceil  (quot12("ceiling", args)); } },
        sFTruncate("ftruncate", Features.HAVE_NUMBERS, 1, 2) { @Override Object apply(LambdaJ intp, ConsCell args) { return cl_truncate(quot12("truncate", args)); } },

        sSqrt("sqrt", Features.HAVE_NUMBERS, 1)              { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.sqrt (toDouble("sqrt",  car(args))); } },
        sLog("log", Features.HAVE_NUMBERS, 1, 2)             { @Override Object apply(LambdaJ intp, ConsCell args) { return (cdr(args) == null) ? Math.log(toDouble("log", car(args))) : Math.log(toDouble("log", car(args))) / Math.log(toDouble("log", cadr(args))); } },
        sLog10("log10", Features.HAVE_NUMBERS, 1)            { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.log10(toDouble("log10", car(args))); } },
        sExp("exp", Features.HAVE_NUMBERS, 1)                { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.exp  (toDouble("exp",   car(args))); } },
        sExpt("expt", Features.HAVE_NUMBERS, 2)              { @Override Object apply(LambdaJ intp, ConsCell args) { return Math.pow  (toDouble("expt",  car(args)), toDouble("expt", cadr(args))); } },

        sMod("mod", Features.HAVE_NUMBERS, 2)                { @Override Object apply(LambdaJ intp, ConsCell args) { return cl_mod(toDouble("mod", car(args)), toDouble("mod", cadr(args))); } },
        sRem("rem", Features.HAVE_NUMBERS, 2)                { @Override Object apply(LambdaJ intp, ConsCell args) { return toDouble("rem", car(args)) % toDouble("rem", cadr(args)); } },
        sRandom("random", Features.HAVE_NUMBERS, 1, 2)       { @Override Object apply(LambdaJ intp, ConsCell args) {
            Object state = cadr(args);
            if (state == null) state = intp.getRandom();
            return random(car(args), state);
        } },
        sMakeRandomState("make-random-state", Features.HAVE_NUMBERS, 0, 1) { @Override Object apply(LambdaJ intp, ConsCell args) {
            final Object state = car(args);
            return makeRandomState(state == null ? intp.getRandom() : null, state);
        } },

        // vectors, sequences
        sMakeArray(MAKE_ARRAY, Features.HAVE_VECTOR, 1, 3)             { @Override Object apply(LambdaJ intp, ConsCell args) { return makeArray(intp.sBit, intp.sCharacter, args); } },
        sVectorAdd("vector-add", Features.HAVE_VECTOR, 2)              { @Override Object apply(LambdaJ intp, ConsCell args) { return vectorAdd(car(args), cadr(args)); } },
        sVectorCopy("vector-copy", Features.HAVE_VECTOR, 1, 2)         { @Override Object apply(LambdaJ intp, ConsCell args) { return vectorCopy(car(args), cadr(args) != null); } },
        sVectorFill("vector-fill", Features.HAVE_VECTOR, 2, 4)         { @Override Object apply(LambdaJ intp, ConsCell args) { return vectorFill(car(args), cadr(args), caddr(args), cadddr(args)); } },

        sVectorLength("vector-length", Features.HAVE_VECTOR, 1)        { @Override Object apply(LambdaJ intp, ConsCell args) { return vectorLength(car(args)); } },
        sVectorToList("vector->list", Features.HAVE_VECTOR, 1)         { @Override Object apply(LambdaJ intp, ConsCell args) { return vectorToList(intp, car(args)); } },
        sListToVector("list->vector", Features.HAVE_VECTOR, 1, 2)      { @Override Object apply(LambdaJ intp, ConsCell args) { return listToVector(car(args), cadr(args) != null); } },

        sSvLength("svlength", Features.HAVE_VECTOR, 1)                 { @Override Object apply(LambdaJ intp, ConsCell args) { return svlength(car(args)); } },
        sSvRef("svref", Features.HAVE_VECTOR, 2)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return svref(car(args), toNonnegInt("svref", cadr(args))); } },
        sSvSet("svset", Features.HAVE_VECTOR, 3)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return svset(car(args), toNonnegInt("svset", cadr(args)), caddr(args)); } },
        sSVectorToList("simple-vector->list", Features.HAVE_VECTOR, 1) { @Override Object apply(LambdaJ intp, ConsCell args) { return simpleVectorToList(intp, car(args)); } },
        sListToSVector("list->simple-vector", Features.HAVE_VECTOR, 1) { @Override Object apply(LambdaJ intp, ConsCell args) { return listToArray(car(args)); } },
        sVector(VECTOR, Features.HAVE_VECTOR, -1)                      { @Override Object apply(LambdaJ intp, ConsCell args) { return listToArray(args); } },
        sVect(VECT, Features.HAVE_VECTOR, 1, -1)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return listToArray(requireList(VECT, cdr(args)), toInt(VECT, car(args))); } },

        sString("string", Features.HAVE_STRING, 1)                     { @Override Object apply(LambdaJ intp, ConsCell args) { return stringDesignatorToString(car(args)); } },
        sSLength("slength", Features.HAVE_STRING, 1)                   { @Override Object apply(LambdaJ intp, ConsCell args) { return slength(car(args)); } },
        sSRef("sref", Features.HAVE_STRING, 2)                         { @Override Object apply(LambdaJ intp, ConsCell args) { return sref(car(args), toNonnegInt("sref", cadr(args))); } },
        sSSet("sset", Features.HAVE_STRING, 3)                         { @Override Object apply(LambdaJ intp, ConsCell args) { return sset(car(args), toNonnegInt("sset", cadr(args)), requireChar("sset", caddr(args))); } },
        sSEq("string=", Features.HAVE_STRING, 2)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(stringEq(car(args), cadr(args))); } },
        sStringToList("string->list", Features.HAVE_STRING, 1)         { @Override Object apply(LambdaJ intp, ConsCell args) { return stringToList(intp, car(args)); } },
        sListToString("list->string", Features.HAVE_STRING, 1, 2)      { @Override Object apply(LambdaJ intp, ConsCell args) { return listToString(car(args), cadr(args) != null); } },

        sCharCode("char-code", Features.HAVE_STRING, 1)                { @Override Object apply(LambdaJ intp, ConsCell args) { return (long) requireChar("char-code", car(args)); } },
        sCodeChar("code-char", Features.HAVE_STRING, 1)                { @Override Object apply(LambdaJ intp, ConsCell args) { return (char) toInt("code-char", car(args)); } },

        sBvLength("bvlength", Features.HAVE_VECTOR, 1)                 { @Override Object apply(LambdaJ intp, ConsCell args) { return bvlength(car(args)); } },
        sBvRef("bvref", Features.HAVE_VECTOR, 2)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return bvref(car(args), toNonnegInt("bvref", cadr(args))); } },
        sBvSet("bvset", Features.HAVE_VECTOR, 3)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return bvset(car(args), toNonnegInt("bvset", cadr(args)), requireIntegralNumber("bvset", caddr(args), 0, 1).longValue()); } },
        sBvEq("bv=", Features.HAVE_VECTOR, 2)                          { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(bvEq(car(args), cadr(args))); } },
        sBvToList("bit-vector->list", Features.HAVE_VECTOR, 1)         { @Override Object apply(LambdaJ intp, ConsCell args) { return bitVectorToList(intp, car(args)); } },
        sListToBv("list->bit-vector", Features.HAVE_VECTOR, 1, 2)      { @Override Object apply(LambdaJ intp, ConsCell args) { return listToBitVector(car(args), cadr(args) != null); } },

        sSeqRef("seqref", Features.HAVE_VECTOR, 2)                     { @Override Object apply(LambdaJ intp, ConsCell args) { return seqref(car(args), toNonnegInt("seqref", cadr(args))); } }, // todo nicht auf int begrenzen wg. list
        sSeqSet("seqset", Features.HAVE_VECTOR, 3)                     { @Override Object apply(LambdaJ intp, ConsCell args) { return seqset(car(args), toNonnegInt("seqset", cadr(args)), caddr(args)); } }, // todo nicht auf int begrenzen wg. list

        // Hash tables
        sHash(HASH, Features.HAVE_HASH, -1)                            { @Override Object apply(LambdaJ intp, ConsCell args) { return hash(intp.getSymbolTable(), args); } },
        sMakeHash(MAKE_HASH_TABLE, Features.HAVE_HASH, 0, 2)           { @Override Object apply(LambdaJ intp, ConsCell args) { return makeHashTable(intp.getSymbolTable(), car(args), cadr(args) == null ? DEFAULT_HASH_SIZE : toNonnegInt(MAKE_HASH_TABLE, cadr(args))); } },
        sHashRef("hashref", Features.HAVE_HASH, 2, 3)                  { @Override Object apply(LambdaJ intp, ConsCell args) { final Object[] ret = hashref(car(args), cadr(args), cddr(args) == null ? NO_DEFAULT_VALUE : caddr(args)); intp.values = intp.cons(ret[0], intp.cons(ret[1], null)); return ret[0]; } },
        sHashSet("hashset", Features.HAVE_HASH, 2, 3)                  { @Override Object apply(LambdaJ intp, ConsCell args) { return hashset(args); } },
        sHashTableCount("hash-table-count", Features.HAVE_HASH, 1)     { @Override Object apply(LambdaJ intp, ConsCell args) { return hashTableCount(car(args)); } },
        sClrHash("clrhash", Features.HAVE_HASH, 1)                     { @Override Object apply(LambdaJ intp, ConsCell args) { return clrhash(car(args)); } },
        sHashRemove("hash-table-remove", Features.HAVE_HASH, 1, 2)     { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.boolResult(hashRemove(args)); } },
        sSxHash("sxhash", Features.HAVE_HASH, 1)                       { @Override Object apply(LambdaJ intp, ConsCell args) { return sxhash(car(args)); } },
        sScanHash("scan-hash-table", Features.HAVE_HASH, 1)            { @Override Object apply(LambdaJ intp, ConsCell args) { return scanHash(intp, car(args)); } },

        // I/O
        sRead("read", Features.HAVE_IO, 0, 1)                          { @Override Object apply(LambdaJ intp, ConsCell args) { return read(intp.getLispReader(), args); } },
        sReadFromString("read-from-string", Features.HAVE_IO, 1, 4)    { @Override Object apply(LambdaJ intp, ConsCell args) { final Object[] ret = readFromString(intp.getSymbolTable(), intp.featuresEnvEntry, args); intp.values = intp.cons(ret[0], intp.cons(ret[1], null)); return ret[0]; } },
        sReadallLines("read-textfile-lines", Features.HAVE_IO, 1, 2)   { @Override Object apply(LambdaJ intp, ConsCell args) { return readTextfileLines(args); } },
        sReadString("read-textfile", Features.HAVE_IO, 1, 2)           { @Override Object apply(LambdaJ intp, ConsCell args) { return readTextfile(args); } },
        sWriteLines("write-textfile-lines", Features.HAVE_IO, 2, 4)    { @Override Object apply(LambdaJ intp, ConsCell args) { return writeTextfileLines(args); } },
        sWriteString("write-textfile", Features.HAVE_IO, 2, 4)         { @Override Object apply(LambdaJ intp, ConsCell args) { return writeTextfile(args); } },
        sWriteToString("write-to-string", Features.HAVE_IO, 1, 2)      { @Override Object apply(LambdaJ intp, ConsCell args) { return writeToString(car(args), cdr(args) == null || cadr(args) != null); } },
        sWrite("write", Features.HAVE_IO, 1, 3)                        { @Override Object apply(LambdaJ intp, ConsCell args) { return write(intp.getLispPrinter(caddr(args), intp.getLispPrinter()), car(args), cdr(args) == null || cadr(args) != null); } },
        sWriteln("writeln", Features.HAVE_IO, 0, 3)                    { @Override Object apply(LambdaJ intp, ConsCell args) { return writeln(intp.getLispPrinter(caddr(args), intp.getLispPrinter()), args, cdr(args) == null || cadr(args) != null); } },
        sLnwrite("lnwrite", Features.HAVE_IO, 0, 3)                    { @Override Object apply(LambdaJ intp, ConsCell args) { return lnwrite(intp.getLispPrinter(caddr(args), intp.getLispPrinter()), args, cdr(args) == null || cadr(args) != null); } },
        sFormat("format", Features.HAVE_UTIL, 2, -1)                   { @Override Object apply(LambdaJ intp, ConsCell args) { return format(intp.getLispPrinter(car(args), null), intp.have(Features.HAVE_IO), args); } },
        sFormatLocale("format-locale", Features.HAVE_UTIL,3,-1)        { @Override Object apply(LambdaJ intp, ConsCell args) { return formatLocale(intp.getLispPrinter(car(args), null), intp.have(Features.HAVE_IO), args); } },

        // misc
        sValues(VALUES, Features.HAVE_XTRA, -1)                        { @Override Object apply(LambdaJ intp, ConsCell args) { if (args != null && cdr(args) == null) intp.values = NO_VALUES; else intp.values = args; return car(args); } },
        sGensym("gensym", Features.HAVE_XTRA, 0, 1)                    { @Override Object apply(LambdaJ intp, ConsCell args) { return gensym(car(args)); } },
        sTrace("trace", Features.HAVE_XTRA, -1)                        { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.trace(args); } },
        sUntrace("untrace", Features.HAVE_XTRA, -1)                    { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.untrace(args); } },
        sMacroexpand1("macroexpand-1", Features.HAVE_XTRA, 1)          { @Override Object apply(LambdaJ intp, ConsCell args) { return macroexpand1(intp, args); } },
        sError(ERROR, Features.HAVE_UTIL, 1, -1)                       { @Override Object apply(LambdaJ intp, ConsCell args) { error(intp.typeSpecs(), car(args), listToArray(cdr(args))); return null; } },
        sImplType("lisp-implementation-type", Features.HAVE_UTIL, 0)   { @Override Object apply(LambdaJ intp, ConsCell args) { return "JMurmel"; } },
        sImplVersion("lisp-implementation-version", Features.HAVE_UTIL, 0) { @Override Object apply(LambdaJ intp, ConsCell args) { return ENGINE_VERSION_NUM; } },

        // time
        sRealtime("get-internal-real-time", Features.HAVE_UTIL, 0)     { @Override Object apply(LambdaJ intp, ConsCell args) { return getInternalRealTime(); } },
        sRuntime("get-internal-run-time", Features.HAVE_UTIL, 0)       { @Override Object apply(LambdaJ intp, ConsCell args) { return getInternalRunTime(); } }, // user + system
        sUniversalTime("get-universal-time", Features.HAVE_UTIL, 0)    { @Override Object apply(LambdaJ intp, ConsCell args) { return getUniversalTime(); } },   // seconds since 1.1.1900
        sSleep("sleep", Features.HAVE_UTIL, 1)                         { @Override Object apply(LambdaJ intp, ConsCell args) { return sleep(car(args)); } },
        sDecodedTime("get-decoded-time", Features.HAVE_UTIL, 0)        { @Override Object apply(LambdaJ intp, ConsCell args) { return getDecodedTime(intp.new CountingListBuilder(), intp::boolResult); } },

        // Java FFI
        sJmethod(JMETHOD,   Features.HAVE_FFI, false, 2, -1)     { @Override Object apply(LambdaJ intp, ConsCell args) { return JFFI.findMethod(requireString(JMETHOD, car(args)), requireString(JMETHOD, cadr(args)), requireList(JMETHOD, cddr(args))); } },
        sJproxy("jproxy",   Features.HAVE_FFI, 3, -1)            { @Override Object apply(LambdaJ intp, ConsCell args) { return JFFI.makeProxy(intp, intp.compiledProgram, args); } },

        // Turtle graphics
        sMakeFrame("make-frame", Features.HAVE_GUI, 1, 4)        { @Override Object apply(LambdaJ intp, ConsCell args) { final String title = requireString("make-frame", car(args));
                                                                                                                         final TurtleFrame ret = new TurtleFrame(title, requireNumberOrNull("make-frame", cadr(args)), requireNumberOrNull("make-frame", caddr(args)), requireNumberOrNull("make-frame", cadddr(args)));
                                                                                                                         intp.current_frame = ret;
                                                                                                                         return ret; } },
        sOpenFrame("open-frame", Features.HAVE_GUI, 0, 1)        { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("open-frame",    car(a)).open();    } },
        sCloseFrame("close-frame", Features.HAVE_GUI, 0, 1)      { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("close-frame",   car(a)).close();   } },
        sResetFrame("reset-frame", Features.HAVE_GUI, 0, 1)      { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("reset-frame",   car(a)).reset();   } },
        sClearFrame("clear-frame", Features.HAVE_GUI, 0, 1)      { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("clear-frame",   car(a)).clear();   } },
        sRepaintFrame("repaint-frame", Features.HAVE_GUI, 0, 1)  { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("repaint-frame", car(a)).repaint(); } },
        sFlushFrame("flush-frame", Features.HAVE_GUI, 0, 1)      { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("flush-frame",   car(a)).flush();   } },

        // set new current frame, return previous frame
        sCurrentFrame("current-frame", Features.HAVE_GUI, 0, 1)  { @Override Object apply(LambdaJ intp, ConsCell a) { final Object prev = intp.current_frame; if (car(a) != null) intp.current_frame = intp.requireFrame("current-frame", car(a)); return prev; } },

        sPushPos("push-pos", Features.HAVE_GUI, 0, 1)            { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("push-pos",car(a)).pushPos(); } },
        sPopPos("pop-pos", Features.HAVE_GUI, 0, 1)              { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("pop-pos", car(a)).popPos();  } },

        sPenUp("pen-up", Features.HAVE_GUI, 0, 1)                { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("pen-up",   car(a)).penUp();   } },
        sPenDown("pen-down", Features.HAVE_GUI, 0, 1)            { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("pen-down", car(a)).penDown(); } },

        sColor("color", Features.HAVE_GUI, 1, 2)                 { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("color",   cadr(a)).color  (toInt("color",   car(a))); } },
        sBgColor("bgcolor", Features.HAVE_GUI, 1, 2)             { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("bgcolor", cadr(a)).bgColor(toInt("bgcolor", car(a))); } },

        sText("text", Features.HAVE_GUI, 1, 2)                   { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("text",    cadr(a)).text   (car(a).toString()); } },

        sRight("right", Features.HAVE_GUI, 1, 2)                 { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("right",   cadr(a)).right  (toDouble("right",   car(a))); } },
        sLeft("left", Features.HAVE_GUI, 1, 2)                   { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("left",    cadr(a)).left   (toDouble("left",    car(a))); } },
        sForward("forward", Features.HAVE_GUI, 1, 2)             { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("forward", cadr(a)).forward(toDouble("forward", car(a))); } },

        sMoveTo("move-to", Features.HAVE_GUI, 2, 3)              { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("move-to",  caddr(a)).moveTo(toDouble("move-to",  car(a)), toDouble("move-to", cadr(a)));  } },
        sLineTo("line-to", Features.HAVE_GUI, 2, 3)              { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("line-to",  caddr(a)).lineTo(toDouble("line-to",  car(a)), toDouble("line-to", cadr(a)));  } },
        sMoveRel("move-rel", Features.HAVE_GUI, 2, 3)            { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("move-rel", caddr(a)).moveRel(toDouble("move-rel", car(a)), toDouble("move-rel", cadr(a))); } },
        sLineRel("line-rel", Features.HAVE_GUI, 2, 3)            { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("line-rel", caddr(a)).lineRel(toDouble("line-rel", car(a)), toDouble("line-rel", cadr(a))); } },

        sMakeBitmap("make-bitmap",   Features.HAVE_GUI, 2, 3)    { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("make-bitmap",    caddr(a)).makeBitmap(toInt("make-bitmap",  car(a)), toInt("make-bitmap", cadr(a))); } },
        sDiscardBitmap("discard-bitmap",Features.HAVE_GUI, 0, 1) { @Override Object apply(LambdaJ intp, ConsCell a) { return intp.requireFrame("discard-bitmap", car(a)).discardBitmap(); } },

        sSetPixel("set-pixel", Features.HAVE_GUI, 3, 4)          { @Override Object apply(LambdaJ intp, ConsCell args) { return intp.requireFrame("set-pixel", cadddr(args)).setRGB(toInt("set-pixel", car(args)), toInt("set-pixel", cadr(args)), toInt("set-pixel", caddr(args))); } },
        sRgbToPixel("rgb-to-pixel", Features.HAVE_GUI, 3)        { @Override Object apply(LambdaJ intp, ConsCell args) { //noinspection RedundantCast
                                                                                                                         return (long)(int)(toInt("rgb-to-pixel", car(args)) << 16
                                                                                                                                            | toInt("rgb-to-pixel", cadr(args)) << 8
                                                                                                                                            | toInt("rgb-to-pixel", caddr(args))); } },
        sHsbToPixel("hsb-to-pixel", Features.HAVE_GUI, 3)        { @Override Object apply(LambdaJ intp, ConsCell args) { return (long)Color.HSBtoRGB(toFloat("hsb-to-pixel", car(args)),
                                                                                                                                                     toFloat("hsb-to-pixel", cadr(args)),
                                                                                                                                                     toFloat("hsb-to-pixel", caddr(args))); } },

        ;

        final WellknownSymbolKind kind;
        final String sym;

        private final int min, max;
        private final Features feature;
        public final boolean stmtExpr;       // true for primitives that will be emitted by the compiler as a stmt expression, i.e. preceeding "values = null;" or "result =" is not needed
        public final boolean singleValues;   // true for primitives that will be emitted by the compiler to clear multiple values

        WellknownSymbol(String sym, WellknownSymbolKind kind) {
            assert kind != WellknownSymbolKind.PRIM;
            this.sym = sym; this.kind = kind; min = max = -2;
            feature = null;
            stmtExpr = false;
            singleValues = false;
        }

        WellknownSymbol(String sym, Features feature, int nArgs) {
            this(sym, feature, feature != Features.HAVE_NUMBERS, nArgs);
        }

        WellknownSymbol(String sym, Features feature, boolean stmtExpr, int nArgs) {
            assert nArgs >= -1 && nArgs <= 3;
            this.sym = sym; this.kind = WellknownSymbolKind.PRIM; min = max = nArgs;
            this.feature = feature;
            this.stmtExpr = stmtExpr;
            singleValues = stmtExpr && !isMv(sym);
        }

        WellknownSymbol(String sym, Features feature, int minArgs, int maxArgs) {
            this(sym, feature, feature != Features.HAVE_NUMBERS, minArgs, maxArgs);
        }

        WellknownSymbol(String sym, Features feature, boolean stmtExpr, int minArgs, int maxArgs) {
            assert minArgs >= 0;
            this.sym = sym; this.kind = WellknownSymbolKind.PRIM; min = minArgs; max = maxArgs;
            this.feature = feature;
            this.stmtExpr = stmtExpr;
            singleValues = stmtExpr && !isMv(sym);
        }

        private static boolean isMv(String sym) {
            return "hashref".equals(sym) || "macroexpand-1".equals(sym) || "read-from-string".equals(sym) || "values".equals(sym);
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
                default: assert false: "minimum argcount " + min + " is not implemented";
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

        private static final Map<String, WellknownSymbol> valuesBySymbolName;
        static {
            final WellknownSymbol[] values = values();
            final HashMap<String, WellknownSymbol> m = JavaUtil.newHashMap(values.length);
            for (WellknownSymbol s: values) {
                m.put(s.sym, s);
            }
            valuesBySymbolName = m;
        }

        /** case sensitive lookup because it's faster, and this should only used from Java code during initialisation with the correct case */
        static @NotNull WellknownSymbol of(@NotNull String name) {
            final WellknownSymbol ret = valuesBySymbolName.get(name);
            if (ret == null) throw errorInternal("Wellknown symbol %s not found", name);
            return ret;
        }
    }

    private @NotNull Supplier<Object> expTrue;

    private Object makeExpTrue() {
        if (have(Features.HAVE_T)) return sT; // should look up the symbol t in the env and use it's value (which by convention is t so it works either way)
        else if (have(Features.HAVE_QUOTE)) return cons(intern(QUOTE), cons(sT, null));
        else throw new UnboundVariable("truthiness needs support for '" + T + "' or '" + QUOTE + "'");
    }

    final @NotNull LambdaJSymbol intern(@NotNull String sym) {
        return symtab.intern(sym);
    }

    final @NotNull LambdaJSymbol internWellknown(@NotNull String sym) {
        final LambdaJSymbol ret = symtab.intern(new LambdaJSymbol(sym, true));
        assert ret.wellknownSymbol != WellknownSymbol.interned : "cannot intern wellknown symbol " + sym + ": was already interned as regular symbol";
        return ret;
    }

    private static final class OpenCodedPrimitive implements Writeable {
        private final @NotNull LambdaJSymbol symbol;

        private OpenCodedPrimitive(@NotNull LambdaJSymbol symbol) { this.symbol = symbol; }

        @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
        @Override public String toString() { return "#<opencoded primitive: " + symbol + '>'; }
    }
    private OpenCodedPrimitive ocEval, ocApply;



    /// ### Global environment - define'd symbols go into this map

    final Map<Object, ConsCell> globals = new IdentityHashMap<>(200);

    private ConsCell lookupEnvEntry(@NotNull Object symbol, ConsCell lexenv) {
        final ConsCell lexEntry = fastassq(symbol, lexenv);
        if (lexEntry != null) return lexEntry;
        return lookupGlobalEntry(symbol);
    }

    private ConsCell lookupGlobalEntry(@NotNull Object symbol) {
        return globals.get(symbol);
    }

    final void extendGlobal(@NotNull Object sym, Object value) {
        globals.put(sym, cons(sym, value));
    }

    final void extendGlobal(@NotNull ConsCell envEntry) {
        globals.put(car(envEntry), envEntry);
    }

    void extendGlobal(@NotNull String sym, Object value) {
        extendGlobal(intern(sym), value);
    }


    /** Build environment, setup Lisp reader and writer.
     *  Needs to be called once before {@link #eval(Object, ConsCell, int, int, int)} */
    ObjectReader init(@NotNull ReadSupplier in, @NotNull WriteConsumer out) {
        final SExpressionReader parser = new SExpressionReader(features, trace, tracer, symtab, featuresEnvEntry, in, null);
        final ObjectWriter outWriter = makeWriter(out);
        return init(parser, outWriter, null);
    }

    ObjectReader init(ObjectReader inReader, ObjectWriter outWriter, ConsCell customEnv) {
        speed = 1;  debug = 3;
        resetCounters();
        clearMacros();
        modules.clear();
        handlers = null;
        setReaderPrinter(inReader, outWriter);
        globals.clear();
        if (customEnv != null) for (Object o: customEnv) {
            globals.put(car(o), (ConsCell)o);
        }
        featuresEnvEntry.rplacd(makeFeatureList(symtab));
        if (have(Features.HAVE_XTRA)) {
            assert conditionHandlerEnvEntry != null : "when feature XTRA is enabled conditionHandlerEnvEntry should be != null";
            conditionHandlerEnvEntry.rplacd(null);
        }
        if (have(Features.HAVE_NUMBERS)) {
            assert randomStateEnvEntry != null : "when feature NUMBERs is enabled randomStateEnvEntry should be != null";
            randomStateEnvEntry.rplacd(null);
        }
        environment();
        return inReader;
    }

    void clearMacros() {
        for (LambdaJSymbol entry: symtab) {
            if (entry != null) entry.macro = null;
        }
    }

    final Set<Object> modules = new HashSet<>();
    /** will be set to 1 by {@link #init}, changed by (declaim (optimize (speed... */
    short speed = -1, debug = -1;


    /// ###  eval - the heart of most if not all Lisp interpreters

    private Object eval(Object form, ConsCell env, int stack, int level, int traceLvl) {
        final boolean doOpencode = speed >= 1;
        Object func = null;
        Object result = null;  /* should be assigned even if followed by a "return" because it will be used in the "finally" clause */
        Deque<Object> traceStack = null;
        ConsCell restore = null;
        ConsCell localCatchTags = null;
        boolean isTc = false;
        try {
            stack++;

            tailcall:
            while (true) {
                /// eval - lookup symbols in the current environment
                if (symbolp(form)) { result = evalSymbol(form, env); break tailcall; }

                /// eval - atoms that are not symbols eval to themselves
                if (atom(form)) { result = form; break tailcall; }

                if (Thread.interrupted()) throw new InterruptedException("got interrupted");

                level++;
                if (traceOn) dbgEvalStart(isTc ? "eval TC" : EVAL, form, env, stack, level);

                /// eval - form is not an atom - must be a cons (nonempty list) containing either a special form or a function application
                final ConsCell ccForm = (ConsCell)form;

                // first element of the of the form should be a symbol that evals to a funtion or lambda
                // or an expression that evals to a funtion or lambda
                final Object operator = car(ccForm);
                assert operator != null && operator != sNil : "not a function: nil - should have been caught by expandForm()";

                final ConsCell ccArguments = (ConsCell)cdr(ccForm);   // list with remaining atoms/ expressions

                final boolean funcall;
                ConsCell ccForms = null;

                ConsCell argList = null;

                final LambdaJSymbol symOperator; // will be the car of the form as a LambdaJSymbol if it is a symbol, null otherwise
                if (symbolp(operator)) special_forms: switch ((symOperator = (LambdaJSymbol)operator).wellknownSymbol) {
                /// eval - special forms

                /// eval - (quote exp) -> exp
                case sQuote:
                case sDefmacro: {
                    result = car(ccArguments);  break tailcall;
                }

                /// eval - (lambda dynamic? (params...) forms...) -> lambda or closure
                case sLambda: {
                    final ConsCell ccParamsAndForms = (ConsCell)cdr(ccForm);
                    nCells++;
                    result = Closure.of(car(ccParamsAndForms), (ConsCell)cdr(ccParamsAndForms), env);  break tailcall;
                }

                case sLambdaDynamic: {
                    final ConsCell ccParamsAndForms = (ConsCell)cdr(ccForm);
                    nCells++;
                    result = new DynamicLambda(car(ccParamsAndForms), (ConsCell)cdr(ccParamsAndForms));  break tailcall;
                }

                case sSetQ: {
                    result = evalSetq(ccArguments, env, stack, level, traceLvl);  break tailcall;
                }

                case sDeclaim: {
                    evalDeclaim(level, ccArguments);  result = null;  break tailcall;
                }


                /// eval - special forms that change the global environment

                /// eval - (define symbol exp) -> symbol with a side of global environment extension
                case sDefine: {
                    result = evalDefine(ccArguments, env, stack, level, traceLvl);  break tailcall;
                }

                /// eval - (defun symbol (params...) forms...) -> symbol with a side of global environment extension
                // shortcut for (define symbol (lambda (params...) forms...)) with one difference: defun will early bind recursive invocations
                case sDefun: {
                    result = evalDefun(ccArguments, env);  break tailcall;
                }


                /// eval - special forms that run expressions

                /// eval - (load filespec) -> object
                case sLoad: {
                    result = loadFile(LOAD, car(ccArguments));  break tailcall;
                }

                /// eval - (require modulename optfilespec) -> object
                case sRequire: {
                    result = evalRequire(ccArguments);  break tailcall;
                }

                /// eval - (provide modulename) -> nil
                case sProvide: {
                    evalProvide(ccArguments);  result = null;  break tailcall;
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

                case sTry: {
                    final Object oldHandler = cdr(conditionHandlerEnvEntry);
                    conditionHandlerEnvEntry.rplacd(null);
                    try {
                        result = eval(car(ccArguments), env, stack, level, traceLvl);  break tailcall;
                    }
                    catch (ReturnException e) { throw e; }
                    catch (Exception e) {
                        final Object errorObj = eval(cadr(ccArguments), env, stack, level, traceLvl);
                        values = list(errorObj, e);
                        result = errorObj;  break tailcall;
                    }
                    finally { conditionHandlerEnvEntry.rplacd(oldHandler); }
                }

                /// eval - (cond (condform forms...)... ) -> object
                case sCond: {
                    for (ConsCell l = ccArguments; l != null; l = (ConsCell)cdr(l)) {
                        final ConsCell clause = (ConsCell)car(l);
                        if (eval(car(clause), env, stack, level, traceLvl) != null) {
                            ccForms = (ConsCell) cdr(clause);
                            funcall = false;
                            break special_forms; // fall through to "eval a list of forms"
                        }
                    }
                    result = null;  break tailcall;
                }

                /// eval - (if condform form optionalform) -> object
                case sIf: {
                    if (eval(car(ccArguments), env, stack, level, traceLvl) != null) form = cadr(ccArguments);
                    else form = caddr(ccArguments);
                    values = NO_VALUES;
                    if (form == null) { result = null;  break tailcall; }
                    isTc = true; continue tailcall;
                }


                /// eval - (labels ((symbol (params...) forms...)...) forms...) -> object
                case sLabels: {
                    assert car(ccArguments) != null : "labels w/o local functions should have been transformed to progn";
                    env = evalLabels((ConsCell)car(ccArguments), env);
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
                    final LetRetVal formsAndEnv = evalLet(symOperator.wellknownSymbol, ccArguments, env, restore, stack, level, traceLvl);
                    ccForms = formsAndEnv.body;  env = formsAndEnv.env;  restore = formsAndEnv.restore;
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (multiple-value-bind (symbols...) values-form bodyforms...) -> object
                case sMultipleValueBind: {
                    env = evalMultipleValueBind(ccArguments, env, stack, level, traceLvl);
                    ccForms = (ConsCell)cddr(ccArguments);
                    funcall = false;
                    break; // fall through to "eval a list of forms"
                }

                /// eval - (multiple-value-call function-form values-form*) -> object
                case sMultipleValueCall: {
                    final Object funcOrSymbol = car(ccArguments);
                    func = eval(funcOrSymbol, env, stack, level, traceLvl); // could add the same performance cheat as in function call below
                    argList = evalMultipleValuesArgs(cdr(ccArguments), env, stack, level, traceLvl);
                    if (doOpencode && funcOrSymbol instanceof LambdaJSymbol && ((LambdaJSymbol)funcOrSymbol).primitive()) {
                        result = ((LambdaJSymbol)funcOrSymbol).wellknownSymbol.apply(this, argList);  break tailcall;
                    }
                    funcall = true;
                    break; // fall through to "actually perform..."
                }

                /// eval - function call
                /// eval - (operatorform argforms...) -> object
                default: {
                    // check if we forgot to handle a special form. All special forms should be handled in the cases above.
                    assert !symOperator.specialForm() : "unexpected special form " + symOperator;

                    // check if expandForm() has expanded all macros and make sure that expandForm() is used prior to any eval() call with a form that may contain macro calls
                    // macros can be unexpanded if the macro was defined after the defun
                    if (symOperator.macro != null) errorNotAFunction("function application: not a primitive or " + LAMBDA + ": %s is a macro not a function", symOperator.toString());

                    if (doOpencode && symOperator.primitive()) {
                        result = symOperator.wellknownSymbol.apply(this, evlis(ccArguments, env, stack, level, traceLvl));  break tailcall;
                    }

                    // respect evaluation order: the operator could be an undefined symbol, and we want that to fail before evaluation the arguments.
                    // E.g. if "when" was not defined as a macro then "(when (< i 10) (loop (1+ i)))" should fail and not make an endless recursion.
                    func = evalSymbol(symOperator, env);
                    argList = evlis(ccArguments, env, stack, level, traceLvl);

                    funcall = true;
                    // fall through to "actually perform..."
                }
                }
                else {
                    /// eval - apply a function to an argument list
                    /// eval - (apply form argform) -> object
                    if (operator == ocApply) {
                        twoArgs(APPLY, ccArguments);
                        final Object funcOrSymbol = car(ccArguments);
                        func = symbolp(funcOrSymbol) ? evalSymbol(funcOrSymbol, env) : funcOrSymbol; // could add the same performance cheat as in function call above
                        argList = listOrMalformed(APPLY, cadr(ccArguments));
                        if (doOpencode && funcOrSymbol instanceof LambdaJSymbol && ((LambdaJSymbol)funcOrSymbol).primitive()) {
                            result = ((LambdaJSymbol)funcOrSymbol).wellknownSymbol.apply(this, argList);  break tailcall;
                        }
                        // fall through to "actually perform..."
                    }

                    /// eval - (eval form) -> object
                    else if (operator == ocEval) {
                        varargs1_2(EVAL, ccArguments);
                        form = expandForm(car(ccArguments));
                        if (cdr(ccArguments) == null) env = null;
                        else {
                            final Object additionalEnv = cadr(ccArguments);
                            if (!listp(additionalEnv)) errorMalformed(EVAL, "'env' to be a list", additionalEnv);
                            env = (ConsCell)additionalEnv;
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
                    if (func instanceof Primitive) {
                        result = applyPrimitive((Primitive) func, argList, stack, level);  break tailcall;
                    }

                    else if (func instanceof Closure) {
                        final Closure ccFunc = (Closure)func;
                        env = ccFunc.zip(this, argList, env);

                        if (traceFunc)  tracer.println(pfx(stack, level) + " #<" + LAMBDA + " " + ccFunc.params() + "> " + printSEx(argList));
                        ccForms = ccFunc.body;
                        // fall through to "eval a list of forms"
                    }

                    else if (func instanceof OpenCodedPrimitive) {
                        form = cons(func, argList);
                        if (traced != null) traceStack = push(func, traceStack);
                        func = null;
                        isTc = true; continue tailcall;
                    }

                    else if (func instanceof MurmelJavaProgram.CompilerPrimitive) {
                        // compiler runtime func
                        result = applyCompilerPrimitive((MurmelJavaProgram.CompilerPrimitive) func, argList, stack, level);  break tailcall;
                    }

                    else if (func instanceof MurmelFunction) {
                        // compiled function
                        result = applyCompiledFunction((MurmelFunction) func, argList, stack, level);  break tailcall;
                    }

                    /* something like
                         (define l '(lambda () 'hello))
                         (l)
                       would end up here. That was legal in CLtL1 and was made illegal in Common Lisp, and wouldn't work in compiled Murmel,
                       nor would something similar work in Common Lisp (see "Issue FUNCTION-TYPE Writeup" http://www.lispworks.com/documentation/lw71/CLHS/Issues/iss175_w.htm).
                     */
                    else if (func instanceof ConsCell && car(func) == sLambda && have(Features.HAVE_OLDLAMBDA)) {
                        final Object paramsAndBody = cdr(func);
                        env = zip("old " + LAMBDA + " application", car(paramsAndBody), argList, env, true);

                        if (traceFunc)  tracer.println(pfx(stack, level) + " #<list " + LAMBDA + " " + paramsAndBody + "> " + printSEx(argList));
                        ccForms = (ConsCell) cdr(paramsAndBody);
                        // fall through to "eval a list of forms"
                    }

                    else {
                        errorNotAFunction("function application: not a primitive or " + LAMBDA + ": %s", printSEx(func));
                    }
                }

                /// eval - eval a list of forms
                if (ccForms == null) {
                    result = null;  break tailcall;  // lambda/ progn/ labels/... w/o body
                }

                for (; consp(cdr(ccForms)); ccForms = (ConsCell)cdr(ccForms)) {
                    eval(car(ccForms), env, stack, level, traceLvl);
                }
                assert cdr(ccForms) == null : "dotted list of forms is illegal";
                if (traced != null) traceStack = push(operator, traceStack);
                form = car(ccForms); func = null; values = NO_VALUES; isTc = true;
            }
            return result;
        }

        catch (ReturnException re) { return result = nonlocalReturn(re, localCatchTags); }
        catch (Exception e) {
            if (have(Features.HAVE_UTIL)) {
                final Object handler = cdr(conditionHandlerEnvEntry);
                if (functionp(handler)) {
                    conditionHandlerEnvEntry.rplacd(prev());
                    try { eval(list(handler, e), env, stack, level, traceLvl); }
                    catch (ReturnException re) { return result = nonlocalReturn(re, localCatchTags); }
                    finally { conditionHandlerEnvEntry.rplacd(handler); }
                }
            }
            if (e instanceof InterruptedException) Thread.currentThread().interrupt(); // wenn der conditionhandler ein nonlocal return macht, geht das verschütt
            throw new LambdaJError(e, false, e.getMessage(), form);
        }
        finally {
            if (traceOn) dbgEvalDone(isTc ? "eval TC" : EVAL, form, env, stack, level);
            traceLvl = cleanupTrace(traceLvl, func, result, traceStack);
            final ReturnException e = cleanup(env, stack, level, traceLvl, restore);
            if (e != null) return nonlocalReturn(e, localCatchTags);
        }
    }

    private int cleanupTrace(int traceLvl, Object func, Object result, Deque<Object> traceStack) {
        if (func != null && traced != null) traceLvl = traceExit(func, result, traceLvl);
        if (traceStack != null) {
            Object s;
            while ((s = traceStack.pollLast()) != null) traceLvl = traceExit(s, result, traceLvl);
        }
        return traceLvl;
    }

    private @Null ReturnException cleanup(ConsCell env, int stack, int level, int traceLvl, ConsCell restore) {
        LambdaJError e = null;
        for (ConsCell c = restore; c != null; c = (ConsCell) cdr(c)) {
            final Object o = car(c);
            if (o instanceof RestoreDynamic) ((RestoreDynamic)o).restore();
            else {
                try { eval(o, env, stack, level, traceLvl); }
                catch (LambdaJError le) { e = le; }
            }
        }
        if (e instanceof ReturnException) return (ReturnException)e;
        if (e != null) throw e;
        return null;
    }

    private @NotNull Object nonlocalReturn(@NotNull ReturnException re, ConsCell localCatchTags) {
        if (localCatchTags != null) {
            final Object thrownTag = re.tag;
            for (ConsCell i = localCatchTags; i != null; i = (ConsCell)cdr(i)) {
                if (car(i) == thrownTag) { values = re.valuesAsList(); return re.result; }
            }
        }
        throw re;
    }

    final Object eval(Object form, ConsCell env) {
        return eval(form, env, 0, 0, 0);
    }

    final Object expandAndEval(Object form, ConsCell env) {
        if (form == null) return null;
        final Object expansion = expandForm(form);
        if (consp(expansion) && car((ConsCell)expansion) == sProgn) {
            ConsCell rest;
            for (rest = (ConsCell)cdr((ConsCell)expansion); cdr(rest) != null; rest = (ConsCell)cdr(rest)) {
                // must expand a second time in case the progn contained a load/require that contained defmacro
                expandAndEval(car(rest), env);
            }
            values = NO_VALUES;
            return expandAndEval(car(rest), env);
        }
        return eval(expansion, env);
    }

    /** expand all macros within a form and do some syntax checks. Macro-expansion is done in a copy, i.e. form will not be modified. */
    Object expandForm(Object form) {
        try {
            if (form instanceof String)
                return ((String)form).intern();
            if (atom(form)) return form;
            final ConsCell ccForm = ((ConsCell)form).copy();
            final Object op = car(ccForm);
            if (op == null) throw new UndefinedFunction("function application: not a primitive or " + LAMBDA + ": " + NIL);

            if (consp(op)) {
                expandForms("function application", ccForm);
                return ccForm;
            }

            if (!symbolp(op)) errorNotAFunction("function application: not a primitive or " + LAMBDA + ": %s", printSEx(op));
            final LambdaJSymbol symOp = (LambdaJSymbol)op;

            if (!symOp.specialForm()) {
                // not a special form, must be a function or macro application
                if (symOp.macro != null) {
                    final Object expansion = macroexpandImpl(this, ccForm);
                    assert cadr(values) != null : ccForm.lineInfo() + "macro " + symOp + " was not expanded - secondary value is " + NIL + ", form was " + form;
                    assert expansion != ccForm : ccForm.lineInfo() + "macro " + symOp + " was not expanded - expansion == ccForm, form was " + form;
                    values = NO_VALUES;
                    return expandForm(expansion);
                }
                expandForms("function application", ccForm);
                if (symOp.primitive()) symOp.wellknownSymbol.argCheck(listOrMalformed("function application", cdr(ccForm)));
                return ccForm;
            }

            final ConsCell ccArgs = cdrShallowCopyList(EVAL, ccForm);
            switch (symOp.wellknownSymbol) {
            case sQuote:
                oneArg(QUOTE, ccArgs);
                return form;

            case sLambda:
                if (car(ccArgs) == sDynamic) {
                    varargsMin(LAMBDA_DYNAMIC, ccArgs, 2);
                    checkLambdaList(LAMBDA_DYNAMIC, cadr(ccArgs));
                    expandForms(LAMBDA_DYNAMIC, cddrShallowCopyList(LAMBDA_DYNAMIC, ccArgs));
                    return cons(sLambdaDynamic, cdr(ccArgs));
                }
                else if (!have(Features.HAVE_LEXC)) {
                    varargsMin(LAMBDA_DYNAMIC, ccArgs, 1);
                    checkLambdaList(LAMBDA_DYNAMIC, car(ccArgs));
                    expandForms(LAMBDA_DYNAMIC, cdrShallowCopyList(LAMBDA_DYNAMIC, ccArgs));
                    return cons(sLambdaDynamic, ccArgs);
                }
                else {
                    varargsMin(LAMBDA, ccArgs, 1);
                    checkLambdaList(LAMBDA, car(ccArgs));
                    expandForms(LAMBDA, cdrShallowCopyList(LAMBDA, ccArgs));
                    return ccForm;
                }

            case sIf:
                varargsMinMax(IF, ccArgs, 2, 3);
                expandForms(IF, ccArgs);
                return ccForm;

            case sCond:
                if (ccArgs == null) return null;
                for (ConsCell i = ccArgs; i != null; i = cdrShallowCopyList(COND, i)) {
                    if (!consp(car(i))) errorMalformed(COND, "a list (condexpr forms...)", car(i));
                    expandForms(COND, carShallowCopyList(COND, i));
                }
                return ccForm;

            case sProgn:
                if (ccArgs == null) return null;
                if (cdr(ccArgs) == null) return expandForm(car(ccArgs));
                expandForms(PROGN, ccArgs);
                return ccForm;

            case sLabels:
                varargs1(LABELS, ccArgs);
                if (car(ccArgs) == null) return expandForm(cons(sProgn, cdr(ccArgs)));
                for (ConsCell i = carShallowCopyList(LABELS, ccArgs); i != null; i = cdrShallowCopyList(LABELS, i)) {
                    if (!consp(car(i))) errorMalformed(LABELS, "a list (symbol (params...) forms...)", i);
                    final ConsCell localFunc = carShallowCopyList(LABELS, i);
                    varargsMin(LABELS, localFunc, 2);
                    final LambdaJSymbol funcSymbol = symbolOrMalformed(LABELS, car(localFunc));
                    if (funcSymbol.macro != null) throw new ProgramError("local function %s is also a macro which would shadow the local function", funcSymbol, localFunc);
                    checkLambdaList(funcSymbol.toString(), cadr(localFunc));
                    if (cddr(localFunc) != null) expandForms(LABELS, cddrShallowCopyList(LABELS, localFunc));
                }
                if (cdr(ccArgs) != null) expandForms(LABELS, cdrShallowCopyList(LABELS, ccArgs));
                return ccForm;

            case sDefine:
                varargs1_2(DEFINE, ccArgs);
                symbolOrMalformed(DEFINE, car(ccArgs));
                if (cdr(ccArgs) != null) {
                    final ConsCell valueForm = cdrShallowCopyList(DEFINE, ccArgs);
                    valueForm.rplaca(expandForm(car(valueForm)));
                }
                return ccForm;

            case sDefun:
                varargsMin(DEFUN, ccArgs, 2);
                checkLambdaList(DEFUN, cadr(ccArgs));
                if (cdddr(ccArgs) != null && stringp(caddr(ccArgs))) cdrShallowCopyList(DEFUN, ccArgs).rplacd(cdddr(ccArgs)); // remove (ignore) docstring
                if (cddr(ccArgs) != null) expandForms(DEFUN, cddrShallowCopyList(DEFUN, ccArgs));
                return ccForm;

            case sDefmacro:
                varargs1(DEFMACRO, ccArgs);
                final LambdaJSymbol sym1 = symbolOrMalformed(DEFMACRO, car(ccArgs));
                if (cdr(ccArgs) == null) sym1.macro = null;
                else {
                    if (cdddr(ccArgs) != null && stringp(caddr(ccArgs))) cdrShallowCopyList(DEFMACRO, ccArgs).rplacd(cdddr(ccArgs)); // remove (ignore) docstring
                    if (cddr(ccArgs) != null) expandForms(DEFMACRO, cddrShallowCopyList(DEFMACRO, ccArgs));
                    final Object params = cadr(ccArgs);
                    checkLambdaList(DEFMACRO, params);
                    sym1.macro = makeClosure(params, (ConsCell)cddr(ccArgs), null);
                }
                return ccForm;

            case sLet:
            case sLetStar:
            case sLetrec:
                final Object tagOrBindings = car(ccArgs);
                if (tagOrBindings == null) return expandForm(cons(sProgn, cdr(ccArgs)));

                final String sfName = symOp.toString();
                final boolean letDynamic, namedLet;
                final Object tag;
                final ConsCell bindingsAndBody;
                if (symbolp(tagOrBindings)) {
                    tag = tagOrBindings;
                    if (tag == sDynamic) {
                        letDynamic = true;
                        namedLet = false;
                    }
                    else {
                        final LambdaJSymbol sTag = (LambdaJSymbol)tag;
                        notReserved(sfName, sTag);
                        if (sTag.macro != null) throw new ProgramError("named-let label %s is also a macro which would shadow the local function", sTag);
                        letDynamic = false;
                        namedLet = true;
                    }
                    if (letDynamic && symOp.wellknownSymbol == WellknownSymbol.sLetrec) throw errorMalformed(sfName, DYNAMIC + " is only allowed with let and let*");
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
                                assert ccBinding != null;
                                final ConsCell valueFormList = cdrShallowCopyList(sfName, ccBinding);
                                valueFormList.rplaca(expandForm(car(valueFormList)));
                            }
                        }
                        else if (symbolp(binding)) i.rplaca(cons(binding, null)); // change (let (a) ...) -> (let ((a)) ...)
                        else throw errorMalformed(getOp(sfName, letDynamic, namedLet), "bindings to contain lists and/or symbols", binding);
                        final LambdaJSymbol sym = symbolOrMalformed(sfName, caar(i));

                        // don't use notReserved(), this way getOp() only allocates space for string concatenation if needed to actually display an error message
                        if (reserved(sym)) errorReserved(getOp(sfName, letDynamic, namedLet), sym);
                        if (sym == tag && sym != sDynamic) errorMalformedFmt(getOp(sfName, false, true), "can't use loop symbol %s as a variable", sym);

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
                varargsMin(MULTIPLE_VALUE_BIND, ccArgs, 2);
                expandForms(MULTIPLE_VALUE_BIND, cdrShallowCopyList(MULTIPLE_VALUE_BIND, ccArgs));
                checkLambdaList(MULTIPLE_VALUE_BIND, car(ccArgs));
                return ccForm;

            case sCatch:
                varargs1(CATCH, ccArgs);
                if (cdr(ccArgs) == null) return null;
                expandForms(CATCH, cdrShallowCopyList(CATCH, ccArgs));
                return ccForm;

            case sThrow:
                twoArgs(THROW, ccArgs);
                expandForms(THROW, ccArgs);
                return ccForm;

            case sUnwindProtect:
                varargs1(UNWIND_PROTECT, ccArgs);
                if (cdr(ccArgs) == null) return expandForm(car(ccArgs));
                expandForms(UNWIND_PROTECT, ccArgs);
                return ccForm;

            case sTry:
                varargs1_2(TRY, ccArgs);
                expandForms(THROW, ccArgs);
                return ccForm;

            case sSetQ:
                for (ConsCell pairs = ccArgs; pairs != null; pairs = cdrShallowCopyList(SETQ, pairs)) {
                    symbolOrMalformed(SETQ, car(pairs));
                    if (cdr(pairs) == null) errorMalformed(SETQ, "odd number of arguments");
                    pairs = cdrShallowCopyList(SETQ, pairs);
                    pairs.rplaca(expandForm(car(pairs)));
                }
                return ccForm;

            // no macroexpansion in declaim, load, require, provide forms
            case sDeclaim:
                return form;
            case sLoad:
                oneArg(LOAD, ccArgs);
                return form;
            case sRequire:
                varargs1_2(REQUIRE, ccArgs);
                return form;
            case sProvide:
                oneArg(PROVIDE, ccArgs);
                return form;

            case sMultipleValueCall:
                varargs1(MULTIPLE_VALUE_CALL, ccArgs);
                expandForms(MULTIPLE_VALUE_CALL, ccArgs);
                return ccForm;

            default:
                assert false : ccForm.lineInfo() + "unexpected special form " + symOp;
                return null; // can't happen
            }
        }
        catch (LambdaJError e) {
            throw new LambdaJError(e, form);
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

    private Object evalSymbol(Object form, ConsCell env) {
        if (form == null || form == sNil) return null;
        if (form == sT) return form;
        final ConsCell envEntry = lookupEnvEntry(form, env);
        if (envEntry != null) {
            final Object value = cdr(envEntry);
            if (value == UNASSIGNED) errorUnassigned(EVAL, form);
            return value;
        }
        throw errorUnbound(EVAL, form);
    }

    private Object evalSetq(ConsCell pairs, ConsCell env, int stack, int level, int traceLvl) {
        Object res = null;
        while (pairs != null) {
            final LambdaJSymbol symbol = (LambdaJSymbol)car(pairs);
            final ConsCell envEntry = lookupEnvEntry(symbol, env);

            pairs = (ConsCell) cdr(pairs);
            final Object value = eval(car(pairs), env, stack, level, traceLvl);
            if (envEntry == null) extendGlobal(symbol, value);
            else envEntry.rplacd(value);
            res = value;
            pairs = (ConsCell) cdr(pairs);
        }
        values = NO_VALUES;
        return res;
    }

    private @NotNull Object evalDefine(ConsCell ccArguments, ConsCell env, int stack, int level, int traceLvl) {
        final Object symbol;
        extendGlobal(symbol = car(ccArguments), eval(cadr(ccArguments), env, stack, level, traceLvl));
        values = NO_VALUES;
        return symbol;
    }

    private @NotNull Object evalDefun(ConsCell ccArguments, ConsCell env) {
        final Object symbol = car(ccArguments);
        final AbstractConsCell selfEnvEntry = new ListConsCell(symbol, null);
        final Object closure = makeClosure(cadr(ccArguments), (ConsCell)cddr(ccArguments), cons(selfEnvEntry, env));
        selfEnvEntry.rplacd(closure);
        extendGlobal(symbol, closure); // this will create a new env entry in the global environment, changing the global value won't change early bound recursive invocations
        return symbol;
    }

    private Object evalRequire(ConsCell arguments) {
        if (!stringp(car(arguments))) errorMalformed(REQUIRE, "a string argument", arguments);
        final Object modName = car(arguments);
        if (!modules.contains(modName)) {
            Object modFilePath = cadr(arguments);
            if (modFilePath == null) modFilePath = modName;
            final Object ret = loadFile(REQUIRE, modFilePath);
            if (!modules.contains(modName)) throw new ProgramError("require'd file '%s' does not provide '%s'", modFilePath, modName);
            return ret;
        }
        return null;
    }

    private void evalProvide(ConsCell arguments) {
        if (!stringp(car(arguments))) errorMalformed(PROVIDE, "a string argument", arguments);
        modules.add(car(arguments));
    }

    void evalDeclaim(int level, ConsCell arguments) {
        if (level != 1) errorMalformed(DECLAIM, "must be a toplevel form");
        if (caar(arguments) == intern(OPTIMIZE)) {
            final Object rest = cdar(arguments);
            final Object speedCons = assq(intern(SPEED), rest);
            if (speedCons != null) {
                final Object speed = cadr(speedCons);
                if (!numberp(speed)) throw new ProgramError(DECLAIM + ": argument to " + SPEED + " must be a number, found %s", speed);
                this.speed = ((Number)speed).shortValue();
            }

            final Object debugCons = assq(intern(DEBUG), rest);
            if (debugCons != null) {
                final Object debug = cadr(debugCons);
                if (!numberp(debug)) throw new ProgramError(DECLAIM + ": argument to " + DEBUG + " must be a number, found %s", debug);
                this.debug = ((Number)debug).shortValue();
            }
        }
    }

    private ConsCell evalLabels(ConsCell localFunctions, ConsCell env) {
        final ListConsCell extEnv = acons(PSEUDO_SYMBOL, UNASSIGNED, env);
        for (Object localFunction : localFunctions) {
            final ConsCell currentFunc = (ConsCell)localFunction;
            final ConsCell ccParamsAndForms = (ConsCell)cdr(currentFunc);
            insertFront(extEnv, new ListConsCell(car(currentFunc), makeClosure(car(ccParamsAndForms), (ConsCell)cdr(ccParamsAndForms), extEnv)));
        }
        return extEnv;
    }

    private ArrayList<Object> handlers;
    private class RestoreHandler extends RestoreDynamic {
        private final boolean doPop;

        RestoreHandler(ConsCell entry, Object oldValue) {
            super(entry, oldValue);
            if (oldValue != null) {
                if (handlers == null) handlers = new ArrayList<>();
                handlers.add(oldValue);
                doPop = true;
            }
            else doPop = false;
        }

        @Override void restore() {
            if (doPop) handlers.remove(handlers.size()-1);
            super.restore();
        }
    }

    private Object prev() { final ArrayList<Object> handlers = this.handlers; if (handlers == null || handlers.isEmpty()) return null; return handlers.get(handlers.size()-1); }

    private static class LetRetVal { final ConsCell body, env, restore; LetRetVal(ConsCell b, ConsCell e, ConsCell r) { body = b; env = e; restore = r; } }

    private LetRetVal evalLet(WellknownSymbol operator, final ConsCell arguments, ConsCell env, ConsCell restore, int stack, int level, int traceLvl) {
        final Object maybeLoopSymbol = car(arguments);
        final boolean letDynamic, namedLet;
        final ConsCell bindingsAndBodyForms;
        if (maybeLoopSymbol == sDynamic)                   { letDynamic = true;  namedLet = false; bindingsAndBodyForms = (ConsCell)cdr(arguments); }
        else if (maybeLoopSymbol instanceof LambdaJSymbol) { letDynamic = false; namedLet = true;  bindingsAndBodyForms = (ConsCell)cdr(arguments); }
        else                                               { letDynamic = false; namedLet = false; bindingsAndBodyForms = arguments; }

        final ArrayList<Object> seen = new ArrayList<>(); // hopefully Hotspot will stackallocate this
        final ConsCell ccBindings = (ConsCell)car(bindingsAndBodyForms);
        assert namedLet || ccBindings != null : "let w/o bindings should have been replaced in expandForm";

        final ListConsCell params = new ListConsCell(null, null);
        ConsCell extenv = env;
        if (ccBindings != null) {
            final boolean letStar  = operator == WellknownSymbol.sLetStar;
            final boolean letRec   = operator == WellknownSymbol.sLetrec;

            ListConsCell newValues = null; // used for let dynamic
            ListConsCell insertPos = params; // used for named let
            if (letRec) extenv = acons(PSEUDO_SYMBOL, UNASSIGNED, env);
            for (Object binding : ccBindings) {
                final ConsCell ccBinding = (ConsCell)binding;
                final LambdaJSymbol sym = (LambdaJSymbol)car(ccBinding);

                final ConsCell newBinding;
                if (letDynamic) newBinding = lookupGlobalEntry(sym); // hier wird nur im global env gesucht. wenns gleichnamige globale UND lexical variablen gibt, bleibt die lexical unveraendert
                else if (letStar) newBinding = fastassq(sym, extenv);
                else if (letRec) newBinding = insertFront(extenv, cons(sym, UNASSIGNED));
                else newBinding = null;

                final Object val = eval(cadr(ccBinding), letStar || letRec ? extenv : env, stack, level, traceLvl);

                if (letDynamic && newBinding != null) {
                    if (!seen.contains(sym)) {
                        seen.add(sym);
                        // todo wenn die let dynamic form in der tailposition ist koennte man einen allenfalls existierenden restore eintrag ersetzen (nur die letzte aenderung einer dynamic variablen muss rueckgaengig gemacht werden)
                        if (sym == sConditionHandler) restore = cons(new RestoreHandler(newBinding, cdr(newBinding)), restore);
                        else restore = cons(new RestoreDynamic(newBinding, cdr(newBinding)), restore);
                    }
                    if (letStar) newBinding.rplacd(val); // das macht effektiv ein let* dynamic
                    else newValues = acons(newBinding, val, newValues);
                }
                else if (letStar && newBinding != null || letRec)
                    //noinspection ConstantConditions
                    newBinding.rplacd(val);
                else extenv = acons(sym, val, extenv);

                if (namedLet) { final ListConsCell c; insertPos.rplacd(c = cons(sym, null)); insertPos = c; }
            }
            if (newValues != null) for (Object o: newValues) { final ListConsCell c = (ListConsCell)o; ((ConsCell)car(c)).rplacd(cdr(c)); }
        }
        final ConsCell bodyForms = (ConsCell)cdr(bindingsAndBodyForms);
        if (namedLet) {
            final ListConsCell c;
            extenv = cons(c = cons(maybeLoopSymbol, null), extenv);
            c.rplacd(makeClosure(params.cdr(), bodyForms, extenv));
        }
        return new LetRetVal(bodyForms, extenv, restore);
    }

    private static String getOp(String operator, boolean letDynamic, boolean namedLet) {
        return letDynamic ? (operator + ' ' + DYNAMIC) : (namedLet ? "named " : "") + operator;
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

    Object evalMacro(Object operator, final Closure macroClosure, final ConsCell arguments) {
        if (traceFunc)  tracer.println(pfx(0, 0) + " #<macro " + operator + "> " + printSEx(arguments));

        final ConsCell menv = macroClosure.zip(this, arguments, null);
        Object expansion = null;
        if (macroClosure.body != null) for (Object macroform: macroClosure.body) // loop over macro body so that e.g. "(defmacro m (a b) (write 'hallo) `(+ ,a ,b))" will work
            expansion = eval(macroform, menv, 0, 0, 0);
        return expansion;
    }

    /** Insert a new symbolentry at the front of env, env is modified in place, address of the list will not change.
     *  Returns the newly created (and inserted) symbolentry (symbol . value) */
    private ConsCell insertFront(ConsCell env, ConsCell symbolEntry) {
        final Object oldCar = car(env);
        final Object oldCdr = cdr(env);
        env.rplaca(symbolEntry);
        env.rplacd(cons(oldCar, oldCdr));
        return symbolEntry;
    }

    /** build an extended environment for a function invocation.
     *  Similar to CL pairlis, but {@code #zip} will also pair the last cdr of a dotted list with the rest of {@code args},
     *  e.g. (zip '(a b . c) '(1 2 3 4 5)) -> ((a . 1) (b . 2) (c 3 4 5)) */
    final ConsCell zip(String func, Object params, ConsCell args, ConsCell env, boolean match) {
        if (params == null && args == null) return env; // shortcut for no params/ no args

        final ListBuilder ret = new ListBuilder();
        while (consp(params)) {
            if (match && args == null) errorApplicationArgCount("%s: not enough arguments. Parameters w/o argument: %s", func, params);
            final Object sym = car((ConsCell)params);
            ret.append(cons(sym, car(args)));
            env = peel(sym, env);
            params = cdr((ConsCell)params);
            args = (ConsCell)cdr(args);
        }
        // if paramList is a dotted list whose last cdr is a non-nil symbol: the last param will be bound to the list of remaining args
        if (params != null && symbolp(params)) {
            ret.append(cons(params, args));
            env = peel(params, env);
        }
        else if (match && args != null) errorApplicationArgCount("%s: too many arguments. Remaining arguments: %s", func, args);
        ret.appendLast(env);
        return (ConsCell)ret.first();
    }

    /** this helps in limiting environment growth for recursive calls of dynamic lambdas:
     *  parameters in the current dynamic environment will be peeled off before adding them.
     *  Not too useful for lexical closures except when the closure's parameters hide closed over variables (as is the case in recursion). */
    private static ConsCell peel(Object sym, ConsCell env) {
        return env != null && sym == car(env.car()) ? (ConsCell)env.cdr() : env;
    }

    static void errorApplicationArgCount(String msg, String func, Object params) {
        throw new ProgramError(msg, func, printSEx(params));
    }

    /** eval a list of forms and return a list of results */
    private ConsCell evlis(ConsCell forms, ConsCell env, int stack, int level, int traceLvl) {
        if (traceOn) dbgEvalStart("evlis", forms, env, stack, level);
        ListConsCell head = null;
        ListConsCell insertPos = null;
        for (ConsCell rest = forms; rest != null; rest = (ConsCell)cdr(rest)) {
            final ListConsCell currentArg = cons(eval(car(rest), env, stack, level, traceLvl), null);
            if (head == null) {
                insertPos = head = currentArg;
            }
            else {
                insertPos.rplacd(currentArg);
                insertPos = currentArg;
            }
        }
        values = NO_VALUES;
        if (traceOn) dbgEvalDone("evlis", forms, head, stack, level);
        return head;
    }

    /** eval a list of forms and return a list that is all individual values appended */
    private ConsCell evalMultipleValuesArgs(Object valueForms, ConsCell env, int stack, int level, int traceLvl) {
        ConsCell allArgs = null, appendPos = null;
        if (valueForms != null) for (Object valueForm : listOrMalformed(MULTIPLE_VALUE_CALL, valueForms)) {
            final Object prim = eval(valueForm, env, stack, level, traceLvl);
            final ConsCell newValues;
            if (values == NO_VALUES) newValues = cons(prim, null);
            else { newValues = values; values = NO_VALUES; }

            if (allArgs == null) allArgs = appendPos = newValues;
            else if (newValues != null) {
                while (cdr(appendPos) != null) appendPos = (ConsCell)cdr(appendPos);
                appendPos.rplacd(newValues);
            }
        }
        return allArgs;
    }

    private ConsCell evalMultipleValueBind(ConsCell varsAndValuesForm, ConsCell env, int stack, int level, int traceLvl) {
        values = NO_VALUES;
        final Object prim = eval(cadr(varsAndValuesForm), env, stack, level, traceLvl);
        final ConsCell newValues;
        if (values == NO_VALUES) newValues = cons(prim, null);
        else { newValues = values; values = NO_VALUES; }
        env = zip(MULTIPLE_VALUE_BIND, car(varsAndValuesForm), newValues, env, false);
        return env;
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
        return have(Features.HAVE_LEXC) ? Closure.of(params, body, env) : new DynamicLambda(params, body);
    }

    private Object applyPrimitive(Primitive primfn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<primitive> " + printSEx(args));
        try { return primfn.applyPrimitive(args); }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(e); }
    }

    /** in case compiled code calls "(eval)" */
    MurmelJavaProgram compiledProgram = null;

    private Object applyCompilerPrimitive(MurmelJavaProgram.CompilerPrimitive primfn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<compiler primitive> " + printSEx(args));
        assert compiledProgram != null;
        assert values == NO_VALUES;
        try {
            final Object ret = primfn.applyCompilerPrimitive(listToArray(args));
            if (compiledProgram.values != null) values = list(compiledProgram.values);
            return ret;
        }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(e); }
    }

    private Object applyCompiledFunction(MurmelFunction fn, ConsCell args, int stack, int level) {
        if (traceFunc) tracer.println(pfx(stack, level) + " #<compiled function> " + printSEx(args));
        assert compiledProgram != null;
        assert values == NO_VALUES;
        try {
            final Object ret = compiledProgram.funcall(fn, listToArray(args));
            if (compiledProgram.values != null) values = list(compiledProgram.values);
            return ret;
        }
        catch (LambdaJError e) { throw e; }
        catch (Exception e) { throw new LambdaJError(e); }
    }


    private Object loadFile(String func, Object argument) {
        final Path prev = currentSource;
        final Path p = findFile(func, argument);
        currentSource = p;
        try {
            final SExpressionReader parser = makeReader(ReadSupplier.of(p), p);
            final Object eof = "EOF";
            Object result = null;
            for (;;) {
                final Object form = parser.readObj(true, eof);
                if (form == eof) return result;

                result = expandAndEval(form, null);
            }
        }
        catch (ReaderError re) {
            throw wrap(re);
        }
        catch (IOException e) {
            errorReaderErrorFmt(func, "%s: error reading file '%s': %s", func, argument, e.getMessage());
            return null; // notreached
        }
        finally {
            currentSource = prev;
        }
    }

    final Path findFile(String func, Object argument) {
        if (!stringp(argument)) errorMalformed(func, "a string argument", printSEx(argument));
        final String _filename = (String)argument;
        final String filenameLC = _filename.toLowerCase(Locale.ENGLISH);
        final String filename = filenameLC.endsWith(".lisp") ? _filename : _filename + ".lisp";

        final Path path = Paths.get(filename);
        if (path.isAbsolute()) return path;

        Path current = currentSource;
        if (current == null) current = Paths.get("dummy");
        final Path ret = current.resolveSibling(path);
        if (Files.isReadable(ret)) return ret;
        return libDir.resolve(path);
    }


    /// ### debug support - trace and untrace
    private Map<Object, LambdaJSymbol> traced;

    final Object trace(ConsCell symbols) {
        if (symbols == null) return traced == null ? null : new ArraySlice(traced.values().toArray(), 0);
        if (traced == null) traced = new HashMap<>();
        for (Object sym: symbols) {
            if (!symbolp(sym)) throw new ProgramError("trace: can't trace %s: not a symbol", printSEx(sym)); // todo sbcl gibt keinen fehler, nur eine warning
            if (((LambdaJSymbol)sym).specialForm()) {
                throw new ProgramError("trace: can't trace %s: it is a special form", printSEx(sym));
            }
            final ConsCell envEntry = lookupGlobalEntry(sym);
            if (envEntry == null) errorNotAFunction("trace: can't trace %s: not bound", printSEx(sym));
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
                    final ConsCell envEntry = lookupGlobalEntry(sym);
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
        assert traced != null;
        if (op instanceof LambdaJSymbol) {
            if (((LambdaJSymbol)op).specialForm()) return traceStack;
            final ConsCell entry = lookupGlobalEntry(op);
            if (entry == null) return traceStack;
            op = cdr(entry);
        }
        if (!traced.containsKey(op)) return traceStack;
        if (traceStack == null) traceStack = new ArrayDeque<>();
        traceStack.addLast(op);
        return traceStack;
    }

    private int traceEnter(Object op, ConsCell args, int level) {
        assert traced != null;
        final LambdaJSymbol sym;
        if (null == (sym = traced.get(op))) return level;
        enter(sym, args, level);
        return level + 1;
    }

    private void enter(Object op, ConsCell args, int level) {
        final StringBuilder sb = new StringBuilder();

        tracePfx(sb, level);

        sb.append('(').append(level+1).append(" enter ").append(op);
        printArgs(sb, args);
        sb.append(')');
        tracer.println(sb);
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
        tracer.println(sb);
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

    void resetCounters() {
        nCells = maxEnvLen = maxEvalStack = maxEvalLevel = 0;
    }

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

    final   ListConsCell acons(Object key, Object datum, ConsCell alist) { return cons(cons(key, datum), alist); }

    private static Object carCdrError(@NotNull String func, @NotNull Object o) { throw errorArgTypeError("list or string", func, o); }

    static Object   car(ConsCell c)    { return c == null ? null : c.car(); }
    static Object   car(Object o)      { return o == null ? null
                                                          : o instanceof ListConsCell ? ((ListConsCell)o).car()
                                                          : o instanceof ConsCell ? ((ConsCell)o).car()
                                                          : o instanceof CharSequence ? ((CharSequence)o).length() == 0 ? null : ((CharSequence)o).charAt(0)
                                                          : o instanceof char[] ? ((char[])o).length == 0 ? null : ((char[])o)[0]
                                                          : carCdrError(CAR, o); }

    static Object   caar(ConsCell c)   { return c == null ? null : car(car(c)); }

    static Object   cadr(ConsCell c)   { return c == null ? null : car(cdr(c)); }
    static Object   cadr(Object o)     { return o == null ? null : car(cdr(o)); }

    //static Object   cadar(ConsCell c)  { return c == null ? null : car(cdar(c)); }

    static Object   caddr(ConsCell c)  { return c == null ? null : car(cddr(c)); }
    //static Object   caddr(Object c)    { return c == null ? null : car(cddr(c)); }

    static Object   cadddr(ConsCell o) { return o == null ? null : car(cdddr(o)); }

    static Object   cdr(ConsCell c)    { return c == null ? null : c.cdr(); }
    static Object   cdr(Object o)      { return o == null ? null
                                                          : o instanceof ListConsCell ? ((ListConsCell)o).cdr()
                                                          : o instanceof ConsCell ? ((ConsCell)o).cdr()
                                                          : o instanceof String ? ((String)o).length() <= 1 ? null : ((String)o).substring(1)
                                                          : carCdrError(CDR, o); }

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



    /** the following predicates more or less implement Murmel's type system, see also {@link Subr#typep} and {@link Subr#adjustableArrayP} */
    static boolean consp(Object o)       { return o instanceof ConsCell; }
    static boolean atom(Object o)        { return !consp(o); }

    static boolean symbolp(Object o)     { return o == null || o instanceof LambdaJSymbol; }

    static boolean numberp(Object o)     { return integerp(o) || floatp(o) || o instanceof Number; }
    static boolean floatp(Object o)      { return o instanceof Double || o instanceof Float || o instanceof BigDecimal; }
    static boolean integerp(Object o)    { return o instanceof Long || o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof BigInteger; }

    static boolean characterp(Object o)  { return o instanceof Character; }

    static boolean randomstatep(Object o){ return o instanceof Random; }

    static boolean vectorp(Object o)     { return stringp(o) || bitvectorp(o) || svectorp(o) || o instanceof List; }
    static boolean svectorp(Object o)    { return o != null && o.getClass().isArray() && !bitvectorp(o) && !stringp(o); }
    static boolean stringp(Object o)     { return sstringp(o) || o instanceof CharSequence; }
    static boolean sstringp(Object o)    { return o instanceof String || o instanceof char[]; }
    static boolean bitvectorp(Object o)  { return sbitvectorp(o) || o instanceof Bitvector; }
    static boolean sbitvectorp(Object o) { return o instanceof boolean[]; }

    static boolean hashtablep(Object o)  { return o instanceof Map; }

    final  boolean functionp(Object o)   { return functionp0(o) || have(Features.HAVE_OLDLAMBDA) && consp(o) && car(o) == sLambda; }
    static boolean functionp0(Object o)  { return o instanceof Primitive || o instanceof Closure || o instanceof MurmelJavaProgram.CompilerPrimitive || o instanceof MurmelFunction || o instanceof OpenCodedPrimitive; }

    static boolean listp(Object o)       { return o == null || consp(o); }


    // these *should* have no usages as these checks would be superfluous.
    // The purpose of these functions is: if such extra checks were made then this would be discovered during testing.
    @SuppressWarnings("unused")
    static boolean consp(ConsCell ignored)  { throw errorInternal("consp(ConsCell c) should NOT be called"); }
    @SuppressWarnings("unused")
    static boolean listp(ConsCell ignored)  { throw errorInternal("listp(ConsCell c) should NOT be called"); }

    static class TypeSpec {
        final String name; final Predicate<Object> pred; final Consumer<String> thrower;
        TypeSpec(String name, Predicate<Object> pred, Consumer<String> thrower) { this.name = name; this.pred = pred; this.thrower = thrower; }
    }
    private static final TypeSpec[] TYPE_SPECS = {
    new TypeSpec("simple-error", o -> o instanceof SimpleError, msg -> { throw new SimpleError(msg); }),

    new TypeSpec("unbound-variable",    o -> o instanceof UnboundVariable, msg -> { throw new UnboundVariable(msg);  }),
    new TypeSpec("undefined-function",  o -> o instanceof UndefinedFunction, msg -> { throw new UndefinedFunction(msg);  }),
    new TypeSpec("cell-error",          o -> o instanceof CellError, msg -> { throw new CellError(msg);  }),

    new TypeSpec("control-error",       o -> o instanceof ControlError, msg -> { throw new ControlError(msg);  }),

    new TypeSpec("program-error",       o -> o instanceof ProgramError, msg -> { throw new ProgramError(msg);  }),

    new TypeSpec("parse-error",         o -> o instanceof ParseError || o instanceof ReaderError, msg -> { throw new ParseError(msg);  }),


    // extends RuntimeException
    new TypeSpec("arithmetic-error",    o -> o instanceof ArithmeticException, msg -> { throw new ArithmeticException(msg);  }),

    new TypeSpec("simple-type-error",   o -> o instanceof SimpleTypeError, msg -> { throw new SimpleTypeError(msg);  }),
    new TypeSpec("type-error",          o -> o instanceof ClassCastException || o instanceof IndexOutOfBoundsException, msg -> { throw new ClassCastException(msg);  }),
    new TypeSpec("invalid-index-error", o -> o instanceof IndexOutOfBoundsException, msg -> { throw new InvalidIndexError(msg);  }),

    new TypeSpec("file-error",          o -> o instanceof InvalidPathException, msg -> { throw new InvalidPathException("(filename)", msg == null ? "(unknown reason)" : msg);  }),


    // extends IOException
    new TypeSpec("end-of-file",  o -> o instanceof EOFException, msg -> wrap0(new EOFException(msg))),
    new TypeSpec("reader-error", o -> o instanceof ReaderError,  msg -> wrap0(new ReaderError(msg))),
    new TypeSpec("stream-error", o -> o instanceof IOException,  msg -> wrap0(new IOException(msg))),


    // extends Throwable
    new TypeSpec("error", o -> o instanceof Exception, msg -> wrap0(new Exception(msg))),
    new TypeSpec("condition", o -> o instanceof Throwable, msg -> wrap0(new Throwable(msg))),
    };

    Map<LambdaJSymbol, TypeSpec> typeSpecs;
    Map<LambdaJSymbol, TypeSpec> typeSpecs() {
        if (typeSpecs == null) {
            final Map<LambdaJSymbol, TypeSpec> map = new IdentityHashMap<>(JavaUtil.hashMapCapacity(TYPE_SPECS.length));
            fillTypespecs(symtab, map);
            typeSpecs = map;
        }
        return typeSpecs;
    }

    static void fillTypespecs(SymbolTable st, Map<LambdaJSymbol, TypeSpec> map) {
        for (TypeSpec typeSpec : TYPE_SPECS) map.put(st.intern(typeSpec.name), typeSpec);
    }


    static ConsCell arraySlice(Object[] o, int offset) { return o == null || offset >= o.length ? null : new ArraySlice(o, offset); }
    static ConsCell arraySlice(Object... elems) {
        if (elems == null || elems.length == 0) return null;
        return new ArraySlice(elems, 0);
    }

    final Object boolResult(boolean b) { return b ? expTrue.get() : null; }

    /** return a list, count the conscells. See also {@link ConsCell#list} */
    private ConsCell list(Object... elems) {
        if (elems == null || elems.length == 0) return null;
        final ConsCell ret = cons(elems[0], null);
        if (elems.length == 1) return ret;
        ConsCell insertPos = ret;
        final int n = elems.length;
        for (int i = 1; i < n; i++) {
            final ConsCell cons = cons(elems[i], null);
            insertPos.rplacd(cons);
            insertPos = cons;
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

        for (Object entry: requireList(ASSQ, lst)) {
            if (entry != null && atom == car(entry)) {
                return (ConsCell)entry; // cast can't fail if car() succeeded
            }
        }
        return null;
    }

    /** faster assq for internal use for environment lookup. ccList must be a proper list that only contains cons cells. */
    static ConsCell fastassq(Object atom, ConsCell ccList) {
        //int n = 0;
        for (; ccList != null; ccList = (ConsCell)ccList.cdr()) {
            //n++;
            final ConsCell ccEntry = (ConsCell)ccList.car();
            if (atom == ccEntry.car()) {
                //if (n >= 20) System.out.printf("assq: %s %d%n", atom, n);
                return ccEntry;
            }
        }
        return null;
    }


    /// ###  Misc. helpers and printing of S-expressions

    static String requireString(String func, Object c) {
        if (c instanceof char[]) return String.valueOf((char[])c);
        if (!(c instanceof CharSequence)) errorNotAString(func, c);
        return c.toString();
    }

    /** return {@code a} cast to a list, error if {@code a} is not a list (nil is acceptable) */
    static ConsCell requireList(String func, Object a) {
        if (a == null) return null;
        if (!consp(a)) errorNotAList(func, a);
        return (ConsCell)a;
    }

    /** convert a (possibly empty aka nil/ null) list to a (possibly empty) Object[] */
    static Object[] listToArray(Object maybeList) {
        if (maybeList == null) return EMPTY_ARRAY;
        if (maybeList instanceof ArraySlice) return ((ArraySlice)maybeList).listToArray();
        if (!consp(maybeList)) errorNotAList("listToArray", maybeList);
        final List<Object> ret = new ArrayList<>();
        ((ConsCell) maybeList).forEach(ret::add); // todo forEach behandelt dotted und proper lists gleich -> im interpreter gibt (apply < '(1 2 3 4 . 5)) einen fehler, im compiler nicht
        //for (Object rest = maybeList; rest != null; rest = cdr(rest)) ret.add(car(rest));
        return ret.toArray();
    }

    static Object[] listToArray(ConsCell lst, int len) {
        if (lst == null) {
            if (len == 0) return EMPTY_ARRAY;
            errorReaderErrorFmt("", VECTOR + " of length %d cannot be initialized from ()", len); // todo posinfo
            assert false; //notreached
        }
        if (len < 0) len = listLength(lst);
        final Object[] ret = new Object[len];
        int i = 0;
        for (Object o: lst) {
            if (i == len) errorReaderErrorFmt("", VECTOR + " is longer than the specified length: #%d%s", len, printSEx(lst)); // todo posinfo
            ret[i++] = o;
        }
        final Object last = ret[i-1];
        if (last != null) Arrays.fill(ret, i, len, last);
        return ret;
    }

    enum CompareMode { NUMBER, EQL, EQUAL }

    /** compare two objects. {@code mode} determines which types are compared by their value and which are compared by their identity.
     *
     *  <p>Implementation note: this relies on the hope that {@link System#identityHashCode(Object)} will return different values for different objects that are not numbers.
     *  This is strongly suggested but not guaranteed by the Java spec:
     *  "As much as is reasonably practical, the hashCode method defined by class {@code Object}
     *  does return distinct integers for distinct objects." */
    static int compare(Object o1, Object o2, CompareMode mode) {
        if (o1 == o2) return 0;
        if (o1 == null) return -1;
        if (o2 == null) return 1;

        if (integerp(o1) && integerp(o2)) {
            if (o1 instanceof BigInteger && o2 instanceof BigInteger) return ((BigInteger)o1).compareTo((BigInteger)o2);
            if (o1 instanceof BigInteger)                             return ((BigInteger)o1).compareTo(BigInteger.valueOf(((Number)o2).longValue()));
            if (o2 instanceof BigInteger)                             return BigInteger.valueOf(((Number)o1).longValue()).compareTo((BigInteger)o2);
            return Long.compare(((Number)o1).longValue(), ((Number)o2).longValue());
        }

        if (floatp(o1) && floatp(o2)) {
            if (o1.getClass() != o2.getClass()) return System.identityHashCode(o1) - System.identityHashCode(o2);
            if (o1 instanceof BigDecimal && o2 instanceof BigDecimal) return ((BigDecimal)o1).compareTo((BigDecimal)o2);
            return Double.compare(((Number)o1).doubleValue(), ((Number)o2).doubleValue());
        }
        if (mode == CompareMode.NUMBER) return compareHash(o1, o2);

        if (o1 instanceof Character && o2 instanceof Character) { return ((Character)o1).compareTo((Character)o2); }
        if (mode == CompareMode.EQL) return compareHash(o1, o2);

        if (o1 instanceof CharSequence) {
            if (o2 instanceof CharSequence) return JavaUtil.compare((CharSequence)o1, (CharSequence)o2);
            if (o2 instanceof char[])       return JavaUtil.compare((CharSequence)o1, (char[])o2);
        }
        if (o1 instanceof char[]) {
            if (o2 instanceof CharSequence) return -JavaUtil.compare((CharSequence)o2, (char[])o1);
            if (o2 instanceof char[])       return JavaUtil.compare((char[])o1, (char[])o2);
        }

        if (bitvectorp(o1) && bitvectorp(o2)) { return Bitvector.of(o1).compareTo(Bitvector.of(o2)); }

        if (consp(o1) && consp(o2)) { //noinspection ConstantConditions
                                      return ((ConsCell)o1).compareToEqual((ConsCell)o2); }

        return compareHash(o1, o2);
    }

    private static int compareHash(@NotNull Object o1, @NotNull Object o2) { return Integer.compare(System.identityHashCode(o1), System.identityHashCode(o2)); }

    static int sxhashSigned(Object o) {
        if (o == null) return 97;

        if (integerp(o)) return Long.hashCode(((Number)o).longValue()); // byte..BigInteger have different hash codes for negative numbers
        if (o instanceof StringBuilder) return o.toString().hashCode();
        if (o instanceof StringBuffer) return o.toString().hashCode();
        if (o instanceof char[]) return String.valueOf((char[])o).hashCode();
        if (o instanceof boolean[]) return Bitvector.of(o).hashCode();

        if (symbolp(o) || characterp(o) || numberp(o) || consp(o) || stringp(o) || bitvectorp(o)) return o.hashCode();

        return o.getClass().getName().hashCode(); // see https://stackoverflow.com/questions/21126507/why-does-sxhash-return-a-constant-for-all-structs
    }

    static Object macroexpandImpl(@NotNull LambdaJ intp, @NotNull ConsCell form) {
        final Object maybeSymbol = car(form);
        if (maybeSymbol == null || !symbolp(maybeSymbol)) {
            intp.values = intp.cons(form, intp.cons(null, null));
            return form;
        }
        final LambdaJSymbol macroSymbol = (LambdaJSymbol)maybeSymbol;
        final Closure macroClosure = macroSymbol.macro;
        if (macroClosure == null) {
            intp.values = intp.cons(form, intp.cons(null, null));
            return form;
        }
        final ConsCell arguments = (ConsCell) cdr(form);
        final Object expansion = intp.evalMacro(macroSymbol, macroClosure, arguments);
        intp.values = intp.cons(expansion, intp.cons(sT, null));
        return expansion;
    }

    /** transform {@code obj} into an S-expression, atoms are escaped */
    static CharSequence printSEx(Object obj) {
        return printSEx(obj, true);
    }

    static CharSequence printSEx(Object obj, boolean printEscape) {
        if (obj == null) return NIL;
        final StringBuilder sb = new StringBuilder();
        _printSEx(sb::append, obj, obj, printEscape);
        return sb;
    }

    static void printSEx(@NotNull WriteConsumer w, Object obj) {
        _printSEx(w, obj, obj, true);
    }

    static void printSEx(@NotNull WriteConsumer w, Object obj, boolean printEscape) {
        _printSEx(w, obj, obj, printEscape);
    }

    static void _printSEx(@NotNull WriteConsumer sb, Object list, Object obj, boolean escapeAtoms) {
        boolean headOfList = true;
        while (true) {
            if (obj instanceof ArraySlice) { sb.print(((ArraySlice)obj).printSEx(headOfList, escapeAtoms)); return; }
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

    private static void printAtom(@NotNull WriteConsumer sb, Object atom, boolean escapeAtoms) {
        if (atom instanceof Writeable)            { ((Writeable)atom).printSEx(sb, escapeAtoms); }
        else if (escapeAtoms && characterp(atom)) { sb.print(printChar((int)(Character)atom)); }
        else if (vectorp(atom))                   { printVector(sb, atom, escapeAtoms); }
        else if (hashtablep(atom))                { printHash(sb, (Map<?, ?>)atom, escapeAtoms); }
        else if (atom == null)                    { sb.print(NIL); }
        else if (atom instanceof CharSequence)    { sb.print((CharSequence)atom); }
        else if (randomstatep((atom)))            { sb.print("#<random-state>"); }
        else                                      { sb.print(atom.toString()); }
    }

    static String printChar(int c) {
        return "#\\"
               + (c < CTRL.length ? CTRL[c]
                                  : c < 127 ? String.valueOf((char)c)
                                            : String.valueOf(c));
    }

    /** prepend " and \ by a \ */
    static CharSequence escapeString(CharSequence s) {
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
        return ret;
    }

    @SuppressWarnings("rawtypes")
    private static void printVector(@NotNull WriteConsumer sb, Object vector, boolean escapeAtoms) {
        if (vector instanceof boolean[]) {
            sb.print("#*");
            for (boolean b: (boolean[])vector) {
                sb.print(b ? "1" : "0");
            }
            return;
        }
        if (vector instanceof char[]) {
            if (escapeAtoms) sb.print("\"" + escapeString(new String((char[])vector)) + '"');
            else             sb.print(new String((char[])vector));
            return;
        }
        if (vector instanceof CharSequence) {
            if (escapeAtoms) sb.print("\"" + escapeString((CharSequence)vector) + '"');
            else             sb.print(((CharSequence)vector));
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

    private static void printHash(@NotNull WriteConsumer out, Map<?,?> map, boolean escapeAtoms) {
        assert !(map instanceof EqlMap) && !(map instanceof EqualMap) : "should be printed using Writable.printSEx()";
        if (map instanceof EqlTreeMap) out.print("#H(compare-eql");
        else if (map instanceof EqualTreeMap) out.print("#H(compare-equal");
        else if (map instanceof IdentityHashMap) out.print("#H(eq");
        else out.print("#H(t");
        for (Map.Entry<?,?> entry: map.entrySet()) {
            out.print(" ");  printSEx(out, entry.getKey(), escapeAtoms);
            out.print(" ");  printSEx(out, entry.getValue(), escapeAtoms);
        }
        out.print(")");
    }



    /// ##  Error "handlers"

    static void             errorReaderError     (String errorLoc, String msg)                  { wrap0(new ReaderError(msg), errorLoc); }
    static void             errorReaderErrorFmt  (String errorLoc, String msg, Object... args)  { wrap0(new ReaderError(msg, args), errorLoc); }
    static RuntimeException errorNotImplemented  (String msg, Object... args)                   { throw new LambdaJError(true, msg, args); }
    static RuntimeException errorInternal        (String msg, Object... args)                   { throw new LambdaJError(true, "internal error - " + msg, args); }
    static RuntimeException errorInternal        (Throwable t, String msg, Object... args)      { throw new LambdaJError(t, true, "internal error - " + msg, args); }
    static RuntimeException errorMalformed       (String func, String msg)                      { throw new ProgramError("%s: malformed %s: %s", func, func, msg); }
    static RuntimeException errorMalformedFmt    (String func, String msg, Object... params)    { return errorMalformed(func, String.format(msg, params)); }
    static RuntimeException errorMalformed       (String func, String expected, Object actual)  { throw new ProgramError("%s: malformed %s: expected %s but got %s", func, func, expected, printSEx(actual)); }
    static void             errorReserved        (String op, Object sym)                        { errorMalformedFmt(op, "can't use reserved word %s as a symbol", sym == null ? NIL : sym); }
    static RuntimeException errorUnbound         (String func, Object form)                     { throw new UnboundVariable("%s: '%s' is not bound", func, printSEx(form)); }
    @SuppressWarnings("SameParameterValue")
    static void             errorUnassigned      (String func, Object form)                     { throw new UnboundVariable("%s: '%s' is bound but has no assigned value", func, printSEx(form)); }

    static RuntimeException errorNotAFunction    (String msg, CharSequence name)                { throw new UndefinedFunction(msg, name); }

    /** throws a {@link SimpleTypeError} with a message of "'func': expected a 'expected' argument but got 'actual'" */
    static RuntimeException errorArgTypeError(String expected, String func, Object actual)      { throw new SimpleTypeError("%s: expected a %s argument but got %s", func, expected, printSEx(actual)); }
    static RuntimeException errorNotANumber      (String func, Object actual)                   { throw errorArgTypeError("number", func, actual); }
    static RuntimeException errorNotAnInteger    (String func, Object actual)                   { throw errorArgTypeError("integral number", func, actual); }
    static void             errorNotAFixnum      (String msg)                                   { throw new ArithmeticException(msg); }

    static RuntimeException errorNotABit         (String func, Object actual)                   { throw errorArgTypeError("bit", func, actual); }
    static RuntimeException errorNotAVector      (String func, Object actual)                   { throw errorArgTypeError(VECTOR, func, actual); }
    static RuntimeException errorNotASimpleVector(String func, Object actual)                   { throw errorArgTypeError("simple " + VECTOR, func, actual); }
    static void             errorNotAString      (String func, Object actual)                   { throw errorArgTypeError("string", func, actual); }
    static RuntimeException errorNotABitVector   (String func, Object actual)                   { throw errorArgTypeError("bitvector", func, actual); }
    static void             errorNotACons        (String func, Object actual)                   { throw errorArgTypeError(CONS, func, actual); }
    static void             errorNotAList        (String func, Object actual)                   { throw errorArgTypeError(LIST, func, actual); }
    @SuppressWarnings("SameParameterValue")
    static void             errorNotASequence    (String func, Object actual)                   { throw errorArgTypeError("list or " + VECTOR, func, actual); }

    static RuntimeException errorOverflow        (String func, String targetType, Object n)     { throw new ArithmeticException(String.format("%s: value cannot be represented as a %s: %s", func, targetType, n)); }
    static RuntimeException errorIndexTooLarge   (long idx, long actualLength)                  { throw new InvalidIndexError("index %d is too large for a sequence of length %d", idx, actualLength); }

    static void             errorVarargsCount    (String func, int min, int actual)             { throw new ProgramError("%s: expected %s or more but %s", func, expectedArgPhrase(min), actualArgPhrase(actual)); }

    static void errorArgCount(String func, int expectedMin, int expectedMax, int actual, Object form) {
        final String argPhrase = expectedMin == expectedMax
                                 ? expectedArgPhrase(expectedMin)
                                 : expectedMin + " to " + expectedMax + " arguments";

        if (actual < expectedMin) { throw new ProgramError("%s: expected %s but %s", func, argPhrase, actualArgPhrase(actual)); }
        if (actual > expectedMax) { throw new ProgramError("%s: expected %s but got extra arg(s) %s", func, argPhrase, printSEx(nthcdr(expectedMax, form))); }
        assert false: "errorArgCount was called, but there is no error";
    }

    private static String expectedArgPhrase(int expected) { return expected == 0 ? "no arguments" : expected == 1 ? "one argument" : expected == 2 ? "two arguments" : expected + " arguments"; }
    private static String actualArgPhrase(int actual)     { return actual == 0 ? "no argument was given" : actual == 1 ? "only one argument was given" : "got only " + actual; }



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

    /* varargs, 0 or 1 arg * /
    static void varargs0_1(String func, ConsCell a) {
        if (cdr(a) != null) errorArgCount(func, 0, 1, listLength(a), a);
    }*/

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

    static final class Chk {
        private Chk() {}

        /// Additional error checking functions used by primitives only.

        /** at least one arg, the first arg must be a non-nil string */
        static void stringArg(String func, String arg, ConsCell a) {
            if (!stringp(car(a)))
                throw new SimpleTypeError("%s: expected %s to be a string but got %s", func, arg, printSEx(car(a)));
        }

        @SuppressWarnings("SameParameterValue")
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

        @SuppressWarnings("SameParameterValue")
        static void requirePositiveNumber(String func, Object n) {
            if (n instanceof Long && (Long)n > 0L
                || n instanceof Double && (Double)n > 0.0
                || n instanceof Byte && (Byte)n > 0
                || n instanceof Short && (Short)n > 0
                || n instanceof Integer && (Integer)n > 0
                || n instanceof Float && (Float)n > 0
                || n instanceof BigInteger && ((BigInteger)n).compareTo(BigInteger.ZERO) > 0) return;
            throw errorArgTypeError("positive float or integer", func, n);
        }

        static Number requireIntegralNumber(String func, Object n, long minIncl, long maxIncl) {
            if (n == null) errorNotAnInteger(func, null);
            if (n instanceof Long)    { return requireIntegralNumber(func, (Long) n, n, minIncl, maxIncl); }
            if (n instanceof Double)  { return requireIntegralNumber(func, (Double) n, n, minIncl, maxIncl); }
            if (n instanceof Byte)    { return requireIntegralNumber(func, (Byte) n, n, minIncl, maxIncl); }
            if (n instanceof Short)   { return requireIntegralNumber(func, (Short) n, n, minIncl, maxIncl); }
            if (n instanceof Integer) { return requireIntegralNumber(func, (Integer) n, n, minIncl, maxIncl); }
            if (n instanceof Float)   { return requireIntegralNumber(func, (double) (Float) n, n, minIncl, maxIncl); }
            if (n instanceof Number)  { return requireIntegralNumber(func, toDouble(func, n), n, minIncl, maxIncl); }
            throw errorNotAnInteger(func, n);
        }

        private static Number requireIntegralNumber(String func, double d, Object originalValue, long minIncl, long maxIncl) {
            // see https://stackoverflow.com/questions/9898512/how-to-test-if-a-double-is-an-integer
            if (Math.rint(d) == d && !Double.isInfinite(d) && d >= minIncl && d <= maxIncl) return d;
            throw errorNotAnInteger(func, originalValue);
        }

        private static Number requireIntegralNumber(String func, long l, Object originalValue, long minIncl, long maxIncl) {
            if (l >= minIncl && l <= maxIncl) return l;
            throw errorNotAnInteger(func, originalValue);
        }

        @SuppressWarnings("SameParameterValue")
        static Random requireRandom(String func, Object r) {
            if (r instanceof Random) return (Random)r;
            throw errorArgTypeError("random", func, r);
        }


        /** Return {@code c} as a Character, error if {@code c} is not a Character. */
        static Character requireChar(String func, Object c) {
            if (c instanceof Character) return (Character)c;
            throw errorArgTypeError("character", func, c);
        }

        static boolean requireBit(String func, Object value) {
            return requireIntegralNumber(func, value, 0, 1).intValue() != 0;
        }

        @SuppressWarnings("SameParameterValue")
        static Object[] requireSimpleVector(String func, Object c) {
            if (svectorp(c)) return (Object[])c;
            throw errorNotASimpleVector(func, c);
        }

        /** return {@code c} as a String, error if {@code c} is not a string, character or symbol */
        @SuppressWarnings("SameParameterValue")
        static String requireStringDesignator(String func, Object c) {
            if (c == null) return NIL;
            if (c instanceof Character || c instanceof LambdaJSymbol) return c.toString();
            return requireString(func, c);
        }

        static CharSequence requireCharsequence(String func, Object c) {
            if (c instanceof char[]) return String.valueOf((char[])c);
            if (!(c instanceof CharSequence)) errorNotAString(func, c);
            return (CharSequence)c;
        }

        /** Return {@code a} cast to a list, error if {@code a} is not a list or is nil. */
        static ConsCell requireCons(String func, Object a) {
            if (!consp(a)) errorNotACons(func, a);
            return (ConsCell)a;
        }

        @SuppressWarnings("unchecked")
        static Map<Object, Object> requireHash(String func, Object a) {
            if (hashtablep(a)) return (Map<Object, Object>)a;
            throw errorArgTypeError("hashtable", func, a);
        }



        /// Number type conversions

        /** return the argument w/o decimal places as a long, exception if conversion is not possible */
        static long toFixnum(double d) {
            if (Double.isInfinite(d)) errorNotAFixnum("value is Infinite");
            if (Double.isNaN(d)) errorNotAFixnum("value is NaN");
            if (d < MOST_NEGATIVE_FIXNUM_VAL) errorNotAFixnum("underflow");
            if (d > MOST_POSITIVE_FIXNUM_VAL) errorNotAFixnum("overflow");
            return (long)d;
        }

        /** convert {@code a} to a double, error if {@code a} is not a number and/ or cannot be represented as a double (reducing precision is allowed). */
        static double toDouble(Object a) { return toDouble("?", a); }
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
        @SuppressWarnings("SameParameterValue")
        static float toFloat(String func, Object a) {
            final Number n = requireNumber(func, a);

            final float ret = n.floatValue();
            if (n instanceof BigInteger || n instanceof BigDecimal) {
                if (Float.isNaN(ret)) errorOverflow(func, "float", a);
                return ret;
            }
            final double dbl = n.doubleValue();
            if (dbl > Float.MAX_VALUE || dbl < -Float.MAX_VALUE) errorOverflow(func, "float", a);
            return ret;
        }

        /** convert {@code a} to an int, error if {@code a} is not a number. */
        static int toInt(String func, Object a) {
            return requireIntegralNumber(func, a, Integer.MIN_VALUE, Integer.MAX_VALUE).intValue();
        }

        static int toNonnegInt(String func, Object a) {
            return requireIntegralNumber(func, a, 0, Integer.MAX_VALUE).intValue();
        }
    }



    /// Runtime for Lisp programs, i.e. an environment with primitives and predefined global symbols
    static final class Subr {
        private Subr() {}

        /// logic, predicates

        static boolean typep(SymbolTable st, @Null LambdaJ intp, @NotNull Map<LambdaJSymbol, TypeSpec> typeSpecs, @Null Object o, @Null Object typespec) {
            if (typespec == LambdaJ.sT) return true;
            if (typespec == st.intern(NULL)) return null == o;

            if (typespec == st.intern(CONS)) return consp(o);
            if (typespec == st.intern(ATOM)) return atom(o);
            if (typespec == st.intern("symbol")) return symbolp(o);

            if (typespec == st.intern("number")) return numberp(o);
            if (typespec == st.intern("float")) return floatp(o);
            if (typespec == st.intern("integer")) return integerp(o);
            if (typespec == st.intern("bit")) {
                if (!integerp(o)) return false;
                if (o instanceof BigInteger) return false;
                final long l = ((Number)o).longValue();
                return l == 0 || l == 1;
            }

            if (typespec == st.intern("character")) return characterp(o);

            if (typespec == st.intern("random-state")) return randomstatep(o);

            if (typespec == st.intern(VECTOR)) return vectorp(o);
            if (typespec == st.intern("simple-vector")) return svectorp(o);
            if (typespec == st.intern("string")) return stringp(o);
            if (typespec == st.intern("simple-string")) return sstringp(o);
            if (typespec == st.intern("bit-vector")) return bitvectorp(o);
            if (typespec == st.intern("simple-bit-vector")) return sbitvectorp(o);

            if (typespec == st.intern("hash-table")) return hashtablep(o);

            if (typespec == st.intern("function")) return intp == null ? functionp0(o) : intp.functionp(o);

            if (typespec == st.intern(LIST)) return listp(o);
            if (typespec == st.intern("sequence")) return listp(o) || vectorp(o);

            if (o == null) return false; // the object nil aka () is of type null or list or sequence or t which we have already checked

            // conditions
            if (o.getClass() == LambdaJError.class) o = ((LambdaJError)o).getCause();

            @SuppressWarnings("SuspiciousMethodCalls")
            final TypeSpec murmelTypeSpec = typeSpecs.get(typespec);
            if (murmelTypeSpec != null) return murmelTypeSpec.pred.test(o);


            // todo Class.forName().isAssignableFrom() probieren falls JFFI aufgedreht ist


            throw new SimpleError(TYPEP + ": unknown type specifier %s", printSEx(typespec));
        }


        static boolean adjustableArrayP(Object o) {
            //if (!vectorp(o)) throw errorNotAVector("adjustable-array-p", o);  // CL throws this error
            return o instanceof Bitvector || o instanceof StringBuilder || o instanceof StringBuffer || o instanceof List;
        }

        static boolean eql(Object o1, Object o2) {
            return LambdaJ.compare(o1, o2, CompareMode.EQL) == 0;
        }

        static boolean equal(Object o1, Object o2) {
            return LambdaJ.compare(o1, o2, CompareMode.EQUAL) == 0;
        }


        /// conses and lists

        static Object listStar(LambdaJ intp, ConsCell args) {
            if (cdr(args) == null) return car(args);
            if (cddr(args) == null) return intp.cons(car(args), cadr(args));
            final CountingListBuilder b = intp.new CountingListBuilder();
            for (; cdr(args) != null; args = (ConsCell)cdr(args)) {
                b.append(car(args));
            }
            b.appendLast(car(args));
            return b.first();
        }

        /** append args non destructively, all args except the last are shallow copied (list structure is copied, contents is not),
         *  all args except the last must be a list */
        static Object append(LambdaJ intp, ConsCell args) {
            if (args == null) return null;
            if (cdr(args) == null) return car(args);
            if (!listp(car(args))) throw new SimpleTypeError(APPEND + ": first argument %s is not a list", printSEx(car(args)));

            while (args != null && car(args) == null) args = (ConsCell)cdr(args); // skip leading nil args if any

            ConsCell current = args;
            CountingListBuilder lb = null;
            for (; cdr(current) != null; current = (ConsCell)cdr(current)) {
                final Object o = car(current);
                if (o == null) continue;
                if (!consp(o)) throw new SimpleTypeError(APPEND + ": argument is not a list: %s", printSEx(o));
                if (lb == null) lb = intp.new CountingListBuilder();
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
            final ConsCell ccList = requireList(ASSOC, maybeList);
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
        static boolean compare(ConsCell args, String opName, DoubleBiPred pred) {
            double prev = toDouble(opName, car(args));
            for (ConsCell rest = (ConsCell)cdr(args); rest != null; rest = (ConsCell)cdr(rest)) {
                final double next = toDouble(opName, car(rest));
                if (!pred.test(prev, next)) return false;
                prev = next;
            }
            return true;
        }

        /** operator for zero or more args */
        static double addOp(ConsCell _args, String opName, double startVal, DoubleBinaryOperator op) {
            if (_args == null) return startVal;
            ConsCell args = _args;
            double result = toDouble(opName, car(args));

            for (;;) {
                final Object next = cdr(args);
                if (next == null) break;
                if (!consp(next) || next == _args) // missing nested loop check
                    throw new ProgramError("%s: expected a proper list of numbers but got %s", opName, printSEx(_args));
                args = (ConsCell) next;
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
                if (next == null) break;
                if (!consp(next) || next == args) // missing nested loop check
                    throw new ProgramError("%s: expected a proper list of numbers but got %s", opName, printSEx(_args));
                args = (ConsCell) next;
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
            if (n instanceof Long) {
                final long l;
                if ((l = (Long)n) == MOST_POSITIVE_FIXNUM_VAL) errorNotAFixnum("1+: overflow, integer result does not fit in a fixnum");
                return l + 1;
            }
            if (n instanceof Double) return ((Double)n) + 1;
            return incNumber(n);
        }

        private static Number incNumber(Object n) {
            if (n instanceof Byte) return ((Byte)n).intValue() + 1;
            if (n instanceof Short) return ((Short)n).intValue() + 1;
            if (n instanceof Integer) return ((Integer)n).longValue() + 1;
            if (n instanceof BigInteger) {
                final long l;
                try {
                    l = ((BigInteger)n).longValueExact();
                }
                catch (ArithmeticException e) {
                    errorNotAFixnum("1+: overflow, BigInteger argument does not fit in a fixnum");
                    /*notreached*/ throw null;
                }
                if (l == MOST_POSITIVE_FIXNUM_VAL) errorNotAFixnum("1+: overflow, integer result does not fit in a fixnum");
                return l + 1;
            }
            return toDouble("1+", n) + 1;
        }

        static Number dec(Object n) {
            if (n instanceof Double) return ((Double)n) - 1;
            if (n instanceof Long) {
                final long l;
                if ((l = (Long)n) == MOST_NEGATIVE_FIXNUM_VAL) errorNotAFixnum("1-: underflow, integer result does not fit in a fixnum");
                return l - 1;
            }
            return decNumber(n);
        }

        static Number decNumber(Object n) {
            if (n instanceof Byte) return ((Byte)n).intValue() - 1;
            if (n instanceof Short) return ((Short)n).intValue() - 1;
            if (n instanceof Integer) return ((Integer)n).longValue() - 1;
            if (n instanceof BigInteger) {
                final long l;
                try {
                    l = ((BigInteger)n).longValueExact();
                }
                catch (ArithmeticException e) {
                    errorNotAFixnum("1-: underflow, BigInteger argument does not fit in a fixnum");
                    /*notreached*/ throw null;
                }
                if (l == MOST_NEGATIVE_FIXNUM_VAL) errorNotAFixnum("1-: underflow, integer result does not fit in a fixnum");
                return l - 1;
            }
            return toDouble("1-", n) - 1;
        }

        static Number random(Object limit, Object _state) {
            requirePositiveNumber("random", limit);
            final Random state = requireRandom("random", _state);
            if (limit instanceof Long)    return (long)(state.nextDouble() * (Long)limit);
            if (limit instanceof Double)  return state.nextDouble() * (Double)limit;
            if (limit instanceof Byte)    return state.nextInt((Byte)limit);
            if (limit instanceof Short)   return state.nextInt((Short)limit);
            if (limit instanceof Integer) return state.nextInt((Integer)limit);
            if (limit instanceof Float)   return state.nextFloat() * (Float)limit;
            if (limit instanceof BigInteger) {
                // see https://stackoverflow.com/questions/2290057/how-to-generate-a-random-biginteger-value-in-java
                final BigInteger upperLimit = (BigInteger)limit;
                final int nlen = upperLimit.bitLength();
                final BigInteger nm1 = upperLimit.subtract(BigInteger.ONE);
                BigInteger randomNumber, temp;
                do {
                    temp = new BigInteger(nlen + 100, state);
                    randomNumber = temp.mod(upperLimit);
                } while (temp.subtract(randomNumber).add(nm1).bitLength() >= nlen + 100);
                return randomNumber;
            }
            throw errorInternal("can't happen");
        }

        static Random makeRandomState(Random currentState, Object state) {
            if (state == sT) return new Random();
            if (state == null) return copy(currentState);
            if (state instanceof Random) return copy((Random)state);
            if (state instanceof Number) return new Random(((Number)state).longValue());
            throw errorArgTypeError("random or " + T + " or " + NIL + " or number", "make-random-state", state);
        }

        private static final class BOS extends ByteArrayOutputStream {
            BOS() { super(104); }             // Java 1.1 through 20's java.util.Random will be serialized to 104 bytes. Other Roandom classes may be larger, BOS will grow as needed.
            byte[] getBuf() { return buf; }   // provide direct access to buf to avoid copying
        }
        private static Random copy(Random rnd) {
            try {
                final BOS bo = new BOS();
                final ObjectOutputStream oos = new ObjectOutputStream(bo);
                oos.writeObject(rnd);
                oos.close();
                final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bo.getBuf()));
                return (Random)(ois.readObject());
            }
            catch (Exception e) { throw errorInternal(e, "unexpected Exception copying random"); }
        }


        /// vectors

        static final class Bitvector implements Serializable, Writeable, Iterable<Long>, Comparable<Bitvector> {
            class Iter implements Iterator<Long> {
                private int cursor;
                @Override public boolean hasNext() { return cursor < size(); }
                @Override public Long next() { if (cursor == size()) throw new NoSuchElementException(); return get(cursor++); }
            }

            private static final long serialVersionUID = 1L;
            private final BitSet bitSet;
            private int size;

            Bitvector(int capacity, int size) {
                bitSet = new BitSet(capacity);
                this.size = size;
            }

            Bitvector(boolean[] contents) {
                this(contents.length, 0);
                for (boolean b: contents) add(b);
            }

            static Bitvector of(Object o) {
                if (o instanceof Bitvector) return (Bitvector)o;
                if (o instanceof boolean[]) return new Bitvector((boolean[])o);
                throw new SimpleTypeError("not a bitvector: %s", LambdaJ.printSEx(o));
            }

            @Override public Iterator<Long> iterator() { return new Iter(); }

            @Override public boolean equals(Object other) { return other instanceof Bitvector && bitSet.equals(((Bitvector)other).bitSet); }
            @Override public int hashCode() { return bitSet.hashCode(); }

            @Override public int compareTo(Bitvector b2) {
                final int len1 = size();
                final int len2 = b2.size();
                final int lim = Math.min(len1, len2);

                for (int k = 0; k < lim; k++) {
                    final int c1 = (int)get(k);
                    final int c2 = (int)b2.get(k);
                    if (c1 != c2) {
                        return Integer.compare(c1, c2);
                    }
                }
                return Integer.compare(len1, len2);
            }

            int size() { return size; }
            long add(boolean value) { if (value) bitSet.set(size); size++; return size - 1; }
            long get(int idx) { return bitSet.get(idx) ? 1L : 0L; }
            void set(int idx, boolean val) { bitSet.set(idx, val); }

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

            @Override public void printSEx(WriteConsumer sb, boolean escapeAtoms) {
                sb.print("#*");
                int idx = 0;
                for (; idx < bitSet.length(); idx++) sb.print(bitSet.get(idx) ? "1" : "0");
                for (; idx < size; idx++) sb.print("0");
            }
        }

        static Object makeArray(LambdaJSymbol sBit, LambdaJSymbol sCharacter, ConsCell a) {
            final int size = toNonnegInt(MAKE_ARRAY, car(a));
            final Object type = cadr(a);
            final Object cap = caddr(a);
            final boolean adjustable = cap != null;
            final int capacity;
            if (adjustable && cap != sT) capacity = requireIntegralNumber(MAKE_ARRAY, cap, size, ARRAY_DIMENSION_LIMIT_VAL).intValue();
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

            throw new SimpleTypeError(MAKE_ARRAY + ": unsupported or invalid type specification %s", printSEx(type)); // todo sbcl akzeptiert alles als :element-type
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

        static Object vectorCopy(Object vector, boolean adjustablep) {
            if (adjustablep) {
                if (vector instanceof Object[]) return new ArrayList<>(Arrays.asList((Object[])vector));
                if (vector instanceof boolean[]) return new Bitvector((boolean[])vector);
                if (vector instanceof Bitvector) return new Bitvector(((Bitvector)vector).toBooleanArray());
                if (vector instanceof char[]) {
                    final char[] ca = (char[])vector;
                    return new StringBuilder(ca.length + 16).append(ca);
                }
                if (vector instanceof CharSequence) return new StringBuilder((CharSequence)vector);
                if (vector instanceof List<?>) return new ArrayList<>((List<?>)vector);
            }
            else {
                final int length = (int)vectorLength(vector);
                if (vector instanceof Object[]) return Arrays.copyOf((Object[])vector, length);
                if (vector instanceof boolean[]) return Arrays.copyOf((boolean[])vector, length);
                if (vector instanceof Bitvector) return ((Bitvector)vector).toBooleanArray();
                if (vector instanceof char[]) return Arrays.copyOf((char[])vector, length);
                if (vector instanceof StringBuilder) {
                    final StringBuilder sb = (StringBuilder)vector;  final char[] ret = new char[length];  sb.getChars(0, length, ret, 0);
                    return sb;
                }
                if (vector instanceof StringBuffer) {
                    final StringBuffer sb = (StringBuffer)vector;    final char[] ret = new char[length];  sb.getChars(0, length, ret, 0);
                    return sb;
                }
                if (vector instanceof CharSequence) return vector.toString().toCharArray(); // sadly this creates an intermediate String and copies the char[] twice
                if (vector instanceof List<?>) return ((List<?>)vector).toArray(new Object[0]);
            }
            throw errorNotAVector("vector-copy", vector);
        }

        @SuppressWarnings("unchecked")
        static Object vectorFill(Object vector, Object value, Object _start, Object _end) {
            final int length = (int)vectorLength(vector);
            int start = 0, end = length;
            if (_start != null) {
                start = requireIntegralNumber("vector-fill", _start, 0, length).intValue();
                if (_end != null) {
                    end = requireIntegralNumber("vector-fill", _end, start+1, length).intValue();
                }
            }

            if (vector instanceof Object[])      { Arrays.fill((Object[])vector, start, end, value); return vector; }
            if (vector instanceof boolean[])     { Arrays.fill((boolean[])vector, start, end, requireBit("vector-fill", value)); return vector; }
            if (vector instanceof Bitvector)     { ((Bitvector)vector).fill(requireBit("vector-fill", value)); return vector; }
            if (vector instanceof char[])        { Arrays.fill((char[])vector, start, end, requireChar("vector-fill", value)); return vector; }
            if (vector instanceof StringBuilder) { final StringBuilder sb = (StringBuilder)vector; final char c = requireChar("vector-fill", value); for (int i = start; i < end; i++) (sb).setCharAt(i, c); return vector; }
            if (vector instanceof StringBuffer)  { final StringBuffer sb = (StringBuffer)vector;   final char c = requireChar("vector-fill", value); for (int i = start; i < end; i++) (sb).setCharAt(i, c); return vector; }
            if (vector instanceof List)          { @SuppressWarnings("rawtypes") final List list = (List)vector; for (int i = start; i < end; i++) list.set(i, value); return vector; }
            throw errorNotAVector("vector-fill", vector);
        }

        @SuppressWarnings("unchecked")
        static long vectorAdd(Object maybeVector, Object newValue) {
            if (!adjustableArrayP(maybeVector)) throw new InvalidIndexError("vector-add: not an adjustable " + VECTOR + ": %s", printSEx(maybeVector));
            if (maybeVector instanceof StringBuilder) { final StringBuilder sb = (StringBuilder)maybeVector; sb.append(requireChar("vector-add", newValue)); return sb.length() - 1; }
            if (maybeVector instanceof StringBuffer) { final StringBuffer sb = (StringBuffer)maybeVector; sb.append(requireChar("vector-add", newValue)); return sb.length() - 1; }
            if (maybeVector instanceof Bitvector) { final Bitvector bv = (Bitvector)maybeVector; return bv.add(requireBit("vector-add", newValue)); }
            if (maybeVector instanceof List) { @SuppressWarnings("rawtypes") final List l = (List)maybeVector; l.add(newValue); return l.size() - 1; }
            throw errorInternal("vector-add: unknown object type %s", maybeVector);
        }

        static Object vectorToList(LambdaJ intp, Object maybeVector) {
            if (svectorp(maybeVector)) return simpleVectorToList(intp, maybeVector);
            if (stringp(maybeVector)) return stringToList(intp, maybeVector);
            if (sbitvectorp(maybeVector)) return bitVectorToList(intp, maybeVector);

            if (maybeVector instanceof Bitvector || maybeVector instanceof List) {
                final Iterator<?> it = ((Iterable<?>)maybeVector).iterator();
                if (!it.hasNext()) return null;
                final CountingListBuilder ret = intp.new CountingListBuilder();
                do { ret.append(it.next()); }
                while (it.hasNext());
                return ret.first();
            }

            throw errorNotAVector("vector->list", maybeVector);
        }

        static Object listToVector(Object lst, boolean adjustablep) {
            if (lst == null) return adjustablep ? new ArrayList<>() : new Object[0];
            if (adjustablep) {
                final ConsCell l = requireList("list->vector", lst);
                final ArrayList<Object> ret = new ArrayList<>();
                for (Object o: l) ret.add(o);
                return ret;
            }
            return listToArray(lst);
        }

        static long svlength(Object maybeVector) {
            if (maybeVector instanceof Object[]) return ((Object[])maybeVector).length;
            throw errorNotASimpleVector("svlength", maybeVector);
        }

        static Object svref(Object maybeVector, int idx) {
            if (maybeVector instanceof Object[]) return ((Object[])maybeVector)[idx];
            throw errorNotASimpleVector("svref", maybeVector);
        }

        static Object svset(Object maybeVector, int idx, Object newValue) {
            if (maybeVector instanceof Object[]) return ((Object[])maybeVector)[idx] = newValue;
            throw errorNotASimpleVector("svset", maybeVector);
        }

        static Object simpleVectorToList(LambdaJ intp, Object maybeVector) {
            final Object[] s = requireSimpleVector("simple-vector->list", maybeVector);
            if (s.length == 0) return null;
            final CountingListBuilder ret = intp.new CountingListBuilder();
            final int len = s.length;
            for (int i = 0; i < len; i++) ret.append(s[i]);
            return ret.first();
        }


        static long slength(Object maybeVector) {
            if (maybeVector instanceof char[])       return ((char[])maybeVector).length;
            if (!(maybeVector instanceof CharSequence)) errorNotAString("slength", maybeVector);
            return ((CharSequence)maybeVector).length();
        }

        static char sref(Object maybeString, int idx) {
            if (maybeString instanceof char[]) return ((char[])maybeString)[idx];
            return requireCharsequence("sref", maybeString).charAt(idx);
        }

        static char sset(Object maybeString, int idx, char newValue) {
            if (maybeString instanceof char[]) return ((char[])maybeString)[idx] = newValue;
            if (maybeString instanceof StringBuilder) { ((StringBuilder)maybeString).setCharAt(idx, newValue); return newValue; }
            if (maybeString instanceof StringBuffer) { ((StringBuffer)maybeString).setCharAt(idx, newValue); return newValue; }
            if (!(maybeString instanceof String)) errorNotAString("sset", maybeString);
            throw new SimpleTypeError("%s: cannot modify readonly string", "sset");
        }

        static boolean stringEq(Object o1, Object o2) {
            return Objects.equals(requireStringDesignator("string=", o1), requireStringDesignator("string=", o2));
        }

        static Object stringToList(LambdaJ intp, Object maybeString) {
            final CountingListBuilder ret = intp.new CountingListBuilder();
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

        static String stringToImmutableString(Object o) {
            if (o instanceof String) return (String)o;
            if (o instanceof char[]) return new String((char[])o);
            if (o instanceof CharSequence) return o.toString();
            throw new SimpleTypeError("not a string: %s", printSEx(o));
        }

        static Object stringDesignatorToString(Object o) {
            if (o == null) return new char[] { 'n', 'i', 'l' };
            if (o instanceof String) return ((String)o).toCharArray();
            if (o instanceof char[] || o instanceof CharSequence) return o;
            if (o instanceof LambdaJSymbol) return ((LambdaJSymbol)o).name.toCharArray();
            if (o instanceof Character) return new char[] { ((char)o) };
            throw new SimpleTypeError("not a string designator: %s", printSEx(o));
        }

        static Object listToString(Object lst, boolean adjustablep) {
            if (lst == null) return adjustablep ? new StringBuilder() : new char[0];
            final ConsCell l = requireList("list->string", lst);
            final StringBuilder ret = new StringBuilder();
            for (Object c: l) ret.append(requireChar("list->string", c)); // missing nested loop check
            return adjustablep ? ret : ret.toString().toCharArray();
        }


        static long bvlength(Object maybeVector) {
            if (maybeVector instanceof boolean[])    return ((boolean[])maybeVector).length;
            if (maybeVector instanceof Bitvector)    return ((Bitvector)maybeVector).size();
            throw errorNotABitVector("bvlength", maybeVector);
        }

        static long bvref(Object bv, int idx) {
            if (bv instanceof boolean[]) return ((boolean[])bv)[idx] ? 1L : 0L;
            if (bv instanceof Bitvector) { final Bitvector _bv = (Bitvector)bv;   if (idx >= _bv.size()) errorIndexTooLarge(idx, _bv.size()); return _bv.get(idx); }
            throw errorNotABitVector("bvref", bv);
        }

        static long bvset(Object maybeVector, int idx, long newValue) {
            if (maybeVector instanceof boolean[]) {
                final boolean b;
                if (newValue == 0) b = false;
                else if (newValue == 1) b = true;
                else throw errorNotABit("bvset", newValue);
                ((boolean[])maybeVector)[idx] = b;
                return newValue;
            }
            if (maybeVector instanceof Bitvector) { ((Bitvector)maybeVector).set(idx, requireBit("bvset", newValue)); return newValue; }
            throw errorNotABitVector("bvset", maybeVector);
        }

        static boolean bvEq(Object maybeVector1, Object maybeVector2) {
            if (sbitvectorp(maybeVector1) && sbitvectorp(maybeVector2)) return Arrays.equals((boolean[])maybeVector1, (boolean[])maybeVector2);
            if (!bitvectorp(maybeVector1)) throw errorNotABitVector("bv=", maybeVector1);
            if (!bitvectorp(maybeVector2)) throw errorNotABitVector("bv=", maybeVector2);
            if (maybeVector1 == maybeVector2) return true;
            if (vectorLength(maybeVector1) != vectorLength(maybeVector2)) return false;
            for (int i = 0; i < vectorLength(maybeVector1); i++) {
                if (seqref(maybeVector1, i) != seqref(maybeVector2, i)) return false;
            }
            return true;
        }

        static Object bitVectorToList(LambdaJ intp, Object maybeVector) {
            final CountingListBuilder ret;
            if (maybeVector instanceof boolean[]) {
                final boolean[] s = (boolean[])maybeVector;
                final int len = s.length;
                if (len == 0) return null;
                ret = intp.new CountingListBuilder();
                for (int i = 0; i < len; i++) ret.append(s[i] ? 1L : 0L);
            }
            else if (maybeVector instanceof Bitvector) {
                final Bitvector bv = (Bitvector)maybeVector;
                if (bv.size() == 0) return null;
                ret = intp.new CountingListBuilder();
                for (Object bit: bv) ret.append(bit);
            }
            else throw errorNotABitVector("bit-vector->list", maybeVector);
            return ret.first();
        }

        static Object listToBitVector(Object maybeList, boolean adjustablep) {
            final ConsCell lst = requireList("list->bit-vector", maybeList);
            if (adjustablep) {
                final Bitvector bv = new Bitvector(10, 0);
                if (lst != null) for (Object bit: lst) bv.add(requireBit("list->bit-vector", bit));
                return bv;
            }

            if (lst == null) return new boolean[0];
            if (lst instanceof ArraySlice) return ((ArraySlice)lst).listToBooleanArray();
            boolean[] ret = new boolean[32];
            int i = 0;
            final Long zero = 0L, one = 1L;
            for (Object rest = lst; rest != null; rest = cdr(rest)) {
                if (i == ret.length) ret = Arrays.copyOf(ret, ret.length * 2);
                final Object o = car(rest);
                if (zero.equals(o)) ret[i] = false;
                else if (one.equals(o)) ret[i] = true;
                else throw new SimpleTypeError("not a valid value for bitvector: %s", printSEx(o));
                i++;
            }
            return Arrays.copyOf(ret, i);
        }


        /// sequences

        static Object seqref(Object maybeSeq, long idx) {
            checkSequenceBounds(maybeSeq, idx);
            if (maybeSeq instanceof ConsCell)     return ((ConsCell)maybeSeq).elt(idx);
            if (maybeSeq instanceof Object[])     { final Object[]  arry = (Object[])maybeSeq;      if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx]; }
            if (maybeSeq instanceof char[])       { final char[]    arry = (char[])maybeSeq;        if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx]; }
            if (maybeSeq instanceof boolean[])    { final boolean[] arry = (boolean[])maybeSeq;     if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx] ? 1L : 0L; }
            if (maybeSeq instanceof Bitvector)    { final Bitvector bv = (Bitvector)maybeSeq;       if (idx >= bv.size())   errorIndexTooLarge(idx, bv.size());   return bv.get((int)idx); }
            if (maybeSeq instanceof List)         { @SuppressWarnings("rawtypes")
                                                    final List list = (List)maybeSeq;               if (idx >= list.size()) errorIndexTooLarge(idx, list.size()); return list.get((int)idx); }
            if (maybeSeq instanceof CharSequence) { final CharSequence cs = (CharSequence)maybeSeq; if (idx >= cs.length()) errorIndexTooLarge(idx, cs.length()); return cs.charAt((int)idx); }
            throw errorInternal("seqref: unknown object type %s or not implemented", maybeSeq);
        }

        private static void checkSequenceBounds(Object maybeSeq, long idx) {
            if (idx < 0) throw new InvalidIndexError("seqref: index must be >= 0");
            if (maybeSeq == null) errorIndexTooLarge(idx, 0);
        }

        @SuppressWarnings("unchecked")
        static Object seqset(Object maybeSeq, long idx, Object newValue) {
            checkSequenceBounds(maybeSeq, idx);
            if (maybeSeq instanceof ConsCell)      return ((ConsCell)maybeSeq).eltset(newValue, idx);
            if (maybeSeq instanceof Object[])      { final Object[]  arry = (Object[])maybeSeq;  if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx] = newValue; }
            if (maybeSeq instanceof char[])        { final char[]    arry = (char[])maybeSeq;    if (idx >= arry.length) errorIndexTooLarge(idx, arry.length); return arry[(int)idx] = requireChar("seqset", newValue); }
            if (maybeSeq instanceof boolean[])     { final boolean[] arry = (boolean[])maybeSeq; if (idx >= arry.length) errorIndexTooLarge(idx, arry.length);
                                                     final int newBit = requireIntegralNumber("seqset", newValue, 0, 1).intValue();
                                                     if (newBit == 0) { arry[(int)idx] = false; return 0L; }
                                                     if (newBit == 1) { arry[(int)idx] = true;  return 1L; }
                                                     throw errorNotABit("seqset", newValue); }
            if (maybeSeq instanceof Bitvector)     { final Bitvector bv = (Bitvector)maybeSeq; if (idx >= bv.size()) errorIndexTooLarge(idx, bv.size()); bv.set((int)idx, requireBit("seqset", newValue));
                                                    return newValue; }
            if (maybeSeq instanceof StringBuilder) { final StringBuilder sb = (StringBuilder)maybeSeq; if (idx >= sb.length()) errorIndexTooLarge(idx, sb.length());
                                                     final Character c = requireChar("seqset", newValue); sb.setCharAt((int)idx, c);
                                                     return newValue; }
            if (maybeSeq instanceof StringBuffer)  { final StringBuffer sb = (StringBuffer)maybeSeq; if (idx >= sb.length()) errorIndexTooLarge(idx, sb.length());
                                                     final Character c = requireChar("seqset", newValue); sb.setCharAt((int)idx, c);
                                                     return newValue; }
            if (maybeSeq instanceof List)          { @SuppressWarnings("rawtypes") final List list = (List)maybeSeq; if (idx >= list.size()) errorIndexTooLarge(idx, list.size()); list.set((int)idx, newValue);
                                                     return newValue; }
            throw errorInternal("seqset: unknown object type %s or not implemented", maybeSeq);
        }


        /// Hash tables
        static final int DEFAULT_HASH_SIZE = 24; // will give capacity==32
        static final Object NO_DEFAULT_VALUE = new Object();

        /** a hash function that is compatible with equal(o1, o1) aka compare(o1, o2, CompareMode.EQUAL):
         *  two objects that are equal will have the same hash, two objects that are not equal may or may not have the same hash.
         *  Objects with (possibly embedded) loops should be handled as well. */
        static int sxhash(Object o) {
            return sxhashSigned(o) & 0x7fffffff; // Math.abs() won't guarantee a nonnegative number: Math.abs(-2147483648) == -2147483648
        }

        static final class EqlKey implements Comparable<Object> {
            final Object key;
            EqlKey(Object key) { this.key = key; }

            static Object of(Object key) {
                if (key instanceof Float || key instanceof Double || key instanceof BigDecimal) return key;
                return new EqlKey(key);
            }
            @Override public int compareTo(Object o) { if (o instanceof EqlKey) return LambdaJ.compare(this.key, ((EqlKey)o).key, CompareMode.EQL);
                                                       else return LambdaJ.compare(this.key, o, CompareMode.EQL); }
            @Override public int hashCode() { return sxhashSigned(key); }
            @Override public boolean equals(Object o) { if (o instanceof EqlKey) return LambdaJ.compare(this.key, ((EqlKey)o).key, CompareMode.EQL) == 0;
                                                        else return LambdaJ.compare(this.key, o, CompareMode.EQL) == 0; }
        }

        static final class EqualKey implements Comparable<Object> {
            final Object key;
            EqualKey(Object key) { this.key = key; }

            static Object of(Object key) {
                if (key instanceof Float || key instanceof Double || key instanceof BigDecimal) return key;
                return new EqualKey(key);
            }
            @Override public int compareTo(Object o) { if (o instanceof EqualKey) return LambdaJ.compare(this.key, ((EqualKey)o).key, CompareMode.EQUAL);
                                                       else return LambdaJ.compare(this.key, o, CompareMode.EQUAL); }
            @Override public int hashCode() { return sxhashSigned(key); }
            @Override public boolean equals(Object o) { if (o instanceof EqualKey) return LambdaJ.compare(this.key, ((EqualKey)o).key, CompareMode.EQUAL) == 0;
                                                        else return LambdaJ.compare(this.key, o, CompareMode.EQUAL) == 0; }
        }

        /** Note: getEntrySet(), getKeySet() and maybe more Map methods will NOT work as expected! */
        abstract static class MurmelMap extends HashMap<Object, Object> implements Writeable {
            MurmelMap(int size) { super(JavaUtil.hashMapCapacity(size), JavaUtil.DEFAULT_LOAD_FACTOR); }

            abstract String pfx();
            abstract Object makeKey(Object key);
            abstract Object getKey(Map.Entry<?,?> entry);

            @Override public Object put(Object key, Object value) { return super.put(makeKey(key), value); }
            @Override public Object get(Object key) { return super.get(makeKey(key)); }
            @Override public boolean containsKey(Object key) { return super.containsKey(makeKey(key)); }
            @Override public Object remove(Object key) { return super.remove(makeKey(key)); }

            @Override public void printSEx(WriteConsumer out, boolean escapeAtoms) {
                out.print(pfx());
                for (Map.Entry<?,?> entry: entrySet()) {
                    out.print(" ");  LambdaJ.printSEx(out, getKey(entry), escapeAtoms);
                    out.print(" ");  LambdaJ.printSEx(out, entry.getValue(), escapeAtoms);
                }
                out.print(")");
            }
        }

        static class EqlMap extends MurmelMap {
            EqlMap(int size) { super(size); }

            @Override String pfx() { return "#H(eql"; }
            @Override Object makeKey(Object key) { return EqlKey.of(key); }
            @Override Object getKey(Map.Entry<?,?> entry) { if (entry.getKey() instanceof EqlKey) return ((EqlKey)entry.getKey()).key; return entry.getKey(); }
        }

        static class EqualMap extends MurmelMap {
            EqualMap(int size) { super(size); }

            @Override String pfx() { return "#H(equal"; }
            @Override Object makeKey(Object key) { return EqualKey.of(key); }
            @Override Object getKey(Map.Entry<?,?> entry) { if (entry.getKey() instanceof EqualKey) return ((EqualKey)entry.getKey()).key; return entry.getKey(); }
        }

        static class EqlTreeMap extends TreeMap<Object, Object> {
            EqlTreeMap() { super(EqlTreeMap::doCompare); }
            private static int doCompare(Object o1, Object o2) {
                return LambdaJ.compare(o1, o2, CompareMode.EQL);
            }
        }

        static class EqualTreeMap extends TreeMap<Object, Object> {
            EqualTreeMap() { super(EqualTreeMap::doCompare); }
            private static int doCompare(Object o1, Object o2) {
                return LambdaJ.compare(o1, o2, CompareMode.EQUAL);
            }
        }

        static Map<Object,Object> hash(SymbolTable symtab, ConsCell testAndPairs) {
            if (testAndPairs == null) return new EqlMap(DEFAULT_HASH_SIZE);
            final Map<Object,Object> ret = makeHashTable(symtab, car(testAndPairs), DEFAULT_HASH_SIZE);
            final ConsCell pairs = requireList(HASH, testAndPairs.cdr());
            if (pairs == null) return ret;
            final Iterator<?> i = pairs.iterator();
            while (i.hasNext()) {
                final Object key = i.next();
                if (!i.hasNext()) errorMalformedFmt(HASH, "last key/value pair is missing 'value'");
                ret.put(key, i.next());
            }
            return ret;
        }

        static Map<Object,Object> makeHashTable(SymbolTable st, Object test, int size) {
            if (test == sT) return JavaUtil.newHashMap(size);
            if (test == null || test == st.intern(EQL)) return new EqlMap(size);
            if (test == st.intern("compare-eql")) return new EqlTreeMap();
            if (test == st.intern(EQUAL)) return new EqualMap(size);
            if (test == st.intern("compare-equal")) return new EqualTreeMap();
            if (test == st.intern(EQ)) return new IdentityHashMap<>(size);
            throw new SimpleTypeError("only " + NIL + ", " + EQ + ", " + EQL + ", compare-eql, " + EQUAL + ", compare-eql and " + T + " are implemented as 'test', got %s", printSEx(test));
        }

        static Object[] hashref(Object hash, Object key, Object def) {
            final Map<?,Object> map = requireHash("hashref", hash);
            if (map.containsKey(key)) {
                final Object val = map.get(key);
                return new Object[] { val, sT };
            }
            else if (def == NO_DEFAULT_VALUE) return new Object[] { null, null };
            else return new Object[] { def, null };
        }

        static Object hashset(ConsCell args) {
            final Object hashOrGen = car(args);
            if (hashOrGen instanceof IteratorGenerator) return ((IteratorGenerator)hashOrGen).set(cadr(args));
            if (cddr(args) == null) throw new ProgramError("hashset: when the first argument is a hash-table 3 arguments are required");
            return hashset(hashOrGen, cadr(args), caddr(args));
        }

        static Object hashset(Object hash, Object key, Object value) {
            final Map<Object,Object> map = requireHash("hashset", hash);
            map.put(key, value);
            return value;
        }

        static Object hashTableCount(Object hash) {
            return requireHash("hash-table-count", hash).size();
        }

        static Object clrhash(Object hash) {
            requireHash("clrhash", hash).clear();
            return hash;
        }

        static boolean hashRemove(ConsCell args) {
            final Object hashOrGen = car(args);
            if (hashOrGen instanceof IteratorGenerator) return ((IteratorGenerator)hashOrGen).remove();
            if (cdr(args) == null) throw new ProgramError("hash-table-remove: when the first argument is a hash-table 2 arguments are required");
            return hashRemove(hashOrGen, cadr(args));
        }

        static boolean hashRemove(Object hash, Object key) {
            final Map<?,Object> map = requireHash("hash-table-remove", hash);
            final boolean ret = map.containsKey(key);
            map.remove(key);
            return ret;
        }

        interface IteratorGenerator {
            default Object set(Object value) { throw new SimpleError("no such element - hash-table is empty"); }
            default boolean remove() { return false; }
        }

        interface InterpreterIteratorGenerator extends IteratorGenerator, Primitive {}

        static Object scanHash(LambdaJ intp, Object hash) {
            final Map<Object, Object> map = requireHash("scan-hash-table", hash);
            final Function<Map.Entry<?,?>, Object> getKey;
            if (map instanceof MurmelMap) getKey = ((MurmelMap)map)::getKey;
            else getKey = Map.Entry::getKey;

            final Iterator<Map.Entry<Object,Object>> it = map.entrySet().iterator();
            if (it.hasNext()) return new InterpreterIteratorGenerator() {
                private Map.Entry<Object,Object> entry;
                @Override public Object applyPrimitive(ConsCell args) {
                    if (it.hasNext()) { entry = it.next(); final ConsCell tuple = intp.cons(getKey.apply(entry), entry.getValue()); intp.values = intp.cons(tuple, intp.cons(sT, null)); return tuple; }
                    else { entry = null;  intp.values = intp.cons(null, intp.cons(null, null));  return null; }
                }
                @Override public Object set(Object value) { if (entry != null) { entry.setValue(value); return value; } else throw new SimpleError("no such element"); }
                @Override public boolean remove() { it.remove(); entry = null; return true; }
                @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print("#<hash-table generator>"); }
            };
            else return new InterpreterIteratorGenerator() { @Override public Object applyPrimitive(ConsCell args) { intp.values = intp.cons(null, intp.cons(null, null));  return null; }
                                                             @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print("#<empty hash-table generator>"); } };
        }


        /// I/O

        /** (read eof-obj?) -> result */
        static Object read(ObjectReader lispReader, ConsCell a) {
            if (lispReader == null) throw errorUnsupported("read", "%s: lispStdin is " + NIL);
            if (a == null) {
                final Object eof = new Object();
                final Object ret = lispReader.readObj(eof);
                if (ret == eof) wrap0(new EOFException("read: EOF"));
                return ret;
            }
            else {
                return lispReader.readObj(car(a));
            }
        }

        /** (read-from-string str [eof-obj [start [end]]]) -> result, position */
        static Object[] readFromString(SymbolTable st, ConsCell featuresEnvEntry, ConsCell a) {
            final String str = requireString("read-from-string", car(a));
            final StringReader strReader = new StringReader(str);
            a = (ConsCell)cdr(a);

            final long[] count = new long[1];
            final Object eof;
            final long end;
            if (a != null) {
                eof = car(a);
                a = (ConsCell)cdr(a);

                if (a != null) {
                    final long start = requireIntegralNumber("read-from-string", car(a), 0, MOST_POSITIVE_FIXNUM_VAL).longValue();
                    if (start > str.length()) throw new InvalidIndexError("start must be <= string length");
                    try { count[0] = strReader.skip(start); } catch (IOException e) { wrap0(e); }
                    a = (ConsCell)cdr(a);

                    if (a != null) {
                        end = requireIntegralNumber("read-from-string", car(a), 0, MOST_POSITIVE_FIXNUM_VAL).longValue();
                        if (end < start) throw new InvalidIndexError("end must be >= start");
                        if (end > str.length()) throw new InvalidIndexError("end must be <= string length");
                    }
                    else end = -1;
                }
                else end = -1;
            }
            else { eof = null; end = -1; }

            final ObjectReader reader = makeReader(() -> { if (end != -1 && count[0] == end) return EOF; final int c = strReader.read(); if (c != EOF) count[0]++; return c; }, st, featuresEnvEntry);
            final Object ret;
            if (eof == null) {
                final Object myeof = new Object();
                ret = reader.readObj(myeof);
                if (ret == myeof) wrap0(new EOFException("read-from-string: EOF"));
            }
            else ret = reader.readObj(eof);

            return new Object[] { ret, count[0] };
        }

        /** (read-textfile-lines filenamestr [charset]) -> result-string-vector */
        static Object readTextfileLines(ConsCell args) {
            final String fileName = requireString("read-textfile-lines", car(args));
            args = requireList("read-textfile-lines", cdr(args));
            try {
                final Charset cs = args == null ? StandardCharsets.UTF_8 : Charset.forName(requireString("read-textfile-lines", car(args)));
                final List<String> ret = Files.readAllLines(Paths.get(fileName), cs);
                return ret.toArray();
            }
            catch (Exception e) {
                throw wrap(e);
            }
        }

        /** (read-textfile filenamestr [charset]) -> result-string */
        static Object readTextfile(ConsCell args) {
            final String fileName = requireString("read-textfile", car(args));
            args = requireList("read-textfile", cdr(args));
            try {
                final Path p = Paths.get(fileName);
                final Charset cs = args == null ? StandardCharsets.UTF_8 : Charset.forName(requireString("read-textfile", car(args)));
                final CharSequence s = EolUtil.anyToUnixEol(JavaUtil.readString(p, cs));
                return s instanceof StringBuilder ? (StringBuilder)s : new StringBuilder(s);
            }
            catch (Exception e) {
                throw wrap(e);
            }
        }

        /** (write-textfile-lines filenamestr string-sequence  [appendp [charset]]) -> nil */
        @SuppressWarnings("unchecked")
        static Object writeTextfileLines(ConsCell args) {
            final String fileName = requireString("write-textfile-lines", car(args));
            args = (ConsCell)cdr(args);

            final Object seq = car(args);
            if (!listp(seq) && !vectorp(seq)) errorNotASequence("write-textfile-lines", seq);
            args = (ConsCell)cdr(args);

            boolean appendp = false;
            String cs = null;
            if (args != null) {
                if (car(args) != null) appendp = true;
                args = (ConsCell)cdr(args);
                if (args != null) cs = requireString("write-textfile-lines", car(args));
            }
            final Iterator<Object> it;
            if (svectorp(seq)) it = Arrays.asList((Object[])seq).iterator();
            else if (seq instanceof Iterable) it = ((Iterable<Object>)seq).iterator(); // covers ConCell and adjustable array which are ArrayLists
            else throw errorArgTypeError("sequence of strings", "write-textfile-lines", seq);
            try (Writer w = bufferedWriter(fileName, appendp, cs)) {
                final String eol = System.lineSeparator();
                while (it.hasNext()) {
                    final String line = requireString("write-textfile-lines", it.next());
                    w.write(line);
                    w.write(eol);
                }
                return null;
            }
            catch (Exception e) { throw wrap(e); }
        }

        /** (write-textfile filenamestr string [appendp [charset]]) -> nil */
        static Object writeTextfile(ConsCell args) {
            final String fileName = requireString("write-textfile", car(args));
            args = (ConsCell)cdr(args);

            final CharSequence charSeq = requireCharsequence("write-textfile", car(args));
            args = (ConsCell)cdr(args);

            boolean appendp = false;
            String cs = null;
            if (args != null) {
                if (car(args) != null) appendp = true;
                args = (ConsCell)cdr(args);
                if (args != null) cs = requireString("write-textfile", car(args));
            }
            try (BufferedWriter w = bufferedWriter(fileName, appendp, cs)) {
                final String eol = System.lineSeparator();
                if ("\n".equals(eol))
                    w.append(charSeq);
                else for (int i = 0; i < charSeq.length(); i++) {
                    final char c = charSeq.charAt(i);
                    if (c == '\n') w.append(eol);
                    else w.append(c);
                }
                return null;
            }
            catch (Exception e) { throw wrap(e); }
        }

        private static BufferedWriter bufferedWriter(String fileName, boolean appendp, String cs) throws IOException {
            return Files.newBufferedWriter(Paths.get(fileName), cs == null ? StandardCharsets.UTF_8 : Charset.forName(cs),
                                           appendp
                                           ? new OpenOption[]{StandardOpenOption.APPEND, StandardOpenOption.CREATE}
                                           : new OpenOption[]{StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE});
        }

        static Object writeToString(Object arg, boolean printEscape) {
            return printSEx(arg, printEscape);
        }

        static Object write(ObjectWriter lispPrinter, Object arg, boolean printEscape) {
            if (lispPrinter == null) throw errorUnsupported("write", "%s: lispStdout is " + NIL);
            lispPrinter.printObj(arg, printEscape);
            return arg;
        }

        static Object writeln(ObjectWriter lispPrinter, ConsCell arg, boolean printEscape) {
            if (lispPrinter == null) throw errorUnsupported("writeln", "%s: lispStdout is " + NIL);
            if (arg != null) {
                lispPrinter.printObj(car(arg), printEscape);
            }
            lispPrinter.printEol();
            return car(arg);
        }

        static Object lnwrite(ObjectWriter lispPrinter, ConsCell arg, boolean printEscape) {
            if (lispPrinter == null) throw errorUnsupported("lnwrite", "%s: lispStdout is " + NIL);
            lispPrinter.printEol();
            if (arg == null) return null;
            final Object o;
            lispPrinter.printObj(o = car(arg), printEscape);
            lispPrinter.printString(" ");
            return o;
        }

        static String format(ObjectWriter lispPrinter, boolean haveIO, ConsCell a) {
            return format(lispPrinter, haveIO, false, a);
        }

        static String formatLocale(ObjectWriter lispPrinter, boolean haveIO, ConsCell a) {
            return format(lispPrinter, haveIO, true, a);
        }

        private static String format(ObjectWriter lispPrinter, boolean haveIO, boolean locale, ConsCell a) {
            final String func = locale ? "format-locale" : "format";
            varargsMin(func, a, locale ? 3 : 2);
            final boolean toString = car(a) == null;
            a = (ConsCell) cdr(a);

            final String locString;
            if (locale) {
                if (car(a) != null) {
                    stringArg(func, "first argument", a);
                    locString = stringToImmutableString(car(a));
                } else locString = null;
                a = (ConsCell)cdr(a);
            }
            else locString = null;

            stringArg(func, locale ? "third argument" : "second argument", a);
            final String s = stringToImmutableString(car(a));
            final Object[] args = listToArray(cdr(a));
            try {
                if (locString == null) {
                    if (toString) return EolUtil.anyToUnixEol(String.format(s, args)).toString();
                    if (!haveIO) throw errorUnsupported(func, "%s: I/O is disabled");
                    if (lispPrinter == null) throw errorUnsupported(func, "%s: lispStdout is " + NIL);
                    lispPrinter.printString(EolUtil.anyToUnixEol(String.format(s, args)));
                    return null;
                }
                final Locale loc = Locale.forLanguageTag(locString);
                if (toString) return EolUtil.anyToUnixEol(String.format(loc, s, args)).toString();
                if (lispPrinter == null) throw errorUnsupported(func, "%s: lispStdout is " + NIL);
                lispPrinter.printString(EolUtil.anyToUnixEol(String.format(loc, s, args)));
                return null;
            } catch (IllegalFormatException e) {
                // todo sbcl wirft SB-FORMAT:FORMAT-ERROR extends ERROR
                throw new SimpleError("%s: illegal format string and/ or arguments: %s. Error ocurred processing the argument(s) %s", func, e.getMessage(), printSEx(a));
            }
        }

        @NotNull private static RuntimeException errorUnsupported(String func, String msg) { throw new LambdaJError(true, msg, func); }


        /// misc

        static long getInternalRealTime() {
            return System.nanoTime();
        }

        static long getInternalRunTime() {
            return getThreadBean("get-internal-run-time").getCurrentThreadCpuTime();
        }

        @SuppressWarnings("SameParameterValue")
        private static ThreadMXBean getThreadBean(final String func) {
            final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
            if (threadBean == null)
                throw errorUnsupported(func, "%s: ThreadMXBean not supported in this Java Runtime");
            if (!threadBean.isCurrentThreadCpuTimeSupported())
                throw errorUnsupported(func, "%s: ThreadMXBean.getCurrentThreadCpuTime() not supported in this Java Runtime");
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

        interface Boolresult { Object apply(boolean b); }

        static <T extends AbstractListBuilder<T>> ConsCell getDecodedTime(T lb, Boolresult boolResult) {
            final Instant now = Clock.systemDefaultZone().instant();
            final ZonedDateTime n = now.atZone(ZoneId.systemDefault());
            final ZoneRules rules = n.getZone().getRules();
            final boolean daylightSavings = rules.isDaylightSavings(now);
            final double offset = -rules.getOffset(now).get(ChronoField.OFFSET_SECONDS) / 3600.0;
            //get-decoded-time <no arguments> => second, minute, hour, date, month, year, day, daylight-p, zone
            return (ConsCell)lb.appendElements(n.getSecond(), n.getMinute(), n.getHour(),
                                               n.getDayOfMonth(), n.getMonthValue(), n.getYear(), n.getDayOfWeek().getValue() - 1,
                                               boolResult.apply(daylightSavings), offset, null).first();
        }

        /** expand a single macro call */
        static Object macroexpand1(LambdaJ intp, ConsCell args) {
            oneArg("macroexpand-1", args);
            final Object maybeMacroCall = car(args);
            if (!consp(maybeMacroCall)) {
                intp.values = intp.cons(maybeMacroCall, intp.cons(null, null));
                return maybeMacroCall;
            }
            return macroexpandImpl(intp, (ConsCell) maybeMacroCall);
        }

        static Object gensym(Object name) {
            if (name != null) return new LambdaJSymbol(requireString("gensym", name));
            else return new LambdaJSymbol("gensym");
        }

        static void error(Map<LambdaJSymbol, TypeSpec> typeSpecs, Object datum, Object... args) {
            if (datum instanceof Throwable) wrap0((Throwable)datum);

            if (stringp(datum)) { throw new SimpleError(requireString(ERROR, datum), args); }

            final String msg;
            switch (args.length) {
            case 0:  msg = null;  break;
            case 1:  msg = String.format(requireString(ERROR, args[0]));  break;
            default: msg = String.format(requireString(ERROR, args[0]), Arrays.copyOfRange(args, 1, args.length));  break;
            }

            @SuppressWarnings("SuspiciousMethodCalls")
            final TypeSpec murmelTypeSpec = typeSpecs.get(datum);
            if (murmelTypeSpec != null) murmelTypeSpec.thrower.accept(msg);

            throw new SimpleTypeError("error: unknown condition type " + printSEx(datum) + ": " + msg);
        }
    }



    /// Murmel runtime support for Java FFI - Murmel calls Java
    static final class JFFI {
        private JFFI() {}

        private static class JavaConstructor implements Primitive, MurmelJavaProgram.CompilerPrimitive {
            private final Constructor<?> constructor;
            private final UnaryOperator<Object>[] argConv;

            private JavaConstructor(Constructor<?> constructor, Iterable<?> paramClassNames) {
                this.constructor = constructor;
                this.argConv = makeArgConv(paramClassNames, constructor.getParameterCount(), 0);
            }

            @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
            @Override public String toString() { return "#<Java constructor: " + constructor.getName() + '>'; }

            @Override public Object applyPrimitive(ConsCell x) { return applyCompilerPrimitive(listToArray(x)); }

            @Override public Object applyCompilerPrimitive(Object... args) {
                final String name = "new " + constructor.getDeclaringClass().getName();
                javaCallArgCheck(name, constructor, argConv, args);
                try { return constructor.newInstance(args); }
                catch (InvocationTargetException ite) { throw new LambdaJError(true, "%s: %s", name, ite.getTargetException().toString()); }
                catch (Exception e)                   { throw new LambdaJError(true, "%s: %s", name, e.toString()); }
            }
        }

        @SuppressWarnings("unchecked")
        static UnaryOperator<Object>[] makeArgConv(Iterable<?> paramClassNames, int paramCount, int skipThis) {
            final UnaryOperator<Object>[] argConv = new UnaryOperator[paramCount + skipThis];
            int i = 0;
            if (paramClassNames != null) for (Object paramClassName: paramClassNames) {
                final String strParamClassName = (String)paramClassName;
                final Object[] entry = classByName.get(strParamClassName);
                if (entry != null) argConv[i + skipThis] = (UnaryOperator<Object>)entry[2];
                i++;
            }
            return argConv;
        }

        private static final class JavaMethod implements Primitive, MurmelJavaProgram.CompilerPrimitive {
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
                if (!isStatic) {
                    final String className = method.getDeclaringClass().getName();
                    final Object[] entry = classByName.get(className);
                    if (entry != null) argConv[0] = (UnaryOperator<Object>)entry[2];
                }

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
                catch (IllegalAccessException iae) { throw new LambdaJError(iae, false, "cannot access " + method.getDeclaringClass().getSimpleName(), method.getName()); }
            }

            @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print(toString()); }
            @Override public String toString() { return "#<Java method: " + method.getDeclaringClass().getName() + '.' + method.getName() + '>'; }

            @Override public Object applyPrimitive(ConsCell x) { return applyCompilerPrimitive(listToArray(x)); }

            @Override public Object applyCompilerPrimitive(Object... args) {
                final Method method = this.method;
                javaCallArgCheck(method.getName(), method, argConv, args);

                if (!Modifier.isStatic(method.getModifiers()) && !method.getDeclaringClass().isInstance(args[0]))
                    throw new SimpleTypeError(JMETHOD + ": %s is not an instance of class %s", args[0], method.getDeclaringClass().getName());

                try { return invoke.invoke(args); }
                catch (ArithmeticException | ClassCastException | IndexOutOfBoundsException e) { throw new LambdaJError(e); }
                catch (LambdaJError e) { throw e; }
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
                catch (ClassNotFoundException e) { throw new LambdaJError(true, JMETHOD + ": exception finding parameter class %s: %s", strParamClassName, e.toString()); }
            }
            final Class<?>[] params = paramClasses.isEmpty() ? null : paramClasses.toArray(EMPTY_CLASS_ARRAY);
            try {
                final Class<?> clazz = findClass(className);
                return "new".equals(methodName)
                       ? new JavaConstructor(clazz.getDeclaredConstructor(params), paramClassNames)
                       : new JavaMethod(clazz.getMethod(methodName, params), paramClassNames);
            }
            catch (LambdaJError le) { throw le; }
            catch (Exception e) { throw new LambdaJError(true, JMETHOD + ": exception finding method %s.%s: %s", className, methodName, e.getMessage()); }
        }

        static final Map<String, Object[]> classByName = JavaUtil.newHashMap(50);

        static {
            classByName.put("boolean",      new Object[] { boolean.class,        "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });
            classByName.put("byte",         new Object[] { byte.class,           "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte)});
            classByName.put("short",        new Object[] { short.class,          "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });
            classByName.put("int",          new Object[] { int.class,            "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });
            classByName.put("long",         new Object[] { long.class,           "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });
            classByName.put("float",        new Object[] { float.class,          "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });
            classByName.put("double",       new Object[] { double.class,         "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble)});

            classByName.put("char",         new Object[] { char.class,           "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });

            classByName.put("boolean...",   new Object[] { boolean[].class,      "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });
            classByName.put("byte...",      new Object[] { byte[].class,         "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte)});
            classByName.put("short...",     new Object[] { short[].class,        "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });
            classByName.put("int...",       new Object[] { int[].class,          "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });
            classByName.put("long...",      new Object[] { long[].class,         "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });
            classByName.put("float...",     new Object[] { float[].class,        "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });
            classByName.put("double...",    new Object[] { double[].class,       "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble)});

            classByName.put("char...",      new Object[] { char[].class,         "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });


            putWithAlias("Object",          new Object[] { Object.class,         "requireNotNull",      (UnaryOperator<Object>)(MurmelJavaProgram::requireNotNull) });
            putWithAlias("Object?",         new Object[] { Object.class,         null,                  null });
            putWithAlias("Number",          new Object[] { Number.class,         "requireNumber",       (UnaryOperator<Object>)(MurmelJavaProgram::requireNumber) });
            putWithAlias("Number?",         new Object[] { Number.class,         "requireNumberOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireNumberOrNull) });
            putWithAlias("Boolean",         new Object[] { Boolean.class,        "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });
            putWithAlias("Byte",            new Object[] { Byte.class,           "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte) });
            putWithAlias("Short",           new Object[] { Short.class,          "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });
            putWithAlias("Integer",         new Object[] { Integer.class,        "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });
            putWithAlias("Long",            new Object[] { Long.class,           "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });
            putWithAlias("Float",           new Object[] { Float.class,          "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });
            putWithAlias("Double",          new Object[] { Double.class,         "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble) });

            putWithAlias("Object...",       new Object[] { Object[].class,       "requireNotNull",      (UnaryOperator<Object>)(MurmelJavaProgram::requireNotNull) });
            putWithAlias("Object?...",      new Object[] { Object[].class,       null,                  null });
            putWithAlias("Number...",       new Object[] { Number[].class,       "requireNumber",       (UnaryOperator<Object>)(MurmelJavaProgram::requireNumber) });
            putWithAlias("Number?...",      new Object[] { Number[].class,       "requireNumberOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireNumberOrNull) });
            putWithAlias("Boolean...",      new Object[] { Boolean[].class,      "toBoolean",           (UnaryOperator<Object>)(MurmelJavaProgram::toBoolean) });
            putWithAlias("Byte...",         new Object[] { Byte[].class,         "toByte",              (UnaryOperator<Object>)(MurmelJavaProgram::toByte) });
            putWithAlias("Short...",        new Object[] { Short[].class,        "toShort",             (UnaryOperator<Object>)(MurmelJavaProgram::toShort) });
            putWithAlias("Integer...",      new Object[] { Integer[].class,      "toInt",               (UnaryOperator<Object>)(MurmelJavaProgram::toInt) });
            putWithAlias("Long...",         new Object[] { Long[].class,         "toLong",              (UnaryOperator<Object>)(MurmelJavaProgram::toLong) });
            putWithAlias("Float...",        new Object[] { Float[].class,        "toFloat",             (UnaryOperator<Object>)(MurmelJavaProgram::toFloat) });
            putWithAlias("Double...",       new Object[] { Double[].class,       "toDouble",            (UnaryOperator<Object>)(MurmelJavaProgram::toDouble) });

            putWithAlias("Object?[]",       new Object[] { Object[].class,       "requireArray",        (UnaryOperator<Object>)(MurmelJavaProgram::requireArray) });


            putWithAlias("Character",       new Object[] { Character.class,      "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });
            putWithAlias("CharSequence",    new Object[] { CharSequence.class,   "requireCharSequence", (UnaryOperator<Object>)(MurmelJavaProgram::requireCharSequence) });
            putWithAlias("String",          new Object[] { String.class,         "requireString",       (UnaryOperator<Object>)(MurmelJavaProgram::requireString) });
            putWithAlias("String?",         new Object[] { String.class,         "requireStringOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireStringOrNull) });

            putWithAlias("Character...",    new Object[] { Character[].class,    "requireChar",         (UnaryOperator<Object>)(MurmelJavaProgram::requireChar) });
            putWithAlias("CharSequence...", new Object[] { CharSequence[].class, "requireCharSequence", (UnaryOperator<Object>)(MurmelJavaProgram::requireCharSequence) });
            putWithAlias("String...",       new Object[] { String[].class,       "requireString",       (UnaryOperator<Object>)(MurmelJavaProgram::requireString) });
            putWithAlias("String?...",      new Object[] { String[].class,       "requireStringOrNull", (UnaryOperator<Object>)(MurmelJavaProgram::requireStringOrNull) });

            putWithUtilAlias("Comparator",  new Object[] { Comparator.class,     "java.util.Comparator.class.cast", (UnaryOperator<Object>)(Comparator.class::cast) });
        }

        private static void putWithAlias(String clsName, Object[] entry) { classByName.put(clsName, entry); classByName.put("java.lang." + clsName, entry); }
        private static void putWithUtilAlias(String clsName, Object[] entry) { classByName.put(clsName, entry); classByName.put("java.util." + clsName, entry); }

        /** find and load the class given by the (possibly abbreviated) name {@code clsName} */
        private static Class<?> findClass(String clsName) throws ClassNotFoundException {
            final Object[] entry = classByName.get(clsName);
            if (entry != null) return (Class<?>)entry[0];
            return Class.forName(clsName);
        }

        private static class DynamicProxy implements InvocationHandler {
            private final Map<Method, MurmelFunction> methods;

            DynamicProxy(Map<Method, MurmelFunction> methods) { this.methods = methods; }

            @Override public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
                final MurmelFunction func = methods.get(method);
                if (func == null) errorNotAFunction("no function for method %s", method.getName());
                if (args == null) return func.apply();
                else return func.apply(args);
            }
        }

        // todo ConsCell args umstellen auf Object... args? intf: statt name vergleichen: klasse laden und isInstance?
        static Object makeProxy(LambdaJ intp, MurmelJavaProgram program, ConsCell args) {
            final String intf = requireString("jproxy", car(args));
            final String method = requireString("jproxy", cadr(args));
            if ("java.util.Comparator".equals(intf) && "compare".equals(method)) {
                return new Comparator<Object>() { private final MurmelFunction compare = getFunction(intp, program, caddr(args), int.class);
                                                  @Override public String toString() { return "#<Java proxy: java.util.Comparator>"; }
                                                  @Override public int compare(Object o1, Object o2) { // the (int)-cast is safe because JFFI#getFunction() constructs a function that contains a type conversion
                                                                                                       try { return (int)compare.apply(o1, o2); }
                                                                                                       catch (Exception e) { throw wrap(e); } } };
            }
            else if ("java.lang.Runnable".equals(intf) && "run".equals(method)) {
                return new Runnable() { private final MurmelFunction f = getFunction(intp, program, caddr(args), void.class);
                                        @Override public String toString() { return "#<Java proxy: java.lang.Runnable>"; }
                                        @Override public void run() { try { f.apply(); }
                                                                      catch (Exception e) { wrap0(e); } } };
            }
            else return makeDynamicProxy(intp, program, intf, args);
        }

        private static Object makeDynamicProxy(LambdaJ intp, MurmelJavaProgram program, String intf, ConsCell args) {
            try {
                final Class<?> clazz = findClass(intf);
                final Map<Method, MurmelFunction> methodToMurmelFunction = new HashMap<>(); // todo kann/ soll das eine IdentityHashMap sein?
                final Map<String, Method> nameToMethod = new HashMap<>();

                final MurmelFunction notImplemented = a -> { throw new UndefinedFunction("method is not implemented"); };
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
                                           a -> { final WriteConsumer out = (WriteConsumer)a[0]; out.print(asString); return null; });

                for (ConsCell lst = requireList("jproxy", cdr(args)); lst != null; ) {
                    if (cdr(lst) == null) throw new ProgramError("jproxy: odd number of method/functions");

                    final Object form = cadr(lst);
                    if (form == null) throw new UndefinedFunction("jproxy: not a function: " + NIL);

                    final String name = requireString("jproxy", car(lst));
                    final Method method = nameToMethod.get(name);
                    if (method == null) throw new UndefinedFunction("jproxy: method %s does not exist in interface %s or is not accessible", name, intf);
                    methodToMurmelFunction.put(method, getFunction(intp, program, form, method.getReturnType()));

                    lst = (ConsCell)cddr(lst);
                }
                return Proxy.newProxyInstance(LambdaJ.class.getClassLoader(), new Class<?>[] { clazz, Writeable.class }, new DynamicProxy(methodToMurmelFunction));
            }
            catch (ClassNotFoundException | NoSuchMethodException e) {
                throw new LambdaJError(true, "exception loading class %s", intf);
            }
        }

        static @NotNull MurmelFunction getFunction(LambdaJ intp, MurmelJavaProgram program, Object function, Class<?> returnType) {
            final String funcName = printSEx(function).toString();
            final Function<Object, Object> convertReturnType = makeConvertReturnType(funcName, returnType);
            if (function instanceof MurmelJavaProgram.CompilerPrimitive)  { return args -> convertReturnType.apply(((MurmelJavaProgram.CompilerPrimitive)function).applyCompilerPrimitive(args)); }
            if (function instanceof Primitive)                            { return args -> convertReturnType.apply(((Primitive)function).applyPrimitiveVarargs(args)); }
            if (function instanceof Closure && intp != null)              { final CallLambda callLambda = intp.new CallLambda((Closure)function);
                                                                            return args -> convertReturnType.apply(callLambda.apply(args)); }
            if (function instanceof MurmelFunction && program != null)    { return args -> convertReturnType.apply(program.funcall((MurmelFunction)function, args)); /* must use the TCO trampoline */ }

            throw errorNotAFunction("getFunction: not a primitive or " + LAMBDA + ": %s", funcName);
        }

        private static Function<Object, Object> makeConvertReturnType(String func, Class<?> returnType) {
            if (Boolean.class.equals(returnType)   || boolean.class.equals(returnType)) return Objects::nonNull;
            if (Byte.class.equals(returnType)      || byte.class.equals(returnType))    return value -> requireIntegralNumber(func, value, Byte.MIN_VALUE, Byte.MAX_VALUE).byteValue();
            if (Short.class.equals(returnType)     || short.class.equals(returnType))   return value -> requireIntegralNumber(func, value, Short.MIN_VALUE, Short.MAX_VALUE).shortValue();
            if (Integer.class.equals(returnType)   || int.class.equals(returnType))     return value -> requireIntegralNumber(func, value, Integer.MIN_VALUE, Integer.MAX_VALUE).intValue();
            if (Long.class.equals(returnType)      || long.class.equals(returnType))    return value -> requireIntegralNumber(func, value, Long.MIN_VALUE, Long.MAX_VALUE).longValue();
            if (Double.class.equals(returnType)    || double.class.equals(returnType))  return value -> requireNumber(func, value).doubleValue();
            if (Character.class.equals(returnType) || char.class.equals(returnType))    return value -> requireChar(func, value);
            if (Void.class.equals(returnType)      || void.class.equals(returnType))    return value -> null;

            if (Number.class.equals(returnType))       return value -> requireNumber(func, value);
            if (String.class.equals(returnType))       return value -> requireString(func, value);
            if (CharSequence.class.equals(returnType)) return value -> requireCharsequence(func, value);

            // todo weitere typen und/ oder error oder converter aus der HashMap auslesen? was passiert bei arrays?
            return value -> value == null ? null : returnType.cast(value);
        }
    }



    ConsCell values = NO_VALUES;

    Random getRandom() {
        assert have(Features.HAVE_NUMBERS) : "getRandom() should only be called when feature NUMBERs is enabled";
        assert randomStateEnvEntry != null;
        if (cdr(randomStateEnvEntry) == null)
            randomStateEnvEntry.rplacd(new Random());
        return (Random)cdr(randomStateEnvEntry);
    }

    TurtleFrame current_frame;

    /** Return {@code a} as a TurtleFrame or current_frame if null, error if {@code a} is not of type frame. */
    TurtleFrame requireFrame(String func, Object a) {
        final TurtleFrame ret;
        if (a == null) {
            ret = current_frame;
        }
        else {
            if (!(a instanceof TurtleFrame)) throw errorArgTypeError("frame", func, a);
            ret = (TurtleFrame) a;
        }
        if (ret == null) throw new UnboundVariable("%s: no frame argument and no current frame", func);
        return ret;
    }

    private ObjectReader lispReader;
    private ObjectWriter lispPrinter;

    /** return the current stdin */
    public ObjectReader getLispReader()  { return lispReader; }

    /** return the current stdout */
    public ObjectWriter getLispPrinter() { return lispPrinter; }

    ObjectWriter getLispPrinter(Object consumer, ObjectWriter defaultIfNull) {
        if (consumer == null) return defaultIfNull;
        if (consumer == sT) return lispPrinter;
        if (consumer instanceof Appendable) return new SExpressionWriter(csq -> { try { ((Appendable)consumer).append(csq); } catch (IOException e) { wrap0(e); } });
        throw new SimpleTypeError("cannot coerce %s into a printer", printSEx(consumer));
    }

    /** set new stdin/stdout */
    public void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) {
        this.lispReader = lispStdin;
        this.lispPrinter = lispStdout;
    }


    /** build an environment by prepending the previous environment {@code env} with the primitive functions,
     *  generating symbols in the {@link SymbolTable} {@link #symtab} on the fly */
    private void environment() {
        WellknownSymbol.forAllPrimitives(features, w -> extendGlobal(internWellknown(w.sym), (Primitive)a -> w.applyPrimitive(this, a)));

        if (have(Features.HAVE_T)) extendGlobal(sT, sT);
        if (have(Features.HAVE_NIL)) extendGlobal(sNil, null);
        if (have(Features.HAVE_VECTOR)) extendGlobal(ARRAY_DIMENSION_LIMIT, ARRAY_DIMENSION_LIMIT_VAL);

        if (have(Features.HAVE_APPLY)) {
            final LambdaJSymbol sApply = intern(APPLY);
            ocApply = new OpenCodedPrimitive(sApply);
            extendGlobal(sApply, ocApply);
        }

        if (have(Features.HAVE_XTRA)) {
            final LambdaJSymbol sEval = intern(EVAL);
            ocEval = new OpenCodedPrimitive(sEval);
            extendGlobal(sEval, ocEval);

            assert conditionHandlerEnvEntry != null : "when feature XTRA is enabled conditionHandlerEnvEntry should be != null";
            extendGlobal(conditionHandlerEnvEntry);
        }

        if (have(Features.HAVE_UTIL)) {
            extendGlobal(featuresEnvEntry);
            extendGlobal(INTERNAL_TIME_UNITS_PER_SECOND, (long)1e9);
        }

        if (have(Features.HAVE_NUMBERS)) {
            extendGlobal(PI, Math.PI);
            extendGlobal(MOST_POSITIVE_FIXNUM, MOST_POSITIVE_FIXNUM_VAL);
            extendGlobal(MOST_NEGATIVE_FIXNUM, MOST_NEGATIVE_FIXNUM_VAL);
            assert randomStateEnvEntry != null : "when feature NUMBERs is enabled randomStateEnvEntry should be != null";
            extendGlobal(randomStateEnvEntry);
        }
    }


    ///
    /// ## Invoking the interpreter
    ///

    /// JMurmel native embed API: Java calls Murmel with getValue() and getFunction()

    /** embed API: interface for compiled lambdas as well as primitives and jmethods, used for embedding as well as compiled Murmel */
    public interface MurmelFunction { Object apply(Object... args) throws Exception; }

    /** embed API: Return the value of {@code globalSymbol} in the interpreter's current global environment */
    public Object getValue(String globalSymbol) {
        final ConsCell envEntry = lookupGlobalEntry(intern(globalSymbol));
        if (envEntry != null) return cdr(envEntry);
        throw errorUnbound("getValue", globalSymbol);
    }

    private class CallLambda implements MurmelFunction {
        private final Closure lambda;
        CallLambda(Closure lambda) { this.lambda = lambda; }
        @Override public Object apply(Object... args) {
            return eval(cons(lambda, arraySlice(args, 0)), null);
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
    public @NotNull MurmelFunction getFunction(String funcName) {
        return getFunction(this, funcName, getValue(funcName));
    }

    private static @NotNull MurmelFunction getFunction(LambdaJ intp, String funcName, Object function) {
        if (function instanceof MurmelJavaProgram.CompilerPrimitive)  { return ((MurmelJavaProgram.CompilerPrimitive)function)::applyCompilerPrimitive; }
        if (function instanceof Primitive)                            { return ((Primitive)function)::applyPrimitiveVarargs; }
        if (function instanceof Closure)                              { return intp.new CallLambda((Closure)function); }
        if (function instanceof MurmelFunction)                       { return args -> intp.compiledProgram.funcall((MurmelFunction)function, args); /* must use the TCO trampoline */ }

        throw errorNotAFunction("getFunction: not a primitive or " + LAMBDA + ": %s", funcName);
    }

    public interface MurmelProgram {
        Object getValue(String globalSymbol);
        @NotNull MurmelFunction getFunction(String funcName);

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

        CallProgram(String program, ReadSupplier in, WriteConsumer out) {
            this.program = program;
            this.in = in;
            this.out = out;
        }

        @Override public Object getValue(String globalSymbol) { return LambdaJ.this.getValue(globalSymbol); }
        @Override public @NotNull MurmelFunction getFunction(String funcName) { return LambdaJ.this.getFunction(funcName); }

        @Override public void setCommandlineArgumentList(ConsCell args) {
            extendGlobal(intern(COMMAND_LINE_ARGUMENT_LIST), args);
        }
        @Override public ObjectReader getLispReader() { return LambdaJ.this.getLispReader(); }
        @Override public ObjectWriter getLispPrinter() { return LambdaJ.this.getLispPrinter(); }
        @Override public void setReaderPrinter(ObjectReader reader, ObjectWriter writer) { LambdaJ.this.setReaderPrinter(reader, writer); }

        @Override public Object body() {
            return interpretExpressions(new StringReader(program)::read, in, out);
        }
    }


    /// JMurmel JSR-223 embed API - Java calls Murmel with JSR223 eval

    /** <p>evalScript is for JSR-223 support. */
    public Object evalScript(Reader program, Reader in, Writer out, Map<String, Object> engineBindings) {
        final SExpressionReader lispStdin = makeReader(in::read, null);
        final SExpressionWriter lispStdout = new SExpressionWriter(new WrappingWriter(out)::append);

        if (speed == -1) init(lispStdin, lispStdout, null);
        else setReaderPrinter(lispStdin, lispStdout);
        if (engineBindings != null) for (Map.Entry<String, Object> entry: engineBindings.entrySet()) {
            extendGlobal(entry.getKey(), entry.getValue()); // create new or replace existing binding
        }

        final ObjectReader scriptParser = makeReader(program::read, null);
        currentSource = null;
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
        traceStats(tStart);
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
        return interpretExpressions(program, inReader, outWriter, customEnv, true);
    }

    public Object interpretExpressions(ObjectReader program, ObjectReader inReader, ObjectWriter outWriter, CustomEnvironmentSupplier customEnv, boolean reset) {
        final ConsCell customEnvironment = customEnv == null ? null : customEnv.customEnvironment(symtab);
        if (reset || globals.isEmpty()) init(inReader, outWriter, customEnvironment);
        else setReaderPrinter(null, outWriter);
        currentSource = program.getInput();
        final boolean traceStats = trace.ge(TraceLevel.TRC_STATS);
        final Object eof = "EOF";
        Object result = null;
        Object exp;
        while ((exp = program.readObj(true, eof)) != eof) {
            final long tStart = traceStats ? System.nanoTime() : 0;
            result = expandAndEval(exp, null);
            if (traceStats) traceStats(tStart);
        }
        return result;
    }

    /** print and reset interpreter stats and wall time. preceeded and followed by a newline. */
    void traceStats(long startNanos) {
        if (trace.ge(TraceLevel.TRC_STATS)) {
            tracer.println("");
            tracer.println("*** max Murmel evaluator recursion: " + maxEvalLevel + " ***");
            tracer.println("*** max eval() on Java stack:       " + maxEvalStack + " ***");

            tracer.println("*** total ConsCells:                " + nCells + " ***");
            if (trace.ge(TraceLevel.TRC_ENVSTATS)) tracer.println("*** max env length:                 " + maxEnvLen + " ***");

            final long nanos = System.nanoTime() - startNanos;
            final long millis = (long)(nanos * 0.000001D);
            final String ms = Long.toString(millis) + '.' + ((long) (nanos * 0.001D + 0.5D) - (long) (millis * 1000D));
            tracer.println("*** elapsed wall time:              " + ms + "ms ***");
            tracer.println("");

            resetCounters();
        }
    }



    /// static void main() - run JMurmel from the command prompt (interactive)

    /** static main() function for commandline use of the Murmel interpreter */
    public static void main(String[] args) {
        final int rc = Cli.mainInternal(args);
        // if rc == 0 then don't System.exit() but simply return from main so that the program will only end after all TurtleFrames have been closed
        if (rc != 0) System.exit(rc);
    }

    /** wrap all the CLI stuff in a utility class.
     *  For embedded use of JMurmel/ LambdaJ one could remove the function {@link #main} and the class {@link Cli},
     *  and (unless it is used) the class {@link MurmelJavaCompiler} as well. */
    static final class Cli {
        private Cli() {}

        private enum Action { INTERPRET, TO_JAVA, TO_JAR, COMPILE_AND_RUN, }

        static class Exit extends RuntimeException {
            final int rc;
            Exit(int rc) { super(null, null, false, false); this.rc = rc; }
        }

        static final Exit EXIT_SUCCESS =       new Exit(0);

        static final Exit EXIT_PROGRAM_ERROR = new Exit(1);

        static final Exit EXIT_CMDLINE_ERROR = new Exit(128);
        static final Exit EXIT_IO_ERROR =      new Exit(129);
        static final Exit EXIT_RUNTIME_ERROR = new Exit(255);

        static int mainInternal(String[] args) {
            try {
                final boolean finalResult = finalResult(args);
                final boolean script = hasFlag("--script", args, false);
                final boolean error = handleScript(args);
                final boolean scriptFlagError;
                if (script && (hasFlag("--repl", args, false) || hasFlag("--tty", args, false) || hasFlag("--eval", args, false))) {
                    scriptFlagError = true;
                    System.err.println("LambdaJ: when using --script neither --repl nor --tty nor --eval may be used as well");
                }
                else scriptFlagError = false;


                misc(args);
                final Action action = action(args);
                final TraceLevel trace = trace(args);
                final int features = features(args);

                final boolean istty = hasFlag("--tty", args) || null != System.console(); // starting with Java20ea-27 the behaviour of System.console() has changed: will return != null even with redirected stdin
                                                                                          // old behaviour can be restored with -Djdk.console=jdk.jshell
                                                                                          // see https://bugs.openjdk.org/browse/JDK-8297226 and https://github.com/openjdk/jdk/pull/11421
                final boolean repl = hasFlag("--repl", args);
                final boolean echo = hasFlag("--echo", args);    // used only in repl
                final boolean printResult = hasFlag("--result", args);  // print individual results of toplevel forms, used only when interpreting files given on the commandline or interpreting piped input
                final boolean verbose = hasFlag("--verbose", args);
                final String clsName = flagValue("--class", args);
                final String outDir = flagValue("--outdir", args);
                final String libDir = flagValue("--libdir", args);
                final String immediateForms = flagValues("--eval", args);

                if (argError(args) || error || scriptFlagError) {
                    System.err.println("LambdaJ: exiting because of previous errors.");
                    throw EXIT_CMDLINE_ERROR;
                }

                final Path libPath = getLibPath(libDir);

                final LambdaJ interpreter = new LambdaJ(features, trace, null, null, null, null, null, libPath);

                final List<Object> history = repl ? new ArrayList<>() : null;

                // process files given on the commandline
                final List<String> files = args(args);
                try {
                    if (!files.isEmpty() || immediateForms != null) {
                        switch (action) {
                        case INTERPRET:
                            interpreter.init(NULL_READCHARS, NULL_WRITECHARS);
                            injectCommandlineArgs(interpreter, args);
                            Object result = null;
                            for (String fileName : files) {
                                if ("--".equals(fileName)) continue;
                                if (verbose) System.out.println("interpreting " + fileName + "...");
                                final Path p = Paths.get(fileName);
                                result = interpretStream(interpreter, ReadSupplier.of(p), p, printResult, history);
                            }
                            if (immediateForms != null) {
                                result = interpretStream(interpreter, new StringReadSupplier(immediateForms), null, printResult, history);
                            }
                            if (finalResult && !printResult && result != null) {
                                System.out.println();
                                System.out.println("==> " + printSEx(result));
                            }
                            if (script) exit(result);
                            break;
                        case TO_JAVA:
                            final boolean javaSuccess = compileFiles(files, immediateForms, false, clsName, libPath, outDir);
                            if (!istty && !javaSuccess) throw EXIT_RUNTIME_ERROR;
                            break;
                        case TO_JAR:
                            final boolean jarSuccess = compileFiles(files, immediateForms, true, clsName, libPath, outDir);
                            if (!istty && !jarSuccess) throw EXIT_RUNTIME_ERROR;
                            break;
                        case COMPILE_AND_RUN:
                            final Object res = compileAndRunFiles(files, immediateForms, interpreter, args, verbose, finalResult);
                            if (script) exit(res);
                            break;
                        }
                    }
                }
                catch (IOException e) {
                    System.err.println();  System.err.println(e);
                    throw EXIT_IO_ERROR;
                }
                interpreter.currentSource = null;

                // repl() doesn't return
                if (files.isEmpty() && immediateForms == null && istty || repl) repl(interpreter, (immediateForms != null || !files.isEmpty()) && action == Action.INTERPRET, istty, echo, history, args);

                if (files.isEmpty() && immediateForms == null) {
                    final String consoleCharsetName = System.getProperty("sun.stdout.encoding");
                    final Charset consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);

                    if (action == Action.INTERPRET) {
                        interpreter.init(NULL_READCHARS, NULL_WRITECHARS);
                        injectCommandlineArgs(interpreter, args);
                        final Object result = interpretStream(interpreter, new InputStreamReader(System.in, consoleCharset)::read, null, printResult, null);
                        if (finalResult && !printResult && result != null) {
                            System.out.println();
                            System.out.println("==> " + printSEx(result));
                        }
                    }
                    else {
                        final SExpressionReader parser = interpreter.makeReader(new InputStreamReader(System.in, consoleCharset)::read, null);

                        switch (action) {
                        case TO_JAVA:
                            final boolean successJava = compileToJava(StandardCharsets.UTF_8, interpreter.getSymbolTable(), interpreter.libDir, parser, clsName, outDir);
                            if (successJava) System.out.println("compiled stdin to " + (clsName == null ? "MurmelProgram" : clsName));
                            break;
                        case TO_JAR:
                            final String outFile = outDir != null ? outDir + "/a.jar" : "a.jar";
                            final boolean successJar = compileToJar(interpreter.getSymbolTable(), libPath, parser, clsName, outFile);
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

        /** exit by throwing an {@link Exit} exception, doesn't return. The last form of the program will determine the exitlevel:
         *  nil will result in 0, a number will result in an exitlevel of number&127, any other non-nil value will result in an exitlevel of 1. */
        private static void exit(Object murmelResult) {
            if (murmelResult == null) throw new Exit(0);
            if (numberp(murmelResult)) throw new Exit(((Number)murmelResult).intValue() & 0x7f); // limit to 127, 255 is reserved for EXIT_RUNTIME_ERROR
            throw EXIT_PROGRAM_ERROR;
        }



        /// functions to interpret, compile and/ or run files or input streams
        private static Object interpretStream(final LambdaJ interpreter, ReadSupplier prog, Path fileName, final boolean printResult, List<Object> history) {
            final Path prev = interpreter.currentSource;
            try {
                final ObjectReader reader = interpreter.getLispReader();
                reader.setInput(prog, fileName);
                interpreter.currentSource = fileName;
                final ObjectReader inReader = new SExpressionReader(interpreter.features, TraceLevel.TRC_NONE, null, interpreter.getSymbolTable(), interpreter.featuresEnvEntry, System.in::read, null);
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
                    interpreter.traceStats(tStart);
                    if (printResult) {
                        System.out.println();
                        System.out.print("==> "); outWriter.printObj(result, true); System.out.println();
                    }
                }
                return result;
            }
            catch (Exception e) { return Repl.errorExit(e); }
            finally { interpreter.currentSource = prev; }
        }

        private static boolean compileFiles(final List<String> files, String forms, boolean toJar, String clsName, Path libPath, String outDir) throws IOException {
            final SymbolTable symtab = new ListSymbolTable();
            final MurmelJavaCompiler c = new MurmelJavaCompiler(symtab, libPath, getTmpDir());

            final ObjectReader program = parseFiles(files, forms, c.intp, true);
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

        private static Object compileAndRunFiles(List<String> files, String forms, LambdaJ interpreter, String[] args, boolean verbose, boolean finalResult) throws IOException {
            final ObjectReader program = parseFiles(files, forms, interpreter, verbose);
            return compileAndRunForms(program, args, interpreter, false, finalResult);
        }

        /** compile history to a class and run compiled class */
        static Object compileAndRunForms(ObjectReader history, String[] cmdlineArgs, LambdaJ interpreter, boolean repl, boolean finalResult) {
            final Path tmpDir;
            try { tmpDir = getTmpDir(); }
            catch (IOException e) {
                System.out.println("history NOT run as Java - cannot get/ create tmp directory: " + e.getMessage());
                if (!repl) throw EXIT_IO_ERROR;
                return null;
            }

            MurmelProgram prg = null;
            try {
                final MurmelJavaCompiler c = new MurmelJavaCompiler(interpreter.getSymbolTable(), interpreter.libDir, tmpDir);
                final Class<MurmelProgram> murmelClass = c.formsToJavaClass("MurmelProgram", history, null);
                prg = murmelClass.getDeclaredConstructor().newInstance();
                injectCommandlineArgs(prg, cmdlineArgs);
                final long tStart = System.nanoTime();
                final Object result = prg.body();

                final long nanos = System.nanoTime() - tStart;
                if (interpreter.trace.ge(TraceLevel.TRC_STATS)) {
                    interpreter.tracer.println("");
                    final long millis = (long)(nanos * 0.000001D);
                    final String ms = Long.toString(millis) + '.' + ((long) (nanos * 0.001D + 0.5D) - (long) (millis * 1000D));
                    interpreter.tracer.println("*** elapsed wall time: " + ms + "ms ***");
                    interpreter.tracer.println("");
                }

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
                if (repl) {
                    final String msg = (prg != null ? "runtime error" : "error") + location(prg) + ": " + e.getMessage();
                    System.out.println("history NOT run as Java - " + msg);
                }
                else Repl.errorExit(e);
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

        static boolean compileToJava(Charset charset, SymbolTable st, Path libDir, ObjectReader history, Object className, Object filename) {
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

        static boolean compileToJar(SymbolTable st, Path libDir, ObjectReader history, Object className, Object jarFile) {
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
            String consoleCharsetName = System.getProperty("sun.stdout.encoding");
            if (consoleCharsetName == null) consoleCharsetName = "UTF-8";
            final Charset consoleCharset = Charset.forName(consoleCharsetName);

            final Repl repl = new Repl(new InputStreamReader(System.in, consoleCharset)::read, System.out, interpreter, isInit, echo, prevHistory, args, consoleCharsetName);
            if (!echo) {
                System.out.println("Enter a Murmel form or :command (or enter :h for command help or :q to exit):");
                System.out.println();
            }

            for (;;) {
                if (!repl.echo) {
                    System.out.print("JMurmel> ");
                    if (istty) System.out.flush();
                }

                repl.oneForm(istty, System.lineSeparator());
            }
        }

        static class Repl {
            private final WriteConsumer stdout;
            private final LambdaJ interpreter;
            private boolean isInit, echo;
            private final String[] args;
            private final LambdaJSymbol cmdQuit, cmdHelp, cmdDesc, cmdEcho, cmdNoEcho, cmdEnv, cmdMacros, cmdRes, cmdList, cmdWrite, cmdJava, cmdRun, cmdJar;

            private final LambdaJSymbol define, setq, quote;
            private final LambdaJSymbol form0;
            private final LambdaJSymbol form1, form2, form3;
            private final LambdaJSymbol result1, result2, result3;
            private final LambdaJSymbol values1, values2, values3;

            private final Object eof = "EOF";
            private final List<Object> history;
            private SExpressionReader parser;
            private ObjectWriter outWriter;

            private final Charset consoleCharset;
            private final ReadSupplier echoingSupplier;
            private final ReadSupplier nonechoingSupplier;

            private final boolean replVars;
            private final Object bye;
            private final Runnable initReplVars;

            Repl(@NotNull ReadSupplier consoleReader, @NotNull Appendable stdout, @NotNull LambdaJ interpreter, boolean isInit, boolean echo,
                 List<Object> prevHistory, String[] args, String consoleCharsetName) {
                this.stdout = makeWriteConsumer(stdout);
                this.interpreter = interpreter;
                this.isInit = isInit;
                this.echo = echo;
                this.args = args;
                cmdQuit = interpreter.intern(":q");
                cmdHelp = interpreter.intern(":h");
                cmdDesc = interpreter.intern(":desc");
                cmdEcho = interpreter.intern(":echo");
                cmdNoEcho = interpreter.intern(":noecho");
                cmdEnv = interpreter.intern(":env");
                cmdMacros = interpreter.intern(":macros");
                cmdRes = interpreter.intern(":res");
                cmdList = interpreter.intern(":l");
                cmdWrite = interpreter.intern(":w");
                cmdJava = interpreter.intern(":java");
                cmdRun = interpreter.intern(":r");
                cmdJar = interpreter.intern(":jar");

                define = interpreter.intern(DEFINE);
                setq = interpreter.intern(SETQ);
                quote = interpreter.intern(QUOTE);
                form0 = interpreter.intern("@-");
                form1 = interpreter.intern("@+");
                form2 = interpreter.intern("@++");
                form3 = interpreter.intern("@+++");
                result1 = interpreter.intern("@*");
                result2 = interpreter.intern("@**");
                result3 = interpreter.intern("@***");
                values1 = interpreter.intern("@/");
                values2 = interpreter.intern("@//");
                values3 = interpreter.intern("@///");

                history = prevHistory == null ? new ArrayList<>() : prevHistory;

                consoleCharset = consoleCharsetName == null ? StandardCharsets.UTF_8 : Charset.forName(consoleCharsetName);
                echoingSupplier = () -> {
                    final int c = consoleReader.read();
                    if (c != EOF) stdout.append((char)c);
                    return c;
                };
                nonechoingSupplier = consoleReader;

                replVars = interpreter.have(Features.HAVE_XTRA) && interpreter.have(Features.HAVE_DEFINE);
                bye = new Object();
                initReplVars = () -> {
                    for (Object v : new Object[] { form0, form1, form2, form3, result1, result2, result3, values1, values2, values3 }) {
                        interpreter.eval(ConsCell.list(define, v, null), null);
                    }
                    interpreter.eval(ConsCell.list(define,
                                                   interpreter.intern("quit"),
                                                   (Primitive)a -> { throw new ReturnException(bye, 0, (Object[])null); }),
                                     null);
                };

                if (isInit) {
                    interpreter.resetCounters();
                    parser = new SExpressionReader(interpreter.features, interpreter.trace, interpreter.tracer, interpreter.getSymbolTable(), interpreter.featuresEnvEntry,
                                                   echo ? echoingSupplier : nonechoingSupplier, null);
                    outWriter = interpreter.getLispPrinter();
                    if (replVars) initReplVars.run();
                }
            }

            /** read one form (or :command) from the stdin that was passed to the constructor Repl(), write results to stdout, formatted in REPL-style with "==>" or "-->".
             *  This may block if reading from stdin blocks. If stdin is exhausted (returns -1) then a bye message is followed by throw EXIT_SUCCESS.
             *  The command ":q" or form "(quit)" will throw the exception EXIT_SUCCESS, if "istty" is false then any error will throw EXIT_RUNTIME_ERROR. */
            void oneForm(boolean istty, String nl) {
                final LambdaJ interpreter = this.interpreter;
                final WriteConsumer stdout = this.stdout;
                if (!isInit) {
                    interpreter.resetCounters();
                    parser = new SExpressionReader(interpreter.features, interpreter.trace, interpreter.tracer, interpreter.getSymbolTable(), interpreter.featuresEnvEntry,
                                                   echo ? echoingSupplier : nonechoingSupplier, null);
                    outWriter = makeWriter(stdout);
                    interpreter.init(parser, outWriter, null);
                    if (args != null) injectCommandlineArgs(interpreter, args);
                    if (replVars) initReplVars.run();
                    isInit = true;
                }

                try {
                    if (istty) parser.resetPos();
                    final Object exp = parser.readObj(true, eof);

                    if (exp != null) {
                        if (exp == eof
                            || exp == cmdQuit) { stdout.print("bye." + nl + nl);  throw EXIT_SUCCESS; }
                        if (exp == cmdHelp)   { showHelp(nl);  return; }
                        if (exp == cmdDesc)   { final Object name = parser.readObj(eof);  if (name == eof) return;
                                                if (!symbolp(name)) { stdout.print(name + " is not a symbol" + nl); return; }
                                                final LambdaJSymbol symbol = (LambdaJSymbol)name;
                                                final ConsCell envEntry = interpreter.globals.get(name);
                                                if (envEntry == null && symbol.macro == null) {
                                                    stdout.print(name + " is not bound" + nl); return;
                                                }
                                                if (symbol.macro != null) {
                                                    stdout.print("macro " + symbol + ":" + nl);
                                                    printClosureInfo(symbol.macro, nl);
                                                }
                                                if (cdr(envEntry) instanceof LambdaJ.Closure) {
                                                    stdout.print("function " + symbol + ":" + nl);
                                                    printClosureInfo((Closure)cdr(envEntry), nl);
                                                }
                                                stdout.print(LambdaJ.printSEx(cdr(envEntry), true) + nl);
                                                return; }
                        if (exp == cmdEcho)   { echo = true; parser.setInput(echoingSupplier, null); return; }
                        if (exp == cmdNoEcho) { echo = false; parser.setInput(nonechoingSupplier, null); return; }
                        if (exp == cmdRes)    { isInit = false; history.clear(); return; }
                        if (exp == cmdList)   { listHistory(history, nl); return; }
                        if (exp == cmdWrite)  { writeHistory(history, parser.readObj(false), nl); return; }
                        if (exp == cmdJava)   { compileToJava(consoleCharset, interpreter.getSymbolTable(), interpreter.libDir, makeReader(history), parser.readObj(false), parser.readObj(false)); return; }
                        if (exp == cmdRun)    { compileAndRunForms(makeReader(history), null, interpreter, true, false); return; }
                        if (exp == cmdJar)    { compileToJar(interpreter.getSymbolTable(), interpreter.libDir, makeReader(history), parser.readObj(false), parser.readObj(false)); return; }
                        //if (":peek".equals(exp.toString())) { System.out.println("gensymcounter: " + interpreter.gensymCounter); return; }
                        if (exp == cmdEnv)    {
                            final List<Map.Entry<Object, ConsCell>> toSort = new ArrayList<>(interpreter.globals.entrySet());
                            toSort.sort(Comparator.comparing(entry -> entry.getKey().toString()));
                            for (Map.Entry<Object, ConsCell> e : toSort) stdout.print(e.getValue() + nl);
                            stdout.print("env length: " + interpreter.globals.size() + nl + nl);
                            return;
                        }
                        if (exp == cmdMacros) {
                            final ArrayList<LambdaJSymbol> names = new ArrayList<>();
                            for (LambdaJSymbol entry: interpreter.getSymbolTable()) {
                                if (entry != null && entry.macro != null) names.add(entry);
                            }
                            names.sort(Comparator.comparing(Object::toString));
                            for (LambdaJSymbol name: names) stdout.print(name + ": " + printSEx(ConsCell.cons(name.macro.params(), name.macro.body)) + nl);
                            stdout.print("number of macros: " + names.size() + nl + nl);
                            return;
                        }
                    }

                    if (replVars) interpreter.eval(ConsCell.list(setq, form0, ConsCell.list(quote, exp)), null);

                    interpreter.values = NO_VALUES;
                    final long tStart = System.nanoTime();
                    final Object result = interpreter.expandAndEval(exp, null);
                    final ConsCell resultMv = interpreter.values;
                    interpreter.traceStats(tStart);

                    history.add(exp);

                    if (replVars) {
                        interpreter.eval(ConsCell.list(setq, form3, form2), null);
                        interpreter.eval(ConsCell.list(setq, form2, form1), null);
                        interpreter.eval(ConsCell.list(setq, form1, form0), null);

                        interpreter.eval(ConsCell.list(setq, result3, result2), null);
                        interpreter.eval(ConsCell.list(setq, result2, result1), null);
                        interpreter.eval(ConsCell.list(setq, result1, ConsCell.list(quote, result)), null);

                        interpreter.eval(ConsCell.list(setq, values3, values2), null);
                        interpreter.eval(ConsCell.list(setq, values2, values1), null);
                        interpreter.eval(ConsCell.list(setq, values1, ConsCell.list(quote, resultMv == NO_VALUES ? ConsCell.list(result) : resultMv)), null);
                    }

                    stdout.print(nl);
                    if (resultMv == NO_VALUES) {
                        stdout.print("==> "); outWriter.printObj(result, true); stdout.print(nl);
                    }
                    else if (resultMv != null) {
                        for (Object value : resultMv) {
                            stdout.print(" -> ");  outWriter.printObj(value, true);  stdout.print(nl);
                        }
                    }
                }
                catch (ReturnException ex) {
                    if (ex.tag == bye) {
                        if (istty) stdout.print("bye." + nl);
                        stdout.print(nl);
                        throw EXIT_SUCCESS;
                    }
                    else {
                        if (istty) errorContinue("uncaught throw tag " + LambdaJ.printSEx(ex.tag), nl);
                        else errorExit("uncaught throw tag " + LambdaJ.printSEx(ex.tag));
                    }
                }
                catch (Exit exit) { throw exit; }
                catch (Exception e) {
                    if (istty) errorContinue(e, nl);
                    else errorExit(e);
                }
            }

            /** if "appendable" doesn't throw then cast, else wrap */
            private static WriteConsumer makeWriteConsumer(Appendable appendable) {
                final WriteConsumer wc;
                if (appendable instanceof StringBuilder) wc = ((StringBuilder)appendable)::append;
                else if (appendable instanceof CharBuffer) wc = ((CharBuffer)appendable)::append;
                else if (appendable instanceof StringBuffer) wc = ((StringBuffer)appendable)::append;
                else if (appendable instanceof StringWriter) wc = ((StringWriter)appendable)::append;
                else if (appendable instanceof PrintWriter) wc = ((PrintWriter)appendable)::append;
                else if (appendable instanceof PrintStream) wc = ((PrintStream)appendable)::append;
                else if (appendable instanceof CharArrayWriter) wc = ((CharArrayWriter)appendable)::append;
                else wc = cs -> {
                    try { appendable.append(cs); }
                    catch (IOException e) { wrap0(e); }
                };
                return wc;
            }

            private static ObjectReader makeReader(List<Object> forms) {
                final Iterator<Object> i = forms.iterator();
                return (eof) -> i.hasNext() ? i.next() : eof;
            }

            private void printClosureInfo(Closure closure, String nl) {
                if (closure.body instanceof SExpConsCell) {
                    final String info = closure.body.lineInfo();
                    if (!info.isEmpty()) stdout.print(info + nl);
                }
                stdout.print(LambdaJ.printSEx(ConsCell.cons(LambdaJ.sLambda, ConsCell.cons(closure.params(), closure.body))) + nl);
            }

            private void errorContinue(Object e, String nl) {
                stdout.print(nl + "Error: " + LambdaJ.printSEx(e, true) + nl);
            }

            static Object errorExit(Object e) {
                System.err.println();
                System.err.println("Error: " + LambdaJ.printSEx(e, true));
                throw EXIT_RUNTIME_ERROR;
            }

            private void listHistory(List<Object> history, String nl) {
                for (Object sexp : history) {
                    stdout.print(printSEx(sexp));
                    stdout.print(nl);
                }
            }

            private void writeHistory(List<Object> history, Object filename, String nl) {
                try {
                    final Path p = Paths.get(filename.toString());
                    Files.createFile(p);
                    Files.write(p, history.stream()
                                          .map(LambdaJ::printSEx)
                                          .collect(Collectors.toList()));
                    stdout.print("wrote history to file '" + p + '\'' + nl);
                }
                catch (Exception e) {
                    stdout.print("history NOT written - error: " + e.getClass().getSimpleName() + ": " + e.getMessage() + nl);
                }
            }

            private void showHelp(String nl) {
                stdout.print("Available commands:\n"
                               + "  :h ............................. this help screen\n"
                               + "  :echo .......................... print forms to screen before eval'ing\n"
                               + "  :noecho ........................ don't print forms\n"
                               + "  :env ........................... list current global environment\n"
                               + "  :desc <symbol> ................. display interpreter data about <symbol>\n"
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
                               + "Available variables:\n"
                               + "  @- ............................. currently evaluated form\n"
                               + "  @+, @++, @+++ .................. recently evaluated forms\n"
                               + "  @*, @**, @*** .................. recently returned primary results\n"
                               + "  @/, @//, @/// .................. recently returned values\n"
                               + "\n"
                               + "  If 'classname' is nil then 'MurmelProgram' will be used as the classname (in the Java default package).\n"
                               + "  If 'jarfilename' is nil then 'a.jar' will be used as the jar file name.\n"
                               + "  classname, directory and jarfilename may need to be enclosed in double quotes if they contain spaces or are longer than SYMBOL_MAX (" + SYMBOL_MAX + ")\n"
                               + "\n"
                               + "  :q ............................. quit JMurmel\n");
                stdout.print(nl);
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
            if (hasFlag("--no-hash", args))     features &= ~Features.HAVE_HASH.bits();
            if (hasFlag("--no-io", args))       features &= ~Features.HAVE_IO.bits();
            if (hasFlag("--no-gui", args))      features &= ~Features.HAVE_GUI.bits();
            if (hasFlag("--no-util", args))     features &= ~Features.HAVE_UTIL.bits();

            if (hasFlag("--no-define", args))   features &= ~Features.HAVE_DEFINE.bits();
            if (hasFlag("--no-labels", args))   features &= ~Features.HAVE_LABELS.bits();
            if (hasFlag("--no-cons", args))     features &= ~Features.HAVE_CONS.bits();
            if (hasFlag("--no-cond", args))     features &= ~Features.HAVE_COND.bits();
            if (hasFlag("--no-apply", args))    features &= ~Features.HAVE_APPLY.bits();

            if (hasFlag("--no-atom", args))     features &= ~Features.HAVE_ATOM.bits();
            if (hasFlag("--no-eq", args))       features &= ~Features.HAVE_EQ.bits();
            if (hasFlag("--no-quote", args))    features &= ~Features.HAVE_QUOTE.bits();

            if (hasFlag("--XX-dyn", args))       features &= ~Features.HAVE_LEXC.bits();
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

        private static String flagValues(String flag, String[] args) {
            for (int i = 0; i < args.length; i++) {
                final String arg = args[i];
                if ("--".equals(arg)) return null;
                if (flag.equals(arg)) {
                    if (args.length < i + 2) {
                        System.err.println("LambdaJ: commandline argument " + flag + " requires a value");
                        return null;
                    }
                    args[i] = null; // consume the arg

                    final StringBuilder forms = new StringBuilder();
                    for (int ii = i+1; ii < args.length; ii++) {
                        final String form = args[ii];
                        if ("--".equals(form)) break;
                        args[ii] = null;
                        if (form != null) forms.append(form).append(' ');
                    }
                    return forms.toString();
                }
            }
            return null;
        }

        private static boolean argError(String[] args) {
            boolean err = false;
            for (String arg: args) {
                if ("--".equals(arg)) return err;
                if (arg != null && arg.startsWith("-")) {
                    System.err.println("LambdaJ: unknown or duplicate commandline argument " + arg + " or missing value");
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

        static void injectCommandlineArgs(LambdaJ intp, String[] args) {
            int n = 0;
            for (String arg: args) {
                n++;
                if ("--".equals(arg)) break;
            }

            intp.extendGlobal(intp.intern(COMMAND_LINE_ARGUMENT_LIST), arraySlice(args, n));
        }

        private static void injectCommandlineArgs(MurmelProgram prg, String[] args) {
            int n = 0;
            if (args != null) for (String arg: args) {
                n++;
                if ("--".equals(arg)) break;
            }

            prg.setCommandlineArgumentList(arraySlice(args, n));
        }



        /// functions that print info to the screen
        private static void showVersion() {
            System.out.println(ENGINE_VERSION);
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
                               + "--eval <forms> ...  Process the given forms after processing any files given as well.\n"
                               + "                    All commandline arguments up to (but not including) '--'\n"
                               + "                    will be processed as Murmel forms.\n"
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
                               + "--no-extra ....  no special forms if, defun, defmacro,\n"
                               + "                 let, let*, letrec, progn, setq,\n"
                               + "                 multiple-value-call, multiple-value-bind,\n"
                               + "                 load, require, provide, declaim,\n"
                               + "                 catch, throw, unwind-protect, try\n"
                               + "                 no primitive functions eval, rplaca, rplacd, trace, untrace,\n"
                               + "                 values, macroexpand-1\n"
                               + "                 no symbol *condition-handler*\n"
                               + "--no-number ...  no number support\n"
                               + "--no-string ...  no string support\n"
                               + "--no-vector ...  no vector support\n"
                               + "--no-hash .....  no hash-table support\n"
                               + "--no-io .......  no primitive functions read, write, writeln, lnwrite,\n"
                               + "--no-util .....  no primitive functions consp, symbolp, listp, null, error,\n"
                               + "                 append, assoc, assq, list, list*, format, format-locale,\n"
                               + "                 no time related primitives or symbols\n"
                               + "                 no symbol *features*\n"
                               + "\n"
                               + "--min+ ........  turn off all above features, leaving a Lisp\n"
                               + "                 with 11 special forms and primitives:\n"
                               + "                   S-expressions\n"
                               + "                   symbols and cons-cells (i.e. lists)\n"
                               + "                   function application\n"
                               + "                   the special forms quote, lambda, cond, labels, define\n"
                               + "                   the primitive functions atom, eq, cons, car, cdr, apply\n"
                               + "                   the symbols nil, t\n"
                               + "\n"
                               + "--no-nil ......  don't predefine symbol nil (hint: use '()' instead)\n"
                               + "--no-t ........  don't predefine symbol t (hint: use '(quote t)' instead)\n"
                               + "--no-apply ....  no function 'apply'\n"
                               + "--no-labels ...  no special form 'labels' (hint: use Y-combinator instead)\n"
                               + "--no-define ...  no special form 'define'\n"
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
                               + "                 bare bones Lambda calculus + environment:\n"
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

        private static class MultiFileReadSupplier implements ReadSupplier {
            private final boolean verbose;
            private final Iterator<Path> paths;
            private String forms;
            private final LambdaJ intp;
            private final ObjectReader delegate;

            private Reader reader;

            MultiFileReadSupplier(List<Path> paths, String forms, LambdaJ intp, ObjectReader delegate, boolean verbose) {
                this.paths = paths.iterator();
                this.forms = forms;
                this.intp = intp;
                this.delegate = delegate;
                this.verbose = verbose;
            }

            private void next() throws IOException {
                final Reader old = reader;
                reader = null;
                if (old != null) old.close();
                final Path p = paths.next();
                if (verbose) System.out.println("parsing " + p + "...");
                reader = Files.newBufferedReader(p);
                delegate.setInput(this, p);
                intp.currentSource = p;
            }

            private void forms() throws IOException {
                final Reader old = reader;
                reader = null;
                if (old != null) old.close();
                if (verbose) System.out.println("parsing commandline forms...");
                reader = new StringReader(forms);
                forms = null;
                delegate.setInput(this, null);
                intp.currentSource = null;
            }

            @Override public int read() throws IOException {
                if (reader == null) {
                    if (paths.hasNext()) next();
                    else if (forms != null) forms();
                    else return EOF;
                }
                try {
                    final int ret = reader.read();
                    if (ret != EOF) return ret;
                    if (paths.hasNext()) next();
                    else if (forms != null) forms();
                    else return EOF;
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

        private static ObjectReader parseFiles(List<String> files, String forms, LambdaJ interpreter, boolean verbose) {
            final List<Path> paths = new ArrayList<>(files.size());
            for (String fileName : files) {
                if ("--".equals(fileName)) break;
                paths.add(Paths.get(fileName));
            }
            final ObjectReader reader = interpreter.makeReader(NULL_READCHARS, null);
            reader.setInput(new MultiFileReadSupplier(paths, forms, interpreter, reader, verbose), null);
            return reader;
        }
    }

    public static class StringRepl extends Cli.Repl {
        private final StringBuilderSupplier inBuffer;
        private final StringBuilder outBuffer;

        private static class StringBuilderSupplier implements ReadSupplier {
            private final StringBuilder sb = new StringBuilder();
            private int pos;

            @Override public int read() {
                if (pos >= sb.length()) return -1;
                return sb.charAt(pos++) & 0xffff;
            }

            void reset(String s) { sb.setLength(0);  sb.append(s); pos = 0; }
            boolean eof() { return pos >= sb.length(); }
        }

        /** create an object of class StringRepl whose main method is {@link #evalString(String)} */
        public static StringRepl makeStringRepl() {
            final StringBuilderSupplier inBuffer = new StringBuilderSupplier();
            return new StringRepl(inBuffer, new StringBuilder(100));
        }

        private StringRepl(StringBuilderSupplier in, StringBuilder out) {
            super(in, out, new LambdaJ(), false, false, null, null, "UTF-8");
            this.inBuffer = in;
            outBuffer = out;
        }

        /** eval all forms in the String "forms" and return a String consisting of the forms' output and their results prepended by "==>" or multiple "-->".
         *  The returned String looks like REPL output. A prompt is NOT displayed.
         *  
         *  @throws Cli.Exit if ":q" was passed as a form */
        public String evalString(String forms) {
            inBuffer.reset(forms);
            while (!inBuffer.eof()) {
                try {
                    oneForm(true, "\n");
                }
                catch (Cli.Exit e) {
                    // probably due to EOF, ignore
                    break;
                }
            }
            final String ret = outBuffer.toString();
            outBuffer.setLength(0);
            return ret;
        }
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
            public Object setForTry(Object value) { return this.value = value; }

            public void push() { dynamicStack = ConsCell.cons(value, dynamicStack); }
            public void push(Object value) { dynamicStack = ConsCell.cons(this.value, dynamicStack); this.value = value; }
            public void pop() { value = car(dynamicStack); dynamicStack = (ConsCell)cdr(dynamicStack); }
        }

        public static final CompilerGlobal UNASSIGNED_GLOBAL = new CompilerGlobal(null) { @Override public Object get() { throw new LambdaJError(false, "unassigned value"); } };
        public static final Object UNASSIGNED_LOCAL = "#<value is not assigned>";

        public static final Object[] NOARGS = new Object[0];

        public interface CompilerPrimitive extends Writeable {
            Object applyCompilerPrimitive(Object... args);
            @Override default void printSEx(WriteConsumer out, boolean ignored) { out.print("#<compiler primitive>"); }
        }

        private final SymbolTable symtab = new ListSymbolTable();
        private static final LambdaJSymbol sBit = new LambdaJSymbol(true, "bit"), sCharacter = new LambdaJSymbol(true, "character"), sDynamic = new LambdaJSymbol(true, DYNAMIC);

        private final @NotNull ConsCell featuresEnvEntry;
        private final @NotNull ConsCell commandlineArgumentListEnvEntry;
        private ObjectReader lispReader;
        private ObjectWriter lispPrinter;
        private TurtleFrame current_frame;

        private LambdaJ intp;

        protected MurmelJavaProgram() {
            // hack so that symbols don't get interned as regular symbols which would break eval at least
            symtab.intern(LambdaJ.sT);
            symtab.intern(LambdaJ.sNil);
            symtab.intern(LambdaJ.sLambda);
            symtab.intern(LambdaJ.sProgn);
            for (WellknownSymbol ws: WellknownSymbol.values()) {
                symtab.intern(new LambdaJSymbol(ws.sym, true));
            }
            symtab.intern(sDynamic);
            symtab.intern(sBit);
            symtab.intern(sCharacter);

            features.set(makeFeatureList(symtab));
            featuresEnvEntry = ConsCell.cons(intern(FEATURES), features.get());
            commandlineArgumentListEnvEntry = ConsCell.cons(intern(COMMAND_LINE_ARGUMENT_LIST), null);

            lispReader = LambdaJ.makeReader(System.in::read, symtab, featuresEnvEntry);
            lispPrinter = LambdaJ.makeWriter(System.out::print);
        }

        private LambdaJ intpForEval() {
            LambdaJ intp = this.intp;
            if (intp == null) {
                final ConsCell conditionHandlerEnvEntry = ConsCell.cons(intern(CONDITION_HANDLER), conditionHandler.get());
                final ConsCell randomStateEnvEntry = ConsCell.cons(intern(RANDOM_STATE), randomState.get());
                this.intp = intp = new LambdaJ(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null, symtab, featuresEnvEntry, conditionHandlerEnvEntry, randomStateEnvEntry, null);
                intp.compiledProgram = this;
                intp.init(lispReader, lispPrinter, null);
                intp.extendGlobal(commandlineArgumentListEnvEntry);
                intp.typeSpecs = typeSpecs();
            }
            else {
                assert intp.conditionHandlerEnvEntry != null : "MurmelJavaProgram has an interpreter with feature XTRA enabled and conditionHandlerEnvEntry should be != null";
                intp.conditionHandlerEnvEntry.rplacd(conditionHandler.get());
                assert intp.randomStateEnvEntry != null : "MurmelJavaProgram has an interpreter with feature NUMBERs enabled and randomStateEnvEntry should be != null";
                intp.randomStateEnvEntry.rplacd(randomState.get());
                intp.setReaderPrinter(lispReader, lispPrinter);
            }
            featuresEnvEntry.rplacd(features.get());
            commandlineArgumentListEnvEntry.rplacd(commandlineArgumentList.get());
            intp.current_frame = current_frame;
            return intp;
        }

        private void afterEval() {
            final LambdaJ intp = this.intp;
            if (intp.values == LambdaJ.NO_VALUES) clrValues();
            else values = toArray(intp.values);
            features.set(cdr(featuresEnvEntry));
            conditionHandler.set(cdr(intp.conditionHandlerEnvEntry));
            randomState.set(cdr(intp.randomStateEnvEntry));
            commandlineArgumentList.set(cdr(commandlineArgumentListEnvEntry));
            randomState.set(cdr(intp.randomStateEnvEntry));
            current_frame = intp.current_frame;
        }

        private Random getRandom() {
            if (randomState.get() == null) randomState.set(new Random());
            return (Random)randomState.get();
        }

        private ObjectWriter getLispPrinter(Object[] args, int nth, ObjectWriter defaultIfNull) {
            final Object consumer;
            if (nth >= args.length || (consumer = args[nth]) == null) return defaultIfNull;
            if (consumer == sT) return lispPrinter;
            if (consumer instanceof Appendable) return new SExpressionWriter(csq -> { try { ((Appendable)consumer).append(csq); } catch (IOException e) { wrap0(e); } });
            throw new SimpleTypeError("cannot coerce %s into a printer", printSEx(consumer));
        }

        /// JMurmel native embed API - Java calls compiled Murmel
        @Override public final ObjectReader getLispReader()  { return lispReader; }
        @Override public final ObjectWriter getLispPrinter() { return lispPrinter; }
        @Override public final void setReaderPrinter(ObjectReader lispStdin, ObjectWriter lispStdout) { lispReader = lispStdin; lispPrinter = lispStdout; }

        @Override public final @NotNull MurmelFunction getFunction(String func) {
            final Object maybeFunction = getValue(func);
            if (maybeFunction instanceof MurmelFunction) {
                return args -> funcall((MurmelFunction)maybeFunction, args);
            }
            if (maybeFunction instanceof CompilerPrimitive) {
                return args -> funcall((CompilerPrimitive)maybeFunction, args);
            }
            throw LambdaJ.errorNotAFunction("getFunction: not a primitive or " + LAMBDA + ": %s", func);
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
            clrValues();
            throw new LambdaJError(e, e.getMessage() + "\nError occured in " + loc);
        }



        /// predefined global variables
        public static final LambdaJSymbol _t = LambdaJ.sT;

        public static final double _pi = Math.PI;

        /// predefined aliased global variables
        public static final int arrayDimensionLimit = ARRAY_DIMENSION_LIMIT_VAL;
        public static final long mostPositiveFixnum = MOST_POSITIVE_FIXNUM_VAL;
        public static final long mostNegativeFixnum = MOST_NEGATIVE_FIXNUM_VAL;

        public static final long itups = (long)1e9;

        // *COMMAND-LINE-ARGUMENT-LIST*: will be assigned/ accessed from generated code
        public final CompilerGlobal commandlineArgumentList = new CompilerGlobal(null);

        public final CompilerGlobal features = new CompilerGlobal(null);
        public final CompilerGlobal conditionHandler = new CompilerGlobal(null);
        public final CompilerGlobal randomState = new CompilerGlobal(null);

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
            twoArgs(APPLY, args);
            return apply(args);
        }
        public final Object apply(Object... args) {
            Object fn = args[0];
            if (fn == null) errorNotAFunction(sNil);
            if (symbolp(fn)) fn = getValue(fn.toString());
            return tailcall(fn, listToArray(args[1]));
        }
        public final Object _eval(Object... args) {
            varargs1_2(EVAL, args);
            final LambdaJ intp = intpForEval();
            final Object ret = intp.expandAndEval(args[0], args.length == 2 ? LambdaJ.requireList(EVAL, args[1]) : null);
            afterEval();
            return ret;
        }


        // logic, predicates
        private Object bool(boolean result) { clrValues(); return result ? _t : null; }

        public final Object _eq        (Object... args) { twoArgs(EQ, args);     return bool(args[0] == args[1]); }

        public final Object _eql       (Object... args) { twoArgs(EQL, args);    return bool(LambdaJ.Subr.eql(args[0], args[1])); }
        public final Object _eql(Object o1, Object o2)  {                        return bool(LambdaJ.Subr.eql(o1, o2)); }

        public final Object _equal     (Object... args) { twoArgs(EQUAL, args);  return bool(LambdaJ.Subr.equal(args[0], args[1])); }
        public final Object _equal(Object o1, Object o2) {                       return bool(LambdaJ.Subr.equal(o1, o2)); }

        public final Object _consp     (Object... args) { oneArg(CONSP, args);                 return bool(consp(args[0])); }
        public final Object _consp     (Object    arg)  {                                      return bool(consp(arg)); }
        public final Object _atom      (Object... args) { oneArg(ATOM, args);                  return bool(atom(args[0])); }
        public final Object _atom      (Object    arg)  {                                      return bool(atom(arg)); }
        public final Object _symbolp   (Object... args) { oneArg(SYMBOLP, args);               return bool(symbolp(args[0])); }
        public final Object _symbolp   (Object    arg)  {                                      return bool(symbolp(arg)); }
        public final Object _null      (Object... args) { oneArg(NULL, args);                  return bool(args[0] == null); }
        public final Object _numberp   (Object... args) { oneArg(NUMBERP, args);               return bool(numberp(args[0])); }
        public final Object _numberp   (Object    arg)  {                                      return bool(numberp(arg)); }
        public final Object _floatp    (Object... args) { oneArg(FLOATP, args);                return bool(floatp(args[0])); }
        public final Object _floatp    (Object    arg)  {                                      return bool(floatp(arg)); }
        public final Object _integerp  (Object... args) { oneArg(INTEGERP, args);              return bool(integerp(args[0])); }
        public final Object _integerp  (Object    arg)  {                                      return bool(integerp(arg)); }
        public final Object _characterp(Object... args) { oneArg(CHARACTERP, args);            return bool(characterp(args[0])); }
        public final Object _randomstatep(Object... args){oneArg(RANDOM_STATE_P, args);        return bool(randomstatep(args[0])); }

        public final Object _vectorp   (Object... args) { oneArg(VECTORP, args);               return bool(vectorp(args[0])); }
        public final Object _vectorp   (Object    arg)  {                                      return bool(vectorp(arg)); }
        public final Object svectorp   (Object... args) { oneArg(SIMPLE_VECTOR_P, args);       return bool(LambdaJ.svectorp(args[0])); }
        public final Object svectorp   (Object    arg)  {                                      return bool(LambdaJ.svectorp(arg)); }
        public final Object _stringp   (Object... args) { oneArg(STRINGP, args);               return bool(stringp(args[0])); }
        public final Object _stringp   (Object    arg)  {                                      return bool(stringp(arg)); }
        public final Object sstringp   (Object... args) { oneArg(SIMPLE_STRING_P, args);       return bool(LambdaJ.sstringp(args[0])); }
        public final Object sstringp   (Object    arg)  {                                      return bool(LambdaJ.sstringp(arg)); }
        public final Object bitvectorp (Object... args) { oneArg(BIT_VECTOR_P, args);          return bool(LambdaJ.bitvectorp(args[0])); }
        public final Object bitvectorp (Object    arg)  {                                      return bool(LambdaJ.bitvectorp(arg)); }
        public final Object sbitvectorp(Object... args) { oneArg(SIMPLE_BIT_VECTOR_P, args);   return bool(LambdaJ.sbitvectorp(args[0])); }
        public final Object sbitvectorp(Object    arg)  {                                      return bool(LambdaJ.sbitvectorp(arg)); }
        public final Object hashtablep (Object... args) { oneArg(HASH_TABLE_P, args);          return bool(LambdaJ.hashtablep(args[0])); }
        public final Object hashtablep (Object    arg)  {                                      return bool(LambdaJ.hashtablep(arg)); }

        public final Object _functionp (Object... args) { oneArg(FUNCTIONP, args);             return bool(LambdaJ.functionp0(args[0])); }

        public final Object _listp     (Object... args) { oneArg(LISTP, args);                 return bool(listp(args[0])); }
        public final Object _listp     (Object    arg)  {                                      return bool(listp(arg)); }
        public final Object _typep     (Object... args) { twoArgs(TYPEP, args);                return bool(typep(symtab, null, typeSpecs(), args[0], args[1])); }
        public final Object _typep     (Object o, Object t) {                                  return bool(typep(symtab, null, typeSpecs(), o, t)); }

        private Map<LambdaJSymbol, TypeSpec> typeSpecs;
        private Map<LambdaJSymbol, TypeSpec> typeSpecs() {
            if (typeSpecs == null) {
                final Map<LambdaJSymbol, TypeSpec> map = new IdentityHashMap<>(JavaUtil.hashMapCapacity(TYPE_SPECS.length));
                fillTypespecs(symtab, map);
                typeSpecs = map;
            }
            return typeSpecs;
        }

        public final Object adjustableArrayP(Object... args) { oneArg(ADJUSTABLE_ARRAY_P, args); return bool(LambdaJ.Subr.adjustableArrayP(args[0])); }


        // conses and lists
        public final Object _car       (Object... args) { oneArg(CAR,       args); return _car(args[0]); }
        public final Object _car       (Object l)       { clrValues(); return LambdaJ.car(l); } // also used by generated code
        public final Object _car       (ConsCell l)     { clrValues(); return LambdaJ.car(l); }

        public final Object _cdr       (Object... args) { oneArg(CDR,       args); return _cdr(args[0]); }
        public final Object _cdr       (Object l)       { clrValues(); return LambdaJ.cdr(l); } // also used by generated code
        public final Object _cdr       (ConsCell l)     { clrValues(); return LambdaJ.cdr(l); }

        public final ConsCell _cons   (Object... args)      { twoArgs(CONS,     args); return _cons(args[0], args[1]); }
        public final ConsCell _cons(Object car, Object cdr) { clrValues(); return ConsCell.cons(car, cdr); } // also used by generated code

        public final ConsCell _rplaca (Object... args)           { twoArgs(RPLACA, args);  return _rplaca(args[0], args[1]); }
        public final ConsCell _rplaca(Object l, Object newCar)   { clrValues(); return requireCons(RPLACA, l).rplaca(newCar); }
        public final ConsCell _rplaca(ConsCell l, Object newCar) { clrValues(); return l.rplaca(newCar); }

        public final ConsCell _rplacd (Object... args)           { twoArgs(RPLACD, args);  return _rplacd(args[0], args[1]); }
        public final ConsCell _rplacd(Object l, Object newCdr)   { clrValues(); return requireCons(RPLACD, l).rplacd(newCdr); }
        public final ConsCell _rplacd(ConsCell l, Object newCdr) { clrValues(); return l.rplacd(newCdr); }

        public final ConsCell _list    (Object... args) { clrValues(); return ConsCell.list(args); }
        public final Object   listStar (Object... args) { clrValues(); varargs1(LISTSTAR, args); return ConsCell.listStar(args); }
        public final Object   listStar0(Object... args) { clrValues();                           return ConsCell.listStar(args); }
        public final Object   _append  (Object... args) {
            clrValues();
            int nArgs;
            if (args == null || (nArgs = args.length) == 0) return null;
            if (nArgs == 1) return args[0];
            if (!listp(args[0])) throw new SimpleTypeError(APPEND + ": first argument %s is not a list", printSEx(args[0]));

            nArgs--;
            int first = 0;
            while (first < nArgs && args[first] == null) first++; // skip leading nil args if any

            ListBuilder lb = null;
            for (int i = first; i < nArgs; i++) {
                final Object o = args[i];
                if (o == null) continue;
                if (!consp(o)) throw new SimpleTypeError(APPEND + ": argument %d is not a list: %s", i+1, printSEx(o));
                if (lb == null) lb = new ListBuilder();
                for (Object obj: (ConsCell)o) lb.append(obj);
            }
            if (lb == null) return args[first];
            lb.appendLast(args[nArgs]);
            return lb.first();
        }
        public final ConsCell _assq    (Object... args) { clrValues(); twoArgs(ASSQ, args); return assq(args[0], args[1]); }
        public final ConsCell _assoc   (Object... args) { clrValues(); twoArgs(ASSOC, args); return assoc(args[0], args[1]); }


        // numbers, characters

        public final double add        (Object... args) { clrValues(); if (args.length > 0) { double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret += toDouble(args[i]); return ret; } return 0.0; }
        public final double mul        (Object... args) { clrValues(); if (args.length > 0) { double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret *= toDouble(args[i]); return ret; } return 1.0; }

        public final double sub        (Object... args) { clrValues(); varargs1("-", args);
                                                          if (args.length == 1) return 0.0 - toDouble(args[0]);
                                                          double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret -= toDouble(args[i]); return ret; }
        public final double quot       (Object... args) { clrValues(); varargs1("/", args);
                                                          if (args.length == 1) return 1.0 / toDouble(args[0]);
                                                          double ret = toDouble(args[0]); for (int i = 1; i < args.length; i++) ret /= toDouble(args[i]); return ret; }

        public final Object numbereq   (Object... args) { return compare("=",  args, (d1, d2) -> d1 == d2); }
        public final Object ne         (Object... args) { return compare("/=", args, (d1, d2) -> d1 != d2); }
        public final Object lt         (Object... args) { return compare("<",  args, (d1, d2) -> d1 <  d2); }
        public final Object le         (Object... args) { return compare("<=", args, (d1, d2) -> d1 <= d2); }
        public final Object ge         (Object... args) { return compare(">=", args, (d1, d2) -> d1 >= d2); }
        public final Object gt         (Object... args) { return compare(">",  args, (d1, d2) -> d1 >  d2); }
        private Object compare(String op, Object[] args, DoubleBiPred pred) {
            clrValues();
            varargs1(op, args);
            double prev = toDouble(args[0]);
            final int length = args.length;
            for (int i = 1; i < length; i++) {
                final double next = toDouble(args[i]);
                if (!pred.test(prev, next)) return null;
                prev = next;
            }
            return _t;
        }

        public final Number   inc      (Object... args) { clrValues(); oneArg("1+", args); return LambdaJ.Subr.inc(args[0]); }
        public final Number   inc      (Object arg)     { clrValues();                     return LambdaJ.Subr.inc(arg); }
        public final Number   dec      (Object... args) { clrValues(); oneArg("1-", args); return LambdaJ.Subr.dec(args[0]); }
        public final Number   dec      (Object arg)     { clrValues();                     return LambdaJ.Subr.dec(arg); }

        public final Number   _signum  (Object... args) { clrValues(); oneArg("signum", args); return cl_signum (args[0]); }

        public final long     _round   (Object... args) { varargs1_2("round",     args); return toFixnum(cl_round   (quot12(args))); }
        public final long     _floor   (Object... args) { varargs1_2("floor",     args); return toFixnum(Math.floor (quot12(args))); }
        public final long     _ceiling (Object... args) { varargs1_2("ceiling",   args); return toFixnum(Math.ceil  (quot12(args))); }
        public final long     _truncate(Object... args) { varargs1_2("truncate",  args); return toFixnum(cl_truncate(quot12(args))); }

        public final double   _fround   (Object... args) { varargs1_2("fround",   args); return cl_round   (quot12(args)); }
        public final double   _ffloor   (Object... args) { varargs1_2("ffloor",   args); return Math.floor (quot12(args)); }
        public final double   _fceiling (Object... args) { varargs1_2("fceiling", args); return Math.ceil  (quot12(args)); }
        public final double   _ftruncate(Object... args) { varargs1_2("ftruncate",args); return cl_truncate(quot12(args)); }

        public static double cl_round(double d)    { return Math.rint(d); }
        public static double cl_truncate(double d) { return LambdaJ.Subr.cl_truncate(d); }
        public static long   toFixnum(double d)    { return LambdaJ.Chk.toFixnum(d); }
        private double quot12(Object[] args) { clrValues(); return args.length == 2 ? toDouble(args[0]) / toDouble(args[1]) : toDouble(args[0]); }

        public final double   _sqrt    (Object... args) { clrValues(); oneArg("sqrt",          args); return Math.sqrt (toDouble(args[0])); }
        public final double   _log     (Object... args) { clrValues(); varargs1_2("log",       args); return args.length == 1 ? Math.log(toDouble(args[0])) : Math.log(toDouble(args[0])) / Math.log(toDouble(args[1])); }
        public final double   _log10   (Object... args) { clrValues(); oneArg("log10",         args); return Math.log10(toDouble(args[0])); }
        public final double   _exp     (Object... args) { clrValues(); oneArg("exp",           args); return Math.exp  (toDouble(args[0])); }
        public final double   _expt    (Object... args) { clrValues(); twoArgs("expt",         args); return Math.pow  (toDouble(args[0]), toDouble(args[1])); }

        public final double   _mod     (Object... args) { twoArgs("mod",          args); return cl_mod(toDouble(args[0]), toDouble(args[1])); }
        public final double cl_mod(double lhs, double rhs) { clrValues(); return LambdaJ.Subr.cl_mod(lhs, rhs); }
        public final double   _rem     (Object... args) { clrValues(); twoArgs("rem",          args); return toDouble(args[0]) % toDouble(args[1]); }

        public final Number _random(Object... args) {
            clrValues(); varargs1_2("random", args);
            final Object state;
            if (args.length == 2) state = args[1];
            else state = getRandom();
            return random(args[0], state);
        }
        public final Random makeRandomState(Object... args) {
            clrValues(); varargs0_1("make-random-state", args);
            final Object state;
            final Random current;
            if (args.length == 1 && args[0] != null) { state = args[0]; current = null; }
            else                                     { state = null;    current = getRandom(); }
            return Subr.makeRandomState(current, state);
        }


        // vectors, sequences

        public final Object   makeArray(Object... args)    { clrValues(); varargsMinMax(MAKE_ARRAY, args, 1, 3);
                                                             if (args.length == 1) return new Object[toArrayIndex(args[0])];
                                                             return LambdaJ.Subr.makeArray(sBit, sCharacter, arraySlice(args)); }
        public final long     vectorLength(Object... args) { clrValues(); oneArg("vector-length", args); return LambdaJ.Subr.vectorLength(args[0]); }
        public final Object   vectorCopy  (Object... args) { clrValues(); varargs1_2("vector-copy", args);   return LambdaJ.Subr.vectorCopy(args[0], secondArgNotNull(args)); }
        public final Object   vectorFill  (Object... args) { clrValues(); varargsMinMax("vector-fill", args, 2, 4);
                                                             return LambdaJ.Subr.vectorFill(args[0], args[1], nth(2, args), nth(3, args)); }
        public final long     vectorAdd   (Object... args) { clrValues(); twoArgs("vector-add", args); return LambdaJ.Subr.vectorAdd(args[0], args[1]); }
        public final Object   vectorToList (Object... args) {
            clrValues(); oneArg("vector->list", args);
            final Object maybeVector = args[0];

            if (LambdaJ.svectorp(maybeVector))    return simpleVectorToList(args);
            if (stringp(maybeVector))             return stringToList(args);
            if (LambdaJ.sbitvectorp(maybeVector)) return bitVectorToList(args);

            if (maybeVector instanceof Bitvector || maybeVector instanceof List) {
                final Iterator<?> it = ((Iterable<?>)maybeVector).iterator();
                if (!it.hasNext()) return null;
                final ListBuilder ret = new ListBuilder();
                do { ret.append(it.next()); }
                while (it.hasNext());
                return ret.first();
            }

            throw errorNotAVector("vector->list", maybeVector);
        }
        public final Object   listToVector(Object... args) { clrValues(); varargs1_2("list->vector", args); return LambdaJ.Subr.listToVector(args[0], secondArgNotNull(args)); }

        public final long     _svlength   (Object... args) { clrValues(); oneArg("svlength", args); return svlength(args[0]); }
        public final Object   _svref      (Object... args) { twoArgs("svref",   args); return _svref(args[0], args[1]); }
        public final Object   _svref(Object v, Object idx) { clrValues(); return LambdaJ.Subr.svref(v, toArrayIndex(idx)); }
        public final Object   _svset      (Object... args) { threeArgs("svref", args); return _svset(args[0], args[1], args[2]); }
        public final Object   _svset(Object v, Object idx, Object val) { clrValues(); return LambdaJ.Subr.svset(v, toArrayIndex(idx), val); }
        public final Object   simpleVectorToList (Object... args) {
            clrValues(); oneArg("simple-vector->list", args);
            final Object maybeVector = args[0];
            final Object[] s = LambdaJ.Chk.requireSimpleVector("simple-vector->list", maybeVector);
            final ListBuilder ret = new ListBuilder();
            final int len = s.length;
            for (int i = 0; i < len; i++) ret.append(s[i]);
            return ret.first();
        }
        public final Object listToSimpleVector(Object... args) { clrValues(); oneArg("list->simple-vector", args); return LambdaJ.listToArray(args[0]); }
        public final Object _vector    (Object... args) { clrValues(); return args; }
        public final Object _vect      (Object... args) { clrValues(); varargs1(VECT, args); return LambdaJ.listToArray(arraySlice(args, 1), toInt(args[0])); }

        public final Object    _string (Object... args) { clrValues(); oneArg("string", args); return stringDesignatorToString(args[0]); }
        public final long      _slength(Object... args) { clrValues(); oneArg("slength", args); return slength(args[0]); }
        public final char      _sref   (Object... args) { clrValues(); twoArgs("sref", args);   return LambdaJ.Subr.sref(args[0], toArrayIndex(args[1])); }
        public final char      _sset   (Object... args) { clrValues(); threeArgs("sset", args); return LambdaJ.Subr.sset(args[0], toArrayIndex(args[1]), requireChar(args[2])); }
        public final Object   stringeq (Object... args) { twoArgs("string=", args); return bool(LambdaJ.Subr.stringEq(args[0], args[1])); }
        public final Object   stringToList (Object... args) {
            clrValues(); oneArg("string->list", args);
            final Object maybeString = args[0];
            final ListBuilder ret = new ListBuilder();
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
        public final Object listToString(Object... args) { clrValues(); varargs1_2("list->string", args); return LambdaJ.Subr.listToString(args[0], secondArgNotNull(args)); }

        public final long   charInt     (Object... args) { clrValues(); oneArg("char-code",     args); return (long) LambdaJ.Chk.requireChar("char-code", args[0]); }
        public final long   charInt     (Object arg)     { clrValues();                                       return (long) LambdaJ.Chk.requireChar("char-code", arg); }
        public final char   intChar     (Object... args) { clrValues(); oneArg("code-char",     args); return (char) toInt(args[0]); }
        public final char   intChar     (Object arg)     { clrValues();                                       return (char) toInt(arg); }

        public final long   _bvlength   (Object... args)             { clrValues(); oneArg("bvlength", args);      return bvlength(args[0]); }
        public final long   _bvref      (Object... args)             { twoArgs("bvref", args);        return _bvref(args[0], args[1]); }
        public final long   _bvref      (Object v, Object idx)       { clrValues(); return LambdaJ.Subr.bvref(v, toArrayIndex(idx)); }
        public final long   _bvref      (Object v, long idx)         { clrValues(); return LambdaJ.Subr.bvref(v, toArrayIndex(idx)); }
        public final long   _bvset      (Object... args)             { threeArgs("bvset", args);      return _bvset(args[0], args[1], args[2]); }
        public final long   _bvset(Object v, Object idx, Object val) { clrValues(); return LambdaJ.Subr.bvset(v, toArrayIndex(idx), toBit(val)); }
        public final long   _bvset(Object v, Object idx, long val)   { clrValues(); return LambdaJ.Subr.bvset(v, toArrayIndex(idx), toBit(val)); }
        public final long   _bvset(Object v, long idx, long val)     { clrValues(); return LambdaJ.Subr.bvset(v, toArrayIndex(idx), toBit(val)); }
        public final Object bvEq        (Object... args)             { twoArgs("bv=", args); return bool(LambdaJ.Subr.bvEq(args[0], args[1])); }
        public final Object bitVectorToList(Object... args) {
            clrValues(); oneArg("bit-vector->list", args);
            final Object maybeVector = args[0];
            if (maybeVector instanceof boolean[]) {
                final boolean[] s = (boolean[])maybeVector;
                final int len = s.length;
                if (len == 0) return null;
                final ListBuilder ret = new ListBuilder();
                for (int i = 0; i < len; i++) ret.append(s[i] ? 1L : 0L);
                return ret.first();
            }
            else if (maybeVector instanceof Bitvector) {
                final Bitvector bv = (Bitvector)maybeVector;
                final ListBuilder ret = new ListBuilder();
                for (Object bit: bv) ret.append(bit);
                return ret.first();
            }
            else throw errorNotABitVector("bit-vector->list", maybeVector);
        }
        public final Object listToBitVector(Object... args) {
            clrValues(); varargs1_2("list->bit-vector", args);
            return LambdaJ.Subr.listToBitVector(LambdaJ.requireList("list->bit-vector", args[0]), secondArgNotNull(args));
        }

        public final Object _seqref       (Object... args)      { clrValues(); twoArgs("seqref",   args); return LambdaJ.Subr.seqref(args[0], toArrayIndex(args[1])); }
        public final Object _seqset       (Object... args)      { clrValues(); threeArgs("seqset", args); return LambdaJ.Subr.seqset(args[0], toArrayIndex(args[1]), args[2]); }


        // Hashtables
        public final Object _hash         (Object... args)      { clrValues(); return LambdaJ.Subr.hash(symtab, arraySlice(args)); }
        public final Object makeHash      (Object... args)      { clrValues(); varargsMinMax(MAKE_HASH_TABLE, args, 0, 2);
                                                                  return makeHashTable(symtab, nth(0, args), args.length > 1 ? toNonnegInt(MAKE_HASH_TABLE, args[1]) : DEFAULT_HASH_SIZE); }

        public final Object _hashref      (Object... args)      { varargsMinMax("hashref", args, 2, 3);  values = hashref(args[0], args[1], args.length > 2 ? args[2] : NO_DEFAULT_VALUE); return values[0]; }
        public final Object _hashset      (Object... args)      { clrValues(); varargsMinMax("hashset", args, 2, 3);  return hashset(arraySlice(args)); }
        public final Object hashTableCount(Object... args)      { clrValues(); oneArg("hash-table-count", args);      return LambdaJ.Subr.hashTableCount(args[0]); }
        public final Object _clrhash      (Object... args)      { clrValues(); oneArg("clrhash", args);               return LambdaJ.Subr.clrhash(args[0]); }
        public final Object hashRemove    (Object... args)      { varargs1_2("hash-table-remove", args);              return bool(LambdaJ.Subr.hashRemove(arraySlice(args))); }
        public final Object _sxhash       (Object... args)      { clrValues(); oneArg("sxhash", args);                return LambdaJ.Subr.sxhash(args[0]); }
        public final Object _sxhash       (Object    obj)       { clrValues();                                        return LambdaJ.Subr.sxhash(obj); }
        public final Object scanHash      (Object... args)      { clrValues(); oneArg("scan-hash-table", args);       return scanHashCompiler(args[0]); }

        interface CompilerIteratorGenerator extends IteratorGenerator, CompilerPrimitive {}

        private CompilerIteratorGenerator scanHashCompiler(Object hash) {
            final Map<Object, Object> map = requireHash("scan-hash-table", hash);
            final Function<Map.Entry<?,?>, Object> getKey;
            if (map instanceof MurmelMap) getKey = ((MurmelMap)map)::getKey;
            else getKey = Map.Entry::getKey;

            final Iterator<Map.Entry<Object,Object>> it = map.entrySet().iterator();
            if (it.hasNext()) return new CompilerIteratorGenerator() {
                private Map.Entry<Object,Object> entry;
                @Override public Object applyCompilerPrimitive(Object... args) {
                    if (it.hasNext()) { entry = it.next(); final ConsCell tuple = ConsCell.cons(getKey.apply(entry), entry.getValue()); values = new Object[] { tuple, sT }; return tuple; }
                    else { entry = null;  values = new Object[] { null, null };  return null; }
                }
                @Override public Object set(Object value) { if (entry != null) { entry.setValue(value); return value; } else throw new SimpleError("no such element"); }
                @Override public boolean remove() { it.remove(); entry = null; return true; }
                @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print("#<hash-table generator>"); }
            };
            else return new CompilerIteratorGenerator() { @Override public Object applyCompilerPrimitive(Object... args) { values = new Object[] { null, null };  return null; }
                                                          @Override public void printSEx(WriteConsumer out, boolean ignored) { out.print("#<empty hash-table generator>"); } };
        }


        // I/O
        public final Object _read             (Object... args)  { clrValues(); varargs0_1("read",                    args);       return LambdaJ.Subr.read(lispReader, arraySlice(args)); }
        public final Object readFromStr       (Object... args)  { varargsMinMax("read-from-string",     args, 1, 4);
                                                                  featuresEnvEntry.rplacd(features.get());
                                                                  return retn(LambdaJ.Subr.readFromString(symtab, featuresEnvEntry, arraySlice(args))); }
        public final Object readTextfileLines (Object... args)  { clrValues(); varargs1_2("read-textfile-lines",     args);       return LambdaJ.Subr.readTextfileLines(arraySlice(args)); }
        public final Object readTextfile      (Object... args)  { clrValues(); varargs1_2("read-textfile",           args);       return LambdaJ.Subr.readTextfile(arraySlice(args)); }
        public final Object writeTextfileLines(Object... args)  { clrValues(); varargsMinMax("write-textfile-lines", args, 2, 4); return LambdaJ.Subr.writeTextfileLines(arraySlice(args)); }
        public final Object writeTextfile     (Object... args)  { clrValues(); varargsMinMax("write-textfile",       args, 2, 4); return LambdaJ.Subr.writeTextfile(arraySlice(args)); }
        public final Object writeToString     (Object... args)  { clrValues(); varargs1_2("write-to-string",         args);       return LambdaJ.Subr.writeToString(args[0], noSecondArgOrNotNull(args)); }
        public final Object _write            (Object... args)  { clrValues(); varargsMinMax("write",                args, 1, 3); return LambdaJ.Subr.write  (getLispPrinter(args, 2, lispPrinter), args[0], noSecondArgOrNotNull(args)); }

        public final Object _writeln          (Object... args)  { clrValues(); varargsMinMax("writeln",              args, 0, 3); return LambdaJ.Subr.writeln(getLispPrinter(args, 2, lispPrinter), arraySlice(args), noSecondArgOrNotNull(args)); }
        public final Object _lnwrite          (Object... args)  { clrValues(); varargsMinMax("lnwrite",              args, 0, 3); return LambdaJ.Subr.lnwrite(getLispPrinter(args, 2, lispPrinter), arraySlice(args), noSecondArgOrNotNull(args)); }

        public final Object format            (Object... args)  { clrValues(); varargs2("format",                    args);       return LambdaJ.Subr.format(getLispPrinter(args, 0, null), true, arraySlice(args)); }
        public final Object formatLocale      (Object... args)  { clrValues(); varargs3("format-locale",             args);       return LambdaJ.Subr.formatLocale(getLispPrinter(args, 0, null), true, arraySlice(args)); }


        // misc
        public Object[] values;
        public final Object _values    (Object... args) { if (args.length == 1) clrValues(); else values = args; if (args.length == 0) return null; return args[0]; }
        public final Object _gensym    (Object... args) { clrValues(); varargs0_1("gensym", args); return LambdaJ.Subr.gensym(args.length == 0 ? null : args[0]); }
        public final Object _trace     (Object... args) { clrValues(); return null; }
        public final Object _untrace   (Object... args) { clrValues(); return null; }

        public final Object _error     (Object... args) { clrValues(); varargs1(ERROR, args); LambdaJ.Subr.error(typeSpecs(), args[0], Arrays.copyOfRange(args, 1, args.length)); return null; }
        public final Object error1     (Object a1)      { clrValues(); LambdaJ.Subr.error(typeSpecs(), a1, NOARGS); return null; }
        public final Object error2     (Object a1, Object a2) { clrValues(); LambdaJ.Subr.error(typeSpecs(), a1, a2); return null; }
        public final Object error3     (Object a1, Object a2, Object a3) { clrValues(); LambdaJ.Subr.error(typeSpecs(), a1, a2, a3); return null; }
        public final Object error4     (Object a1, Object a2, Object a3, Object a4) { clrValues(); LambdaJ.Subr.error(typeSpecs(), a1, a2, a3, a4); return null; }
        public final Object errorN     (Object a1, Object a2, Object a3, Object... args) {
            clrValues();
            final Object[] newArgs = new Object[args.length + 2];
            newArgs[0] = a2;
            newArgs[1] = a3;
            System.arraycopy(args, 0, newArgs, 2, args.length);
            LambdaJ.Subr.error(typeSpecs(), a1, newArgs);
            return null;
        }

        public final Object implType   (Object... args) { clrValues(); noArgs("lisp-implementation-type",    args); return "JMurmel"; }
        public final Object implVersion(Object... args) { clrValues(); noArgs("lisp-implementation-version", args); return LambdaJ.ENGINE_VERSION_NUM; }


        // time
        public final long   getInternalRealTime(Object... args) { clrValues(); noArgs("get-internal-real-time", args); return LambdaJ.Subr.getInternalRealTime(); }
        public final long   getInternalRunTime (Object... args) { clrValues(); noArgs("get-internal-run-time",  args); return LambdaJ.Subr.getInternalRunTime(); }
        public final Object sleep              (Object... args) { clrValues(); oneArg("sleep",                  args); return LambdaJ.Subr.sleep(args[0]); }
        public final long   getUniversalTime   (Object... args) { clrValues(); noArgs("get-universal-time",     args); return LambdaJ.Subr.getUniversalTime(); }
        public final Object getDecodedTime     (Object... args) { clrValues(); noArgs("get-decoded-time",       args); return LambdaJ.Subr.getDecodedTime(new ListBuilder(), this::bool); }


        // Java FFI
        public final Object _jmethod   (Object... args) {
            clrValues(); varargs2(JMETHOD, args);
            return JFFI.findMethod(LambdaJ.requireString(JMETHOD, args[0]), LambdaJ.requireString(JMETHOD, args[1]), arraySlice(args, 2));
        }
        public final Primitive findMethod(Object className, Object methodName, Object... paramClasses) {
            clrValues();
            return JFFI.findMethod(LambdaJ.requireString(JMETHOD, className), LambdaJ.requireString(JMETHOD, methodName), arraySlice(paramClasses));
        }

        // makeProxy kann auch interpretierte funktionen. wenn intp==null ist, kanns aber keine geben
        public final Object _jproxy    (Object... args) { clrValues(); varargs3("jproxy", args); return JFFI.makeProxy(intp, this, arraySlice(args)); }


        // graphics
        public final Object makeFrame  (Object... args) {
            clrValues(); varargsMinMax("make-frame", args, 1, 4);
            final String title = LambdaJ.requireString("make-frame", args[0]);
            final TurtleFrame ret = new TurtleFrame(title, LambdaJ.Chk.requireNumberOrNull("make-frame", nth(1, args)), LambdaJ.Chk.requireNumberOrNull("make-frame", nth(2, args)), LambdaJ.Chk.requireNumberOrNull("make-frame", nth(3, args)));
            current_frame = ret;
            return ret;
        }

        public final Object openFrame    (Object... args) { varargs0_1("open-frame",    args); return requireFrame("open-frame",     0, args).open();    }
        public final Object closeFrame   (Object... args) { varargs0_1("close-frame",   args); return requireFrame("close-frame",    0, args).close();   }
        public final Object resetFrame   (Object... args) { varargs0_1("reset-frame",   args); return requireFrame("reset-frame",    0, args).reset();   }
        public final Object clearFrame   (Object... args) { varargs0_1("clear-frame",   args); return requireFrame("clear-frame",    0, args).clear();   }
        public final Object repaintFrame (Object... args) { varargs0_1("repaint-frame", args); return requireFrame("repaint-frame",  0, args).repaint(); }
        public final Object flushFrame   (Object... args) { varargs0_1("flush-frame",   args); return requireFrame("flush-frame",    0, args).flush();   }

        // set new current frame, return previous frame
        public final Object currentFrame (Object... args) { varargs0_1("current-frame", args);
                                                            final Object prev = current_frame;
                                                            if (args.length > 0 && args[0] != null) current_frame = requireFrame("current-frame", args[0]);
                                                            return prev; }

        public final Object pushPos      (Object... args) { varargs0_1("push-pos",         args); return requireFrame("push-pos",             0, args).pushPos(); }
        public final Object popPos       (Object... args) { varargs0_1("pop-pos",          args); return requireFrame("pop-pos",              0, args).popPos();  }

        public final Object penUp        (Object... args) { varargs0_1("pen-up",           args); return requireFrame("pen-up",               0, args).penUp();   }
        public final Object penDown      (Object... args) { varargs0_1("pen-down",         args); return requireFrame("pen-down",             0, args).penDown(); }

        public final Object color        (Object... args) { varargs1_2("color",            args); return requireFrame("color",                1, args).color   (toInt(args[0])); }
        public final Object bgColor      (Object... args) { varargs1_2("bgcolor",          args); return requireFrame("bgcolor",              1, args).bgColor (toInt(args[0])); }

        public final Object text         (Object... args) { varargs1_2("text",             args); return requireFrame("text",                 1, args).text   (args[0].toString()); }

        public final Object right        (Object... args) { varargs1_2("right",            args); return requireFrame("right",                1, args).right  (toDouble(args[0])); }
        public final Object left         (Object... args) { varargs1_2("left",             args); return requireFrame("left",                 1, args).left   (toDouble(args[0])); }
        public final Object forward      (Object... args) { varargs1_2("forward",          args); return requireFrame("forward",              1, args).forward(toDouble(args[0])); }

        public final Object moveTo       (Object... args) { varargsMinMax("move-to",       args, 2, 3); return requireFrame("move-to",        2, args).moveTo (toDouble(args[0]), toDouble(args[1]));  }
        public final Object lineTo       (Object... args) { varargsMinMax("line-to",       args, 2, 3); return requireFrame("line-to",        2, args).lineTo (toDouble(args[0]), toDouble(args[1]));  }
        public final Object moveRel      (Object... args) { varargsMinMax("move-rel",      args, 2, 3); return requireFrame("move-rel",       2, args).moveRel(toDouble(args[0]), toDouble(args[1])); }
        public final Object lineRel      (Object... args) { varargsMinMax("line-rel",      args, 2, 3); return requireFrame("line-rel",       2, args).lineRel(toDouble(args[0]), toDouble(args[1])); }

        public final Object makeBitmap   (Object... args) { varargsMinMax("make-bitmap",   args, 2, 3); return requireFrame("make-bitmap",    2, args).makeBitmap(toInt(args[0]), toInt(args[1]));  }
        public final Object discardBitmap(Object... args) { varargs0_1("discard-bitmap",   args);       return requireFrame("discard-bitmap", 0, args).discardBitmap();   }

        public final Object setPixel     (Object... args) { varargsMinMax("set-pixel",     args, 3, 4); return setPixel(toInt(args[0]), toInt(args[1]), toInt(args[2]), nth(3, args)); }
        public final Object setPixel     (Object x, Object y, Object rgb) { return setPixel(x, y, rgb, null);  }
        public final Object setPixel     (Object x, Object y, Object rgb, Object frame) { clrValues(); return requireFrame("set-pixel", frame).setRGB(toInt(x), toInt(y), toInt(rgb));  }

        public final  long rgbToPixel    (Object... args) { threeArgs("rgb-to-pixel", args); return rgbToPixel(args[0], args[1], args[2]); }
        @SuppressWarnings("RedundantCast")
        public final  long rgbToPixel    (Object red, Object green, Object blue) { clrValues(); return (int)((toInt(red) << 16) | (toInt(green) << 8) | toInt(blue)); }

        public final  long hsbToPixel    (Object... args) { threeArgs("hsb-to-pixel", args); return hsbToPixel(args[0], args[1], args[2]); }
        public final  long hsbToPixel    (Object h, Object s, Object b) { clrValues(); return Color.HSBtoRGB(toFloat(h), toFloat(s), toFloat(b)); }


        private static Object nth(int n, Object[] args) { return args.length > n ? args[n] : null; }

        private static boolean secondArgNotNull    (Object[] args) { return args.length > 1 && args[1] != null; }
        private static boolean noSecondArgOrNotNull(Object[] args) { return args.length < 2 || args[1] != null; }


        private Object retn(Object[] _values) { assert _values.length > 1; values = _values; return _values[0]; }

        public final boolean clrValues(boolean b) { clrValues(); return b; }
        public final Object  clrValues(Object o)  { clrValues(); return o; }
        public final void    clrValues()          { values = null; }

        /// Helpers that the Java code compiled from Murmel will use, i.e. compiler intrinsics
        public final LambdaJSymbol intern(String symName) { clrValues(); return symtab.intern(symName); }

        public final Object arrayToList(Object[] args, int start) {
            clrValues();
            if (start >= args.length) return null;
            if (args.length-start == 1) return ConsCell.cons(args[start], null);
            final ListBuilder ret = new ListBuilder();
            for (int i = start; i < args.length; i++) ret.append(args[i]);
            return ret.first();
        }

        public final Map<Object,Object> hash(ConsCell args) { return LambdaJ.Subr.hash(symtab, args); }

        public static ConsCell arraySlice(Object[] o, int offset) { return LambdaJ.arraySlice(o, offset); }
        public static ConsCell arraySlice(Object[] o) { return arraySlice(o, 0); }

        /** convert null, an array or a list to a (possibly empty) Object[] */
        public static Object[] toArray(Object o) {
            if (o == null) return NOARGS;
            if (o instanceof Object[]) return (Object[])o;
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
            return LambdaJ.Chk.toDouble(n);
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
            final Number n = LambdaJ.Chk.requireNumber("toFloat", o);
            final double d = n.doubleValue();
            if (d >= -Float.MAX_VALUE && d <= Float.MAX_VALUE) return n.floatValue();
            throw errorOverflow("toFloat", "java.lang.Float", o);
        }
        public static boolean toBoolean(Object n)  { return n != null; }
        public static byte toByte(Object n)  { return requireIntegralNumber("toByte", n, Byte.MIN_VALUE, Byte.MAX_VALUE).byteValue(); }
        public static short toShort(Object n) { return requireIntegralNumber("toShort", n, Short.MIN_VALUE, Short.MAX_VALUE).shortValue(); }


        /** used by generated Java code */
        public static Object requireNotNull(Object obj) {
            if (obj == null) { throw new SimpleTypeError("object is " + NIL); }
            return obj;
        }

        public static Object[] requireArray(Object obj) {
            if (obj == null) { throw new SimpleTypeError("object is " + NIL); }
            if (obj instanceof Object[]) return (Object[])obj;
            if (obj instanceof List) return ((List<?>)obj).toArray(new Object[0]);
            throw new SimpleTypeError("not an array: %s", printSEx(obj));
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
            if (o instanceof char[]) return String.valueOf((char[])o);
            if (!stringp(o)) errorNotAString(o);
            return o.toString();
        }

        /** used by JFFI and generated inline JFFI */
        public static String requireStringOrNull(Object o) {
            if (o == null) return null;
            if (o instanceof char[]) return String.valueOf((char[])o);
            if (!stringp(o)) errorNotAString(o);
            return o.toString();
        }

        /** used by JFFI and generated inline JFFI */
        public static Number requireNumber(Object o) {
            return LambdaJ.Chk.requireNumber("?", o);
        }

        /** used by JFFI and generated inline JFFI */
        public static Number requireNumberOrNull(Object o) {
            if (o == null) return null;
            return LambdaJ.Chk.requireNumber("?", o);
        }

        private TurtleFrame requireFrame(String func, int n, Object[] arg) { return requireFrame(func, nth(n, arg)); }
        private TurtleFrame requireFrame(String s, Object o) {
            clrValues();
            if (o == null) o = current_frame;
            if (o instanceof TurtleFrame) return (TurtleFrame)o;
            throw errorNotAFrame(s, o);
        }

        public static Object[] unassigned(int length) { final Object[] ret = new Object[length]; Arrays.fill(ret, UNASSIGNED_LOCAL); return ret; }

        public static void argCheck(String expr, int paramCount, Object[] args) { final int argCount = args.length; if (paramCount != argCount) errorArgCount(expr, paramCount, paramCount, argCount); }
        public static void argCheckVarargs(String expr, int paramCount, Object[] args) { final int argCount = args.length; if (argCount < paramCount - 1) errorArgCount(expr, paramCount - 1, Integer.MAX_VALUE, argCount); }

        @SuppressWarnings("unchecked")
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

        /** invoke *condition-handler* if any or rethrow, similar to Java's throw fling() doesn't return */
        private void fling(Exception e) {
            final Object handler = conditionHandler.get();
            if (LambdaJ.functionp0(handler)) {
                conditionHandler.pop(); // disable current handler, make previous handler active
                try { funcall(handler, e); }
                finally { conditionHandler.push(handler); /* restore current handler */ }
            }
            wrap0(e);
        }

        public static Object tailcall(CompilerPrimitive fn, Object... args) { return funcall(fn, args); }

        /** used for (apply sym form) */
        public static Object applyHelper(CompilerPrimitive fn, Object argList) { return funcall(fn, toArray(argList)); }

        /** used for (apply sym form) */
        public static Object applyTailcallHelper(CompilerPrimitive fn, Object argList) { return funcall(fn, toArray(argList)); }



        /** TCO trampoline, used for function calls, and also for let, labels, progn */
        public final Object funcall(@NotNull MurmelFunction fn, Object... args) {
            ConsCell cleanups = null;
            try {
                while (true) {
                    final Object r = fn.apply(args);
                    if (r instanceof Tailcall) {
                        final Tailcall functionCall = (Tailcall)r;
                        if (functionCall.cleanup != null) cleanups = ConsCell.cons(functionCall.cleanup, cleanups);
                        if (Thread.interrupted()) throw new InterruptedException("got interrupted");
                        fn = functionCall.fn;
                        args = functionCall.args;
                        continue;
                    }
                    return r;
                }
            }
            catch (ReturnException re) { throw re; }
            catch (Exception e) {
                fling(e);
                //noinspection ConstantConditions because fling() doesn't return
                throw null;
            }
            finally { if (cleanups != null) runCleanups(cleanups); }
        }

        private static void runCleanups(@NotNull ConsCell cleanups) {
            LambdaJError ex = null;
            for (Object cl: cleanups) {
                try { ((MurmelFunction)cl).apply((Object[])null); }
                //catch (LambdaJError e) { if (ex == null) ex = e; else ex.addSuppressed(e); }
                //catch (Exception e)    { if (ex == null) ex = new LambdaJError(e); else ex.addSuppressed(e); }
                catch (LambdaJError e) { ex = e; }
                catch (Exception e)    { ex = new LambdaJError(e); }
            }
            if (ex != null) throw ex;
        }

        public final Object funcall(Object fn, Object... args) {
            if (fn instanceof MurmelFunction)    return funcall((MurmelFunction)fn, args);
            if (fn instanceof CompilerPrimitive) return funcall((CompilerPrimitive)fn, args);
            return funcallIntp(fn, args);
        }

        private Object funcallIntp(Object fn, Object[] args) {
            if (fn instanceof Primitive)         { final Object ret = ((Primitive)fn).applyPrimitive(arraySlice(args));  afterEval();  return ret; }
            if (fn instanceof Closure)           return interpret(fn, args);

            throw errorNotAFunction(fn);
        }

        private Object interpret(Object fn, Object[] args) {
            final LambdaJ intp = intpForEval();
            final Object ret = intp.eval(ConsCell.cons(intern(APPLY),
                                                       ConsCell.cons(fn,
                                                                     ConsCell.cons(ConsCell.cons(intern(QUOTE),
                                                                                                 ConsCell.cons(arraySlice(args),
                                                                                                               null)),
                                                                                   null))),
                                         null);
            afterEval();
            return ret;
        }

        private static final class Tailcall {
            MurmelFunction fn;
            MurmelFunction cleanup;
            Object[] args;
        }

        private final Tailcall tailcall = new Tailcall();
        /** used for function calls */
        public final Tailcall tailcall(MurmelFunction fn, Object... args) { return tailcallWithCleanup(fn, null, args); }

        public final Tailcall tailcallWithCleanup(MurmelFunction fn, MurmelFunction cleanup, Object... args) {
            final Tailcall tailcall = this.tailcall;
            tailcall.fn = fn;
            tailcall.cleanup = cleanup;
            tailcall.args = args;
            return tailcall;
        }

        public final Object tailcall(Object fn, Object... args) { return tailcallWithCleanup(fn, null, args); }

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

        public final Object catchHelper(Object tag, Exception e) {
            if (e instanceof ReturnException) {
                final ReturnException re = (ReturnException)e;
                if (tag == re.tag) { values = re.values; return re.result; }
                throw re;
            }
            try {
                fling(e);
            }
            catch (Exception e2) { return catchHelper(tag, e2); }
            assert false: "notreached"; return null;
        }

        public final Object doThrow(Object tag, Object primaryResult) {
            // todo checken obs tag gibt, sonst (error 'control-error)
            throw new ReturnException(tag, primaryResult, values);
        }

        public final Object doTry(MurmelFunction protectedForm, Object errorObj) {
            final Object oldHandler = conditionHandler.get();
            conditionHandler.set(null);
            try {
                return protectedForm.apply(NOARGS);
            }
            catch (ReturnException e) { throw e; }
            catch (Exception e) {
                values = new Object[] { errorObj, new LambdaJError(e, true, loc) };
                return errorObj;
            }
            finally { conditionHandler.setForTry(oldHandler); }
        }



        /// ## Error "handlers" for compiled code, see also LambdaJ.error...()

        private static RuntimeException errorNotANumber(Object n) { throw new SimpleTypeError("not a number: %s", printSEx(n)); }
        private static RuntimeException errorNotABit(Object n) { throw new SimpleTypeError("not a bit: %s", printSEx(n)); }
        private static RuntimeException errorNotAnArrayIndex(Object n) { throw new SimpleTypeError("invalid array index/ size: %s", printSEx(n)); }
        private static void errorNotAList(Object s)   { throw new SimpleTypeError("not a cons/list: %s", printSEx(s)); }
        private static void errorNotACharacter(Object s) { throw new SimpleTypeError("not a character: %s", printSEx(s)); }
        private static void errorNotAString(Object s) { throw new SimpleTypeError("not a string: %s", printSEx(s)); }
        private static RuntimeException errorNotAFunction(Object fn) { throw LambdaJ.errorNotAFunction("not a function: %s", printSEx(fn)); }
        private static RuntimeException errorNotAFrame(String s, Object o) {
            if (o != null) throw new SimpleTypeError("%s: not a frame: %s", s, printSEx(o));
            throw new SimpleTypeError("%s: no frame argument and no current frame", s);
        }

        private static void errorArgCount(String expr, int expectedMin, int expectedMax, int actual) {
            if (actual < expectedMin) throw new ProgramError("%s: not enough arguments", expr);
            if (expectedMax != -1 && actual > expectedMax) throw new ProgramError("%s: too many arguments", expr);
        }



        /// ##  Error checking functions, see also LambdaJ.varargs...()

        private static void noArgs(String expr, Object[] args)      { final int argCount = args.length;  if (0 != argCount)               errorArgCount(expr, 0, 0, argCount); }
        private static void oneArg(String expr, Object[] args)      { final int argCount = args.length;  if (1 != argCount)               errorArgCount(expr, 1, 1, argCount); }
        private static void twoArgs(String expr, Object[] args)     { final int argCount = args.length;  if (2 != argCount)               errorArgCount(expr, 2, 2, argCount); }
        private static void threeArgs(String expr, Object[] args)   { final int argCount = args.length;  if (3 != argCount)               errorArgCount(expr, 3, 3, argCount); }

        /** 0..1 args */
        private static void varargs0_1(String expr, Object[] args) { final int argCount = args.length;  if (argCount > 1)                 errorArgCount(expr, 0, 1, argCount); }
        /** one or more arguments */
        private static void varargs1(String expr, Object[] args)   { final int argCount = args.length;  if (argCount == 0)                errorArgCount(expr, 1, -1, 0); }
        /** 1..2 args */
        private static void varargs1_2(String expr, Object[] args) { final int argCount = args.length;  if (argCount < 1 || argCount > 2) errorArgCount(expr, 1, 2, argCount); }
        /** two or more arguments */
        private static void varargs2(String expr, Object[] args)   { final int argCount = args.length;  if (argCount < 2)                 errorArgCount(expr, 2, -1, argCount); }
        private static void varargs3(String expr, Object[] args)   { final int argCount = args.length;  if (argCount < 3)                 errorArgCount(expr, 3, -1, argCount); }

        private static void varargsMinMax(String expr, Object[] args, int min, int max) {
            final int argCount = args.length;
            if (argCount < min || argCount > max)
                errorArgCount(expr, min, max, argCount);
        }



        @SuppressWarnings("unused") // used by multiple-value-call
        public class ValuesBuilder {
            private final ArrayList<Object> allValues = new ArrayList<>();

            public ValuesBuilder() { clrValues(); }

            public ValuesBuilder add(Object primary) {
                if (values == null) {
                    allValues.add(primary);
                } else if (values.length > 0) {
                    allValues.addAll(Arrays.asList(values));
                }
                clrValues();
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

        public final Object[] mv(Object prim, int nVars) {
            final Object[] ret;
            if (values != null && values.length == nVars) {
                ret = values;
                return ret;
            }
            ret  = new Object[nVars];
            if (values != null) {
                for (int m = 0; m < nVars && m < values.length; ++m) ret[m] = values[m];
            }
            else ret[0] = prim;
            return ret;
        }

        public final Object[] mvVarargs(Object prim, int nVars) {
            final Object[] ret = new Object[nVars];
            if (values != null) {
                int m = 0;
                for (; m < nVars-1 && m < values.length; ++m) ret[m] = values[m];
                if (m < values.length) ret[m] = arraySlice(Arrays.copyOfRange(values, m, values.length));
            }
            else ret[0] = prim;
            return ret;
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
            commandlineArgumentList.set(args);
        }

        @Override public Object getValue(String symbol) {
            switch (symbol) {

            // predefined global variables
            case NIL: return null;
            case T: return _t;
            case PI: return _pi;

            case ARRAY_DIMENSION_LIMIT: return arrayDimensionLimit;
            case MOST_POSITIVE_FIXNUM: return mostPositiveFixnum;
            case MOST_NEGATIVE_FIXNUM: return mostNegativeFixnum;
            case INTERNAL_TIME_UNITS_PER_SECOND: return itups;

            case COMMAND_LINE_ARGUMENT_LIST: return commandlineArgumentList.get(); // this will be assigned by genereted code at runtime
            case FEATURES: return features.get();
            case CONDITION_HANDLER: return conditionHandler.get();
            case RANDOM_STATE: return randomState.get();

            // basic primitives
            case APPLY: return (CompilerPrimitive)this::_apply;
            case EVAL: return (CompilerPrimitive)this::_eval;

            // logic, predicates
            case EQ: return (CompilerPrimitive)this::_eq;
            case EQL: return (CompilerPrimitive)this::_eql;
            case EQUAL: return (CompilerPrimitive)this::_equal;

            case CONSP: return (CompilerPrimitive)this::_consp;
            case ATOM: return (CompilerPrimitive)this::_atom;
            case SYMBOLP: return (CompilerPrimitive)this::_symbolp;
            case NULL: return (CompilerPrimitive)this::_null;
            case NUMBERP: return (CompilerPrimitive)this::_numberp;
            case FLOATP: return (CompilerPrimitive)this::_floatp;
            case INTEGERP: return (CompilerPrimitive)this::_integerp;
            case CHARACTERP: return (CompilerPrimitive)this::_characterp;
            case RANDOM_STATE_P: return (CompilerPrimitive)this::_randomstatep;

            case VECTORP: return (CompilerPrimitive)this::_vectorp;
            case SIMPLE_VECTOR_P: return (CompilerPrimitive)this::svectorp;
            case STRINGP: return (CompilerPrimitive)this::_stringp;
            case SIMPLE_STRING_P: return (CompilerPrimitive)this::sstringp;
            case BIT_VECTOR_P: return (CompilerPrimitive)this::bitvectorp;
            case SIMPLE_BIT_VECTOR_P: return (CompilerPrimitive)this::sbitvectorp;
            case HASH_TABLE_P: return (CompilerPrimitive)this::hashtablep;

            case FUNCTIONP: return (CompilerPrimitive)this::_functionp;

            case LISTP: return (CompilerPrimitive)this::_listp;
            case TYPEP: return (CompilerPrimitive)this::_typep;
            case ADJUSTABLE_ARRAY_P: return (CompilerPrimitive)this::adjustableArrayP;

            // conses and lists
            case CAR: return (CompilerPrimitive)this::_car;
            case CDR: return (CompilerPrimitive)this::_cdr;
            case CONS: return (CompilerPrimitive)this::_cons;
            case RPLACA: return (CompilerPrimitive)this::_rplaca;
            case RPLACD: return (CompilerPrimitive)this::_rplacd;

            case LIST: return (CompilerPrimitive)this::_list;
            case LISTSTAR: return (CompilerPrimitive)this::listStar;
            case APPEND: return (CompilerPrimitive)this::_append;
            case ASSQ: return (CompilerPrimitive)this::_assq;
            case ASSOC: return (CompilerPrimitive)this::_assoc;

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
            case "random": return (CompilerPrimitive)this::_random;
            case "make-random-state": return (CompilerPrimitive)this::makeRandomState;

            // vectors, sequences
            case MAKE_ARRAY: return (CompilerPrimitive)this::makeArray;

            case "vector-length": return (CompilerPrimitive)this::vectorLength;
            case "vector-copy": return (CompilerPrimitive)this::vectorCopy;
            case "vector-fill": return (CompilerPrimitive)this::vectorFill;
            case "vector-add": return (CompilerPrimitive)this::vectorAdd;
            case "vector->list": return (CompilerPrimitive)this::vectorToList;
            case "list->vector": return (CompilerPrimitive)this::listToVector;

            case "svlength": return (CompilerPrimitive)this::_svlength;
            case "svref": return (CompilerPrimitive)this::_svref;
            case "svset": return (CompilerPrimitive)this::_svset;
            case "simple-vector->list": return (CompilerPrimitive)this::simpleVectorToList;
            case "list->simple-vector": return (CompilerPrimitive)this::listToSimpleVector;
            case VECTOR: return (CompilerPrimitive)this::_vector;
            case VECT: return (CompilerPrimitive)this::_vect;

            case "string": return (CompilerPrimitive)this::_string;
            case "slength": return (CompilerPrimitive)this::_slength;
            case "sref": return (CompilerPrimitive)this::_sref;
            case "sset": return (CompilerPrimitive)this::_sset;
            case "string=": return (CompilerPrimitive)this::stringeq;
            case "string->list": return (CompilerPrimitive)this::stringToList;
            case "list->string": return (CompilerPrimitive)this::listToString;

            case "char-code": return (CompilerPrimitive)this::charInt;
            case "code-char": return (CompilerPrimitive)this::intChar;

            case "bvlength": return (CompilerPrimitive)this::_bvlength;
            case "bvref": return (CompilerPrimitive)this::_bvref;
            case "bvset": return (CompilerPrimitive)this::_bvset;
            case "bv=": return (CompilerPrimitive)this::bvEq;
            case "bit-vector->list": return (CompilerPrimitive)this::bitVectorToList;
            case "list->bit-vector": return (CompilerPrimitive)this::listToBitVector;

            case "seqref": return (CompilerPrimitive)this::_seqref;
            case "seqset": return (CompilerPrimitive)this::_seqset;

            // Hash tables
            case HASH: return (CompilerPrimitive)this::_hash;
            case MAKE_HASH_TABLE: return (CompilerPrimitive)this::makeHash;
            case "hashref": return (CompilerPrimitive)this::_hashref;
            case "hashset": return (CompilerPrimitive)this::_hashset;
            case "hash-table-count": return (CompilerPrimitive)this::hashTableCount;
            case "clrhash": return (CompilerPrimitive)this::_clrhash;
            case "hash-table-remove": return (CompilerPrimitive)this::hashRemove;
            case "sxhash": return (CompilerPrimitive)this::_sxhash;
            case "scan-hash-table": return (CompilerPrimitive)this::scanHash;

            // I/O
            case "read": return (CompilerPrimitive)this::_read;
            case "read-from-string": return (CompilerPrimitive)this::readFromStr;
            case "read-textfile-lines": return (CompilerPrimitive)this::readTextfileLines;
            case "read-textfile": return (CompilerPrimitive)this::readTextfile;
            case "write-textfile-lines": return (CompilerPrimitive)this::writeTextfileLines;
            case "write-textfile": return (CompilerPrimitive)this::writeTextfile;
            case "write-to-string": return (CompilerPrimitive)this::writeToString;
            case "write": return (CompilerPrimitive)this::_write;
            case "writeln": return (CompilerPrimitive)this::_writeln;
            case "lnwrite": return (CompilerPrimitive)this::_lnwrite;

            case "format": return (CompilerPrimitive)this::format;
            case "format-locale": return (CompilerPrimitive)this::formatLocale;

            // misc
            case VALUES: return (CompilerPrimitive)this::_values;
            case "gensym": return (CompilerPrimitive)this::_gensym;
            case "trace": return (CompilerPrimitive)this::_trace;
            case "untrace": return (CompilerPrimitive)this::_untrace;
            case ERROR: return (CompilerPrimitive)this::_error;
            case "lisp-implementation-type": return (CompilerPrimitive)this::implType;
            case "lisp-implementation-version": return (CompilerPrimitive)this::implVersion;

            // time
            case "get-internal-real-time": return (CompilerPrimitive)this::getInternalRealTime;
            case "get-internal-run-time": return (CompilerPrimitive)this::getInternalRunTime;
            case "sleep": return (CompilerPrimitive)this::sleep;
            case "get-universal-time": return (CompilerPrimitive)this::getUniversalTime;
            case "get-decoded-time": return (CompilerPrimitive)this::getDecodedTime;

            // Java FFI
            case JMETHOD: return (CompilerPrimitive)this::_jmethod;
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

            default: throw errorUnbound("getValue", symbol);
            }
        }
    }



    ///
    /// ## class MurmelJavaCompiler
    /// class MurmelJavaCompiler - compile Murmel to Java or to a in-memory Class-object and optionally to a .jar file
    ///
    public static class MurmelJavaCompiler {
        private static final boolean USE_SWITCH_EXPR = JavaUtil.jvmVersion() >= 14;

        private final JavaCompilerHelper javaCompiler;
        final @NotNull LambdaJ intp;

        private final LambdaJSymbol sApply, sLambda, sList;

        public MurmelJavaCompiler(SymbolTable st, Path libDir, Path outPath) {
            final LambdaJ intp = new LambdaJ(Features.HAVE_ALL_LEXC.bits(), TraceLevel.TRC_NONE, null, st, null, null, null, libDir);
            intp.init(NULL_READCHARS, System.out::print);
            this.intp = intp;

            sApply = intern(APPLY);
            sLambda = intern(LAMBDA);
            sList = intern(LIST);

            this.javaCompiler = outPath == null ? null : new JavaCompilerHelper(outPath);
        }

        public SymbolTable getSymbolTable() { return intp.getSymbolTable(); }

        private static void note(ConsCell ccForm, String msg) { System.err.println("; Note - " + (ccForm == null ? "" : ccForm.lineInfo()) + msg); }
        private static void noteDead(ConsCell ccForm, Object form) { note(ccForm, "removing dead code " + (form == null ? "" : printSEx(form, true))); }


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
            if (sfx != 0) mangled.append('_').append(sfx);
            return mangled.toString();
        }

        private String mangleFunctionName(String symname, int sfx) {
            return mangle(currentFunctionName.substring(1) + symname, sfx);
        }


        /// environment
        /** extend the environment by putting (symbol mangledsymname) in front of {@code prev},
         *  symbols that are reserved words throw an error. */
        private static ConsCell extenv(String func, Object symbol, int sfx, ConsCell prev) {
            final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(func, symbol);
            return extenvIntern(sym, mangle(sym.toString(), sfx), prev);
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
        private String javasym(Object form, ConsCell env, ConsCell containingForm) {
            if (form == null || form == sNil) return "(Object)null";
            final ConsCell symentry = fastassq(form, env);
            if (symentry == null) {
                if (passTwo) errorMalformedFmt("compilation unit", "undefined symbol %s", form);
                note(containingForm, "implicit declaration of " + form); // todo lineinfo of containing form
                implicitDecl.add(form);
                return mangle(form.toString(), 0) + ".get()"; // on pass 1 assume that undeclared variables are forward references to globals
            }
            else //noinspection SuspiciousMethodCalls
                if (!passTwo && globalDecl.contains(form)) implicitDecl.remove(form);

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



        /// Environment for compiled Murmel
        private static final String[] globalvars = {NIL, T, PI};
        private static final String[][] aliasedGlobals = {
        { MOST_POSITIVE_FIXNUM, "mostPositiveFixnum" }, { MOST_NEGATIVE_FIXNUM, "mostNegativeFixnum" }, { ARRAY_DIMENSION_LIMIT, "arrayDimensionLimit" },
        { INTERNAL_TIME_UNITS_PER_SECOND, "itups" },
        { COMMAND_LINE_ARGUMENT_LIST, "commandlineArgumentList.get()" },
        { FEATURES, "features.get()" }, { CONDITION_HANDLER, "conditionHandler.get()" }, { RANDOM_STATE, "randomState.get()" },
        };
        private static final String[] primitives = {
        CAR, CDR, CONS, RPLACA, RPLACD,
        /*"apply",*/ EVAL, EQ, EQL, EQUAL, NULL, "read", "write", "writeln", "lnwrite",
        ATOM, CONSP, FUNCTIONP, LISTP, SYMBOLP, NUMBERP, STRINGP, CHARACTERP, INTEGERP, FLOATP, VECTORP, TYPEP,
        ASSOC, ASSQ, LIST, VECT, VECTOR, "seqref", "seqset", "svref", "svset", "svlength", "string", "slength", "sref", "sset", "bvref", "bvset", "bvlength",
        APPEND, VALUES,
        "round", "floor", "ceiling", "truncate",
        "fround", "ffloor", "fceiling", "ftruncate",
        "sqrt", "log", "log10", "exp", "expt", "mod", "rem", "signum", "random",
        "gensym", "trace", "untrace",
        ERROR, JMETHOD, "jproxy",
        };
        private static final String[][] aliasedPrimitives = {
        {"+", "add"}, {"*", "mul"}, {"-", "sub"}, {"/", "quot"},
        {"=", "numbereq"}, {"<=", "le"}, {"<", "lt"}, {">=", "ge"}, {">", "gt"}, { "/=", "ne" },
        {"1+", "inc"}, {"1-", "dec"},
        {"read-from-string", "readFromStr"}, {"read-textfile-lines", "readTextfileLines"}, {"read-textfile", "readTextfile"},
        {"write-textfile-lines", "writeTextfileLines"}, {"write-textfile", "writeTextfile"}, {"write-to-string", "writeToString"}, {"format", "format"}, {"format-locale", "formatLocale" }, {"char-code", "charInt"}, {"code-char", "intChar"},
        {"string=", "stringeq"}, {"string->list", "stringToList"}, {"list->string", "listToString"},
        {ADJUSTABLE_ARRAY_P, "adjustableArrayP"}, {"vector-add", "vectorAdd"},
        {"vector->list", "vectorToList"}, {"list->vector", "listToVector"}, {"simple-vector->list", "simpleVectorToList"}, {"list->simple-vector", "listToSimpleVector"},
        {"bit-vector->list", "bitVectorToList"}, {"list->bit-vector", "listToBitVector"},
        {"vector-length", "vectorLength"}, {"vector-copy", "vectorCopy"}, {"vector-fill", "vectorFill"},
        {SIMPLE_VECTOR_P, "svectorp"}, {SIMPLE_STRING_P, "sstringp"}, {RANDOM_STATE_P, "_randomstatep"}, {"make-random-state", "makeRandomState"},
        {BIT_VECTOR_P, "bitvectorp"}, {"bv=", "bvEq"}, {SIMPLE_BIT_VECTOR_P, "sbitvectorp"}, {HASH_TABLE_P, "hashtablep"}, {MAKE_ARRAY, "makeArray"},
        {HASH, "_hash"}, {MAKE_HASH_TABLE, "makeHash"}, {"hashref", "_hashref"}, {"hashset", "_hashset"},
        {"hash-table-count", "hashTableCount"}, {"clrhash", "_clrhash"}, {"hash-table-remove", "hashRemove"}, {"sxhash", "_sxhash"}, {"scan-hash-table", "scanHash"},

        {LISTSTAR, "listStar"},
        //{ "macroexpand-1", "macroexpand1" },
        {"lisp-implementation-type", "implType"}, {"lisp-implementation-version", "implVersion"},
        {"get-internal-real-time", "getInternalRealTime" }, {"get-internal-run-time", "getInternalRunTime" },
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
            predefinedEnv = extenvIntern(sApply, "((MurmelFunction)rt()::_apply)", predefinedEnv);

            final WrappingWriter ret = new WrappingWriter(w);

            final String clsName;
            final int dotpos = unitName.lastIndexOf('.');
            if (dotpos == -1) {
                clsName = unitName;
            }
            else {
                ret.append("package ").append(unitName.substring(0, dotpos)).append(";\n\n");
                clsName = unitName.substring(dotpos+1);
            }
            ret.append("import java.util.function.Function;\n"
                       + "import java.util.function.Supplier;\n"
                       + "import io.github.jmurmel.LambdaJ.*;\n\n"
                       + "@SuppressWarnings(\"unchecked\")\n"
                       + "public class ").append(clsName).append(" extends MurmelJavaProgram {\n"
                       + "    protected ").append(clsName).append(" rt() { return this; }\n\n"
                                                                  + "    public static void main(String[] args) {\n"
                                                                  + "        final ").append(clsName).append(" program = new ").append(clsName).append("();\n"
                                                                  + "        program.commandlineArgumentList.set(program.arrayToList(args,0));\n"
                                                                  + "        main(program);\n"
                                                                  + "    }\n\n");

            final ArrayList<Object> bodyForms = new ArrayList<>();
            final StringBuilder globals = new StringBuilder();

            /// first pass: emit toplevel define/ defun forms
            final short prevSpeed = intp.speed, prevDebug = intp.debug;
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
                    throw e;
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
            ret.append("        return super.getValue(symbol);\n"
                       + "    }\n\n"
                       + "    // toplevel forms\n"
                       + "    protected Object runbody() throws Exception {\n");

            /// second pass: emit toplevel forms that are not define or defun as well as the actual assignments for define/ defun
            intp.speed = prevSpeed;  intp.debug = prevDebug;
            passTwo = true;
            emitToplevelForms(ret, bodyForms, globalEnv, globalEnv);

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

                assert op != null && op != sNil : "not a function: nil - should have been caught by expandForm()";

                if (symbolp(op)) {
                    switch (((LambdaJSymbol)op).wellknownSymbol) {

                    case sDefine: {
                        globalEnv = defineToJava(ret, ccForm, globalEnv, 0);
                        intp.eval(ccForm, null);
                        bodyForms.add(ccForm);
                        globals.append("        case \"").append(cadr(ccForm)).append("\": return ").append(javasym(cadr(ccForm), globalEnv, ccForm)).append(";\n");
                        return globalEnv;
                    }

                    case sDefun: {
                        globalEnv = defunToJava(ret, ccForm, globalEnv);
                        intp.eval(ccForm, null);
                        bodyForms.add(ccForm);
                        globals.append("        case \"").append(cadr(ccForm)).append("\": return ").append(javasym(cadr(ccForm), globalEnv, ccForm)).append(";\n");
                        return globalEnv;
                    }

                    case sDefmacro: {
                        LambdaJ.symbolOrMalformed(DEFMACRO, cadr(ccForm));
                        intp.eval(ccForm, null);
                        bodyForms.add(ccForm); // needed if compiled code calls macroexpand-1
                        return globalEnv;
                    }

                    case sProgn: {
                        // toplevel progn will be replaced by the (macroexpanded) forms it contains.
                        // Macroexpand is needed in case the progn contained a load or require that contains defmacro forms, seel also LambdaJ#expandAndEval()
                        final ConsCell body = listOrMalformed(PROGN, cdr(ccForm));
                        for (Object prognForm : body) {
                            globalEnv = toplevelFormToJava(ret, bodyForms, globals, globalEnv, intp.expandForm(prognForm));
                        }
                        return globalEnv;
                    }

                    case sLet:
                    case sLetStar:
                    case sLetrec: {
                        if (cadr(ccForm) instanceof LambdaJSymbol) break;
                        final ConsCell ccBodyForms = (ConsCell)cddr(ccForm);
                        globalEnv = toplevelLetBody(ret, globals, globalEnv, ccBodyForms, 1);
                        bodyForms.add(ccForm);
                        return globalEnv;
                    }

                    case sMultipleValueBind: {
                        final ConsCell ccBodyForms = (ConsCell)cdddr(ccForm);
                        globalEnv = toplevelLetBody(ret, globals, globalEnv, ccBodyForms, 1);
                        bodyForms.add(ccForm);
                        return globalEnv;
                    }

                    case sLoad: {
                        final ConsCell ccArgs = listOrMalformed(LOAD, cdr(ccForm));
                        oneArg(LOAD, ccArgs);
                        if (ccForm instanceof SExpConsCell) {
                            final SExpConsCell sExpConsCell = (SExpConsCell)ccForm;
                            intp.currentSource = sExpConsCell.path();
                        } // todo unschoener hack
                        globalEnv = loadFile(LOAD, ret, car(ccArgs), globalEnv, bodyForms, globals);
                        return globalEnv;
                    }

                    case sRequire: {
                        final ConsCell ccArgs = listOrMalformed(REQUIRE, cdr(ccForm));
                        varargs1_2(REQUIRE, ccArgs);
                        if (!stringp(car(ccArgs))) errorMalformed(REQUIRE, "a string argument", ccArgs);
                        final Object modName = car(ccArgs);
                        if (!intp.modules.contains(modName)) {
                            Object modFilePath = cadr(ccArgs);
                            if (modFilePath == null) modFilePath = modName;
                            if (ccForm instanceof SExpConsCell) {
                                final SExpConsCell sExpConsCell = (SExpConsCell)ccForm;
                                intp.currentSource = sExpConsCell.path();
                            } // todo unschoener hack
                            globalEnv = loadFile(REQUIRE, ret, modFilePath, globalEnv, bodyForms, globals);
                            if (!intp.modules.contains(modName)) errorMalformedFmt(REQUIRE, "require'd file '%s' does not provide '%s'", modFilePath, modName);
                        }
                        return globalEnv;
                    }

                    case sProvide: {
                        final ConsCell ccArgs = listOrMalformed(PROVIDE, cdr(ccForm));
                        oneArg(PROVIDE, ccArgs);
                        if (!stringp(car(ccArgs))) errorMalformed(PROVIDE, "a string argument", ccArgs);
                        final Object modName = car(ccArgs);
                        intp.modules.add(modName);
                        return globalEnv;
                    }

                    case sDeclaim: {
                        intp.evalDeclaim(1, (ConsCell)cdr(ccForm)); // cast is safe because expandForm will fail on dotted forms
                        bodyForms.add(ccForm);
                        return globalEnv;
                    }

                    default:
                        break;
                    }

                    if (null != ((LambdaJSymbol)op).macro) {
                        errorInternal("unexpected unexpanded macrocall: %s", printSEx(form));
                    }
                }
            }

            bodyForms.add(form);
            return globalEnv;
        }

        private ConsCell toplevelLetBody(WrappingWriter ret, StringBuilder globals, ConsCell globalEnv, ConsCell ccBodyForms, int rsfx) {
            for (Object letbodyform : ccBodyForms) {
                if (consp(letbodyform)) globalEnv = toplevelLet(ret, globals, globalEnv, (ConsCell)letbodyform, rsfx+1);
            }
            return globalEnv;
        }

        private ConsCell toplevelLet(WrappingWriter ret, StringBuilder globals, ConsCell globalEnv, ConsCell ccForm, int rsfx) {
            final Object op = car(ccForm);

            if (symbolp(op)) switch (((LambdaJSymbol)op).wellknownSymbol) {

            case sDefine:
            case sDefun:
                final Object symbol = cadr(ccForm);
                globalEnv = defineToJava(ret, ConsCell.list(intern(DEFINE), symbol, null), globalEnv, rsfx);
                globals.append("        case \"").append(symbol).append("\": return ").append(javasym(symbol, globalEnv, ccForm)).append(";\n");
                break;

            case sLet:
            case sLetStar:
            case sLetrec:
                final Object maybeBindings = cadr(ccForm);
                if (listp(maybeBindings)) return toplevelLetBody(ret, globals, globalEnv, (ConsCell)cddr(ccForm), rsfx+1);
                break;

            case sMultipleValueBind:
                return toplevelLetBody(ret, globals, globalEnv, (ConsCell)cdddr(ccForm), rsfx+1);

            case sProgn:
                return toplevelLetBody(ret, globals, globalEnv, (ConsCell)cdr(ccForm), rsfx + 1);
            }

            return globalEnv;
        }


        /** Emit a member for {@code symbol} and a function that assigns {@code form} to {@code symbol}.
         *  @param form a list (define symbol form) */
        private ConsCell defineToJava(WrappingWriter sb, ConsCell form, ConsCell env, int rsfx) {
            varargs1_2(DEFINE, listOrMalformed(DEFINE, cdr(form)));
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed(DEFINE, cadr(form));
            notDefined(DEFINE, symbol, env);
            globalDecl.add(symbol);

            final String javasym = mangle(symbol.toString(), rsfx);
            env = extenvIntern(symbol, javasym + ".get()", env);

            sb.append("    // ").append(form.lineInfo()).append("(define ").append(symbol).append(" ...)\n"
                    + "    public CompilerGlobal ").append(javasym).append(" = UNASSIGNED_GLOBAL;\n");
            if (rsfx > 0) {
                sb.append("\n");
                return env;
            }

            sb.append("    private Object define").append(javasym).append("() {\n");
            emitClearValues(sb, form);
            sb.append("        try { final Object value = "); emitForm(sb, caddr(form), env, env, 0, false); sb.append(";\n"
                    + "        ").append(javasym).append(" = new CompilerGlobal(value); }\n"
                    + "        catch (Exception e) { rterror(e); }\n");
            sb.append("        return intern(\"").append(symbol).append("\");\n"
                    + "    }\n\n");
            return env;
        }

        private void emitClearValues(WrappingWriter sb, ConsCell form) {
            emitClearValues(sb);
            emitLoc(sb, form, 40);
        }

        private static void emitClearValues(WrappingWriter sb) {
            sb.append("        clrValues();\n");
        }

        private void emitLoc(WrappingWriter sb, ConsCell form, int maxlen) {
            if (intp.debug == 0) sb.append("        // loc = \"");
            else sb.append("        loc = \"");
            stringToJava(sb, form.lineInfo(), -1);
            stringToJava(sb, printSEx(form), maxlen);
            sb.append("\";\n");
        }

        /** @param form a list (defun symbol ((symbol...) forms...)) */
        private ConsCell defunToJava(WrappingWriter sb, ConsCell form, ConsCell topEnv) {
            final ConsCell symbolParamsAndForms = (ConsCell)cdr(form);
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed(DEFUN, car(symbolParamsAndForms));
            notDefined(DEFUN, symbol, topEnv);
            globalDecl.add(symbol);

            final Object params = cadr(symbolParamsAndForms);
            final ConsCell body = (ConsCell)cddr(symbolParamsAndForms);

            final String javasym = mangleFunctionName(symbol.toString(), 0);

            sb.append("    // ").append(form.lineInfo()).append("(defun ").append(symbol).append(' '); printSEx(sb::append, params); sb.append(" forms...)\n"
                      + "    public CompilerGlobal ").append(javasym).append(" = UNASSIGNED_GLOBAL;\n");

            sb.append("    private LambdaJSymbol defun").append(javasym).append("() {\n");
            emitLoc(sb, form, 40);
            sb.append("        final MurmelFunction func = ");

            emitNamedLambda(DEFUN, sb, symbol, params, body, extenvIntern(symbol, javasym, topEnv), topEnv, 0, true);

            sb.append(";\n        ").append(javasym).append(" = new CompilerGlobal(func);\n"
                    + "        return intern(\"").append(symbol).append("\");\n"
                    + "    }\n\n");

            return extenvIntern(symbol, javasym + ".get()", topEnv);
        }

        private String currentFunctionName = "_";
        private void emitNamedLambda(String func, WrappingWriter sb, LambdaJSymbol symbol, Object params, ConsCell body, ConsCell env, ConsCell topEnv, int rsfx, boolean emitSelf) {
            final String javasym = mangleFunctionName(symbol.toString(), rsfx);
            final String prevName = currentFunctionName;
            currentFunctionName = javasym + '_';

            sb.append("new MurmelFunction() {\n");
            if (emitSelf) sb.append("        private final MurmelFunction ").append(javasym).append(" = this;\n");
            sb.append("        public final Object apply(Object... args").append(rsfx).append(") {\n"
                    + "        return ").append(javasym).append("(args").append(rsfx).append(");\n        }\n" 
                    + "        private Object ").append(javasym).append("(Object[] args").append(rsfx).append(") {\n");
            final ConsCell extenv = params(func, sb, params, env, rsfx, symbol.toString(), true);
            final String ret = "ret" + rsfx;
            sb.append("        Object ").append(ret).append(" = null;\n        ").append(javasym).append(": while (true) {\n");
            final int minParams, maxParams;
            if (params == null) {
                minParams = maxParams = 0;
            }
            else if (symbolp(params)) {
                minParams = 0; maxParams = -1;
            }
            else if (dottedList(params)) {
                minParams = listLength((ConsCell)params) - 1;
                maxParams = -1;
            }
            else {
                minParams = maxParams = listLength((ConsCell)params);
            }
            emitStmts(sb, body, extenv, topEnv, rsfx, "        " + ret + " = ", symbol, "args" + rsfx, minParams, maxParams, false, false);
            sb.append("        break;\n        }\n        return ").append(ret).append(";\n");
            sb.append("        } }");

            currentFunctionName = prevName;
        }


        /// emitToplevelForms - compile a list of Murmel forms to Java source
        /** generate Java code for a list of forms. Each form but the last will be emitted as an assignment
         *  to the local variable "ignoredN" because some forms are emitted as ?: expressions which is not a valid statement by itself. */
        private void emitToplevelForms(WrappingWriter sb, @NotNull Iterable<Object> forms, ConsCell env, ConsCell topEnv) {
            final Iterator<Object> it = forms.iterator();
            if (!it.hasNext()) {
                emitClearValues(sb);
                sb.append("        return null;\n");
                return;
            }

            Object next = it.next();
            if (it.hasNext()) {
                final String retVar = "ignored" + 0;
                final String retLhs = "        " + retVar + " = ";
                sb.append("        Object ").append(retVar).append(";\n");
                do {
                    emitToplevelStmt(sb, next, env, topEnv, retLhs, true);
                    next = it.next();
                } while (it.hasNext()); 
            }
            emitToplevelStmt(sb, next, env, topEnv, "        return ", false);
        }

        private void emitStmts(WrappingWriter sb, ConsCell ccBody, ConsCell env, ConsCell topEnv, int rsfx, String retLhs, boolean toplevel, boolean hasNext) {
            emitStmts(sb, ccBody, env, topEnv, rsfx, retLhs, null, null, -1, -1, toplevel, hasNext);
        }

        private void emitStmts(WrappingWriter sb, ConsCell ccBody, ConsCell env, ConsCell topEnv, int rsfx, String retLhs, LambdaJSymbol recur, String recurArgs, int minParams, int maxParams, boolean toplevel, boolean hasNext) {
            rsfx++;

            if (cdr(ccBody) == null) {
                emitStmt(sb, car(ccBody), env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext, true);
                return;
            }

            final String ignoredVar = "ignored" + rsfx;
            final String lhs = "        " + ignoredVar + " = ";
            sb.append("        {\n        Object ").append(ignoredVar).append(";\n");
            do {
                emitStmt(sb, car(ccBody), env, topEnv, rsfx, lhs, recur, recurArgs, minParams, maxParams, toplevel, true, true);
                ccBody = (ConsCell)cdr(ccBody);
            } while (cdr(ccBody) != null);
            emitStmt(sb, car(ccBody), env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext, true);
            sb.append("        }\n");
        }

        private void emitToplevelStmt(WrappingWriter sb, Object form, ConsCell env, ConsCell topEnv, String retLhs, boolean hasNext) {
            emitStmt(sb, form, env, topEnv, 0, retLhs, null, null, -1, -1, true, hasNext, true);
        }

        private void emitStmt(WrappingWriter sb, Object form, ConsCell env, ConsCell topEnv, int rsfx, String retLhs, LambdaJSymbol recur, String recurArgs, int minParams, int maxParams, boolean toplevel, boolean hasNext, boolean clearValues) {
            if (hasNext) {
                if (atom(form)) {
                    if (form != null) noteDead(null, form); // don't note nil as that would generate a lot of notes for e.g. "(if a nil (dosomething))"
                    return; // must be dead code
                }
                if (symbolEq(car(form), QUOTE)) {
                    noteDead((ConsCell)form, form);
                    return; // must be dead code
                }
                if (symbolEq(car(form), DECLAIM)) {
                    intp.evalDeclaim(1, (ConsCell)cdr(form)); // cast is safe because expandForm will fail on dotted forms
                    return; // ignore return value, must be dead code
                }
            }

            if (atom(form)) {
                if (clearValues) emitClearValues(sb);
                sb.append(retLhs);
                emitForm(sb, form, env, topEnv, rsfx, !toplevel && !hasNext);
                sb.append(";\n");
                return;
            }

            final ConsCell ccForm = (ConsCell)form;
            final Object op = car(ccForm);      // first element of the of the form should be a symbol or a form that computes a symbol
            assert op != null && op != sNil : "not a function: nil - should have been caught by expandForm()";
            final ConsCell ccArguments = listOrMalformed("emitStmt", cdr(ccForm));   // list with remaining atoms/ forms

            final LambdaJSymbol symop;
            final WellknownSymbol ws;
            final boolean isDefOrLet, isStmtExpr;
            if (symbolp(op)) {
                symop = (LambdaJSymbol)op;
                ws = symop.wellknownSymbol;
                isDefOrLet = ws == WellknownSymbol.sDefine || ws == WellknownSymbol.sDefun || ws == WellknownSymbol.sDefmacro
                             || ws == WellknownSymbol.sLet || ws == WellknownSymbol.sLetStar || ws == WellknownSymbol.sLetrec;

                // whether a form needs to be preceeded by "values = null;" and "... = ".
                // This is needed before some special forms (?) and before some primitives that will be opencoded in a special way
                isStmtExpr = isDefOrLet || !needsClrValues(symop);
            }
            else {
                symop = null; ws = null; isDefOrLet = isStmtExpr = false;
            }

            if (clearValues) {
                if (!isStmtExpr) emitClearValues(sb);

                if (!isDefOrLet) emitLoc(sb, ccForm, 100);
            }

            if (symop != null) {
                switch (ws) {

                /// * special forms:

                ///     - quote
                case sQuote: break;

                case sIf: {
                    sb.append("        if (");
                    emitTruthiness(sb, false, car(ccArguments), env, topEnv, rsfx);
                    sb.append(") {\n");
                    emitStmt(sb, cadr(ccArguments), env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext, false);
                    sb.append("        }\n");
                    if (caddr(ccArguments) != null) {
                        sb.append("        else {\n");
                        emitStmt(sb, caddr(ccArguments), env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext, false);
                        sb.append("        }\n");
                    }
                    else if (!hasNext) {
                        sb.append("        else {\n").append(retLhs).append("null;\n        }\n");
                    }
                    return;
                }

                case sCond: {
                    boolean first = true;
                    for (final Iterator<Object> iterator = ccArguments.iterator(); iterator.hasNext(); ) {
                        final Object clause = iterator.next();
                        sb.append("        ");
                        if (first) first = false;
                        else sb.append("else ");
                        final Object condExpr = car(clause), condForms = cdr(clause);
                        if (condExpr == sT) {
                            sb.append("{\n");  emitStmts(sb, (ConsCell)condForms, env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext);  sb.append("        }\n");
                            if (iterator.hasNext()) {
                                final String msg = "forms following default 't' form will be ignored";
                                note(ccForm, msg);
                            }
                            return;
                        }
                        else {
                            sb.append("if (");  emitTruthiness(sb, false, condExpr, env, topEnv, rsfx);  sb.append(") {\n");
                            emitStmts(sb, (ConsCell)condForms, env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext);  sb.append("        }\n");
                        }
                    }
                    if (!hasNext) sb.append("        else {\n").append(retLhs).append("null;\n        }\n");
                    return;
                }

                case sCatch: {
                    sb.append("        try {\n");
                    emitStmts(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, true, hasNext);
                    sb.append("        }\n"
                            + "        catch (Exception e) {\n");
                    if (hasNext) sb.append("        ");
                    else sb.append(retLhs);
                    sb.append("catchHelper("); emitForm(sb, car(ccArguments), env, topEnv, rsfx, false); sb.append(", e);\n        }\n");
                    return;
                }

                case sSetQ: {
                    if (ccArguments == null) {
                        if (!hasNext) sb.append(retLhs).append("null;\n");
                    }
                    else {
                        for (Object pairs = ccArguments; pairs != null; pairs = cddr(pairs)) {
                            if (hasNext || cddr(pairs) != null) sb.append("        ");
                            else sb.append(retLhs);
                            emitSetq(sb, pairs, env, topEnv, rsfx);
                            sb.append(";\n");
                        }
                    }
                    return;
                }

                case sProgn: {
                    final ConsCell ccBody = listOrMalformed(PROGN, cdr(ccForm));
                    emitStmts(sb, ccBody, env, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext);
                    return;
                }

                case sLet:
                case sLetStar:
                case sLetrec: {
                    final Object bindings = cadr(ccForm);
                    if (bindings instanceof LambdaJSymbol) {
                        if (clearValues) {
                            emitLoc(sb, ccForm, 100);
                        }
                        break;
                    }
                    assert bindings != null : "let w/o bindings should have been replaced in expandForm";
                    rsfx++;
                    final ConsCell ccBindings = listOrMalformed(symop.name, bindings);
                    final ConsCell ccBody = listOrMalformed(symop.name, cddr(ccForm));

                    final boolean asRunnable = hasNext && toplevel;
                    if (asRunnable) {
                        sb.append("        new Runnable() { public void run() {\n"
                                  + "        Object tmp = null;\n");
                        retLhs = "        tmp = ";
                    }
                    else sb.append("        {\n");

                    if (clearValues) {
                        emitLoc(sb, ccForm, 100);
                    }
                    final String vName = "v" + rsfx;
                    final int nVars = listLength(ccBindings);
                    sb.append("        final Object[] ").append(vName);
                    if (symop.wellknownSymbol == WellknownSymbol.sLetrec) sb.append(" = unassigned(").append(nVars).append(");\n");
                    else sb.append(" = new Object[").append(nVars).append("];\n");

                    ConsCell letrecEnv = env;
                    if (symop.wellknownSymbol == WellknownSymbol.sLetrec) {
                        int localCtr = 0;
                        for (Object binding : ccBindings) {
                            final ConsCell ccBinding = (ConsCell)binding;
                            final Object sym = car(ccBinding);
                            final String name = vName + '[' + localCtr++ + ']';
                            letrecEnv = extenvIntern((LambdaJSymbol)sym, name, letrecEnv);
                        }
                    }

                    ConsCell extEnv = env;
                    ConsCell letStarEnv = env;
                    int localCtr = 0;
                    final ArrayList<Object> varNames = new ArrayList<>(nVars);
                    for (Object binding : ccBindings) {
                        final ConsCell ccBinding = (ConsCell)binding;
                        final Object sym = car(ccBinding);
                        if (!varNames.contains(sym)) {
                            varNames.add(sym);
                            final String name = vName + '[' + localCtr++ + ']';
                            extEnv = extenvIntern((LambdaJSymbol)sym, name, extEnv);
                        }
                        final ConsCell env1 = symop.wellknownSymbol == WellknownSymbol.sLet ? env : symop.wellknownSymbol == WellknownSymbol.sLetStar ? letStarEnv : letrecEnv;
                        emitStmt(sb, cadr(ccBinding), env1, topEnv, rsfx, "        " + javasym(sym, extEnv, ccBinding) + " = ", null, null, -1, -1, true, false, false);
                        letStarEnv = extEnv;
                    }

                    if (asRunnable) {
                        emitStmts(sb, ccBody, extEnv, topEnv, rsfx, retLhs, toplevel, hasNext);
                        sb.append("        } }.run();\n");
                    }
                    else {
                        emitStmts(sb, ccBody, extEnv, topEnv, rsfx, retLhs, recur, recurArgs, minParams, maxParams, toplevel, hasNext);
                        sb.append("        }\n");
                    }
                    return;
                }

                case sMultipleValueBind: {
                    ConsCell extenv = env;
                    final Object varDef = car(ccArguments);
                    rsfx++;
                    if (varDef != null) {
                        final String prim = "prim" + rsfx;
                        sb.append("        {\n        Object ").append(prim).append(";\n");
                        emitStmt(sb, cadr(ccArguments), env, topEnv, rsfx + 1, "        " + prim + " = ", null, null, -1, -1, true, false, false);

                        int n = 0;
                        if (consp(varDef)) {
                            final ConsCell varList = (ConsCell)varDef;
                            for (Object arg : varList) {
                                extenv = extenvIntern((LambdaJSymbol)arg, "mv" + rsfx + '[' + n++ + ']', extenv);
                            }
                            if (dottedList(varList))
                                sb.append("        Object mv").append(rsfx).append("[] = mvVarargs(").append(prim).append(", ").append(n).append(");\n");
                            else
                                sb.append("        Object mv").append(rsfx).append("[] = mv(").append(prim).append(", ").append(n).append(");\n");
                        }
                        else if (symbolp(varDef)) {
                            extenv = extenvIntern((LambdaJSymbol)varDef, "mv" + rsfx + "[0]", extenv);
                            sb.append("        Object mv").append(rsfx).append("[] = mvVarargs(").append(prim).append(", 1);\n");
                        }
                        else throw errorMalformedFmt(MULTIPLE_VALUE_BIND, "expected a list or a symbol but got %s", printSEx(varDef));

                        // emit the body
                        emitStmts(sb, (ConsCell)cddr(ccArguments), extenv, topEnv, rsfx, retLhs, toplevel, hasNext);
                        sb.append("        }\n");
                    }
                    else {
                        // no variables. Emit the values form (for any side-effects) and the body 
                        emitStmts(sb, (ConsCell)cdr(ccArguments), extenv, topEnv, rsfx, retLhs, toplevel, hasNext);
                    }

                    return;
                }

                case sDefine:
                case sDefun: {
                    final LambdaJSymbol symbol = (LambdaJSymbol)car(ccArguments);
                    if (rsfx == 0 || fastassq(symbol, topEnv) == null) {
                        if (hasNext) {
                            sb.append("        ");
                            emitForm(sb, form, env, topEnv, rsfx, false);
                            sb.append(";\n");
                            return;
                        }
                        break;
                    }

                    emitLoc(sb, ccForm, 40);
                    final String javasym = mangleFunctionName(symbol.toString(), rsfx);
                    sb.append("        ").append(javasym).append(" = new CompilerGlobal(");
                    if (ws == WellknownSymbol.sDefine) emitForm(sb, cadr(ccArguments), env, topEnv, rsfx, false);
                    else emitNamedLambda(DEFUN, sb, symbol, cadr(ccArguments), (ConsCell)cddr(ccArguments), extenvIntern(symbol, javasym, env), topEnv, rsfx, true);
                    sb.append(");\n");
                    if (!hasNext) sb.append(retLhs).append("intern(\"").append(symbol).append("\");\n");

                    return;
                }

                case sDefmacro: {
                    if (hasNext) {
                        sb.append("        ");
                        emitForm(sb, form, env, topEnv, rsfx, false);
                        sb.append(";\n");
                        return;
                    }
                    break;
                }

                default: break;
                }
            }

            if (!hasNext && minParams == maxParams && recur != null && recur == symop) {
                final int nArgs = listLength(ccArguments);
                try { if (nArgs != minParams) errorArgCount(printSEx(recur).toString(), minParams, maxParams, nArgs, form); }
                catch (Exception e) { throw new LambdaJError(e, form); }
                if (nArgs > 0) {
                    sb.append("        {\n");
                    ConsCell args = ccArguments;
                    for (int i = 0; i < nArgs; ++i) {
                        sb.append("        final Object tmp").append(i).append(" = ");
                        emitForm(sb, car(args), env, topEnv, rsfx+1, false);
                        sb.append(";\n");
                        args = (ConsCell)cdr(args);
                    }
                    for (int i = 0; i < nArgs; ++i) {
                        sb.append("        ").append(recurArgs).append('[').append(i).append("] = tmp").append(i).append(";\n");
                    }
                    sb.append("        }\n");
                }
                sb.append("        continue;\n");
                return;
            }

            if (hasNext && isStmtExpr) sb.append("        ");
            else                       sb.append(retLhs);

            emitForm(sb, form, env, topEnv, rsfx, !toplevel && !hasNext);
            sb.append(";\n");
        }

        /// emitForm - compile a Murmel form to Java source. Note how this is somehow similar to eval:
        private void emitForm(WrappingWriter sb, Object form, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final LambdaJ intp = this.intp;
            rsfx++;
            try {

                /// * symbols
                if (symbolp(form)) {
                    sb.append(javasym(form, env, null));  return;
                }
                /// * atoms that are not symbols
                if (atom(form)) {
                    emitAtom(sb, form);  return;
                }

                assert consp(form);
                final ConsCell ccForm = (ConsCell)form;
                final Object op = car(ccForm);      // first element of the of the form should be a symbol or a form that computes a symbol
                assert op != null && op != sNil : "not a function: nil - should have been caught by expandForm()";
                final ConsCell ccArguments = listOrMalformed("emitForm", cdr(ccForm));   // list with remaining atoms/ forms

                if (symbolp(op)) {
                    final LambdaJSymbol symop = (LambdaJSymbol)op;
                    switch (symop.wellknownSymbol) {

                    /// * special forms:

                    ///     - quote
                    case sQuote: {
                        emitQuotedForm(sb, car(ccArguments), true);
                        return;
                    }

                    ///     - if
                    case sIf: {
                        sb.append('(');
                        emitTruthiness(sb, false, car(ccArguments), env, topEnv, rsfx);
                        sb.append("\n        ? ("); emitForm(sb, cadr(ccArguments), env, topEnv, rsfx, isLast);
                        if (caddr(ccArguments) != null) { sb.append(")\n        : ("); emitForm(sb, caddr(ccArguments), env, topEnv, rsfx, isLast); sb.append("))"); }
                        else sb.append(")\n        : (Object)null)");
                        return;
                    }

                    ///     - cond
                    case sCond: {
                        emitCond(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    /// eval - (catch tagform forms...) -> object
                    case sCatch: {
                        emitCatch(sb, ccArguments, env, topEnv, rsfx);
                        return;
                    }

                    /// eval - (throw tagform resultform) -> |
                    case sThrow: {
                        emitThrow(sb, ccArguments, env, topEnv, rsfx);
                        return;
                    }

                    /// try - (try protected-form . errorobj) -> result
                    case sTry: {
                        emitTry(sb, ccArguments, env, topEnv, rsfx);
                        return;
                    }

                    ///     - lambda
                    case sLambda: {
                        emitLambda(sb, ccArguments, env, topEnv, rsfx, true);
                        return;
                    }

                    case sLambdaDynamic: {
                        errorNotImplemented(LAMBDA_DYNAMIC + " is not supported in compiled Murmel");
                        //NOTREACHED
                    }

                    ///     - setq
                    case sSetQ: {
                        if (ccArguments == null) sb.append("(Object)null"); // must cast to Object in case it will be used as the only argument to a vararg function
                        else if (cddr(ccArguments) == null)
                            emitSetq(sb, ccArguments, env, topEnv, rsfx);
                        else {
                            sb.append("((Supplier<Object>)(() -> {\n");
                            String javaName = null;
                            for (Object pairs = ccArguments; pairs != null; pairs = cddr(pairs)) {
                                sb.append("        ");
                                javaName = emitSetq(sb, pairs, env, topEnv, rsfx - 1);
                                sb.append(";\n");
                            }
                            sb.append("        return ").append(javaName).append(";})).get()");
                        }
                        return;
                    }

                    case sDefine: {
                        if (rsfx != 1) errorNotImplemented("define as non-toplevel form is not implemented");
                        defined(DEFINE, car(ccArguments), env);
                        final String javasym = mangle(car(ccArguments).toString(), 0);
                        sb.append("define").append(javasym).append("()");
                        return;
                    }

                    case sDefun: {
                        if (rsfx != 1) errorNotImplemented("defun as non-toplevel form is not implemented");
                        defined(DEFUN, car(ccArguments), env);
                        final String javasym = mangle(car(ccArguments).toString(), 0);
                        sb.append("defun").append(javasym).append("()");
                        return;
                    }

                    case sDefmacro: {
                        if (rsfx != 1) errorNotImplemented("defmacro as non-toplevel form is not implemented");
                        intp.expandForm(form); // this will process the macro definition as a side effect in case macroexpand-1 was used
                        sb.append("intern(\"").append(car(ccArguments)).append("\")");
                        return;
                    }

                    ///     - progn
                    case sProgn: {
                        emitProgn(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - unwind-protect
                    case sUnwindProtect: {
                        emitUnwindProtect(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - labels: (labels ((symbol (params...) forms...)...) forms...) -> object
                    // note how labels is similar to let: let binds values to symbols, labels binds functions to symbols
                    case sLabels: {
                        emitLabels(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let: (let ((sym form)...) forms...) -> object
                    ///     - named let: (let sym ((sym form)...) forms...) -> object
                    case sLet: {
                        if (car(ccArguments) == intp.sDynamic)
                            emitLetLetStarDynamic(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, false, isLast);
                        else
                            emitLet(sb, ccArguments, env, topEnv, rsfx, isLast);
                        return;
                    }

                    ///     - let*: (let* ((sym form)...) forms...) -> Object
                    ///     - named let*: (let sym ((sym form)...) forms...) -> Object
                    case sLetStar: {
                        if (car(ccArguments) == intp.sDynamic)
                            emitLetLetStarDynamic(sb, (ConsCell)cdr(ccArguments), env, topEnv, rsfx, true, isLast);
                        else
                            emitLetStarLetrec(sb, ccArguments, env, topEnv, rsfx, false, isLast);
                        return;
                    }

                    ///     - letrec:       (letrec ((sym form)...) forms) -> Object
                    ///     - named letrec: (letrec sym ((sym form)...) forms) -> Object
                    case sLetrec: {
                        emitLetStarLetrec(sb, ccArguments, env, topEnv, rsfx, true, isLast);
                        return;
                    }

                    case sMultipleValueCall: {
                        sb.append(isLast ? "tailcall(" : "funcall(");
                        emitForm(sb, car(ccArguments), env, topEnv, rsfx, false);
                        if (cdr(ccArguments) != null) {
                            sb.append(", rt().new ValuesBuilder()");
                            for (Object arg : listOrMalformed(MULTIPLE_VALUE_CALL, cdr(ccArguments))) {
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
                    case sMultipleValueBind: {
                        final Object vars = car(ccArguments);
                        int length;
                        final boolean varargs;
                        if (consp(vars)) {
                            varargs = dottedList(vars);
                            length = listLength((ConsCell)vars);
                            if (varargs) length--;
                        }
                        else if (symbolp(vars)) {
                            varargs = true;
                            length = 0;
                        }
                        else throw errorMalformedFmt(MULTIPLE_VALUE_BIND, "expected a list or a symbol but got %s", printSEx(vars));
                        sb.append(isLast ? "tailcall(" : "funcall(");
                        emitLambda(sb, cons(vars, cddr(ccArguments)), env, topEnv, rsfx, false);
                        if (cadr(ccArguments) != null) {
                            sb.append(", rt().new ValuesBuilder()\n        .add(");
                            emitForm(sb, cadr(ccArguments), env, topEnv, rsfx, false);
                            sb.append(")\n        .build(").append(length).append(',').append(String.valueOf(!varargs)).append(')');
                        }
                        else sb.append(", NOARGS");
                        sb.append(')');
                        return;
                    }

                    case sLoad: {
                        // pass1 has replaced all toplevel (load)s with the file contents
                        throw errorNotImplemented(LOAD + " as non-toplevel form is not implemented");
                    }

                    case sRequire: {
                        // pass1 has replaced all toplevel (require)s with the file contents
                        throw errorNotImplemented(REQUIRE + " as non-toplevel form is not implemented");
                    }

                    case sProvide: {
                        // pass 2 shouldn't see this
                        throw errorNotImplemented(PROVIDE + " as non-toplevel form is not implemented");
                    }

                    case sDeclaim: {
                        intp.evalDeclaim(rsfx, ccArguments);
                        sb.append("(Object)null");
                        return;
                    }

                    default:
                        /// * macro expansion - all macros were already expanded
                        if (null != symop.macro) errorNotAFunction("function application: not a primitive or " + LAMBDA + ": %s is a macro not a function", symop.toString());

                        /// * special case (hack) for calling macroexpand-1: only quoted forms are supported which can be performed a compile time
                        if (symbolEq(symop, "macroexpand-1")) {
                            oneArg("macroexpand-1", ccArguments);
                            if (!consp(car(ccArguments)) || !symbolEq(caar(ccArguments), QUOTE)) errorNotImplemented("general macroexpand-1 is not implemented, only quoted forms are: (macroexpand-1 '...");
                            final Object expandedForm, expanded;
                            final Object maybeMacroCall = car((ConsCell)cdar(ccArguments));
                            if (consp(maybeMacroCall)) { expandedForm = macroexpandImpl(intp, (ConsCell)maybeMacroCall); expanded = cadr(intp.values) == sT ? "rt()._t" : "null"; }
                            else { expandedForm = maybeMacroCall; expanded = "null"; }
                            sb.append("rt()._values(");  emitQuotedForm(sb, expandedForm, true);  sb.append(", ").append(expanded).append(')');
                            return;
                        }

                        /// * some functions and operators are opencoded:
                        if (intp.speed >= 1 && opencode(sb, symop, ccArguments, env, topEnv, rsfx, isLast)) return;
                    }
                }

                if (intp.speed >= 1 && consp(op) && symbolEq(car(op), JMETHOD)
                    && emitJmethod(sb, listOrMalformed(JMETHOD + " application", cdr(op)), env, topEnv, rsfx, true, ccArguments)) {
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
            }
            catch (ArithmeticException | ClassCastException | IndexOutOfBoundsException | LambdaJError e) {
                throw new LambdaJError(e, form);
            }
            catch (Exception e) {
                //e.printStackTrace();
                throw errorInternal(e, "emitForm: caught exception %s: %s", e.getClass().getName(), e.getMessage(), form); // convenient breakpoint for errors
            }
        }

        private void emitTruthiness(WrappingWriter sb, boolean negate, Object form, ConsCell env, ConsCell topEnv, int rsfx) {
            final String jTrue, jFalse, isNotNull, maybeBang;
            if (negate) { jTrue = "false";  jFalse = "true";   isNotNull = " == null";  maybeBang = "!"; }
            else        { jTrue = "true";   jFalse = "false";  isNotNull = " != null";  maybeBang = ""; }

            if (form == null || form == sNil) { sb.append(jFalse); return; }
            if (form == sT)                   { sb.append(jTrue); return; }
            if (symbolp(form))                { emitForm(sb, form, env, topEnv, rsfx, false); sb.append(isNotNull); return; }
            if (atom(form))                   { sb.append(jTrue); return; } // must be an atom other than nil, t or a symbol -> true. Todo note wg. constant condition?

            final ConsCell ccForm = (ConsCell)form;
            final ConsCell ccArgs = (ConsCell)cdr(ccForm);
            final WellknownSymbol ws = intp.speed >= 1 && symbolp(car(ccForm)) ? ((LambdaJSymbol)car(ccForm)).wellknownSymbol : null;

            if (ws == WellknownSymbol.sNull) {
                // optimize "(null ..."
                emitTruthiness(sb, !negate, car(ccArgs), env, topEnv, rsfx);
                return;
            }

            final boolean clr = !singleValueForm(form);

            if (clr) sb.append("clrValues(");

            if (ws == WellknownSymbol.sEq) { sb.append(maybeBang);  emitEq(sb, false, car(ccArgs), cadr(ccArgs), env, topEnv, rsfx); }
            else if (ws == WellknownSymbol.sLt  && emitBinOp(sb, false, negate ? ">=" : "<",  ccArgs, env, topEnv, rsfx)) { /* emitBinOp did all as a sideeffect */ }
            else if (ws == WellknownSymbol.sNe  && emitBinOp(sb, false, negate ? "==" : "!=", ccArgs, env, topEnv, rsfx)) { /* emitBinOp did all as a sideeffect */ }
            else if (ws == WellknownSymbol.sLe  && emitBinOp(sb, false, negate ? ">"  : "<=", ccArgs, env, topEnv, rsfx)) { /* emitBinOp did all as a sideeffect */ }
            else if (ws == WellknownSymbol.sNeq && emitBinOp(sb, false, negate ? "!=" : "==", ccArgs, env, topEnv, rsfx)) { /* emitBinOp did all as a sideeffect */ }
            else if (ws == WellknownSymbol.sGe  && emitBinOp(sb, false, negate ? "<"  : ">=", ccArgs, env, topEnv, rsfx)) { /* emitBinOp did all as a sideeffect */ }
            else if (ws == WellknownSymbol.sGt  && emitBinOp(sb, false, negate ? "<=" : ">",  ccArgs, env, topEnv, rsfx)) { /* emitBinOp did all as a sideeffect */ }
            else if (ws == WellknownSymbol.sIf) {
                sb.append('(');
                emitTruthiness(sb, negate, car(ccArgs), env, topEnv, rsfx);
                sb.append(" ? ");
                emitTruthiness(sb, negate, cadr(ccArgs), env, topEnv, rsfx);
                sb.append(" : ");
                emitTruthiness(sb, negate, caddr(ccArgs), env, topEnv, rsfx);
                sb.append(')');
            }
            else { sb.append('('); emitForm(sb, ccForm, env, topEnv, rsfx, false); sb.append(")").append(isNotNull); }

            if (clr) sb.append(")");
        }

        /** return true if form won't set multiple values, false if form may set multiple values */
        private boolean singleValueForm(Object form) {
            if (atom(form)) return true;
            final ConsCell ccForm = (ConsCell)form;
            if (symbolEq(car(ccForm), "quote")) return true;
            final ConsCell ccArgs = (ConsCell)cdr(ccForm);

            final Object lhs = car(ccArgs), rhs = cadr(ccArgs);
            final WellknownSymbol ws = intp.speed >= 1 && symbolp(car(ccForm)) ? ((LambdaJSymbol)car(ccForm)).wellknownSymbol : null;

            if (ws != null && ws.singleValues)
                return true;
            if (cdr(ccArgs) != null && cddr(ccArgs) == null && singleValueForm(lhs) && singleValueForm(rhs)) {
                // exactly two args that are both atoms or quoted forms
                if (ws == WellknownSymbol.sEq || ws == WellknownSymbol.sLt || ws == WellknownSymbol.sNe || ws == WellknownSymbol.sLe || ws == WellknownSymbol.sNeq || ws == WellknownSymbol.sGe || ws == WellknownSymbol.sGt) {
                    return true;
                }
            }
            return ws == WellknownSymbol.sIf && singleValueForm(rhs) && singleValueForm(caddr(ccArgs));
        }

        /** write atoms that are not symbols (and "nil" is acceptable, too) */
        private void emitAtom(WrappingWriter sb, Object form) {
            if (form == null || form == sNil) sb.append("(Object)null");
            else if (form instanceof Integer) sb.append(Integer.toString((Integer) form));
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
            else if (hashtablep(form)) emitHashLiteral(sb, form);
            else errorInternal("emitAtom: atom %s is not implemented", form.toString());
        }

        private static void stringToJava(WrappingWriter sb, CharSequence s, int maxlen) {
            if (s == null)       { sb.append("null"); return; }
            if (s.length() == 0) { sb.append(""); return; }

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
            }
            else {
                sb.append('(');
                boolean first = true;
                for (final Iterator<Object> iterator = condForm.iterator(); iterator.hasNext(); ) {
                    final Object clause = iterator.next();
                    if (first) first = false;
                    else sb.append("\n        : ");
                    final Object condExpr = car(clause), condForms = cdr(clause);
                    if (condExpr == sT) {
                        emitProgn(sb, condForms, env, topEnv, rsfx, isLast);  sb.append(')');
                        if (iterator.hasNext()) note(condForm, "forms following default 't' form will be ignored");
                        return;
                    }
                    else {
                        emitTruthiness(sb, false, condExpr, env, topEnv, rsfx);
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
            env = params(LAMBDA, sb, params, env, rsfx, expr, argCheck);
            emitStmts(sb, (ConsCell)cdr(paramsAndForms), env, topEnv, rsfx, "        return ", false, false);
            sb.append("        })");
        }

        private int ignoredCounter = 0;

        /** emit a list of forms as a single Java expression */
        private void emitProgn(WrappingWriter sb, Object forms, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (!listp(forms)) errorMalformed(PROGN, "a list of forms", forms);
            final ConsCell ccForms = (ConsCell)forms;
            if (cdr(ccForms) == null) emitForm(sb, car(ccForms), env, topEnv, rsfx, isLast);
            else if (USE_SWITCH_EXPR) {
                sb.append("switch (0) {\n        default: {\n");
                emitStmts(sb, ccForms, env, topEnv, rsfx, "        yield ", !isLast, false);
                sb.append("        } }");
            }
            else {
                sb.append(isLast ? "tailcall(" : "funcall(").append("(MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n");
                emitStmts(sb, ccForms, env, topEnv, rsfx, "        return ", false, false);
                sb.append("        }, (Object[])null)");
            }
        }

        private void emitCatch(WrappingWriter sb, ConsCell tagAndForms, ConsCell env, ConsCell topEnv, int rsfx) {
            final Object tag = car(tagAndForms);
            final ConsCell bodyForms = (ConsCell)cdr(tagAndForms);
            if (USE_SWITCH_EXPR) {
                sb.append("switch (0) {\n        default: {\n        try {\n");
                emitStmts(sb, bodyForms, env, topEnv, rsfx, "        yield ", true, false);
                sb.append("        }\n        catch (Exception e) {\n        yield catchHelper(");
                emitForm(sb, tag, env, topEnv, rsfx, false);
                sb.append(", e);\n        } } }");
            }
            else {
                final ConsCell body = cons(sLambda, cons(null, bodyForms));
                final ConsCell args = cons(tag, cons(body, null));
                emitCallPrimitive(sb, "doCatch", args, env, topEnv, rsfx);
            }
        }

        private void emitThrow(WrappingWriter sb, ConsCell tagAndResultForm, ConsCell env, ConsCell topEnv, int rsfx) {
            emitCallPrimitive(sb, "doThrow", tagAndResultForm, env, topEnv, rsfx);
        }

        private void emitTry(WrappingWriter sb, ConsCell formAndErrorobj, ConsCell env, ConsCell topEnv, int rsfx) {
            final Object protectedForm = car(formAndErrorobj);
            final Object errorObj = cadr(formAndErrorobj);

            sb.append("doTry((MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n");
            if (consp(protectedForm)) emitLoc(sb, (ConsCell)protectedForm, 100);
            sb.append("        return ");
            emitForm(sb, protectedForm, env, topEnv, rsfx, false);
            sb.append(";\n        },\n        ");
            emitForm(sb, errorObj, env, topEnv, rsfx, false);
            sb.append(')');
        }

        private void emitUnwindProtect(WrappingWriter sb, ConsCell ccForms, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final Object protectedForm = car(ccForms);
            final ConsCell cleanupForms = listOrMalformed(UNWIND_PROTECT, cdr(ccForms));
            if (isLast) {
                sb.append("tailcallWithCleanup(").append("(MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> { return ");
                emitForm(sb, protectedForm, env, topEnv, rsfx, false);
                sb.append("; },\n"
                        + "        (MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n");
                emitStmts(sb, cleanupForms, env, topEnv, rsfx, "        return ", false, false);
                sb.append("        },\n"
                        + "        (Object[])null)");
            }
            else {
                if (USE_SWITCH_EXPR) sb.append("switch (0) {\n        default: {\n        try { yield ");
                else sb.append("funcall(").append("(MurmelFunction)(Object... ignoredArg").append(ignoredCounter++).append(") -> {\n        try { return ");

                emitForm(sb, protectedForm, env, topEnv, rsfx, true);
                sb.append("; }\n"
                        + "        finally {\n");
                final String tmp = "tmp" + rsfx;
                sb.append("        Object ").append(tmp).append(";\n");
                emitStmts(sb, cleanupForms, env, topEnv, rsfx, "        " + tmp + " = ", false, true);
                sb.append("        }\n");

                if (USE_SWITCH_EXPR) sb.append("        } }");
                else sb.append("        }, (Object[])null)");
            }
        }

        private String emitSetq(WrappingWriter sb, Object pairs, ConsCell env, ConsCell topEnv, int rsfx) {
            final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed(SETQ, car(pairs));
            final String javaName = javasym(symbol, env, (ConsCell)pairs);

            if (cdr(pairs) == null) errorMalformed(SETQ, "odd number of arguments");
            final Object valueForm = cadr(pairs);

            notAPrimitive(SETQ, symbol, javaName);

            String clrValues = "", closingParen = "";
            if (cddr(pairs) == null) {
                if (consp(valueForm)) {
                    final Object valueOp = car((ConsCell)valueForm);
                    if (valueOp instanceof LambdaJSymbol) {
                        if (valueOp != intern(LAMBDA) && (valueOp == intern(VALUES) || needsClrValues((LambdaJSymbol)valueOp))) {
                            clrValues = "clrValues(";
                            closingParen = ")";
                        }
                    }
                }
                else {
                    clrValues = "clrValues(";
                    closingParen = ")";
                }
            }

            if (fastassq(symbol, env) == fastassq(symbol, topEnv)) {
                if (javaName.endsWith(".get()")) {
                    // either a userdefined global or a
                    final String symName = javaName.substring(0, javaName.length()-6);
                    sb.append(symName).append(".set(").append(clrValues); emitForm(sb, valueForm, env, topEnv, rsfx, false); sb.append(closingParen).append(")");
                }
                else {
                    // immutable runtime globals such as pi are implemented as regular Java class members (and not as objects of class CompilerGlobal)
                    errorMalformed(SETQ, "can't modify constant " + symbol);
                }
            }
            else {
                sb.append(javaName).append(" = ").append(clrValues);  emitForm(sb, valueForm, env, topEnv, rsfx, false); sb.append(closingParen);
            }
            return javaName;
        }

        private static boolean needsClrValues(LambdaJSymbol sym) {
            final WellknownSymbol ws = sym.wellknownSymbol;
            if (ws.stmtExpr
                //|| ws == WellknownSymbol.sLambda
                || ws == WellknownSymbol.sIf
                || ws == WellknownSymbol.sCond
                || ws == WellknownSymbol.sSetQ
                || ws == WellknownSymbol.sProgn
                || ws == WellknownSymbol.sCatch)
                return false;
            if (ws == WellknownSymbol.interned || ws == WellknownSymbol.notInterned) return false;

            return true;
        }

        /** args = (((symbol (sym...) form...)...) form...) */
        private void emitLabels(WrappingWriter sb, final ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (args == null) errorMalformed(LABELS, "expected at least one argument");

            final Object localFuncs = car(args);
            if (localFuncs == null || cddr(args) == null && atom(cadr(args))) {
                // no local functions or body is one single atom (the latter can't use the functions so skip them
                emitProgn(sb, cdr(args), env, topEnv, rsfx, isLast);
                return;
            }

            sb.append(isLast ? "tailcall(" : "funcall(");
            sb.append("new MurmelFunction() {\n");

            int ctr = 0;
            for (Object localFunc: paramList(LABELS, localFuncs, true)) {
                final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(LABELS, localFunc);
                final String javaName = "lf" + ctr++ + '_' + rsfx; // don't use the Murmel symbol name in case several local functions' names are gensymmed
                env = extenvIntern(sym, javaName, env);
            }

            for (Object symbolParamsAndBody: (ConsCell) localFuncs) {
                final ConsCell ccSymbolParamsAndBody = (ConsCell)symbolParamsAndBody;
                final LambdaJSymbol symbol = LambdaJ.symbolOrMalformed(Names.LABELS, car(ccSymbolParamsAndBody));
                sb.append("        private final MurmelFunction ").append(javasym(symbol, env, ccSymbolParamsAndBody)).append(" = ");
                emitNamedLambda(LABELS, sb, symbol, cadr(ccSymbolParamsAndBody), (ConsCell)cddr(ccSymbolParamsAndBody), env, topEnv, rsfx+1, false);
                sb.append(";\n");
            }

            sb.append("        public final Object apply(Object... ignored) {\n");
            emitStmts(sb, (ConsCell)cdr(args), env, topEnv, rsfx, "        return ", false, false);
            sb.append("        } }, NOARGS)");
        }

        /** let and named let */
        private void emitLet(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            final boolean named = car(args) instanceof LambdaJSymbol;
            final LambdaJSymbol loopLabel;
            final Object bindings;
            final ConsCell body;
            if (named) { loopLabel = (LambdaJSymbol)car(args); args = (ConsCell)cdr(args); }
            else       { loopLabel = null; }
            bindings = car(args);  body = (ConsCell)cdr(args);
            assert named || bindings != null : "let w/o bindings should have been replaced in expandForm";
            if (bindings == null && body == null) { sb.append("(Object)null"); return; }

            sb.append(isLast ? "tailcall(" : "funcall(");

            final String op = named ? "named " + LET : LET;
            final ConsCell ccBindings = (ConsCell)bindings;
            final ConsCell params = paramList(op, ccBindings, false);

            if (named) emitNamedLambda(op, sb, loopLabel, params, body, extenvIntern(loopLabel, mangleFunctionName(loopLabel.toString(), rsfx + 1), env), topEnv, rsfx + 1, true);
            else emitLambda(sb, cons(params, body), env, topEnv, rsfx + 1, false);

            if (ccBindings != null) {
                for (Object binding : ccBindings) {
                    sb.append("\n        , ");
                    emitForm(sb, cadr(binding), env, topEnv, rsfx, false);
                }
            }
            else sb.append(", NOARGS");
            sb.append(')');
        }

        /** let* and letrec
         *  args = ([name] ((symbol form)...) forms...) */
        private void emitLetStarLetrec(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean letrec, boolean isLast) {
            final boolean named = car(args) instanceof LambdaJSymbol;
            final LambdaJSymbol loopLabel;
            final Object bindings, body;
            if (named) { loopLabel = (LambdaJSymbol)car(args); args = (ConsCell)cdr(args); }
            else       { loopLabel = null; }
            bindings = car(args);  body = cdr(args);
            if (bindings == null && body == null) { sb.append("(Object)null"); return; }

            final String sfName = (named ? "named " : "") + (letrec ? LETREC : LETSTAR);

            sb.append(isLast ? "tailcall(" : "funcall(");

            if (named) {
                env = extenv(sfName, loopLabel, rsfx, env);
                sb.append("new MurmelFunction() {\n");
                sb.append("        private final Object ").append(javasym(loopLabel, env, null)).append(" = this;\n");
                sb.append("        public final Object apply(Object... args").append(rsfx).append(") {\n");
            }
            else {
                sb.append("(MurmelFunction)(args").append(rsfx).append(") -> { {\n");
            }

            if (!listp(bindings)) errorMalformed(sfName, "a list of bindings", bindings);
            final ConsCell ccBindings = (ConsCell)bindings;
            final int argCount = listLength(ccBindings);
            if (argCount != 0) {
                sb.append("        if (args").append(rsfx).append("[0] == UNASSIGNED_LOCAL) {\n");

                // letrec: ALL let-bindings are in the environment during binding of the initial values todo value should be undefined
                int current = 0;
                if (letrec) for (Object binding: ccBindings) {
                    final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(sfName, car(binding));
                    final String symName = "args" + rsfx + '[' + current++ + ']';
                    env = extenvIntern(sym, symName, env);
                }

                // initial assignments. let*: after the assignment add the let-symbol to the environment so that subsequent bindings will see it
                current = 0;
                for (Object binding: ccBindings) {
                    final LambdaJSymbol sym = LambdaJ.symbolOrMalformed(sfName, car(binding));
                    final Object form = cadr(binding);
                    final String symName = "args" + rsfx + '[' + current++ + ']';
                    sb.append("        { ").append(symName).append(" = ");
                    emitForm(sb, form, env, topEnv, rsfx, false);
                    if (!letrec) env = extenvIntern(sym, symName, env);
                    sb.append("; }\n");
                }

                sb.append("        }\n");
                sb.append("        else argCheck(loc, ").append(argCount).append(", args").append(rsfx).append(");\n");
            }
            if (named) sb.append("        ").append(javasym(loopLabel, env, null)).append(": while (true) {\n");
            emitStmts(sb, (ConsCell)body, env, topEnv, rsfx, "        return ", loopLabel, "args" + rsfx, argCount, argCount, false, false);
            if (named) sb.append("        }\n");
            sb.append("        } }, unassigned(").append(argCount).append("))");
        }

        /** let dynamic and let* dynamic */
        private void emitLetLetStarDynamic(WrappingWriter sb, final ConsCell bindingsAndForms, ConsCell env, ConsCell topEnv, int rsfx, boolean letStar, boolean isLast) {
            final Object bindings = car(bindingsAndForms);
            if (bindings == null && cdr(bindingsAndForms) == null) { sb.append("(Object)null"); return; }

            sb.append(isLast ? "tailcallWithCleanup(" : "funcall(").append("(MurmelFunction)(args").append(rsfx).append(" -> {\n");

            final ArrayList<String> globals = new ArrayList<>();

            ConsCell _env = env;
            if (bindings != null) {
                final ConsCell params = paramList(letStar ? ("let* " + DYNAMIC) : ("let " + DYNAMIC), bindings, false);
                if (letStar) {
                    int n = 0;
                    final HashSet<Object> seenSymbols = new HashSet<>();
                    final Iterator<Object> bi = ((ConsCell)bindings).iterator();
                    for (final Object sym: params) {
                        final boolean seen = !seenSymbols.add(sym);
                        final ConsCell maybeGlobal = fastassq(sym, topEnv);
                        if (maybeGlobal != null) {
                            final String javaName = cdr(maybeGlobal).toString();
                            notAPrimitive("let* " + DYNAMIC, sym, javaName);
                            if (!javaName.endsWith(".get()")) errorMalformed("let* " + DYNAMIC, "cannot modify constant " + car(maybeGlobal));
                            final String globalName = javaName.substring(0, javaName.length()-6);
                            if (!seen) {
                                globals.add(globalName);
                                sb.append("        ").append(globalName).append(".push();\n");
                            }
                            sb.append("        ").append(globalName).append(".set(");
                            emitForm(sb, cadr(bi.next()), _env, topEnv, rsfx, false);
                            sb.append(");\n");
                        }
                        else { // letXX dynamic can bind both global as well as new local variables
                            final String javaName;
                            if (seen) javaName = javasym(sym, _env, null);
                            else javaName = "args" + rsfx + "[" + n + "]";
                            sb.append("        ").append(javaName).append(" = ");
                            emitForm(sb, cadr(bi.next()), _env, topEnv, rsfx, false);
                            sb.append(";\n");
                            if (!seen) _env = extenvIntern((LambdaJSymbol)sym, javaName, _env);
                        }
                        n++;
                    }
                }
                else {
                    final ConsCell __env = params("let " + DYNAMIC, sb, params, _env, rsfx, null, false);
                    int n = 0;
                    for (final Object sym: params) {
                        final ConsCell maybeGlobal = fastassq(sym, topEnv);
                        if (maybeGlobal != null) {
                            final String javaName = cdr(maybeGlobal).toString();
                            notAPrimitive("let " + DYNAMIC, sym, javaName);
                            if (!javaName.endsWith(".get()")) errorMalformed("let " + DYNAMIC, "cannot modify constant " + car(maybeGlobal));
                            final String globalName = javaName.substring(0, javaName.length()-6);
                            globals.add(globalName);
                            sb.append("        ").append(globalName).append(".push(").append(javasym(sym, __env, null)).append(");\n");
                        }
                        else {
                            _env = extenvIntern((LambdaJSymbol)sym, "args" + rsfx + "[" + n + "]", _env);
                        }
                        n++;
                    }
                }
            }

            if (isLast) {
                emitStmts(sb, (ConsCell)cdr(bindingsAndForms), _env, topEnv, rsfx, "        return ", false, false);
                sb.append("        })\n");
                if (globals.isEmpty()) {
                    sb.append("        , null");
                }
                else {
                    sb.append("        , (MurmelFunction)(args").append(rsfx).append(" -> {\n");
                    for (String globalName : globals) sb.append("        ").append(globalName).append(".pop();\n");
                    sb.append("        return null;\n        })");
                }
            }
            else {
                if (!globals.isEmpty()) sb.append("        try {\n");

                // set parameter "toplevel" to true to avoid TCO. TCO would effectively disable the finally clause
                emitStmts(sb, (ConsCell)cdr(bindingsAndForms), _env, topEnv, rsfx, "        return ", bindings != null, false);

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
        private static ConsCell params(String func, WrappingWriter sb, Object paramList, ConsCell env, int rsfx, String expr, boolean check) {
            if (paramList == null) {
                if (check) sb.append("        argCheck(\"").append(expr).append("\", 0, args").append(rsfx).append(");\n");
                return env;
            }

            if (symbolp(paramList)) {
                // (lambda a forms...) - style varargs
            }
            else if (dottedList(paramList)) {
                if (check) sb.append("        argCheckVarargs(\"").append(expr).append("\", ").append(listLength((ConsCell)paramList)).append(", args").append(rsfx).append(");\n");
            }
            else if (check) sb.append("        argCheck(\"").append(expr).append("\", ").append(listLength((ConsCell)paramList)).append(", args").append(rsfx).append(");\n");

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
                    final String javaName = "varargs" + rsfx;
                    env = extenvIntern((LambdaJSymbol)params, javaName + "[0]", env);
                    sb.append("        final Object[] ").append(javaName).append(" = new Object[] { arrayToList(args").append(rsfx).append(", ").append(n).append(") };\n");
                    return env;
                }

                else errorMalformed(func, "a symbol or a list of symbols", params);

                params = cdr(params);
            }
            return env;
        }

        private ConsCell loadFile(String func, WrappingWriter sb, Object argument, ConsCell topEnv, List<Object> bodyForms, StringBuilder globals) {
            assert !passTwo;
            final LambdaJ intp = this.intp;
            final Path prev = intp.currentSource;
            final Path p = intp.findFile(func, argument);
            intp.currentSource = p;
            try {
                final SExpressionReader parser = intp.makeReader(ReadSupplier.of(p), p);
                final Object eof = "EOF";
                for (;;) {
                    final Object form = parser.readObj(true, eof);
                    if (form == eof) return topEnv;
                    topEnv = toplevelFormToJava(sb, bodyForms, globals, topEnv, intp.expandForm(form));
                }
            }
            catch (IOException e) {
                throw wrap(new ReaderError(LOAD + ": error reading file '%s': ", e.getMessage()));
            }
            finally {
                intp.currentSource = prev;
            }
        }

        private static boolean dottedList(Object _l) {
            Object l = _l;
            for (;;) {
                if (l == null) return false;
                if (atom(l)) return true;
                l = cdr(l);
                if (l == _l) throw new ProgramError("circular list detected");
            }
        }

        /** opencode some primitives, avoid trampoline for other primitives and avoid some argcount checks */
        private boolean opencode(WrappingWriter sb, LambdaJSymbol op, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean isLast) {
            if (op == null) return false;

            if (op == sApply) {
                final Object applyOp = car(args);
                final Object applyArg = cadr(args);

                if (applyOp == null || applyOp == sNil) throw new UndefinedFunction("function application: not a primitive or " + LAMBDA + ": " + NIL);
                if (applyOp == sList) { sb.append("requireList("); emitForm(sb, applyArg, env, topEnv, rsfx, false); sb.append(')'); return true; }

                if (applyOp != sApply) { // apply needs special treatment for TCO
                    for (String prim: primitives)          if (symbolEq(applyOp, prim))    { opencodeApplyHelper(sb, "_" + prim,  applyArg, env, topEnv, rsfx);  return true; }
                    for (String[] prim: aliasedPrimitives) if (symbolEq(applyOp, prim[0])) { opencodeApplyHelper(sb, prim[1],  applyArg, env, topEnv, rsfx);  return true; }
                }

                sb.append(isLast ? "tailcall" : "funcall").append("((MurmelFunction)rt()::apply, ");
                emitForm(sb, applyOp, env, topEnv, rsfx, false);  sb.append(", ");
                emitForm(sb, applyArg, env, topEnv, rsfx, false);
                sb.append(')');
                return true;
            }

            final WellknownSymbol prim = op.wellknownSymbol;

            switch (prim) {
            case sAdd: assert !prim.stmtExpr; emitAddDbl(sb, "+", 0.0, args, env, topEnv, rsfx); return true;
            case sMul: assert !prim.stmtExpr; emitAddDbl(sb, "*", 1.0, args, env, topEnv, rsfx); return true;
            case sSub: assert !prim.stmtExpr; emitSubDbl(sb, "-", 0.0, args, env, topEnv, rsfx); return true;
            case sDiv: assert !prim.stmtExpr; emitSubDbl(sb, "/", 1.0, args, env, topEnv, rsfx); return true;
            case sMod: assert !prim.stmtExpr;
                sb.append("cl_mod(");
                emitFormAsDouble(sb, "mod", car(args), env, topEnv, rsfx);  sb.append(", ");  emitFormAsDouble(sb, "mod", cadr(args), env, topEnv, rsfx);
                sb.append(')');
                return true;
            case sRem:
                assert !prim.stmtExpr;
                sb.append('(');
                emitFormAsDouble(sb, "rem", car(args), env, topEnv, rsfx);  sb.append(" % ");  emitFormAsDouble(sb, "rem", cadr(args), env, topEnv, rsfx);
                sb.append(')');
                return true;
            case sRound:     assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "round", "cl_round", true);  return true;
            case sFloor:     assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "floor", "Math.floor", true);  return true;
            case sCeiling:   assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "ceiling", "Math.ceil", true);  return true;
            case sTruncate:  assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "truncate", "cl_truncate", true);  return true;

            case sFRound:    assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "fround", "cl_round", false); return true;
            case sFFloor:    assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "ffloor", "Math.floor", false); return true;
            case sFCeiling:  assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "fceiling", "Math.ceil", false); return true;
            case sFTruncate: assert !prim.stmtExpr;  emitDivision(sb, args, env, topEnv, rsfx, "ftruncate", "cl_truncate", false); return true;

            case sNeq:  assert !prim.stmtExpr;  if (emitBinOp(sb, true, "==", args, env, topEnv, rsfx)) return true; break;
            case sNe:   assert !prim.stmtExpr;  if (emitBinOp(sb, true, "!=", args, env, topEnv, rsfx)) return true; break;
            case sLt:   assert !prim.stmtExpr;  if (emitBinOp(sb, true, "<", args, env, topEnv, rsfx)) return true; break;
            case sLe:   assert !prim.stmtExpr;  if (emitBinOp(sb, true, "<=", args, env, topEnv, rsfx)) return true; break;
            case sGe:   assert !prim.stmtExpr;  if (emitBinOp(sb, true, ">=", args, env, topEnv, rsfx)) return true; break;
            case sGt:   assert !prim.stmtExpr;  if (emitBinOp(sb, true, ">", args, env, topEnv, rsfx)) return true; break;
            case sEq:   assert !prim.stmtExpr;  emitEq(sb, true, car(args), cadr(args), env, topEnv, rsfx); return true;
            case sNull: assert !prim.stmtExpr;  emitEq(sb, true, car(args), null, env, topEnv, rsfx); return true;
            case sAppend:
                assert !prim.stmtExpr;
                if (args == null) { // no args
                    sb.append("(Object)null");  return true;
                }
                if (cdr(args) == null) { emitForm(sb, car(args), env, topEnv, rsfx, false); return true; }
                break;
            case sList:
                if (args == null) { sb.append("clrValues(null)");  return true; }
                if (cdr(args) == null) { // one arg
                    sb.append("_cons(");  emitForm(sb, car(args), env, topEnv, rsfx, false);  sb.append(", null)");  return true;
                }
                break;
            case sListStar:
                assert !prim.stmtExpr;
                if (cdr(args) == null) { emitForm(sb, car(args), env, topEnv, rsfx, false); return true; }
                if (cddr(args) == null) {
                    sb.append("_cons("); emitForm(sb, car(args), env, topEnv, rsfx, false); sb.append(", "); emitForm(sb, cadr(args), env, topEnv, rsfx, false); sb.append(')'); return true;
                }
                emitCallPrimitive(sb, "listStar0", args, env, topEnv, rsfx);
                return true;
            case sJmethod:
                assert !prim.stmtExpr;
                if (emitJmethod(sb, args, null, null, -1, false, null)) return true;
                emitCallPrimitive(sb, "findMethod", args, env, topEnv, rsfx);
                return true;
            case sError:
                switch (listLength(args)) {
                case 1:  emitCallPrimitive(sb, "error1", args, env, topEnv, rsfx); return true;
                case 2:  emitCallPrimitive(sb, "error2", args, env, topEnv, rsfx); return true;
                case 3:  emitCallPrimitive(sb, "error3", args, env, topEnv, rsfx); return true;
                case 4:  emitCallPrimitive(sb, "error4", args, env, topEnv, rsfx); return true;
                default: emitCallPrimitive(sb, "errorN", args, env, topEnv, rsfx); return true;
                }
            default:
                break;
            }

            for (String primitive: primitives)          if (symbolEq(op, primitive))    { emitCallPrimitive(sb, "_" + primitive, args, env, topEnv, rsfx);  return true; }
            for (String[] primitive: aliasedPrimitives) if (symbolEq(op, primitive[0])) { emitCallPrimitive(sb, primitive[1], args, env, topEnv, rsfx);  return true; }

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
            checkNonNumber(murmel, car(args));
            if (asLong) sb.append("toFixnum(");
            sb.append(javaOp).append('(');
            if (cdr(args) == null) {
                emitFormAsDouble(sb, murmel, car(args), env, topEnv, rsfx);
            }
            else {
                checkNonNumber(murmel, cadr(args));
                emitFormAsDouble(sb, murmel, car(args), env, topEnv, rsfx);
                sb.append(" / ");
                emitFormAsDouble(sb, murmel, cadr(args), env, topEnv, rsfx);
            }
            sb.append(')');
            if (asLong) sb.append(')');
        }

        /** emit "==" operator */
        private void emitEq(WrappingWriter sb, boolean generalizedBoolean, Object lhs, Object rhs, ConsCell env, ConsCell topEnv, int rsfx) {
            if (generalizedBoolean) sb.append("(");
            sb.append("((Object)(");
            emitForm(sb, lhs, env, topEnv, rsfx, false);
            sb.append(") == (Object)(");
            if (rhs == null) sb.append(NULL); else emitForm(sb, rhs, env, topEnv, rsfx, false);
            sb.append("))");
            if (generalizedBoolean) sb.append(" ? _t : null)");
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
            sb.append('(');
            if (cdr(args) == null) { sb.append(start).append(' ').append(op).append(' '); emitFormAsDouble(sb, op, car(args), env, topEnv, rsfx); }
            else {
                emitFormAsDouble(sb, op, car(args), env, topEnv, rsfx);
                for (Object arg: (ConsCell)cdr(args)) { sb.append(' ').append(op).append(' '); emitFormAsDouble(sb, op, arg, env, topEnv, rsfx); }
            }
            sb.append(')');
        }

        /** emit a call to the primitive {@code func} without going through the trampoline,
         *  if {@code wrapper} is non-null then it will be applied to each function argument  */
        private void emitCallPrimitive(WrappingWriter sb, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            sb.append(func).append('(');
            if (args != null) {
                emitForm(sb, car(args), env, topEnv, rsfx, false);
                if (cdr(args) != null) for (Object arg: (ConsCell)cdr(args)) {
                    sb.append(", ");
                    emitForm(sb, arg, env, topEnv, rsfx, false);
                }
            }
            else sb.append("NOARGS");
            sb.append(')');
        }

        /** if args has two arguments then emit a binary operator (double, double) -> boolean */
        private boolean emitBinOp(WrappingWriter sb, boolean generalizedBoolean, String func, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx) {
            if (cdr(args) == null || cddr(args) != null) return false;
            if (generalizedBoolean) sb.append('(');
            emitFormAsDouble(sb, func, car(args), env, topEnv, rsfx);
            sb.append(' ').append(func).append(' ');
            emitFormAsDouble(sb, func, cadr(args), env, topEnv, rsfx);
            if (generalizedBoolean) sb.append(" ? _t : null)");
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
            if (form == null || form instanceof Character || vectorp(form) || consp(form) && symbolEq(car(form), QUOTE)) errorNotANumber(func, form);
        }

        /** argCount is number of arguments at compiletime if known or -1 for check at runtime */
        private boolean emitJmethod(WrappingWriter sb, ConsCell args, ConsCell env, ConsCell topEnv, int rsfx, boolean emitCall, ConsCell ccArguments) {
            varargsMin(JMETHOD, args, 2);
            final Object strClazz = car(args), strMethod = cadr(args);
            // if class and method are stringliterals (i.e. java.lang.String objects) then we can do this at compiletime.
            // else jmethod() will check the runtime type at runtime
            if (!(strClazz instanceof String) || !(strMethod instanceof String)) return false;

            final Class<?> clazz;
            final String convReceiver;
            final Object[] clazzDesc = JFFI.classByName.get(strClazz);
            if (clazzDesc == null) {
                try {
                    clazz = Class.forName((String)strClazz);
                    convReceiver = clazz.getCanonicalName() + ".class.cast";
                }
                catch (ClassNotFoundException e) {
                    note(args, "using reflection at runtime");
                    return false;
                }
            }
            else {
                clazz = (Class<?>)clazzDesc[0];
                convReceiver = (String)clazzDesc[1];
            }

            // all parameter classes (if any) must be one of the classes that we know how to do Murmel->Java conversion else "return false"
            final ArrayList<Class<?>> paramTypes = new ArrayList<>();
            final ArrayList<String> paramTypeNames = new ArrayList<>();
            if (cddr(args) != null) for (Object arg: (ConsCell)cddr(args)) {
                final String paramType = (String)arg;
                paramTypeNames.add(paramType);

                final Object[] typeDesc = JFFI.classByName.get(paramType);
                if (typeDesc == null) { note(args, "using reflection at runtime"); return false; }
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
            catch (Exception e) { throw new LambdaJError(true, JMETHOD + ": exception finding method: %s", e.getMessage()); }

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
                        if (convReceiver == null) sb.append("(Object)((").append(strClazz).append(')');
                        else                      sb.append("(Object)").append(convReceiver).append('(');
                        emitForm(sb, car(ccArguments), env, topEnv, rsfx, false);
                        sb.append(").").append(strMethod);
                        ccArguments = listOrMalformed((String)strMethod, cdr(ccArguments));
                    }
                }

                sb.append('(');
                boolean first = true;
                if (ccArguments != null) {
                    int i = startArg;
                    String conv = null;
                    for (Object arg : ccArguments) {
                        if (first) first = false;
                        else sb.append("\n        , ");
                        if (!m.isVarArgs() || i - startArg < paramTypeNames.size()) conv = (String) JFFI.classByName.get(paramTypeNames.get(i-startArg))[1];
                        if (conv == null) emitForm(sb, arg, env, topEnv, rsfx, false);
                        else { sb.append(conv).append('(');  emitForm(sb, arg, env, topEnv, rsfx, false);  sb.append(')'); }
                        i++;
                    }
                }
                sb.append(')');
                if (voidMethod) sb.append("; return null; })).get()");
            }
            else {
                // emit a lambda that contains an argcount check
                sb.append("((MurmelFunction)(args -> { "); // (MurmelJavaProgram.CompilerPrimitive) works too but is half as fast?!?
                if (m.isVarArgs()) { sb.append("argCheckVarargs(loc, ").append(paramCount-1).append(", args);  ");}
                else               { sb.append("argCheck(loc, ").append(paramCount).append(", args);  "); }
                if (!voidMethod) sb.append("return ");

                if ("new".equalsIgnoreCase((String) strMethod)) sb.append("new ").append(strClazz);
                else if (Modifier.isStatic(m.getModifiers())) sb.append(strClazz).append('.').append(strMethod);
                else {
                    final Object[] desc = JFFI.classByName.get(strClazz);
                    if (desc != null && desc[1] != null) sb.append(desc[1]).append("(args[0]").append(").").append(strMethod);
                    else sb.append("((").append(strClazz).append(')').append("args[0]").append(").").append(strMethod);
                }

                sb.append('(');
                if (params != null) {
                    boolean first = true;
                    if (m.isVarArgs()) {
                        for (int i = startArg; i < params.length + startArg - 1; i++) {
                            if (first) first = false;
                            else sb.append("\n        , ");
                            final Object[] desc = JFFI.classByName.get(paramTypeNames.get(i - startArg));
                            if (desc == null) sb.append("args[").append(i).append(']');
                            else sb.append(desc[1]).append("(args[").append(i).append("])");
                        }

                        // handle last parameter which is vararg: pass an array of the appropriate type with the remaining args
                        final Object[] desc = JFFI.classByName.get(paramTypeNames.get(params.length-1));
                        final int varargPos = params.length + startArg - 1;
                        final String conv = "(java.util.function.UnaryOperator<Object>)(MurmelJavaProgram::" + desc[1] + ')';
                        sb.append("\n        , toVarargs(args, ").append(String.valueOf(varargPos))
                          .append(", ").append(conv)
                          .append(", new ").append(((Class<?>)desc[0]).getComponentType().getCanonicalName()).append("[args.length - ").append(String.valueOf(varargPos)).append("])");
                    }
                    else {
                        for (int i = startArg; i < params.length + startArg; i++) {
                            if (first) first = false;
                            else sb.append("\n        , ");
                            final String conv = (String)JFFI.classByName.get(paramTypeNames.get(i - startArg))[1];
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
            else errorInternal("emitVectorLiteral: " + VECTOR + " type %s is not implemented", form.toString());
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

        private void emitHashLiteral(WrappingWriter sb, Object form) {
            final StringWriter b = new StringWriter();
            final WrappingWriter qsb = new WrappingWriter(b);

            qsb.append("hash((ConsCell)new ListBuilder()\n        .append(");
            if (form instanceof EqlMap)               qsb.append("intern(\"eql\")");
            else if (form instanceof EqlTreeMap)      qsb.append("intern(\"compare-eql\")");
            else if (form instanceof EqualMap)        qsb.append("intern(\"equal\")");
            else if (form instanceof EqualTreeMap)    qsb.append("intern(\"compare-equal\")");
            else if (form instanceof IdentityHashMap) qsb.append("intern(\"eq\")");
            else if (form instanceof HashMap)         qsb.append("_t");
            else errorInternal("emitHashLiteral: hash-table type %s is not implemented", form.toString());
            qsb.append(')');
            if (form instanceof MurmelMap) {
                final MurmelMap map = (MurmelMap)form;
                for (Map.Entry<?,?> entry: map.entrySet()) {
                    qsb.append("\n        .append(");  emitQuotedForm(qsb, map.getKey(entry), true);  qsb.append(')');
                    qsb.append("\n        .append(");  emitQuotedForm(qsb, entry.getValue(), true);  qsb.append(')');
                }
            } else for (Map.Entry<?,?> entry: ((Map<?,?>)form).entrySet()) {
                qsb.append("\n        .append(");  emitQuotedForm(qsb, entry.getKey(), true);  qsb.append(')');
                qsb.append("\n        .append(");  emitQuotedForm(qsb, entry.getValue(), true);  qsb.append(')');
            }
            qsb.append(".first())");
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
            else if (form == sT) sb.append("_t");

            else if (symbolp(form)) {
                final LambdaJSymbol sym = (LambdaJSymbol)form;
                if (sym.wellknownSymbol == WellknownSymbol.notInterned) {
                    emitGensym(sb, sym);
                }
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
                    qsb.append("_cons("); emitQuotedForm(qsb, car(form), false);
                    qsb.append(", ");     emitQuotedForm(qsb, cdr(form), false);
                    qsb.append(')');
                }
                else if (atom(cddr(form))) {
                    // fast path for 2 element lists or dotted 3 element lists
                    qsb.append("_cons(");   emitQuotedForm(qsb, car(form),  false);
                    qsb.append(", _cons("); emitQuotedForm(qsb, cadr(form), false);
                    qsb.append(", ");       emitQuotedForm(qsb, cddr(form), false);
                    qsb.append("))");
                }
                else {
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

            else throw errorInternal(QUOTE + ": unexpected form", form);
        }

        private final Map<LambdaJSymbol, String> gensyms = new IdentityHashMap<>();

        private void emitGensym(WrappingWriter sb, LambdaJSymbol sym) {
            String ref = gensyms.get(sym);
            if (ref == null) {
                ref = createReference("_gensym(\"" + escapeString(sym.toString()) + "\")");
                gensyms.put(sym, ref);
            }
            sb.append(ref);
        }

        private int qCounter;
        private final List<String> quotedForms = new ArrayList<>();

        /** emit a reference to an existing identical constant in the constant pool
         *  or add a new one to the pool and emit a reference to that */
        private void emitReference(WrappingWriter sb, String s) {
            final int prev = quotedForms.indexOf(s);
            if (prev == -1) sb.append(createReference(s));
            else sb.append("q").append(prev);
        }

        private String createReference(String s) {
            final String ret = "q" + qCounter++;
            quotedForms.add(s);
            return ret;
        }

        private void emitConstantPool(WrappingWriter ret) {
            int ctr = 0;
            for (String quotedForm: quotedForms) {
                ret.append("    public final Object q").append(ctr).append(" = ").append(quotedForm).append(";\n");
                ctr++;
            }
        }


        private static ConsCell cons(Object car, Object cdr) {
            return ConsCell.cons(car, cdr);
        }
    }



    @SuppressWarnings("unused")
    public static final class JFRHelper {

        private JFRHelper() {}

        @jdk.jfr.Relational
        @Target({ ElementType.FIELD })
        @Retention(RetentionPolicy.RUNTIME)
        @interface ParentId {}

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

            @SuppressWarnings("CopyConstructorMissesField")
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
            ret.strArgs = LambdaJ.printSEx(args, false).toString();
            ret.begin();
            return ret;
        }

        public static Object endFunction(JFRFunctionCall call, Object ret) {
            call.end();
            if (!call.shouldCommit()) return ret;

            final String strRet = LambdaJ.printSEx(ret, false).toString();
            call.info = LambdaJ.printSEx(ConsCell.cons(call.name, call.args), false) + " -> " + strRet;
            call.ret = strRet;
            call.commit();
            return ret;
        }
    }

    // Null and NotNull are copied from jakarta.validation-api.jar (and somewhat stripped) in order to avoid this dependency so that "java LambdaJ.java" will work
    @Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
    @Retention(RetentionPolicy.SOURCE)
    @Repeatable(NotNull.List.class)
    @Documented
    public @interface NotNull {

        /**
         * Defines several {@link NotNull} annotations on the same element.
         */
        @Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
        @Retention(RetentionPolicy.SOURCE)
        @Documented
        @interface List {

            NotNull[] value();
        }
    }

    @Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
    @Retention(RetentionPolicy.SOURCE)
    @Repeatable(Null.List.class)
    @Documented
    public @interface Null {

        /**
         * Defines several {@link Null} annotations on the same element.
         */
        @Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
        @Retention(RetentionPolicy.SOURCE)
        @Documented
        @interface List {

            Null[] value();
        }
    }
}


/** a utility class with things that Java should support out of the box */
final class JavaUtil {
    static final float DEFAULT_LOAD_FACTOR = 0.75f;

    private JavaUtil() {}

    // from Java 20 HashMap#calculateHashMapCapacity()
    static int hashMapCapacity(int numMappings) {
        return (int)Math.ceil(numMappings / (double)DEFAULT_LOAD_FACTOR);
    }

    static <K, V> HashMap<K, V> newHashMap(int numMappings) {
        return new HashMap<>(hashMapCapacity(numMappings), DEFAULT_LOAD_FACTOR);
    }

    // Java 11 has CharSequence#compare
    static int compare(CharSequence cs1, CharSequence cs2) {
        for (int i = 0, len = Math.min(cs1.length(), cs2.length()); i < len; i++) {
            final char a = cs1.charAt(i);
            final char b = cs2.charAt(i);
            if (a != b) { return a - b; }
        }
        return Integer.compare(cs1.length(), cs2.length());
    }

    /**
     * return value is 16bits at most so -compare() is safe
     */
    static int compare(CharSequence cs1, char[] cs2) {
        for (int i = 0, len = Math.min(cs1.length(), cs2.length); i < len; i++) {
            final char a = cs1.charAt(i);
            final char b = cs2[i];
            if (a != b) { return a - b; }
        }
        return Integer.compare(cs1.length(), cs2.length);
    }

    static int compare(char[] cs1, char[] cs2) {
        for (int i = 0, len = Math.min(cs1.length, cs2.length); i < len; i++) {
            final char a = cs1[i];
            final char b = cs2[i];
            if (a != b) { return a - b; }
        }
        return Integer.compare(cs1.length, cs2.length);
    }

    /* don't use APIs with default charset
    static String readString(Path p) throws IOException {
        // Java11+ has Files.readString() which does one less copying than this
        return new String(Files.readAllBytes(p));
    }*/

    static String readString(Path p, Charset cs) throws IOException {
        // Java11+ has Files.readString() which does one less copying than this
        return new String(Files.readAllBytes(p), cs);
    }

    private static int jvmVersion = -1;
    static int jvmVersion() {
        if (jvmVersion == -1) {
            String version = System.getProperty("java.version");
            if (version.startsWith("1.")) {
                version = version.substring(2, 3);
            }
            else {
                final int dot = version.indexOf('.');
                if (dot != -1) version = version.substring(0, dot);
                final int dash = version.indexOf('-');
                if (dash != -1) version = version.substring(0, dash);
            }
            return jvmVersion = Integer.parseInt(version);
        }
        return jvmVersion;
    }
}

final class InstallDir {
    /** installation directory */
    static final Path installDir;
    static {
        Path path;
        try {
            final Path p = Paths.get(InstallDir.class.getProtectionDomain().getCodeSource().getLocation().toURI());
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
        installDir = path;
    }

    private InstallDir() {}
}


/// ## class JavaCompilerHelper
/// class JavaCompilerHelper - a helper class that wraps the Java system compiler in tools.jar,
/// used by MurmelJavaCompiler to compile the generated Java to an in-memory class and optionally a .jar file.
final class JavaCompilerHelper {
    private static final Map<String, String> ENV = Collections.singletonMap("create", "true");
    private final @NotNull MurmelClassLoader murmelClassLoader;

    JavaCompilerHelper(@NotNull Path outPath) {
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
        finally { cleanup();} 

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
            final List<String> options = Arrays.asList("-g", "-proc:none" /*, "-source", "1.8", "-target", "1.8"*/);
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

    @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return code;
    }
}

final class MurmelClassLoader extends ClassLoader {
    private final @NotNull Path outPath;

    MurmelClassLoader(@NotNull Path outPath) { //noinspection ConstantConditions 
                                               assert outPath != null; this.outPath = outPath; }

    @Override public Class<?> findClass(String name) throws ClassNotFoundException {
        try {
            final byte[] ba = getBytes(name);
            if (ba == null) return super.findClass(name);
            return defineClass(name, ba, 0, ba.length);
        }
        catch (IOException e) {
            throw new ClassNotFoundException(e.getMessage());
        }
    }

    @NotNull Path getOutPath() { return outPath; }

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
     * @return the input value or a new StringBuilder that has new lines normalized
     */
    static CharSequence anyToUnixEol(CharSequence inputValue){
        if (inputValue == null) return null;
        if (inputValue.length() == 0) return inputValue;

        int index = -1;
        for (int i = 0; i < inputValue.length(); i++) {
            if (inputValue.charAt(i) == '\r') {
                index = i;
                break;
            }
        }
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
            }
            else {
                stringBuilder.append(c);
            }
            index++;
        }

        return stringBuilder;
    }

    static StringBuilder unixToJavaEol(StringBuilder inputValue){
        if (inputValue == null) return null;
        if (inputValue.length() == 0) return inputValue;

        final String platformEol = System.lineSeparator();
        if ("\n".equals(platformEol)) return inputValue;

        int index = -1;
        for (int i = 0; i < inputValue.length(); i++) {
            final char c = inputValue.charAt(i);
            if (c == '\n') {
                index = i;
                break;
            }
        }
        if (index == -1) return inputValue;

        final int len = inputValue.length();
        final StringBuilder stringBuilder = new StringBuilder(len);

        // we get here if we just read a '\n'
        // build up the string builder so it contains all the prior characters
        stringBuilder.append(inputValue, 0, index);
        stringBuilder.append(platformEol);

        index++;
        while (index < len) {
            final char c = inputValue.charAt(index);
            if (c == '\n') stringBuilder.append(platformEol);
            else stringBuilder.append(c);
            index++;
        }

        return stringBuilder;
    }
}

/** A wrapping {@link LambdaJ.WriteConsumer} that translates '\n' to the given line separator {@code eol}. */
final class UnixToAnyEol implements LambdaJ.WriteConsumer {
    private final @NotNull LambdaJ.WriteConsumer wrapped;
    private final String eol;

    UnixToAnyEol(@NotNull LambdaJ.WriteConsumer wrapped, String eol) {
        //noinspection ConstantConditions
        assert wrapped != null;
        this.wrapped = wrapped;
        this.eol = eol;
    }

    @Override public void print(CharSequence s) {
        if (s == null
            || s.length() == 0
            || s.charAt(0) != '\n' && s.charAt(s.length() - 1) != '\n' && !hasNewline(s)) {
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

    private static boolean hasNewline(CharSequence s) {
        for (int i = 1; i < s.length(); i++) {
            if (s.charAt(i) == '\n') return true;
        }
        return false;
    }
}

/** Wrap a java.io.Writer, methods throw unchecked LambdaJError, also add {@code append()} methods for basic data types. */
final class WrappingWriter extends Writer {
    private final @NotNull Writer wrapped;

    WrappingWriter(@NotNull Writer w) { wrapped = w; }

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

    @Override public void write(String s) {
        try { wrapped.write(s); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override public void write(String s, int off, int len) {
        try { wrapped.write(s, off, len); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override public void write(char[] cbuf, int off, int len) {
        try { wrapped.write(cbuf, off, len); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override public void flush() {
        try { wrapped.flush(); }
        catch (IOException e) { throw new LambdaJ.LambdaJError(e.getMessage()); }
    }

    @Override public void close() {
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
        private final @NotNull String s;
        Text(double x, double y, @NotNull String s) { this.x = x; this.y = y; this.s = s; }
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
    private final @NotNull Frame f;
    private final @NotNull LineComponent component;

    TurtleFrame(String title, Number width, Number height, Number padding) {
        f = new Frame(title);
        f.addWindowListener(new WindowAdapter() {
            @Override public void windowClosing(WindowEvent e) {
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

    @Override public String toString() { return "#<frame \"" + f.getTitle() + "\">"; }

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

    double factBitmap(final int w, final int h) {
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

        @Override public void paint(Graphics g) {
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
                for (Text text: texts) g.drawString(text.s, trX(fac, xoff, text.x), h - trY(fac, yoff, text.y));
            }
        }
    }
}
