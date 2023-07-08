package io.github.jmurmel;

import java.io.IOException;
import java.nio.file.Path;

final class JavaCompilerHelper {
    JavaCompilerHelper(Path outPath) {
        throw new UnsupportedOperationException("compiling to Java class is not supported");
    }

    Class<?> javaToClass(String className, String javaSource, String jarFileName) throws Exception {
        return null;
    }

    void cleanup() throws IOException {
    }

    Class<?> javaToClass(String className, String javaSource) throws Exception {
        return null;
    }
}

final class TurtleFrame {
    TurtleFrame(String title, Number width, Number height, Number padding) {
        throw new UnsupportedOperationException("Turtle graphics are not supported");
    }

    TurtleFrame open() {
        return null;
    }

    TurtleFrame close() {
        return null;
    }

    TurtleFrame reset() {
        return null;
    }

    TurtleFrame clear() {
        return null;
    }

    TurtleFrame repaint() {
        return null;
    }

    TurtleFrame flush() {
        return null;
    }

    TurtleFrame pushPos() {
        return null;
    }

    TurtleFrame popPos() {
        return null;
    }

    TurtleFrame color(int newColor) {
        return null;
    }

    TurtleFrame bgColor(int newColor) {
        return null;
    }

    TurtleFrame moveTo(double newx, double newy) {
        return null;
    }

    TurtleFrame lineTo(double newx, double newy) {
        return null;
    }

    TurtleFrame moveRel(double dx, double dy) { return null; }
    TurtleFrame lineRel(double dx, double dy) { return null; }
    TurtleFrame text(String s) { return null; }
    TurtleFrame penUp() { return null; }
    TurtleFrame penDown() { return null; }
    TurtleFrame left(double angleDiff) { return null; }
    TurtleFrame right(double angleDiff) { return null; }

    TurtleFrame forward(double length) {
        return null;
    }


    TurtleFrame makeBitmap(int width, int height) {
        return null;
    }

    TurtleFrame discardBitmap() {
        return null;
    }

    TurtleFrame setRGB(int x, int y, int rgb) {
        return null;
    }
}
