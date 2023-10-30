package io.github.jmurmel;

// eigentlich nur fuer show-bytecode: man sieht, dass der "finally" block bei jedem "return" inline-expandiert wird,
// weil so funktioniert javac seit 1.6.
// Heisst: grosse "finally" bloecke sollte man eher vermeiden, zumindest wenns mehr als ein return gibt.
public class FinallyManualTest {
    private int x;

    public int test() {
        try {
            switch (x) {
            case 1: return 1;
            case 2: return 2;
            case 3: return 3;
            case 4: return 4;
            case 5: return 5;
            default: return -1;
            }
        }
        finally {
            doFinally();
        }
    }
    
    private void doFinally() {
        System.out.println("finally");
    }
}
