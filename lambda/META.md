# Meta-circular or meta-spheric?
Or is it meta-circular recursively applied which then generates unlimited dimensions of meta?

Around the time during development of JMurmel when I had an almost complete interpreter and started coding the compiler,
actually when I coded the compiler part that handles "quote",
things started getting weird.

Almost as if Lisp with their meta-circular-<insert noun here> started taking over the design of JMurmel -
to implement new functionality I no longer coded new functionality but rather connected stuff that was already used
in the previous levels of meta, maybe with some adaptions for the current level of meta.

Either that or the creators of Lisp stole my ideas long before I even had them...

## Different levels of meta
Everything seems to somehow end up connected to other levels:

* LambdaJ (the Java class) has nested classes S-expression reader, S-expression writer, interpreter and compiler

* LambdaJ (the Java object that is the commandline application) has two reader objects, a writer object, an interpreter object, compiler object

* The compiler has an interpreter object

* Compiled programs have an interpreter object

* The interpreter, compiler as well as compiled programs have a reader, writer


* LambdaJ (the Java object that is the commandline application) has a reader that turns data (S-Expressions) into forms (code)
which are then fed to the interpreter or compiler, and a reader that reads S-expressions which are then fed to the interpreted program.

* The interpreter has an "eval" method (or really function) that executes forms  
"eval" has a part that executes the form "quote" that reads code and turns it into in-memory data

* The compiler has a "compile" method (that is structurally very similar to eval) that turns forms into code
  that does the same as executing said forms  
* "compile" has a part that compiles the form "quote" into code that generates in-memory forms (data),
  kind of reversing what the reader did, whose output got fed to compile.
  And then said generated code does the same things again at runtime as the compiler's reader did at compile time WTF.

* the interpreter has an environment
* eval has an environment (that consists of the interpreter's environment with some runtime stuff added)
* compile has an environment
* compiled programs are an environment
* interpreted or compiled functions have a lexical environment
* invoked functions (interpreted or compiled) have additional runtime environment entries

Each level is somehow the same as other levels yet somehow different.

Also everything seems to have corresponding items in other areas of computing:

* The environment in invocations of interpreted functions is a list which seems somewhat similar to a CPU's stack and/ or parameters passed on said stack.
* The location of argument values in interpreted functions is found at runtime, of compiled functions at compile time.
* The global environment in interpreted programs is an associative structure of name/value tupels,
  in case of compiled programs the global environment is - variables.

It seems that Lisp has stolen ideas such as a call stack long before anyone had them.

## Future
I wonder what will happen if I add macros...
or compile time evaluation.