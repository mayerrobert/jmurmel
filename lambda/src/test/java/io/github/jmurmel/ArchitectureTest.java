package io.github.jmurmel;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.domain.properties.HasName;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import org.testng.annotations.Test;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.*;

/**
 * Murmel's "architecture" is kinda messy, try checking/ enforcing some rules.
 * See https://www.archunit.org/userguide/html/000_Index.html
 */
public class ArchitectureTest {
    private static final JavaClasses murmelClasses = new ClassFileImporter()
                                                     .withImportOption(new ImportOption.DoNotIncludeTests())
                                                     .importPackages("io.github.jmurmel");

    @Test
    public void checkMurmel() {

        // LambdaJ.Cli is only used by LambdaJ (main() to be exact)
        checkUsers(LambdaJ.Cli.class, LambdaJ.class);

        // MurmelJavaCompiler uses JavaCompilerHelper uses JavaSourceFromString
        checkUsers(JavaCompilerHelper.class, LambdaJ.MurmelJavaCompiler.class);
        checkUsers(JavaSourceFromString.class, JavaCompilerHelper.class);
        checkUsers(MurmelClassLoader.class, JavaCompilerHelper.class);

        // Utility classes whose methods are leafs in the call hierarchy, i.e. don't use any other Murmel code
        noClasses().that().belongToAnyOf(EolUtil.class)
                   .should().accessClassesThat(HasName.Predicates.nameStartingWith("io")).check(murmelClasses);
    }

    private static void checkUsers(Class<?> clazz, Class<?>... users) {
        theClass(clazz).should().onlyHaveDependentClassesThat().belongToAnyOf(users).check(murmelClasses);
    }
}