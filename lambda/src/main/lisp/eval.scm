;;; scheme eval in scheme programmiert
;;; siehe http://people.cs.aau.dk/~normark/prog3-03/html/notes/languages_themes-list-in-lisp.html



An overview of Scheme constructs

When we are interested in implementing Scheme in Scheme it is important to have a good classification of constructs in Scheme.
As a basic distinction, some forms are denoted as syntax, and others as procedures. (In this particular context,
'procedures' also covers 'functions'). As another distinction, some abstractions are fundamental - they form the core language;
Others are library abstractions in the sense that they can be implemented by use of the fundamental abstractions.
You can consult section 1.3 of the Scheme report [Abelson98] to learn more about these distinctions.

    
What is the basic classification of constructs in Scheme?
    

    Syntax
        Fundamental syntactical constructs such as lambda , define , and if

    Primitive functions and procedures
        Fundamental functions and procedures, which cannot in a reasonable way be implemented in the language

    Library Syntax
        Syntactical extensions which can be implemented by macros

    Library functions and procedures
        Functions and procedures which can be implemented on the ground of more primitive features



Parenthesized prefix notation is used as a common notation for all kinds of constructs
This provides for an uniform notation across the different kinds of constructs in the language.



(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((variable? exp) (lookup-variable-value exp env))
        ((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((lambda? exp) (make-procedure exp env))
        ((conditional? exp) (eval-cond (clauses exp) env))
        ((application? exp)  (apply (eval (operator exp) env)
                                          (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure
            (apply-primitive-procedure procedure arguments)))
        ((compound-procedure? procedure)
            (eval-sequence (procedure-body procedure)
                                  (extend-environment
                                     (parameters procedure)
                                     arguments
                                     (procedure-environment procedure))))
        (else  (error "Unknown procedure type -- APPLY" procedure))))

The two central functions of the language implementation - eval and apply - are made available in the language itself.