# Programming Language Technology

### Lab 1: Parser for a fragment of C++ ###
An LR parser for a fragment of the C++ programming language. A specification of the language is available in the [lab instructions](https://www.cse.chalmers.se/edu/year/2020/course/DAT151/laborations/lab1/index.html). The language's [grammar](https://github.com/beataburreau/Programming-language-technology/blob/main/lab1/src/Lab.cf) is described in Labelled BNF (LBNF), which allows to use the BNF Converter for generating a compiler front-end in Haskell, among other languages. This can be done by installing [BNFC](https://bnfc.digitalgrammars.com) and running first `bnfc -d -m Lab1.cf`, then `make`, in the source directory. For generation in languages other than Haskell, consult the [BNFC documentation](https://bnfc.readthedocs.io/en/latest/user_guide.html#). 

### Lab 2: Type checker and interpreter for C-- ###
A type checker and an interpreter for the fragment "C--" of the C++ programming language, as specified in the [lab instructions](https://www.cse.chalmers.se/edu/year/2020/course/DAT151/laborations/lab2/index.html). 

### Lab 3: Compiler for C-- ###
A compiler from C-- to JVM, the Java Virtual Machine. The compiler produces Java class files by generating [Jasmin](https://jasmin.sourceforge.net) assembly code into text files, which are then processed further by Jasmin to create class files. See [lab instructions](https://www.cse.chalmers.se/edu/year/2020/course/DAT151/laborations/lab3/index.html) for more details.

### Lab 4: Interpreter for a tiny subset of Haskell ###
An interpreter for a small, untyped functional programming language; a tiny subset of Haskell. Supplied with a program written in the language, the interpreter evaluates and prints the result of the program's main function. See [lab instructions](https://www.cse.chalmers.se/edu/year/2020/course/DAT151/laborations/lab4/index.html) for more details.
