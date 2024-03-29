# Makefile for PLT lab 3 in Haskell

## Variables
###########################################################################

# Files of solution.  Add/remove files as needed.  'lab3.hs'  has to be first!
files=lab3.hs jasmin.jar Runtime.java Makefile CMM.cf \
  Annotated.hs TypeChecker.hs Compiler.hs

# Windows and UNIX compability
ifeq ($(OS),Windows_NT)
    dotslash := 
else
    dotslash := ./
endif
###########################################################################

# List of goals not corresponding to file names.
.PHONY : default build sdist clean distclean

# lab3 must be first goal!
default : lab3

build : lab3 Runtime.class

test/progs-test-lab3 :
	$(MAKE) -C test

test : build test/progs-test-lab3
	cd test && $(dotslash)progs-test-lab3 ../.

# The extra dependency CMM/Test ensures that the parser is built from CMM.cf
lab3 : $(files) CMM/Test
	ghc --make lab3.hs -o lab3 -package mtl

# Rules to build the parser:

CMM/Test.hs CMM/Lex.x CMM/Layout.hs CMM/Par.y : CMM.cf
	bnfc -d $<

CMM/Par.hs: CMM/Par.y
	happy -gcai $<

CMM/Lex.hs: CMM/Lex.x
	alex -g $<

CMM/Test: CMM/Test.hs CMM/Par.hs CMM/Lex.hs
	ghc --make $< -o $@

%.class : %.java
	javac $<

# Rules for shipping the solution:

sdist: lab3.tar.gz

tmpdir := $(shell mktemp -d)
lab3.tar.gz: $(files)
	mkdir $(tmpdir)/lab3
	cp $^ $(tmpdir)/lab3/
	tar -C $(tmpdir) -czhf $@ lab3

# Rules for cleaning generated files:

clean:
	-rm -f CMM/*.log CMM/*.aux CMM/*.hi CMM/*.o CMM/*.dvi *.hi *.o

distclean: clean
	-rm -f CMM/Doc.* CMM/Lex.* CMM/Par.* CMM/Layout.* CMM/Skel.* CMM/Print.* CMM/Test.* CMM/Abs.* CMM/Test CMM/ErrM.* CMM/SharedString.* CMM/ComposOp.* CMM/CMM.dtd CMM/XML.* CMM/*.bak
	-rmdir -p CMM/
	-rm -f lab3.tar.gz lab3 Runtime.class

# EOF
