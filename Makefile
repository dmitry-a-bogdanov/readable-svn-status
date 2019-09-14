

.PHONY: all clear

all: svn-status tests

svn-status: svn-status.hs parser.hs
	ghc -o svn-status svn-status.hs parser.hs

tests: tests.hs parser.hs
	ghc -o tests tests.hs
	./tests

clear:
	rm -f svn-status
	rm -f svn-status.hi
	rm -f svn-status.o
