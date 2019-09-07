

.PHONY: all clear

all: svn-status

svn-status: svn-status.hs parser.hs
	ghc -o svn-status svn-status.hs parser.hs

clear:
	rm -f svn-status
	rm -f svn-status.hi
	rm -f svn-status.o
