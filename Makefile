

.PHONY: all build test

all: build test

build:
	cabal build

test: build
	cabal test
