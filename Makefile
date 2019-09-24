

.PHONY: all build test

all: build test

build:
	cabal build --verbose=1

test: build
	cabal test
