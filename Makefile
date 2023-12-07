.PHONY: all test clean
.PRECIOUS: build/%-build

INPUTS = $(shell find src -name 'day*.hs')
SOLVERS = ${INPUTS:src/%.hs=build/%}

all: ${SOLVERS}

clean:
	rm -rf build

build:
	mkdir build

build/%-build: | build
	mkdir $@

build/% build/%-build/Main.hi build/%-build/Main.o &: src/%.hs | build/%-build
	ghc -hidir build/$*-build -odir build/$*-build -O2 $^ -o $@
