.PHONY: all test clean
.PRECIOUS: build/%-build build/%.output build/%.verdict

INPUTS = $(shell find src -name 'day*.hs' | sort)
SOLVERS = ${INPUTS:src/%.hs=build/%}
VERDICTS = ${INPUTS:src/%.hs=build/%.verdict}

all: ${SOLVERS}

test: ${VERDICTS}
	cat ${VERDICTS}

clean:
	rm -rf build

build:
	mkdir build

build/%-build: | build
	mkdir $@

build/% build/%-build/Main.hi build/%-build/Main.o &: src/%.hs | build/%-build
	ghc -hidir build/$*-build -odir build/$*-build -O2 $^ -o $@

inputs/day%.txt:
	tools/fetch.sh $*

build/%.output: build/% inputs/%.txt
	build/$* <inputs/$*.txt >build/$*.output.tmp && mv build/$*.output{.tmp,}

build/%.verdict: inputs/%.output build/%.output
	tools/verdict.sh $* $^ > $@
