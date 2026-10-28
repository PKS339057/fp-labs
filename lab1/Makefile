TOOLS ?= ../tools/run
GHC ?= $(TOOLS) ghc
HLINT ?= $(TOOLS) hlint
FOURMOLU ?= $(TOOLS) fourmolu
FLAGS := -Wall -Werror -isrc
MODULES := $(wildcard src/Euler/*.hs)
SOURCES := $(MODULES) app/Main.hs test/Spec.hs

.PHONY: run test lint format format-check check repl

.build/lab1: app/Main.hs $(MODULES)
	mkdir -p .build/app
	$(GHC) $(FLAGS) -odir .build/app -hidir .build/app app/Main.hs -o $@

.build/tests: test/Spec.hs $(MODULES)
	mkdir -p .build/test
	$(GHC) $(FLAGS) -odir .build/test -hidir .build/test test/Spec.hs -o $@

run: .build/lab1
	.build/lab1

test: .build/lab1 .build/tests
	.build/tests
	cd python && PYTHONDONTWRITEBYTECODE=1 python3 -m unittest -v

lint:
	$(HLINT) src app test

format:
	$(FOURMOLU) --mode inplace $(SOURCES)

format-check:
	$(FOURMOLU) --mode check $(SOURCES)

check: format-check lint test

repl:
	$(GHC) --interactive -isrc $(MODULES)
