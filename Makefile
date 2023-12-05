SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.SECONDARY:

.PHONY: build
build: build/mi-ml

.PHONY: clean
clean:
	rm -rf build/*

# NOTE(vipa, 2023-10-29): Build the compiler

build/mi-ml: src/compiler/syntax.mc $(shell find src/compiler -name "*.mc")
	mkdir -p build
	mi compile src/compiler/main.mc --output $@

src/compiler/syntax.mc: src/compiler/syntax.syn
	mi syn src/compiler/syntax.syn $@
