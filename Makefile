default: build
all: build doc test

build: lib/SpiderMonkeyAST/index.js

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name "*.purs" | sort)

lib/SpiderMonkeyAST/externs.purs lib/SpiderMonkeyAST/index.js README.md: src/SpiderMonkeyAST.purs
	psc-make --verbose-errors -o lib ${BOWER_DEPS} src/SpiderMonkeyAST.purs
	docgen lib/SpiderMonkeyAST/externs.purs > README.md

.PHONY: clean

test: build
	node_modules/.bin/mocha
clean:
	rm -rf lib
