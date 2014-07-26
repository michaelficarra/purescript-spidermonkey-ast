default: build doc
all: build doc test

build: lib/SpiderMonkeyAST.js
doc: README.md

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name "*.purs" | sort)
ISTANBUL = node_modules/.bin/istanbul
MOCHA = node_modules/.bin/_mocha
MOCHA_OPTS = --inline-diffs --check-leaks -u tdd -R dot

lib:
	mkdir lib

lib/SpiderMonkeyAST.js: src/SpiderMonkeyAST.purs lib
	psc --verbose-errors \
	  -m SpiderMonkeyAST \
	  --browser-namespace exports \
	  ${BOWER_DEPS} src/SpiderMonkeyAST.purs \
	  > lib/SpiderMonkeyAST.js

.PHONY: default all build doc clean test

lib/SpiderMonkeyAST.externs.purs: src/SpiderMonkeyAST.purs lib
	psc --verbose-errors \
	  -m SpiderMonkeyAST \
	  --codegen SpiderMonkeyAST \
	  -e lib/SpiderMonkeyAST.externs.purs \
	  ${BOWER_DEPS} src/SpiderMonkeyAST.purs \
	  > /dev/null

README.md: lib/SpiderMonkeyAST.externs.purs
	docgen lib/SpiderMonkeyAST.externs.purs > README.md

test: build
	$(ISTANBUL) cover $(MOCHA) -- $(MOCHA_OPTS) -- test/*.js
clean:
	rm -rf lib
