default: build doc
all: build doc test

build: lib/SpiderMonkeyAST.js
build-tests: $(TESTSOUT)
externs: lib/SpiderMonkeyAST.externs.purs
doc: README.md

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name '*.purs' -type f | sort)
TESTS = $(shell find test -name '*.purs' -type f | sort)
TESTSOUT = $(TESTS:test/%.purs=built-tests/%.js)

ISTANBUL = node_modules/.bin/istanbul
MOCHA = node_modules/.bin/_mocha
MOCHA_OPTS = --inline-diffs --check-leaks -R dot

lib/SpiderMonkeyAST.js: src/SpiderMonkeyAST.purs
	@mkdir -p '$(@D)'
	psc --verbose-errors \
	  -m SpiderMonkeyAST \
	  --browser-namespace exports \
	  ${BOWER_DEPS} '$<' \
	  > lib/SpiderMonkeyAST.js

.PHONY: default all build externs doc clean test build-tests

lib/SpiderMonkeyAST.externs.purs: src/SpiderMonkeyAST.purs
	@mkdir -p '$(@D)'
	psc --verbose-errors \
	  -m SpiderMonkeyAST \
	  --codegen SpiderMonkeyAST \
	  -e lib/SpiderMonkeyAST.externs.purs \
	  ${BOWER_DEPS} '$<' \
	  > /dev/null

README.md: lib/SpiderMonkeyAST.externs.purs
	docgen lib/SpiderMonkeyAST.externs.purs >'$@'

built-tests/%.js: test/%.purs test-helper.purs
	@mkdir -p '$(@D)'
	psc --verbose-errors -m Tests \
	  $(BOWER_DEPS) test-helper.purs '$<' \
	  >'$@'

test: $(TESTSOUT) lib/SpiderMonkeyAST.js
	$(ISTANBUL) cover --root lib $(MOCHA) -- $(MOCHA_OPTS) -- built-tests
clean:
	rm -rf lib built-tests coverage
