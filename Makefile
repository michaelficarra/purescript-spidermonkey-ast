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
MOCHA_OPTS = --inline-diffs --check-leaks --reporter dot
PSC = $(shell command -v psc || { echo "PureScript compiler (psc) not found." exit 1; })
PSCDOCS = $(shell command -v psc-docs || command -v docgen)

lib/SpiderMonkeyAST.js: src/SpiderMonkeyAST.purs
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors \
	  --module SpiderMonkeyAST \
	  --browser-namespace exports \
	  $(BOWER_DEPS) '$<' \
	  > lib/SpiderMonkeyAST.js

.PHONY: default all build externs doc clean test build-tests

lib/SpiderMonkeyAST.externs.purs: src/SpiderMonkeyAST.purs
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors \
	  --module SpiderMonkeyAST \
	  --codegen SpiderMonkeyAST \
	  --externs lib/SpiderMonkeyAST.externs.purs \
	  $(BOWER_DEPS) '$<' \
	  > /dev/null

README.md: lib/SpiderMonkeyAST.externs.purs
	$(PSCDOCS) lib/SpiderMonkeyAST.externs.purs >'$@'

built-tests/%.js: test/%.purs test-helper.purs
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors --module Tests \
	  $(BOWER_DEPS) test-helper.purs '$<' \
	  >'$@'

test: $(TESTSOUT) lib/SpiderMonkeyAST.js
	$(ISTANBUL) cover --root lib $(MOCHA) -- $(MOCHA_OPTS) -- built-tests
clean:
	rm -rf lib built-tests coverage
