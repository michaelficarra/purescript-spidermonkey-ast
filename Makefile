default: build doc
all: build doc test

build: lib/SpiderMonkeyAST.js
externs:: lib/SpiderMonkeyAST.externs.purs
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

.PHONY: default all build externs doc clean test build-tests

lib/SpiderMonkeyAST.externs.purs: src/SpiderMonkeyAST.purs lib
	psc --verbose-errors \
	  -m SpiderMonkeyAST \
	  --codegen SpiderMonkeyAST \
	  -e lib/SpiderMonkeyAST.externs.purs \
	  ${BOWER_DEPS} src/SpiderMonkeyAST.purs \
	  > /dev/null

README.md: lib/SpiderMonkeyAST.externs.purs
	docgen lib/SpiderMonkeyAST.externs.purs > README.md

build-tests: test/high-level.js test/unit.js
test/high-level.js: test-helper.purs src/SpiderMonkeyAST.purs test/high-level.purs
	psc --verbose-errors \
	  -m HighLevelTests \
	  $(BOWER_DEPS) test-helper.purs test/high-level.purs \
	  > test/high-level.js
test/unit.js: test-helper.purs src/SpiderMonkeyAST.purs test/unit.purs
	psc --verbose-errors \
	  -m HighLevelTests \
	  $(BOWER_DEPS) test-helper.purs test/unit.purs \
	  > test/unit.js

test: build build-tests
	$(ISTANBUL) cover $(MOCHA) -- $(MOCHA_OPTS) -- test/*.js
clean:
	rm -rf lib test/*.js
