default: build
all: build doc test

build: lib/SpiderMonkeyAST/index.js

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name "*.purs" | sort)

lib/SpiderMonkeyAST/index.js: src/SpiderMonkeyAST.purs
	psc-make --verbose-errors -o lib ${BOWER_DEPS} src/SpiderMonkeyAST.purs
lib/SpiderMonkeyAST/externs.purs: lib/SpiderMonkeyAST/index.js

doc: README.md
README.md: lib/SpiderMonkeyAST/externs.purs
	echo 'purescript-spidermonkey-ast' > README.md
	echo '---------------------------' >> README.md
	echo >> README.md
	docgen lib/SpiderMonkeyAST/externs.purs >> README.md

.PHONY: clean

test: build
	node_modules/.bin/mocha
clean:
	rm -rf lib
