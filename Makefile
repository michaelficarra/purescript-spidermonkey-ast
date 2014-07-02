default: build
all: build test

build: lib/SpiderMonkeyASTNode.js

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name "*.purs" | sort)

lib/SpiderMonkeyASTNode.js: src/SpiderMonkeyASTNode.purs
	psc-make --verbose-errors -o lib ${BOWER_DEPS} src/SpiderMonkeyASTNode.purs

.PHONY: clean

test: build
	node_modules/.bin/mocha
clean:
	rm -rf lib
